unit DAssembler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCPUtypes, DTokenizer;

type
  TAssemblerError = record
    message: string;
    line: TTokenizedLine;
    token: TToken;
  end;
  TAssemblerErrorList = array of TAssemblerError;

  TAssembler = class
  private
    tLines: TTokenizedLines;
    currentLine: TTokenizedLine;
    linePosition: Integer;
    lineLength: integer;

    lineError: boolean;
    FOpCodes: array of Word;
    CurrentORG: TMemoryAddress;

    FSymbols: array of TNameAddr;
    Fixups: array of TNameAddr;
    DataSymbol: Boolean;

    function peek(count:integer = 0): TToken;
    function hasNext(): boolean;
    function has(count: integer): boolean;
    function next(): TToken;
    procedure skipLine();

    procedure parseLine();

    function isOpenBracket(): Boolean;
    function isCloseBracket(): Boolean;
    function isDirective(): boolean;
    procedure parseDirective();

    procedure AddSymbol(AName: string;Addr: TMemoryAddress; CodePos: Integer);
    procedure AddFixup(AName: string; CodePos: Integer; Addr: TMemoryAddress);
    procedure AddWord(W: Word);
    procedure SetORG(L: TMemoryAddress);
    function GetOpCodes(AIndex: TMemoryAddress): Word; inline;
    function GetSize: TMemoryAddress; inline;
    function GetSymbolCount: Integer; inline;
    function GetSymbols(AIndex: Integer): TNameAddr; inline;
    procedure SetOpCodes(AIndex: TMemoryAddress; const AValue: Word); inline;
    procedure AssembleLabel;
    procedure AssembleData;
    procedure AssembleReserve;
    procedure AssembleInstruction;
    procedure AssembleORG;

    procedure addError(message: string; token: TToken);
    procedure addError(message: string);
  public
    errors: TAssemblerErrorList;
    procedure Assemble(lines: TTokenizedLines);
    property OpCodes[AIndex: TMemoryAddress]: Word read GetOpCodes write SetOpCodes; default;
    property Size: TMemoryAddress read GetSize;

    property Symbols[AIndex: Integer]: TNameAddr read GetSymbols;
    property SymbolCount: Integer read GetSymbolCount;
end;

implementation

procedure TAssembler.AddSymbol(AName: string; Addr: TMemoryAddress; CodePos: Integer);
var
  n: Integer;
begin
  n := Length(FSymbols);
  SetLength(FSymbols, n + 1);
  FSymbols[n].Name:=AName;
  FSymbols[n].Address:=Addr;
  FSymbols[n].ForData:=False;
  FSymbols[n].CodePos:=CodePos;
end;


procedure TAssembler.AddFixup(AName: string; CodePos: Integer; Addr: TMemoryAddress);
begin
  SetLength(Fixups, Length(Fixups) + 1);
  Fixups[High(Fixups)].Name:=AName;
  Fixups[High(Fixups)].Address:=Addr;
  Fixups[High(Fixups)].CodePos:=CodePos;
end;

procedure TAssembler.SetORG(L: TMemoryAddress);
begin
     CurrentORG := L;
end;

procedure TAssembler.AddWord(W: Word);
begin
  while Length(FOpCodes) < CurrentORG+1 do begin
    SetLength(FOpCodes, Length(FOpCodes) + 1);
  end;

  OpCodes[CurrentORG]:=W;
  CurrentORG:=CurrentORG+1;
end;

function TAssembler.GetOpCodes(AIndex: TMemoryAddress): Word;
begin
  Result:=FOpCodes[AIndex];
end;

function TAssembler.GetSize: TMemoryAddress;
begin
  Result:=Length(FOpCodes);
end;

function TAssembler.GetSymbolCount: Integer;
begin
  Result:=Length(FSymbols);
end;

function TAssembler.GetSymbols(AIndex: Integer): TNameAddr;
begin
  Result:=FSymbols[AIndex];
end;

procedure TAssembler.SetOpCodes(AIndex: TMemoryAddress; const AValue: Word);
begin
  FOpCodes[AIndex]:=AValue;
end;

procedure TAssembler.AssembleLabel;
var
  token: TToken;
  name: string;
begin
  token := next();
  if token.length = 1 then begin
    addError('Invalid label format', token);
    exit;
  end;
  name := rightstr(token.strVal, token.length-1);
  AddSymbol(name, CurrentORG, currentLine.sourcePos+token.linePos);
  DataSymbol:=True;
end;

procedure TAssembler.AssembleData;
var
  token: TToken;
  ch: char;
begin
   if not hasNext() then begin
      addError('Invalid format for DAT diretive.');
      exit;
   end;

  if DataSymbol and (Length(FSymbols) > 0) then begin
    FSymbols[High(FSymbols)].ForData:=True;
    DataSymbol:=False;
  end;

  while hasNext() do begin
      token := next();
      if not (token.tokenType in [ttNumber, ttSymbol, ttString]) then begin
         addError('Invalid token in DAT: '+token.strVal);
         break;
      end;

      case token.tokenType of
        ttNumber: addWord(token.intVal and $FFFF);
        ttSymbol: if isCPURegisterName(token.strVal) then begin
                     addError('Cannot use a register in DATA', token);
                     exit;
                  end else begin
                     AddFixup(token.strVal, currentLine.sourcePos+token.linePos, CurrentORG);
                     AddWord(0);
                  end;
        ttString: for ch in token.strVal do begin
                     AddWord(Ord(ch))
                  end;
      end;

      if hasNext() then begin
         token := next();
         if (token.tokenType <> ttDelimiter) or (token.strVal <> ',') then begin
            addError('DAT parameters must be separated by commas', token);
            break;
         end;
      end;
  end;
end;

procedure TAssembler.AssembleReserve;
var
  I: Integer;
  token: TToken;
begin
  if not hasNext() then begin
    addError('Unexpected end of code in RESERVE');
    Exit;
  end;
  if has(2) then begin
    addError('Too many arguments for RESERVE', peek(1));
    Exit;
  end;
  token := next();
  if token.tokenType <> ttNumber then begin
    addError('Number expected in RESERVE', token);
    exit;
  end;

  if DataSymbol and (Length(FSymbols) > 0) then begin
    FSymbols[High(FSymbols)].ForData:=True;
    DataSymbol:=False;
  end;

  for I:=1 to token.intVal do AddWord(0);
end;

procedure TAssembler.AssembleORG;
var
  token: TToken;
begin
  if not hasNext() then begin
    addError('Unexpected end of code in ORG');
    Exit;
  end;
  if has(2) then begin
    addError('Too many arguments for ORG', peek(1));
    Exit;
  end;
  token := next();
  if token.tokenType <> ttNumber then begin
    addError('Number expected in ORG', token);
    exit;
  end;
  CurrentORG := token.intVal;
end;

procedure TAssembler.AssembleInstruction;
var
  InstrToken: TToken;
  Instr: TCPUInstruction;

  procedure DoAssembleInstruction(AInstr: TCPUInstruction);
  var
    I: Integer;
    IAddr: TMemoryAddress;
    Args: array [0..1] of Integer;

    function AssembleArgument(): Integer;
    var
      token1: TToken;
      token2: TToken;
      Reg: TCPURegister;
      hasOffset: boolean;
    begin
      if not hasNext() then begin
        addError('Unexpected end of code while parsing parameter for ' + CPUInstructionNames[AInstr]);
        Exit(0);
      end;
      hasOffset:=false;
      if isOpenBracket() then begin //[reg+offset], [offset+reg], [next word] and [register]
        next(); //skip over open bracket
        if not has(2) then begin
          addError('Unexpected end of code inside memory access parameter for ' + CPUInstructionNames[AInstr]);
          Exit(0);
        end;
        token1 := next(); //first register or offset/symbol
        if not (token1.tokenType in [ttNumber, ttSymbol]) then begin
           addError('Unexpected token type inside memory access parameter for ' + CPUInstructionNames[AInstr]);
           Exit(0);
        end;

        if (peek().tokenType = ttDelimiter) and (peek().strVal = '+') then begin
          hasOffset:=true;
          next(); //skip over the +
          if not has(2) then begin
             addError('Unexpected end of code inside memory access parameter for ' + CPUInstructionNames[AInstr]);
             Exit(0);
          end;
          token2 := next();  //second register or offset/symbol
          if not (token2.tokenType in [ttNumber, ttSymbol]) then begin
             addError('Unexpected token type inside memory access parameter for ' + CPUInstructionNames[AInstr]);
             Exit(0);
          end;
        end;

        if not isCloseBracket() then begin
            addError('Closing bracket ] or ) expected after memory address in ' + CPUInstructionNames[AInstr]);
            exit(0);
        end;
        next(); //skip over close bracket

        if hasOffset then begin //[offset+reg] or [reg+offset]
           if (token1.tokenType = ttSymbol) and (isCPURegisterName(token1.strVal)) then begin
              if (token2.tokenType = ttSymbol) and (isCPURegisterName(token1.strVal)) then begin
                 addError('Allowed format: [reg + offset] or [offset + reg] ' + CPUInstructionNames[AInstr]);
                 exit(0);
              end;
              reg := getCPURegisterID(token1.strVal);
              token1 := token2;
           end else begin //token1 is not a register, token2 must be
               if (token2.tokenType <> ttSymbol) or (not isCPURegisterName(token2.strVal)) then begin
                  addError('Allowed format: [reg + offset] or [offset + reg] ' + CPUInstructionNames[AInstr]);
                  exit(0);
               end;
              reg := getCPURegisterID(token2.strVal);
           end;
           if token1.tokenType = ttSymbol then begin
              AddFixup(token1.strVal, currentLine.sourcePos+token1.linePos, CurrentORG);
              AddWord(0);
           end else begin;
              AddWord(token1.intVal);
           end;
           Exit($10 + Ord(Reg));
        end;

        if token1.tokenType = ttNumber then begin //[next word]
          AddWord(token1.intVal);
          exit($1E);
        end;
        if isCPURegisterName(token1.strVal) then begin //[register]
          exit($08 + ord(getCPURegisterID(token1.strVal)));
        end;
        //[symbol]
        AddFixup(token1.strVal, currentLine.sourcePos+token1.linePos, CurrentORG);
        AddWord(0);
        exit($1E);
      end //END: if isOpenBracket()
      else if peek().tokenType = ttNumber then begin
        token1 := next();
        if token1.intVal < $20 then begin //literal value
          exit(token1.intVal + $20)
        end else begin //literal value in next word
          AddWord(token1.intVal);
          exit($1F);
        end;
      end else begin
        token1 := next();
        if token1.strVal = 'POP' then Exit($18);
        if token1.strVal = 'PEEK' then Exit($19);
        if token1.strVal = 'PUSH' then Exit($1A);
        if token1.strVal = 'SP' then Exit($1B);
        if token1.strVal = 'PC' then Exit($1C);
        if token1.strVal = 'O' then Exit($1D);

        if isCPURegisterName(token1.strVal) then begin
           Exit(Ord(getCPURegisterID(token1.strVal)));
        end;

        //a symbol
        AddFixup(token1.strVal, currentLine.sourcePos+token1.linePos, CurrentORG);
        AddWord(0);
        Exit($1F);
      end;
    end;

  begin
    IAddr:=CurrentORG;
    AddWord(0); //instruction will go here
    for I:=0 to CPUInstructionArguments[AInstr] - 1 do begin
      if lineError then Break;
      if not hasNext() then begin
        addError('Unexpected end of code while parsing the arguments for instruction ' + CPUInstructionNames[AInstr]);
        Exit;
      end;
      if (I > 0) then begin
        if hasNext() and (peek().tokenType=ttDelimiter) and (peek().strVal=',') then begin
          next() //skip over comma
        end else begin
          addError('Comma expected after instruction parameter for ' + CPUInstructionNames[AInstr]);
          exit;
        end;
      end;
      Args[I]:=AssembleArgument();
    end;
    if Ord(AInstr) < Ord(ciReserved) then begin
      OpCodes[IAddr] := Ord(AInstr) or (Args[0] shl 4) or (Args[1] shl 10);
    end else begin
      OpCodes[IAddr]:=((Ord(AInstr) - Ord(ciReserved)) shl 4) or (Args[0] shl 10);
    end;
  end;

begin
  InstrToken := next();
  if InstrToken.tokenType <> ttSymbol then begin
    addError('Syntax error - an instruction was expected here');
    Exit;
  end;
  for Instr:=Low(TCPUInstruction) to High(TCPUInstruction) do begin
    if CPUInstructionNames[Instr]=InstrToken.strVal then begin
      DataSymbol:=False;
      DoAssembleInstruction(Instr);
      Exit;
    end;
  end;
  addError('Unknown instruction: ' + InstrToken.strVal);
end;

procedure TAssembler.parseLine();
var
  head: TToken;
begin
     head := peek();
     if head.tokenType = ttSymbol then begin
        if head.strVal[1] = ':' then begin
           AssembleLabel();
           exit;
        end;
        if isDirective() then begin
           parseDirective();
           exit;
        end;
        AssembleInstruction();
        if hasNext() then begin
           addError('Junk after instruction');
        end;
     end else begin
         addError('Invalid token: ' + head.strVal);
     end;
end;

procedure TAssembler.Assemble(lines: TTokenizedLines);
var
  line: TTokenizedLine;
  i, j: integer;
  found: boolean;
begin
  CurrentORG:=0;
  tLines := lines;
  for line in lines do begin
    lineError:=false;
    linePosition:=0;
    currentLine := line;
    lineLength:= length(line.tokens);
    while hasNext() do
          parseLine();

  end;

  if Length(errors)=0 then begin
     for i:=0 to High(fixups) do begin
       found := false;
       for j:=0 to high(FSymbols) do begin
           if FSymbols[j].Name = Fixups[i].Name then begin
              found:=true;
               OpCodes[Fixups[i].Address] := FSymbols[j].Address;
               break;
           end;
       end;
       if not found then begin
          addError('Undefined symbol: '+Fixups[i].Name);
       end;
     end;
  end;
end;

function TAssembler.isDirective(): boolean;
var
  s: string;
begin
  s := peek().strVal;
  exit(    (s ='DW') or (s = 'DAT') or (s = 'DATA')
        or (s = 'RESW') or (s = 'RESERVE')
        or (s = 'ORG')
        );
end;

procedure TAssembler.parseDirective();
var
  str: string;
begin
     str := next().strVal;
     if (str = 'DW') or (str = 'DAT') or (str = 'DATA') then begin
       AssembleData();
       exit;
     end;
     if (str = 'RESW') or (str = 'RESERVE') then begin
       AssembleReserve();
       exit;
     end;
     if (str = 'ORG') then begin
       AssembleORG;
       exit();
     end;
     addError('Implementation error in parseDirective');
end;

function TAssembler.isOpenBracket(): Boolean;
var
  token: TToken;
begin
  token := peek();
  exit( (token.tokenType = ttDelimiter)
        and (token.strVal[1] in ['(', '[']));
end;

function TAssembler.isCloseBracket(): Boolean;
var
  token: TToken;
begin
  token := peek();
  exit( (token.tokenType = ttDelimiter)
        and (token.strVal[1] in [')', ']']));
end;

//peek count tokens ahead, don't advance position
function TAssembler.peek(count:integer = 0): TToken;
begin
     exit(currentLine.tokens[linePosition+count]);
end;

//return True if there is at least 1 token left
function TAssembler.hasNext(): boolean;
begin
     exit(has(1));
end;
//return True if there are at least count tokens left
function TAssembler.has(count: integer): boolean;
begin
     exit(linePosition+count-1<lineLength);
end;

procedure TAssembler.skipLine();
begin
     linePosition:=lineLength+1;
end;

function TAssembler.next(): TToken;
begin
     inc(linePosition);
     exit(peek(-1));
end;

procedure TAssembler.addError(message: string; token: TToken);
var
  n:integer;
begin
     n := length(errors);
     SetLength(errors, n + 1);
     errors[n].message := message;
     errors[n].line := currentLine;
     errors[n].token := token;
     lineError:=true;
     skipLine();
end;

procedure TAssembler.addError(message: string);
var
  n:integer;
begin
     n := length(errors);
     SetLength(errors, n + 1);
     errors[n].message := message;
     errors[n].line := currentLine;

     errors[n].token.length:=0;
     errors[n].token.intVal:=0;
     errors[n].token.linePos:=0;
     errors[n].token.strVal:='NO TOKEN';
     errors[n].token.tokenType:=ttString;
     lineError:=true;
     skipLine();
end;

end.

