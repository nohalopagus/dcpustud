unit DCPU16;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  EDCPU16Exception = class(Exception);

  TCPURegister = (crA, crB, crC, crX, crY, crZ, crI, crJ, crPC, crSP, crO);
  TCPUInstruction = (ciExtendedPrefix, ciSET, ciADD, ciADX, ciSUB, ciSBX, ciMUL, ciDIV, ciDVI, ciMOD, ciMDI, ciSHL, ciSHR, ciAND, ciBOR, ciXOR, ciIFE, ciIFN, ciIFG, ciIFB, ciReserved, ciJSR);

  TMemoryAddress = $0..$FFFF;
  TResourceAddress = $0..High(TMemoryAddress) + Ord(High(TCPURegister)) + 2;
  TResourceMemory = array [TResourceAddress] of Word;

  TMemoryChangeNotify = procedure(ASender: TObject; MemoryAddress: TMemoryAddress; var MemoryValue: Word) of object;
  TRegisterChangeNotify = procedure(ASender: TObject; CPURegister: TCPURegister; var RegisterValue: Word) of object;
  TBeforeExecutionNotify = function(ASender: TObject; MemoryAddress: TMemoryAddress): Boolean of object;

  { TCPU }

  TCPU = class(TComponent)
  private
    FCycleExact: Boolean;
    FCycles: Integer;
    FOnBeforeExecution: TBeforeExecutionNotify;
    FResourceMemory: TResourceMemory;
    FOnMemoryChange: TMemoryChangeNotify;
    FOnRegisterChange: TRegisterChangeNotify;
    FSkipInstruction: Boolean;
    BurnCycles: Integer;
    FUseBigEndianWords: Boolean;
    function GetCPURegister(Reg: TCPURegister): Word; inline;
    function GetMemory(Address: TMemoryAddress): Word; inline;
    function GetResourceMemory(Address: TResourceAddress): Word; inline;
    procedure SetCPURegister(Reg: TCPURegister; const AValue: Word); inline;
    procedure SetMemory(Address: TMemoryAddress; const AValue: Word); inline;
    procedure SetResourceMemory(Address: TResourceAddress; const AValue: Word);
    function FetchNextWord: Word; inline;
    procedure FetchNextInstruction(out Instruction: TCPUInstruction; out Destination: TResourceAddress; out ValueA, ValueB: Word); inline;
    procedure RunNextInstruction;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Reset;
    procedure RunCycle;
    procedure RunInstruction;
    procedure SaveProgramToFile(AFileName: string; Length: Integer);
    function LoadProgramFromFile(AFileName: string): Integer;
    function DisassembleInstructionAt(var Address: TMemoryAddress): string;
    property ResourceMemory[Address: TResourceAddress]: Word read GetResourceMemory write SetResourceMemory; default;
    property Memory[Address: TMemoryAddress]: Word read GetMemory write SetMemory;
    property CPURegister[Reg: TCPURegister]: Word read GetCPURegister write SetCPURegister;
    property OnMemoryChange: TMemoryChangeNotify read FOnMemoryChange write FOnMemoryChange;
    property OnRegisterChange: TRegisterChangeNotify read FOnRegisterChange write FOnRegisterChange;
    property OnBeforeExecution: TBeforeExecutionNotify read FOnBeforeExecution write FOnBeforeExecution;
    property SkipInstruction: Boolean read FSkipInstruction;
    property CycleExact: Boolean read FCycleExact write FCycleExact;
    property Cycles: Integer read FCycles;
    property UseBigEndianWords: Boolean read FUseBigEndianWords write FUseBigEndianWords;
  end;

  TNameAddr = record
    Name: string;
    Address: TMemoryAddress;
    CodePos: Integer;
    ForData: Boolean;
  end;

  { TAssembler }

  TAssembler = class
  private
    FOpCodes: array of Word;
    CurrentORG: TMemoryAddress;
    Code: string;
    Head, Len: Integer;
    FError: Boolean;
    FErrorMessage: string;
    FErrorPos: Integer;
    FSymbols: array of TNameAddr;
    Fixups: array of TNameAddr;
    TokenHead: Integer;
    DataSymbol: Boolean;
    procedure AddSymbol(AName: string; CodePos: Integer; Addr: TMemoryAddress);
    procedure AddFixup(AName: string; CodePos: Integer; Addr: TMemoryAddress);
    procedure AddWord(W: Word);
    procedure SetORG(L: TMemoryAddress);
    procedure AssembleORG;
    function GetOpCodes(AIndex: TMemoryAddress): Word; inline;
    function GetSize: TMemoryAddress; inline;
    function GetSymbolCount: Integer; inline;
    function GetSymbols(AIndex: Integer): TNameAddr; inline;
    procedure SetOpCodes(AIndex: TMemoryAddress; const AValue: Word); inline;
    procedure SkipSpaces;
    procedure SetError(Message: string; Pos: Integer);
    function NextToken: string;
    function NextNumber: Integer;
    procedure AssembleLabel;
    procedure AssembleData;
    procedure AssembleReserve;
    procedure AssembleInstruction;
  public
    procedure Assemble(ACode: string);
    property OpCodes[AIndex: TMemoryAddress]: Word read GetOpCodes write SetOpCodes; default;
    property Size: TMemoryAddress read GetSize;
    property Error: Boolean read FError;
    property ErrorMessage: string read FErrorMessage;
    property ErrorPos: Integer read FErrorPos;
    property Symbols[AIndex: Integer]: TNameAddr read GetSymbols;
    property SymbolCount: Integer read GetSymbolCount;
  end;

const
  CPURegisterNames: array [TCPURegister] of string = ('A', 'B', 'C', 'X', 'Y', 'Z', 'I', 'J', 'PC', 'SP', 'O');
  CPUInstructionNames: array [TCPUInstruction] of string = ('Extended Prefix', 'SET', 'ADD', 'SUB', 'MUL', 'DIV',
                       'MOD', 'SHL', 'SHR', 'AND', 'BOR', 'XOR', 'IFE', 'IFN', 'IFG', 'IFB', 'Reserved ', 'JSR');
  CPUInstructionCycles: array [TCPUInstruction] of Integer = (0, 1, 2, 2, 2, 3, 3, 2, 2, 1, 1, 1, 2, 2, 2, 2, 0, 2);
  CPUInstructionArguments: array [TCPUInstruction] of Integer = (0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 1);
  Mutators = [ciSET, ciADD, ciSUB, ciMUL, ciDIV, ciMOD, ciSHL, ciSHR, ciAND, ciBOR, ciXOR];
  InvalidDestination = High(TResourceAddress);
  SymbolCharacter = ['A'..'Z', 'a'..'z', '0'..'9', '_'];

function RegisterResourceAddress(Reg: TCPURegister): TResourceAddress; inline;

implementation

function RegisterResourceAddress(Reg: TCPURegister): TResourceAddress;
begin
  Result:=High(TMemoryAddress) + Ord(Reg) + 1;
end;

{ TAssembler }

procedure TAssembler.AddSymbol(AName: string; CodePos: Integer; Addr: TMemoryAddress);
var
  I: Integer;
begin
  SetLength(FSymbols, Length(FSymbols) + 1);
  FSymbols[High(FSymbols)].Name:=AName;
  FSymbols[High(FSymbols)].Address:=Addr;
  FSymbols[High(FSymbols)].CodePos:=CodePos;
  FSymbols[High(FSymbols)].ForData:=False;
  for I:=0 to High(Fixups) do
    if Fixups[I].Name=AName then begin
      Fixups[I].Name:='';
      OpCodes[Fixups[I].Address]:=Addr;
    end;
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
     CurrentORG := L
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

procedure TAssembler.SkipSpaces;
begin
  while (Head <= Len) and (Code[Head] in [#1..#32, ';', '/']) do begin
    if Code[Head] in [';', '/'] then begin
      if (Code[Head]='/') and (Head <= Len - 1) and (Code[Head + 1] <> '/') then Exit;
      while (Head <= Len) and not (Code[Head] in [#10, #13]) do Inc(Head);
    end else
      Inc(Head);
  end;
end;

procedure TAssembler.SetError(Message: string; Pos: Integer);
begin
  if FError then Exit;
  FError:=True;
  FErrorMessage:=Message;
  FErrorPos:=Pos;
end;

function TAssembler.NextToken: string;
begin
  Result:='';
  SkipSpaces;
  TokenHead:=Head;
  while (Head <= Len) and (Code[Head] in SymbolCharacter) do begin
    Result:=Result + UpCase(Code[Head]);
    Inc(Head);
  end;
end;

function TAssembler.NextNumber: Integer;
var
  Negate: Boolean = False;
begin
  Result:=0;
  SkipSpaces;
  if (Head <= Len) and (Code[Head]='-') then begin
    Negate:=True;
    Inc(Head);
  end;
  if (Head <= Len - 2) and ((Code[Head]='$') or ((Code[Head]='0') and (Code[Head + 1] in ['x', 'X']))) then begin
    if Code[Head]='$' then Inc(Head) else Inc(Head, 2);
    while (Head <= Len) and (Code[Head] in ['0'..'9', 'A'..'F', 'a'..'f']) do begin
      if Code[Head] in ['0'..'9'] then
        Result:=Result*16 + (Ord(Code[Head]) - Ord('0'))
      else if Code[Head] in ['A'..'F'] then
        Result:=Result*16 + 10 + (Ord(Code[Head]) - Ord('A'))
      else if Code[Head] in ['a'..'f'] then
        Result:=Result*16 + 10 + (Ord(Code[Head]) - Ord('a'));
      Inc(Head);
    end;
  end else begin
    while (Head <= Len) and (Code[Head] in ['0'..'9']) do begin
      Result:=Result*10 + (Ord(Code[Head]) - Ord('0'));
      Inc(Head);
    end;
  end;
  if Negate then Result:=-Result;
end;

procedure TAssembler.AssembleLabel;
var
  LabelName: string;
begin
  Inc(Head);
  LabelName:=NextToken;
  AddSymbol(LabelName, TokenHead, CurrentORG);
  DataSymbol:=True;
end;

procedure TAssembler.AssembleData;
var
  Ch: Char;
  Token: string;
  Reg: TCPURegister;
  Found: Boolean;
  I: Integer;
begin
  if DataSymbol and (Length(FSymbols) > 0) then begin
    FSymbols[High(FSymbols)].ForData:=True;
    DataSymbol:=False;
  end;
  while (Head <= Len) and not Error do begin
    SkipSpaces;
    if Head > Len then Break;
    if Code[Head] in ['''', '"'] then begin
      Ch:=Code[Head];
      Inc(Head);
      while Head <= Len do begin
        if Code[Head]=Ch then begin
          Inc(Head);
          if (Head <= Len) and (Code[Head]=Ch) then begin
            AddWord(Ord(Ch));
            Inc(Head);
          end else
            Break;
        end else if Code[Head]='\' then begin
          Inc(Head);
          if Head > Len then begin
            SetError('Unexpected end of code in string data', Head);
            Exit;
          end;
          case UpCase(Code[Head]) of
            '0': AddWord($00);
            'A': AddWord($07);
            'B': AddWord($08);
            'T': AddWord($09);
            'N': AddWord($0A);
            'V': AddWord($0B);
            'F': AddWord($0C);
            'R': AddWord($0D);
            'E': AddWord($1B);
            'S': AddWord($20);
            else AddWord(Ord(Code[Head]));
          end;
          Inc(Head);
        end else begin
          AddWord(Ord(Code[Head]));
          Inc(Head);
        end;
      end;
    end else if Code[Head] in ['0'..'9', '$', '-'] then begin
      AddWord(NextNumber and $FFFF);
    end else begin
      Token:=NextToken;
      for Reg:=crA to crO do
        if CPURegisterNames[Reg]=Token then begin
          SetError('Cannot use a register in DATA', TokenHead);
          Exit;
        end;
      Found:=False;
      for I:=0 to High(FSymbols) do
        if Symbols[I].Name=Token then begin
          AddWord(FSymbols[I].Address);
          Found:=True;
          Break;
        end;
      if not Found then begin
        AddFixup(Token, TokenHead, CurrentORG);
        AddWord(0);
      end;
    end;
    SkipSpaces;
    if (Head > Len) or (Code[Head] <> ',') then Break;
    if Code[Head]=',' then Inc(Head);
  end;
end;

procedure TAssembler.AssembleReserve;
var
  I: Integer;
begin
  if DataSymbol and (Length(FSymbols) > 0) then begin
    FSymbols[High(FSymbols)].ForData:=True;
    DataSymbol:=False;
  end;
  SkipSpaces;
  if Head > Len then begin
    SetError('Unexpected end of code in RESERVE', Head);
    Exit;
  end;
  if not (Code[Head] in ['0'..'9', '$', '-']) then begin
    SetError('Number expected in RESERVE', Head);
    Exit;
  end;
  for I:=1 to NextNumber do AddWord(0);
end;

procedure TAssembler.AssembleORG;
var
  I: Integer;
begin
  SkipSpaces;
  if Head > Len then begin
    SetError('Unexpected end of code in ORG', Head);
    Exit;
  end;
  if not (Code[Head] in ['0'..'9', '$', '-']) then begin
    SetError('Number expected in ORG', Head);
    Exit;
  end;
  CurrentORG:=NextNumber;
end;

procedure TAssembler.AssembleInstruction;
var
  InstrName: string;
  Instr: TCPUInstruction;

  procedure DoAssembleInstruction(AInstr: TCPUInstruction);
  var
    I: Integer;
    IAddr: TMemoryAddress;
    Args: array [0..1] of Integer;

    function WriteLiteral(Lit: Word): Integer;
    begin
      if Lit < $20 then
        Result:=Lit + $20
      else begin
        Result:=$1F;
        AddWord(Lit);
      end;
    end;

    function AssembleArgument: Integer;
    label fuckit;
    var
      Token: string;
      I: Integer;
      Reg: TCPURegister;
    begin
      SkipSpaces;
      if Head > Len then begin
        SetError('Unexpected end of code while parsing parameter for ' + CPUInstructionNames[AInstr], Head);
        Exit(0);
      end;
      if Code[Head] in ['[', '('] then begin
        Inc(Head);
        if Head > Len then begin
          SetError('Unexpected end of code inside memory access parameter for ' + CPUInstructionNames[AInstr], Head);
          Exit(0);
        end;
        if Code[Head] in ['0'..'9', '$'] then begin
          I:=NextNumber and $FFFF;
          SkipSpaces;
          if Head > Len then begin
            SetError('Unexpected end of code inside memory access parameter for ' + CPUInstructionNames[AInstr] + ' after address', Head);
            Exit(0);
          end;
          if Code[Head]='+' then begin
            Inc(Head);
            Token:=NextToken;
            if Token='' then begin
              SetError('Syntax error while parsing the base for memory access in ' + CPUInstructionNames[AInstr] + ' after address', TokenHead);
              Exit(0);
            end;
            for Reg:=crA to crJ do
              if CPURegisterNames[Reg]=Token then begin
                SkipSpaces;
                if not ((Head <= Len) and (Code[Head] in [']', ')'])) then
                  SetError('Closing bracket ] or ) expected after memory address in ' + CPUInstructionNames[AInstr], Head)
                else
                  Inc(Head);
                AddWord(I);
                Exit($10 + Ord(Reg));
              end;
            SetError('Cannot use ' + Token + ' as a base for memory access in ' + CPUInstructionNames[AInstr] + ' after address', TokenHead);
            Exit(0);
          end;
          AddWord(I);
          SkipSpaces;
          if not ((Head <= Len) and (Code[Head] in [']', ')'])) then
            SetError('Closing bracket ] or ) expected after memory address in ' + CPUInstructionNames[AInstr], Head)
          else
            Inc(Head);
          Exit($1E);
        end;
        Token:=NextToken;
        if Token='' then begin
          SetError('Syntax error while parsing memory access address in ' + CPUInstructionNames[AInstr], TokenHead);
          Exit(0);
        end;
        for Reg:=crA to crJ do
          if CPURegisterNames[Reg]=Token then begin
            SkipSpaces;
            if not ((Head <= Len) and (Code[Head] in [']', ')'])) then
              SetError('Closing bracket ] or expected after memory address in ' + CPUInstructionNames[AInstr], Head)
            else
              Inc(Head);
            Exit($08 + Ord(Reg));
          end;
        for I:=SymbolCount - 1 downto 0 do
          if FSymbols[I].Name=Token then begin
            AddWord(FSymbols[I].Address);
            goto fuckit;
          end;
        AddFixup(Token, TokenHead, CurrentORG);
        AddWord(0);
        fuckit:
        SkipSpaces;
        if Head > Len then begin
          SetError('Unexpected end of code inside memory access in ' + CPUInstructionNames[AInstr], Head);
          Exit(0);
        end;
        if Code[Head]='+' then begin
          Inc(Head);
          Token:=NextToken;
          if Token='' then begin
            SetError('Syntax error while parsing the base for memory access in ' + CPUInstructionNames[AInstr] + ' after symbol', TokenHead);
            Exit(0);
          end;
          for Reg:=crA to crJ do
            if CPURegisterNames[Reg]=Token then begin
              SkipSpaces;
              if not ((Head <= Len) and (Code[Head] in [']', ')'])) then
                SetError('Closing bracket ] or ) expected after memory address in ' + CPUInstructionNames[AInstr], Head)
              else
                Inc(Head);
              Exit($10 + Ord(Reg));
            end;
          SetError('Cannot use ' + Token + ' as a base for memory access in ' + CPUInstructionNames[AInstr] + ' after address', TokenHead);
          Exit(0);
        end;
        if not ((Head <= Len) and (Code[Head] in [']', ')'])) then
          SetError('Closing bracket ] or ) expected after memory address in ' + CPUInstructionNames[AInstr], Head)
        else
          Inc(Head);
        Exit($1E);
      end else if Code[Head] in ['0'..'9', '-', '$'] then begin
        Exit(WriteLiteral(NextNumber and $FFFF));
      end else begin
        Token:=NextToken;
        if Token='' then begin
          SetError('Syntax error in parameter for ' + CPUInstructionNames[AInstr], Head);
          Exit(0);
        end;
        if Token='POP' then Exit($18);
        if Token='PEEK' then Exit($19);
        if Token='PUSH' then Exit($1A);
        if Token='SP' then Exit($1B);
        if Token='PC' then Exit($1C);
        if Token='O' then Exit($1D);
        for Reg:=crA to crJ do
          if CPURegisterNames[Reg]=Token then
            Exit(Ord(Reg));
        for I:=SymbolCount - 1 downto 0 do
          if FSymbols[I].Name=Token then
            Exit(WriteLiteral(FSymbols[I].Address));
        AddFixup(Token, TokenHead, CurrentORG);
        AddWord(0);
        Exit($1F);
      end;
    end;

  begin
    IAddr:=CurrentORG;
    AddWord(0);
    for I:=0 to CPUInstructionArguments[AInstr] - 1 do begin
      if Error then Break;
      SkipSpaces;
      if Head > Len then begin
        SetError('Unexpected end of code while parsing the arguments for instruction ' + CPUInstructionNames[AInstr], Head);
        Exit;
      end;
      if (I > 0) then begin
        if Code[Head]=',' then
          Inc(Head)
        else
          SetError('Comma expected after instruction parameter for ' + CPUInstructionNames[AInstr], Head);
      end;
      Args[I]:=AssembleArgument;
    end;
    if Ord(AInstr) < Ord(ciReserved) then begin
      OpCodes[IAddr]:=Ord(AInstr) or (Args[0] shl 4) or (Args[1] shl 10);
    end else begin
      OpCodes[IAddr]:=((Ord(AInstr) - Ord(ciReserved)) shl 4) or (Args[0] shl 10);
    end;
  end;

begin
  InstrName:=NextToken;
  if InstrName='' then begin
    SetError('Syntax error - an instruction was expected here', TokenHead);
    Exit;
  end;
  if (InstrName='DW') or(InstrName='DAT') or (InstrName='DATA') then begin
    AssembleData;
    Exit;
  end;
  if (InstrName='RESW') or (InstrName='RESERVE') then begin
    AssembleReserve;
    Exit;
  end;
  if (InstrName='ORG') then begin
    AssembleORG;
    Exit;
  end;
  for Instr:=Low(TCPUInstruction) to High(TCPUInstruction) do begin
    if CPUInstructionNames[Instr]=InstrName then begin
      DataSymbol:=False;
      DoAssembleInstruction(Instr);
      Exit;
    end;
  end;
  SetError('Unknown instruction: ' + InstrName, TokenHead);
end;

procedure TAssembler.Assemble(ACode: string);
var
  I: Integer;
begin
  Code:=ACode;
  Head:=1;
  Len:=Length(ACode);
  FError:=False;
  FErrorPos:=0;
  FErrorMessage:='No error';
  while (Head <= Len) and not Error do begin
    SkipSpaces;
    if Head > Len then Break;
    if Code[Head]=':' then
      AssembleLabel
    else
      AssembleInstruction;
  end;
  for I:=0 to High(Fixups) do
    if Fixups[I].Name <> '' then begin
      SetError('Unknown symbol or label: ' + Fixups[I].Name, Fixups[I].CodePos);
      Break;
    end;
end;

{ TCPU }

function TCPU.GetResourceMemory(Address: TResourceAddress): Word;
begin
  Result:=FResourceMemory[Address];
end;

function TCPU.GetCPURegister(Reg: TCPURegister): Word;
begin
  Result:=ResourceMemory[RegisterResourceAddress(Reg)];
end;

function TCPU.GetMemory(Address: TMemoryAddress): Word;
begin
  Result:=ResourceMemory[Address and $FFFF];
end;

procedure TCPU.SetCPURegister(Reg: TCPURegister; const AValue: Word);
begin
  ResourceMemory[RegisterResourceAddress(Reg)]:=AValue;
end;

procedure TCPU.SetMemory(Address: TMemoryAddress; const AValue: Word);
begin
  ResourceMemory[Address and $FFFF]:=AValue;
end;

procedure TCPU.SetResourceMemory(Address: TResourceAddress; const AValue: Word);
var
  NewValue: Word;
begin
  NewValue:=AValue;
  if Address <= High(TMemoryAddress) then begin
    if Assigned(FOnMemoryChange) then FOnMemoryChange(Self, TMemoryAddress(Address), NewValue);
  end else begin
    if Assigned(FOnRegisterChange) then FOnRegisterChange(Self, TCPURegister(Address - High(TMemoryAddress) - 1), NewValue);
  end;
  FResourceMemory[Address]:=NewValue;
end;

function TCPU.FetchNextWord: Word;
begin
  Result:=Memory[CPURegister[crPC]];
  CPURegister[crPC]:=CPURegister[crPC] + 1;
  Inc(BurnCycles);
end;

procedure TCPU.FetchNextInstruction(out Instruction: TCPUInstruction; out Destination: TResourceAddress; out ValueA, ValueB: Word);
var
  OpCode: Word;

  function DecodeResourceValue(AValue: Word): Word;
  begin
    if AValue in [$00..$07] then
      Result:=CPURegister[TCPURegister(AValue)]
    else if AValue in [$08..$0F] then
      Result:=Memory[CPURegister[TCPURegister(AValue - $08)]]
    else if AValue in [$10..$17] then
      Result:=Memory[FetchNextWord + CPURegister[TCPURegister(AValue - $10)]]
    else if AValue in [$20..$3F] then
      Result:=AValue - $20
    else case AValue of
      $18: if not SkipInstruction then begin
        Result:=Memory[CPURegister[crSP]];
        if CPURegister[crSP]=$FFFF then
          CPURegister[crSP]:=0
        else
          CPURegister[crSP]:=CPURegister[crSP] + 1;
      end;
      $19: Result:=Memory[CPURegister[crSP]];
      $1A: if not SkipInstruction then begin
        if CPURegister[crSP]=$0 then
          CPURegister[crSP]:=$FFFF
        else
          CPURegister[crSP]:=CPURegister[crSP] - 1;
        Result:=Memory[CPURegister[crSP]];
      end;
      $1B: Result:=CPURegister[crSP];
      $1C: Result:=CPURegister[crPC];
      $1D: Result:=CPURegister[crO];
      $1E: Result:=Memory[FetchNextWord];
      $1F: Result:=FetchNextWord;
      else raise EDCPU16Exception.Create('Implementation error - the value ' + IntToStr(AValue) + ' cannot be decoded');
    end;
  end;

  function DecodeResourceDestination(AValue: Word): TResourceAddress;
  begin
    if AValue in [$00..$07] then
      Result:=RegisterResourceAddress(TCPURegister(AValue))
    else if AValue in [$08..$0F] then
      Result:=CPURegister[TCPURegister(AValue - $08)]
    else if AValue in [$10..$17] then
      Result:=(FetchNextWord + CPURegister[TCPURegister(AValue - $10)]) and $FFFF
    else if AValue in [$20..$3F] then
      raise EDCPU16Exception.Create('Instruction at ' + HexStr(CPURegister[crPC] - 1, 4) + ' tried to write to a literal value')
    else case AValue of
      $18: if not SkipInstruction then begin
        Result:=CPURegister[crSP];
        if CPURegister[crSP]=$FFFF then
          CPURegister[crSP]:=0
        else
          CPURegister[crSP]:=CPURegister[crSP] + 1;
      end;
      $19: Result:=CPURegister[crSP];
      $1A: if not SkipInstruction then begin
        if CPURegister[crSP]=$0 then
          CPURegister[crSP]:=$FFFF
        else
          CPURegister[crSP]:=CPURegister[crSP] - 1;
        Result:=CPURegister[crSP];
      end;
      $1B: Result:=RegisterResourceAddress(crSP);
      $1C: Result:=RegisterResourceAddress(crPC);
      $1D: Result:=RegisterResourceAddress(crO);
      $1E: Result:=FetchNextWord;
      $1F: raise EDCPU16Exception.Create('Instruction at ' + HexStr(CPURegister[crPC] - 1, 4) + ' tried to write to a literal value')
      else raise EDCPU16Exception.Create('Implementation error - the destination value ' + IntToStr(AValue) + ' cannot be decoded');
    end;
  end;

begin
  OpCode:=FetchNextWord;
  Instruction:=TCPUInstruction(OpCode and $0F);
  if Instruction=ciExtendedPrefix then begin
    Instruction:=TCPUInstruction(((opCode and $3F0) shr 4) + Ord(ciReserved));
    if Instruction in Mutators then begin
      Destination:=DecodeResourceValue(OpCode shr 10);
      ValueA:=ResourceMemory[Destination];
      ValueB:=0;
    end else begin
      Destination:=InvalidDestination;
      ValueA:=DecodeResourceValue(OpCode shr 10);
      ValueB:=0;
    end;
  end else begin
    if Instruction in Mutators then begin
      Destination:=DecodeResourceDestination((OpCode shr 4) and $3F);
      ValueA:=ResourceMemory[Destination];
      ValueB:=DecodeResourceValue(OpCode shr 10);
    end else begin
      Destination:=InvalidDestination;
      ValueA:=DecodeResourceValue((OpCode shr 4) and $3F);
      ValueB:=DecodeResourceValue(OpCode shr 10);
    end;
  end;
  Inc(BurnCycles, CPUInstructionCycles[Instruction] - 1);
end;

procedure TCPU.RunNextInstruction;
var
  Instruction: TCPUInstruction;
  ValueA, ValueB: Word;
  Destination: TResourceAddress;
begin
  if Assigned(FOnBeforeExecution) then
    if not FOnBeforeExecution(Self, CPURegister[crPC]) then begin
      BurnCycles:=0;
      Exit;
    end;
  FetchNextInstruction(Instruction, Destination, ValueA, ValueB);
  if SkipInstruction then begin
    FSkipInstruction:=False;
    Exit;
  end;
  case Instruction of
    ciExtendedPrefix: raise EDCPU16Exception.Create('Implementation error - unexpected extended prefix at ' + HexStr(CPURegister[crPC] - 1, 4));
    ciSET: ResourceMemory[Destination]:=ValueB;
    ciADD: begin
      ResourceMemory[Destination]:=(ValueA + ValueB) and $FFFF;
      if ValueA + ValueB > $FFFF then
        CPURegister[crO]:=1
      else
        CPURegister[crO]:=0;
    end;
    ciSUB: begin
      ResourceMemory[Destination]:=(ValueA - ValueB) and $FFFF;
      if ValueA - ValueB < 0 then
        CPURegister[crO]:=$FFFF
      else
        CPURegister[crO]:=0;
    end;
    ciMUL: begin
      ResourceMemory[Destination]:=(ValueA * ValueB) and $FFFF;
      CPURegister[crO]:=((ValueA*ValueB) shr 16) and $FFFF;
    end;
    ciDIV: begin
      if ValueB=0 then begin
        ResourceMemory[Destination]:=0;
        CPURegister[crO]:=0;
      end else begin
        ResourceMemory[Destination]:=(ValueA div ValueB) and $FFFF;
        CPURegister[crO]:=((ValueA shl 16) div ValueB) and $FFFF;
      end;
    end;
    ciMOD: begin
      if ValueB=0 then
        ResourceMemory[Destination]:=0
      else
        ResourceMemory[Destination]:=(ValueA mod ValueB) and $FFFF;
    end;
    ciSHL: begin
      ResourceMemory[Destination]:=(ValueA shl ValueB) and $FFFF;
      CPURegister[crO]:=((ValueA shl ValueB) shr 16) and $FFFF;
    end;
    ciSHR: begin
      ResourceMemory[Destination]:=(ValueA shr ValueB) and $FFFF;
      CPURegister[crO]:=((ValueA shl 16) shr ValueB) and $FFFF;
    end;
    ciAND: ResourceMemory[Destination]:=(ValueA and ValueB) and $FFFF;
    ciBOR: ResourceMemory[Destination]:=(ValueA or ValueB) and $FFFF;
    ciXOR: ResourceMemory[Destination]:=(ValueA xor ValueB) and $FFFF;
    ciIFE, ciIFN, ciIFG, ciIFB: begin
      case Instruction of
        ciIFE: FSkipInstruction:=ValueA <> ValueB;
        ciIFN: FSkipInstruction:=ValueA=ValueB;
        ciIFG: FSkipInstruction:=ValueA <= ValueB;
        ciIFB: FSkipInstruction:=(ValueA and ValueB)=0;
      end;
      if FSkipInstruction then Inc(BurnCycles);
    end;
    ciReserved: raise EDCPU16Exception.Create('Implementation error - unexpected reserved extended opcode at ' + HexStr(CPURegister[crPC] - 1, 4));
    ciJSR: begin
      if CPURegister[crSP]=$0 then
        CPURegister[crSP]:=$FFFF
      else
        CPURegister[crSP]:=CPURegister[crSP] - 1;
      Memory[CPURegister[crSP]]:=CPURegister[crPC];
      CPURegister[crPC]:=ValueA;
    end;
  end;
  FCycles:=BurnCycles;
end;

constructor TCPU.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Reset;
end;

procedure TCPU.Reset;
var
  I: TCPURegister;
  J: Integer;
begin
  FSkipInstruction:=False;
  FCycles:=0;
  BurnCycles:=0;
  for I:=crA to crO do CPURegister[I]:=0;
end;

procedure TCPU.RunCycle;
begin
  if (BurnCycles=0) or CycleExact then
    RunNextInstruction
  else
    Dec(BurnCycles);
end;

procedure TCPU.RunInstruction;
begin
  RunCycle;
  BurnCycles:=0;
end;

procedure TCPU.SaveProgramToFile(AFileName: string; Length: Integer);
var
  Stream: TFileStream = nil;
  I: Integer;
begin
  try
    Stream:=TFileStream.Create(Utf8ToAnsi(AFileName), fmCreate);
    if UseBigEndianWords then begin
      for I:=0 to Length - 1 do begin
        Stream.WriteByte(Memory[I] shr 8);
        Stream.WriteByte(Memory[I] and $FF);
      end;
    end else begin
      for I:=0 to Length - 1 do begin
        Stream.WriteByte(Memory[I] and $FF);
        Stream.WriteByte(Memory[I] shr 8);
      end;
    end;
  finally
    FreeAndNil(Stream);
  end;
end;

function TCPU.LoadProgramFromFile(AFileName: string): Integer;
var
  Stream: TFileStream = nil;
  W: Word;
  I: Integer;
begin
  Result:=0;
  try
    Stream:=TFileStream.Create(Utf8ToAnsi(AFileName), fmOpenRead);
    if Stream.Size div 2 > High(TMemoryAddress) + 1 then
      raise EDCPU16Exception.Create('Program too big to fit in DCPU-16 memory!');
    if UseBigEndianWords then begin
      for I:=0 to Stream.Size div 2 - 1 do begin
        W:=Stream.ReadByte shl 8;
        W:=W or Stream.ReadByte;
        ResourceMemory[I]:=W;
      end;
    end else begin
      for I:=0 to Stream.Size div 2 - 1 do begin
        W:=Stream.ReadByte;
        W:=W or Stream.ReadByte shl 8;
        ResourceMemory[I]:=W;
      end;
    end;
    Result:=Stream.Size div 2;
  finally
    FreeAndNil(Stream);
  end;
end;

function TCPU.DisassembleInstructionAt(var Address: TMemoryAddress): string;
var
  OpCode: Word;
  Instruction: TCPUInstruction;

  function GetNextWord: Word;
  begin
    Result:=Memory[Address];
    if Address < High(TMemoryAddress) then Inc(Address) else Address:=0;
  end;

  function DecodeParameter(AValue: Word): string;
  begin
    if AValue in [$00..$07] then
      Result:=CPURegisterNames[TCPURegister(AValue)]
    else if AValue in [$08..$0F] then
      Result:='[' + CPURegisterNames[TCPURegister(AValue - $08)] + ']'
    else if AValue in [$10..$17] then
      Result:='[0x' + HexStr(GetNextWord, 4) + ' + ' + CPURegisterNames[TCPURegister(AValue - $10)] + ']'
    else if AValue in [$20..$3F] then
      Result:='0x00' + HexStr(AValue - $20, 2)
    else case AValue of
      $18: Result:='POP';
      $19: Result:='PEEK';
      $1A: Result:='PUSH';
      $1B: Result:='SP';
      $1C: Result:='PC';
      $1D: Result:='O';
      $1E: Result:='[0x' + HexStr(GetNextWord, 4) + ']';
      $1F: Result:='0x' + HexStr(GetNextWord, 4);
      else raise EDCPU16Exception.Create('Implementation error - the value ' + IntToStr(AValue) + ' cannot be decoded');
    end;
  end;

begin
  OpCode:=GetNextWord;
  Instruction:=TCPUInstruction(OpCode and $0F);
  if Instruction=ciExtendedPrefix then begin
    Instruction:=TCPUInstruction(((opCode and $3F0) shr 4) + Ord(ciReserved));
    if Instruction in [Low(TCPUInstruction)..High(TCPUInstruction)] then
      Result:=CPUInstructionNames[Instruction] + ' ' + DecodeParameter(OpCode shr 10)
    else
      Result:='; Invalid opcode - ' + HexStr(OpCode, 4);
  end else begin
    if Instruction in [Low(TCPUInstruction)..High(TCPUInstruction)] then
      Result:=CPUInstructionNames[Instruction] + ' ' + DecodeParameter((OpCode shr 4) and $3F) + ', ' + DecodeParameter(OpCode shr 10)
    else
      Result:='; Invalid opcode - ' + HexStr(OpCode, 4);
  end;
end;

end.

