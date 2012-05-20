unit DCPU16;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCPUtypes;

type
  EDCPU16Exception = class(Exception);
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
    procedure FetchNextInstruction(out Instruction: TCPUInstruction; out Destination: TResourceAddress; out ValueB, ValueA: Word); inline;
    procedure RunNextInstruction;
    function operandA(opcode: Word): Word; inline;
    function operandB(opcode: Word): Word; inline;
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

const
  CPUInstructionCycles: array [TCPUInstruction] of Integer = (
                        0,
                        1,
                        2, 2, 2, 2,
                        3, 3, 3, 3,
                        1, 1, 1, 1, 1, 1,
                        2, 2, 2, 2, 2, 2, 2, 2,
                        0, 0,
                        3, 3,
                        0, 0,
                        2, 2,
                        0,
                        3,
                        0, 0, 0, 0, 0, 0,
                        4, 1, 1, 3, 2,
                        0, 0, 0,
                        2, 4, 4,
                        0, 0, 0, 0,
                        0, 0, 0, 0,
                        0, 0, 0, 0,
                        0,
                        0);
  Mutators = [ciSET, ciADD, ciSUB, ciMUL, ciDIV, ciDIV, ciMOD, ciMDI, ciSHL, ciSHR, ciAND, ciBOR, ciXOR];
  InvalidDestination = High(TResourceAddress);
  SymbolCharacter = ['A'..'Z', 'a'..'z', '0'..'9', '_'];

function RegisterResourceAddress(Reg: TCPURegister): TResourceAddress; inline;

implementation

function RegisterResourceAddress(Reg: TCPURegister): TResourceAddress;
begin
  Result:=High(TMemoryAddress) + Ord(Reg) + 1;
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

procedure TCPU.FetchNextInstruction(out Instruction: TCPUInstruction; out Destination: TResourceAddress; out ValueB, ValueA: Word);
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
        CPURegister[crSP]:= (CPURegister[crSP] + 1) and $FFFF;
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
      $1D: Result:=CPURegister[crEX];
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
      $1D: Result:=RegisterResourceAddress(crEX);
      $1E: Result:=FetchNextWord;
      $1F: raise EDCPU16Exception.Create('Instruction at ' + HexStr(CPURegister[crPC] - 1, 4) + ' tried to write to a literal value')
      else raise EDCPU16Exception.Create('Implementation error - the destination value ' + IntToStr(AValue) + ' cannot be decoded');
    end;
  end;

begin
  OpCode:=FetchNextWord;
  Instruction:=TCPUInstruction(OpCode and BaseInstrMask);
  if Instruction=ciExtendedPrefix then begin
    Instruction:=TCPUInstruction(((opCode and $3e0) shr BaseInstrBits) + Ord(ciReservedEX));
    if Instruction in Mutators then begin
      Destination:=DecodeResourceValue(operandA(OpCode));
      ValueA:=ResourceMemory[Destination];
      ValueB:=0;
    end else begin
      Destination:=InvalidDestination;
      ValueA:=DecodeResourceValue(operandA(OpCode));
      ValueB:=0;
    end;
  end else begin
    if Instruction in Mutators then begin
      Destination:=DecodeResourceDestination(operandB(OpCode));
      ValueB:=ResourceMemory[Destination];
      ValueA:=DecodeResourceValue(operandA(OpCode));
    end else begin
      Destination:=InvalidDestination;
      ValueB:=DecodeResourceValue(operandB(OpCode));
      ValueA:=DecodeResourceValue(operandA(OpCode));
    end;
  end;
  Inc(BurnCycles, CPUInstructionCycles[Instruction] - 1);
end;

procedure TCPU.RunNextInstruction;
var
  Instruction: TCPUInstruction;
  ValueA, ValueB: Word;
  Destination: TResourceAddress;
  tmpVal: integer;
begin
  if Assigned(FOnBeforeExecution) then
    if not FOnBeforeExecution(Self, CPURegister[crPC]) then begin
      BurnCycles:=0;
      Exit;
    end;
  FetchNextInstruction(Instruction, Destination, ValueB, ValueA);
  if SkipInstruction then begin
    FSkipInstruction:=False;
    Exit;
  end;
  case Instruction of
    ciExtendedPrefix: raise EDCPU16Exception.Create('Implementation error - unexpected extended prefix at ' + HexStr(CPURegister[crPC] - 1, 4));

    ciSET: ResourceMemory[Destination]:=ValueA;

    ciADD: begin
      tmpVal := ValueA + ValueB;
      ResourceMemory[Destination] := tmpVal and $FFFF;
      if tmpVal > $FFFF then
        CPURegister[crEX]:=1
      else
        CPURegister[crEX]:=0;
    end;

    ciSUB: begin
      tmpVal := ValueB - ValueA;
      ResourceMemory[Destination] := tmpVal and $FFFF;
      if tmpVal < 0 then
        CPURegister[crEX]:=$FFFF
      else
        CPURegister[crEX]:=0;
    end;

    { MUL, MLI }
    ciMUL: begin
      tmpVal := ValueA * ValueB;
      ResourceMemory[Destination] := tmpVal and $FFFF;
      CPURegister[crEX]:=(tmpVal shr 16) and $FFFF;
    end;
    ciMLI: begin
      tmpVal := signed(ValueA) * signed(ValueB);
      ResourceMemory[Destination] := tmpVal and $FFFF;
      CPURegister[crEX]:=(tmpVal shr 16) and $FFFF;
    end;

    { DIV, DVI }
    ciDIV: begin
      if ValueA=0 then begin
        ResourceMemory[Destination]:=0;
        CPURegister[crEX]:=0;
      end else begin
        ResourceMemory[Destination]:=(ValueB div ValueA) and $FFFF;
        CPURegister[crEX]:=((ValueB shl 16) div ValueA) and $FFFF;
      end;
    end;
    ciDVI: begin
      if ValueA=0 then begin
        ResourceMemory[Destination]:=0;
        CPURegister[crEX]:=0;
      end else begin
        ResourceMemory[Destination]:=(signed(ValueB) div signed(ValueA)) and $FFFF;
        CPURegister[crEX]:=((signed(ValueB) shl 16) div signed(ValueA)) and $FFFF;
      end;
    end;

    { MOD, MDI }
    ciMOD: begin
      if ValueA=0 then
        ResourceMemory[Destination]:=0
      else
        ResourceMemory[Destination] := (ValueB mod ValueA) and $FFFF;
    end;
    ciMDI: begin
      if ValueA=0 then
        ResourceMemory[Destination]:=0
      else
        ResourceMemory[Destination] := (signed(ValueB) mod signed(ValueA)) and $FFFF;
    end;

    { AND, BOR, XOR }
    ciAND: ResourceMemory[Destination] := (ValueB and ValueA) and $FFFF;
    ciBOR: ResourceMemory[Destination] := (ValueB or ValueA) and $FFFF;
    ciXOR: ResourceMemory[Destination] := (ValueB xor ValueA) and $FFFF;

    { SHR, ASR, SHL }
    ciSHR: begin
      ResourceMemory[Destination]:=(ValueB shr ValueA) and $FFFF;
      CPURegister[crEX]:=((ValueB shl 16) shr ValueA) and $FFFF;
    end;
    ciASR: begin
      ResourceMemory[Destination]:=(ValueB shr ValueA) and $FFFF;
      CPURegister[crEX]:=((ValueB shl 16) shr ValueA) and $FFFF;
    end;
    ciSHL: begin
      tmpVal := ValueB shl ValueA;
      ResourceMemory[Destination] := tmpVal and $FFFF;
      CPURegister[crEX] := (tmpVal shr 16) and $FFFF;
    end;

    { IFx }
    ciIFB, ciIFC, ciIFE, ciIFN, ciIFG, ciIFA, ciIFL, ciIFU: begin
      case Instruction of
        ciIFB: FSkipInstruction:=(ValueB and ValueA) = 0;
        ciIFC: FSkipInstruction:=(ValueB and ValueA) <> 0;

        ciIFE: FSkipInstruction:=ValueB <> ValueA;
        ciIFN: FSkipInstruction:=ValueB=ValueA;

        ciIFG: FSkipInstruction:=ValueB <= ValueA;
        ciIFA: FSkipInstruction:=signed(ValueB) <= signed(ValueA);

        ciIFL: FSkipInstruction:=ValueB >= ValueA;
        ciIFU: FSkipInstruction:=signed(ValueB) >= signed(ValueA);

      end;
      if FSkipInstruction then Inc(BurnCycles);
    end;

    { ADX, SBX }
    ciADX: begin
      tmpVal:= ValueB + ValueA + CPURegister[crEX];
      if tmpVal > $FFFF then
        CPURegister[crEX]:=1
      else
        CPURegister[crEX]:=0;
      ResourceMemory[Destination] := tmpVal and $FFFF;
    end;

    ciSBX: begin
      tmpVal:= ValueB - ValueA + CPURegister[crEX];
      if tmpVal < 0 then
        CPURegister[crEX] := $FFFF
      else
        CPURegister[crEX] := 0;
      ResourceMemory[Destination] := tmpVal and $FFFF;
    end;

    { STI, STD }
    ciSTI: begin
      ResourceMemory[Destination] := ValueA;
      CPURegister[crI] := (CPURegister[crI] + 1) and $FFFF;
      CPURegister[crJ] := (CPURegister[crJ] + 1) and $FFFF;
    end;
    ciSTD: begin
      ResourceMemory[Destination] := ValueA;
      CPURegister[crI] := (CPURegister[crI] - 1) and $FFFF;
      CPURegister[crJ] := (CPURegister[crJ] - 1) and $FFFF;
    end;

    ciReservedEX: raise EDCPU16Exception.Create('Implementation error - unexpected reserved extended opcode at ' + HexStr(CPURegister[crPC] - 1, 4));

    { JSR }
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
  Reg: TCPURegister;
  J: Integer;
begin
  FSkipInstruction:=False;
  FCycles:=0;
  BurnCycles:=0;
  for Reg in TCPURegister do CPURegister[Reg]:=0;
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
      $1D: Result:='EX';
      $1E: Result:='[0x' + HexStr(GetNextWord, 4) + ']';
      $1F: Result:='0x' + HexStr(GetNextWord, 4);
      else raise EDCPU16Exception.Create('Implementation error - the value ' + IntToStr(AValue) + ' cannot be decoded');
    end;
  end;

begin
  OpCode:=GetNextWord;
  Instruction:=TCPUInstruction(OpCode and BaseInstrMask);
  if Instruction=ciExtendedPrefix then begin
    Instruction:=TCPUInstruction(((opCode and $3e0) shr BaseInstrBits) + Ord(ciReservedEX));
    if Instruction in [Low(TCPUInstruction)..High(TCPUInstruction)] then
      Result:=CPUInstructionNames[Instruction] + ' ' + DecodeParameter(operandA(OpCode))
    else
      Result:='; Invalid opcode - ' + HexStr(OpCode, 4);
  end else begin
    if Instruction in [Low(TCPUInstruction)..High(TCPUInstruction)] then
      Result:=CPUInstructionNames[Instruction] + ' ' + DecodeParameter(operandB(OpCode)) + ', ' + DecodeParameter(operandA(OpCode))
    else
      Result:='; Invalid opcode - ' + HexStr(OpCode, 4);
  end;
end;

function TCPU.operandA(opcode: Word): Word; inline;
begin
  exit(opcode shr 10);
end;

function TCPU.operandB(opcode: Word): Word; inline;
begin
  exit((opcode shr BaseInstrBits) and $1F);
end;

end.

