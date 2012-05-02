unit DCPUtypes;

{$mode objfpc}{$H+}

interface
type
    TOpArgumentType = (arRegister, arRegisterRefNW, arOffset, arPOP, arPEEK, arPUSH, arSP, arPC, arO, arReferenceNW, arLiteralNW, arSmallLiteral, arReference);
    TCPURegister = (crA, crB, crC, crX, crY, crZ, crI, crJ, crPC, crSP, crO);
    TCPUInstruction = (ciExtendedPrefix, ciSET, ciADD, ciSUB, ciMUL, ciDIV, ciMOD, ciSHL, ciSHR, ciAND, ciBOR, ciXOR, ciIFE, ciIFN, ciIFG, ciIFB, ciReserved, ciJSR, ciInvalid);
    TTokenType = (ttSymbol, ttNumber, ttString, ttDelimiter);

    TMemoryAddress = $0..$FFFF;
    TCPUWord = TMemoryAddress;
    TWordArray = array of TMemoryAddress;
    TResourceAddress = $0..High(TMemoryAddress) + Ord(High(TCPURegister)) + 2;

    TToken = record
      tokenType: TTokenType;
      intVal: integer;
      strVal: string;
      length: integer;
      linePos: integer; //offset into line where the token starts
    end;
    TTokens = array of TToken;

    TTokenizedLine = record
      tokens: TTokens;
      lineNumber: integer;
      sourceFile: string;
      sourcePos: integer; //offset into source file where line starts
    end;
    TTokenizedLines = array of TTokenizedLine;

    TNameAddr = record
      Name: string;
      Address: TMemoryAddress;
      ForData: Boolean;

      line: TTokenizedLine;
      token: TToken;
    end;

    TNameValue = record
      Name: string;
      Value: TMemoryAddress;
    end;

    TSimpleError = record
       message: string;
       line: integer;
       sourceFile: string;
     end;
     TSimpleErrors = array of TSimpleError;

const
  CPURegisterNames: array [TCPURegister] of string = ('A', 'B', 'C', 'X', 'Y', 'Z', 'I', 'J', 'PC', 'SP', 'EX');
  CPUInstructionNames: array [TCPUInstruction] of string = ('Extended Prefix', 'SET', 'ADD', 'SUB', 'MUL', 'DIV',
                       'MOD', 'SHL', 'SHR', 'AND', 'BOR', 'XOR', 'IFE', 'IFN', 'IFG', 'IFB', 'Reserved ', 'JSR', 'ERROR - Invalid');
  CPUInstructionArguments: array [TCPUInstruction] of Integer = (0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 1, 0);

  defaultDefineValue: TToken = (tokenType:ttSymbol; intVal: 0; strVal:'0'; length: 0; linePos:0 );
  BaseInstrBits: integer = 5;

function isCPURegisterName(symbol: string): boolean;
function getCPURegisterID(symbol: string): TCPURegister;
function getCPUOpcode(opName: string): TCPUInstruction;
function isDirective(str: string): boolean;
function isDData(str: string):boolean;
function isDReserve(str: string):boolean;
function isDORG(str: string):boolean;

implementation

function isCPURegisterName(symbol: string): boolean;
var
  reg: string;
begin
  for reg in CPURegisterNames do
      if reg = symbol then exit(true);
  exit(false);
end;

function getCPURegisterID(symbol: string): TCPURegister;
var
  reg: TCPURegister;
begin
  for reg := crA to crJ do
      if CPURegisterNames[reg] = symbol then exit(reg);
end;

function getCPUOpcode(opName: string): TCPUInstruction;
var
  opID: TCPUInstruction;
begin
     for opID:=Low(TCPUInstruction) to High(TCPUInstruction) do begin
         if CPUInstructionNames[opID]=opName then begin
            exit(opID);
         end;
     end;
     exit(ciInvalid);
end;

function isDirective(str: string): boolean;
begin
  exit(isDData(str) or isDReserve(str) or isDORG(str));
end;

function isDData(str: string):boolean;
begin
  exit( (str='DW') or (str='DAT') or (str='DATA'));
end;

function isDReserve(str: string):boolean;
begin
  exit( (str='RESW') or (str='RESERVE'));
end;

function isDORG(str: string):boolean;
begin
  exit( (str='ORG'));
end;

end.

