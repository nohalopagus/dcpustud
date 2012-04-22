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

    TNameAddr = record
      Name: string;
      Address: TMemoryAddress;
      CodePos: Integer;
      ForData: Boolean;
    end;

    TNameValue = record
      Name: string;
      Value: TMemoryAddress;
    end;

const
  CPURegisterNames: array [TCPURegister] of string = ('A', 'B', 'C', 'X', 'Y', 'Z', 'I', 'J', 'PC', 'SP', 'O');
  CPUInstructionNames: array [TCPUInstruction] of string = ('Extended Prefix', 'SET', 'ADD', 'SUB', 'MUL', 'DIV',
                       'MOD', 'SHL', 'SHR', 'AND', 'BOR', 'XOR', 'IFE', 'IFN', 'IFG', 'IFB', 'Reserved ', 'JSR', 'ERROR - Invalid');
  CPUInstructionArguments: array [TCPUInstruction] of Integer = (0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 1, 0);

function isCPURegisterName(symbol: string): boolean;
function getCPURegisterID(symbol: string): TCPURegister;

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

end.

