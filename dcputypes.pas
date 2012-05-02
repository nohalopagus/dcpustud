unit DCPUtypes;

{$mode objfpc}{$H+}

interface
type
    TOpArgumentType = (arRegister, arRegisterRefNW, arOffset, arPOP, arPEEK, arPUSH, arSP, arPC, arO, arReferenceNW, arLiteralNW, arSmallLiteral, arReference);
    TCPURegister = (crA, crB, crC, crX, crY, crZ, crI, crJ, crPC, crSP, crEX);
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
     TCPUInstruction = (
                     ciExtendedPrefix,
                     ciSET,
                     ciADD, ciSUB,
                     ciMUL, ciMLI,
                     ciDIV, ciDVI,
                     ciMOD, ciMDI,
                     ciAND, ciBOR, ciXOR,
                     ciSHR, ciASR, ciSHL,
                     ciIFB, ciIFC,
                     ciIFE, ciIFN,
                     ciIFG, ciIFA,
                     ciIFL, ciIFU,
                     ciR18, ciR19, //reserved
                     ciADX, ciSBX,
                     ciR1c, ciR1d, //reserved
                     ciSTI, ciSTD,
                     ciReservedEX,
                     ciJSR,
                     ciRE2, ciRE3, ciRE4, //reserved
                     ciRE5, ciRE6, ciRE7, //reserved
                     ciINT, ciIAG, ciIAS, ciRFI, ciIAQ,
                     ciREd, ciREe, ciREf, //reserved
                     ciHWN, ciHWQ, ciHWI,
                     ciRE13, ciRE14, ciRE15, ciRE16, //reserved
                     ciRE17, ciRE18, ciRE19, ciRE1a, //reserved
                     ciRE1b, ciRE1c, ciRE1d, ciRE1e, //reserved
                     ciRE1f, //reserved
                     ciInvalid);
const
   CPUInstructionNames: array [TCPUInstruction] of string = (
                     'Extended Prefix',
                     'SET',
                     'ADD', 'SUB',
                     'MUL', 'MLI',
                     'DIV', 'DVI',
                     'MOD', 'MDI',
                     'AND', 'BOR', 'XOR',
                     'SHR', 'ASR', 'SHL',
                     'IFB', 'IFC',
                     'IFE', 'IFN',
                     'IFG', 'IFA',
                     'IFL', 'IFU',
                     'Reserved ', 'Reserved ',
                     'ADX', 'SBX',
                     'Reserved ', 'Reserved ',
                     'STI', 'STD',
                     'Reserved ',
                     'JSR',
                     'Reserved ', 'Reserved ', 'Reserved ',
                     'Reserved ', 'Reserved ', 'Reserved ',
                     'INT', 'IAG', 'IAS', 'RFI', 'IAQ',
                     'Reserved ', 'Reserved ', 'Reserved ',
                     'HWN', 'HWQ', 'HWI',
                     'Reserved ', 'Reserved ', 'Reserved ', 'Reserved ',
                     'Reserved ', 'Reserved ', 'Reserved ', 'Reserved ',
                     'Reserved ', 'Reserved ', 'Reserved ', 'Reserved ',
                     'Reserved ',
                     'ERROR - Invalid');


  CPURegisterNames: array [TCPURegister] of string = ('A', 'B', 'C', 'X', 'Y', 'Z', 'I', 'J', 'PC', 'SP', 'EX');

  defaultDefineValue: TToken = (tokenType:ttSymbol; intVal: 0; strVal:'0'; length: 0; linePos:0 );
  BaseInstrBits: integer = 5;
  BaseInstrMask: integer = $1F;
function isCPURegisterName(symbol: string): boolean;  inline;
function getCPURegisterID(symbol: string): TCPURegister;
function getCPUOpcode(opName: string): TCPUInstruction;
function isDirective(str: string): boolean;
function isDData(str: string):boolean;
function isDReserve(str: string):boolean;
function isDORG(str: string):boolean;
function instructionArgCount(insn: TCPUInstruction): integer; inline;
function signed(value: integer): integer; inline;
implementation

function signed(value: integer): integer; inline;
begin
  exit((value and $7FFF) + ((value shr 15) * -$FFFF));
end;

function instructionArgCount(insn: TCPUInstruction): integer; inline;
begin
  if insn<ciReservedEX then exit(2);
  exit(1);
end;

function isCPURegisterName(symbol: string): boolean; inline;
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

