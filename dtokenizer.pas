unit DTokenizer;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, DCPUtypes;

type

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

  TTokenizerError = record
    message: string;
    line: integer;
    sourceFile: string;
  end;


  {
   CTokenizer - turns a source file into a list
              of tokenized lines
  }
  CTokenizer = class
  private
    source: string;
    sourcePosition: Integer;
    sourceLength: Integer;

    currentLine: string;
    currentLineNr: integer;
    linePosition: integer;
    lineLength: integer;
    sourceFile: string;

  public
    tokenized: TTokenizedLines;
    errors: array of TTokenizerError;
    upcaseSymbols: boolean;
    procedure tokenize(ASource: string; AFile: string);

  private
    function parseLine(): TTokenizedLine;
    function nextLine(): boolean;
    function srcPeek(count: integer = 0): char;
    function srcHas(count: integer): boolean;
    function srcHasNext(): boolean;
    function srcNext(): char;

    function peek(count: integer = 0): char;
    function has(count: integer): boolean;
    function hasNext(): boolean;
    function next(): char;
    procedure skipLine();

    function isASMComment(): boolean;
    function isCComment(): boolean;
    function isTokenDelimiter(): boolean;
    function isNumeric(): boolean;
    function isWhitespace(): Boolean;

    function nextToken(): TToken;

    procedure skipWhitespace();
    function parseNumber(): integer;
    function parseSymbol(): string;
    function parseString(): string;

    function parseStringEscape(): char;
    procedure addError(message:string);

  end;

  const
    Delimiters = [',', '[', ']', '(', ')', '+'];
implementation

function CTokenizer.parseLine(): TTokenizedLine;
var
   line: TTokenizedLine;
begin
     line.lineNumber:=currentLineNr;
     line.sourceFile:=sourceFile;
     while hasNext() do begin
           SetLength(line.tokens, Length(line.tokens)+1);
           line.tokens[high(line.tokens)] := nextToken();
           skipWhitespace();
     end;
     exit(line);
end;

procedure CTokenizer.tokenize(ASource: string; AFile: string);
var
   lineSrcPos: integer;
begin
     currentLineNr:=1;
     sourcePosition:=1;
     source:=ASource;
     sourceFile:=AFile;
     sourceLength:=length(source);
     lineSrcPos:=1;
     while nextLine() do begin
           skipWhitespace();
           if hasNext() then begin
              SetLength(tokenized, Length(tokenized)+1);
              tokenized[High(tokenized)] := parseLine();
              tokenized[High(tokenized)].sourcePos := lineSrcPos;
           end;

           //for next line
           lineSrcPos := sourcePosition;
           Inc(currentLineNr);
     end;
end;

function CTokenizer.nextToken(): TToken;
var
      start: integer;
      token: TToken;
begin
     start := linePosition;
     if peek() in Delimiters then begin
        token.length:=1;
        token.strVal:=peek();
        token.tokenType:=ttDelimiter;
        token.linePos:=start;
        next();
        exit(token);
     end;
     if peek() = '"' then begin
        token.strVal:=parseString();
        token.length:=length(token.strVal);
        token.tokenType:=ttString;
        token.linePos:=start;
        exit(token);
     end;
     if isNumeric() then begin
        token.intVal:=parseNumber();
        token.length:=linePosition-start;
        token.strVal:=copy(currentLine, start, token.length);
        token.tokenType:=ttNumber;
        token.linePos:=start;
        exit(token);
     end;
     //if it's not any of the above, treat it as a symbol
     token.tokenType:=ttSymbol;
     token.strVal:=parseSymbol();
     token.length:=length(token.strVal);
     token.linePos:=start;
     exit(token);
end;

function CTokenizer.parseSymbol(): string;
var
   start: integer;
   symbol: string;
begin
     start := linePosition;
     while hasNext() and not isTokenDelimiter() do next();
     symbol := copy(currentLine, start, linePosition-start);

     if upcaseSymbols then begin
        exit(upcase(symbol));
     end else begin
        exit(symbol);
     end;
end;

procedure CTokenizer.skipWhitespace();
begin
     while hasNext() and (isWhitespace() or isASMComment() or isCComment()) do begin
       if isASMComment() or isCComment() then begin
          skipLine();
       end else begin
          next();
       end;
     end;
end;

function CTokenizer.isTokenDelimiter(): boolean;
begin
     exit((peek() in Delimiters)
         or isWhitespace()
         or isASMComment()
         or isCComment());
end;

function CTokenizer.next(): char;
begin
     inc(linePosition);
     exit(peek(-1));
end;

function CTokenizer.isWhitespace(): boolean;
begin
     exit(peek() in [#1 .. #32]);
end;

function CTokenizer.isASMComment(): boolean;
begin
     exit(peek() = ';');
end;

function CTokenizer.isCComment(): boolean;
begin
     exit(has(2) and (peek() = '/') and (peek(1) = '/'));
end;

//peek count characters ahead, don't advance position
function CTokenizer.peek(count:integer = 0): char;
begin
     exit(currentLine[linePosition+count]);
end;

//return True if there is a character left
function CTokenizer.hasNext(): boolean;
begin
     exit(has(1));
end;
//return True if there are at least count characters left
function CTokenizer.has(count: integer): boolean;
begin
     exit(linePosition+count-1<=lineLength);
end;

procedure CTokenizer.skipLine();
begin
     linePosition:=lineLength+1;
end;

//peek count characters ahead, don't advance position
function CTokenizer.srcPeek(count:integer = 0): char;
begin
     exit(source[sourcePosition+count]);
end;

//return True if there is a character left
function CTokenizer.srcHasNext(): boolean;
begin
     exit(srcHas(1));
end;
//return True if there are at least count characters left
function CTokenizer.srcHas(count: integer): boolean;
begin
     exit(sourcePosition+count-1<=sourceLength);
end;

function CTokenizer.srcNext(): char;
begin
     inc(sourcePosition);
     exit(source[sourcePosition-1]);
end;

//store the next line in current line and
//set sourcePosition to start of next line
function CTokenizer.nextLine(): boolean;
var
      line: string;
begin
     line := '';
     if not srcHasNext() then exit(False);
     while srcHasNext() and not (srcPeek() in [#10, #13]) do begin
           line+=srcNext();
     end;
     if srcHasNext() then begin //handle CR, LF and CRLF
        if (srcPeek() = #13) then begin
           srcNext();
           if srcHasNext() and (srcPeek() = #10) then
              srcNext();
        end else if (srcPeek() = #10) then
            srcNext();
     end;
     currentLine := trim(line);
     lineLength:= length(currentLine);
     linePosition:=1;
     exit(True);
end;

function CTokenizer.isNumeric(): boolean;
var
      check: char;
begin
     check := peek();
     if (check = '-') then begin
        if not hasNext() then
           exit(False)
        else
           check := peek(1);
     end;
     if (check = '-') then check := peek(1);
     if check in ['0' .. '9'] then exit(True);
     Exit(False);
end;

function CTokenizer.parseString(): string;
begin
     result := '';
     if not (peek() = '"') then begin
        addError('Implementation error while parsing string.');
        skipLine();
        exit(result);
     end;
     next();

     while True do begin
           if not hasNext() then begin
               addError('Invalid string format');
               exit(result);
           end;
           if peek() = '"' then begin //end of string
              next();
              break;
           end;
           if (peek() = '\') and not has(2) then begin
              addError('Unexpected end of string');
              skipLine();
              break;
           end;
           if peek() = '\' then begin
              result += parseStringEscape();
              Continue;
           end;
           result+=next();
     end;
     exit(result);
end;

function CTokenizer.parseStringEscape(): char;
var
      ch: char;
begin
  next(); //skip the backslash
  ch := next();
  case UpCase(ch) of
       '0': exit(chr($00));
       'A': exit(chr($07));
       'B': exit(chr($08));
       'T': exit(chr($09));
       'N': exit(chr($0A));
       'V': exit(chr($0B));
       'F': exit(chr($0C));
       'R': exit(chr($0D));
       'E': exit(chr($1B));
       'S': exit(chr($20));
  end;
  exit(ch);
end;

//reads the next number from the current line
function CTokenizer.parseNumber(): integer;
var
      negate: integer;
      base: integer;
      aDigit: integer;
      tokenStart: integer;
begin
     tokenStart := linePosition; //in case of error
     base := 10; //default, if no format specifier

     negate := 1;
     if peek() = '-' then begin
        negate := -1;
        next();
     end;
     if (peek() = '0') and has(3) and (lowercase(peek(1)) in ['b', 'o', 'x']) then begin
        case lowercase(peek(1)) of
             'b': base := 2;
             'o': base := 8;
             'x': base := 16;
        end;
        //skip over format specifier
        next();
        next();
     end;

     result := 0;
     while hasNext() and (not isTokenDelimiter()) do begin
            result *= base;
            aDigit := ord(lowercase(peek()));
            if aDigit <= ord('9') then begin
               aDigit -= ord('0')
            end else begin
                aDigit -= ord('a');
                aDigit += 10;
            end;
            if (aDigit<0) or (aDigit>base-1) then begin
               linePosition:=tokenStart;
               addError('Invalid number format: '+parseSymbol());
               exit(0);
            end;
            result+=aDigit;
            next();
     end;
     exit(negate * result);
end;

procedure CTokenizer.addError(message: string);
begin
     SetLength(errors, Length(errors)+1);
     errors[High(errors)].message:=message;
     errors[High(errors)].line:=currentLineNr;
     errors[High(errors)].sourceFile:=sourceFile;
end;
end.

