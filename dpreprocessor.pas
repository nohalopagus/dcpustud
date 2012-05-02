unit DPreprocessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DTokenizer, DCPUtypes;
type
  TPPDefine = record
    name: string;
    value: TToken;
    src: TTokenizedLine;
  end;
  TPPDefines = array of TPPDefine;
  TIFV = (ifvTrue, ifvFalse, ifvIgnore);

  { CPreprocessor }
  CPreprocessor = class
    public
      preprocessed: TTokenizedLines;
      errors: TSimpleErrors;
      warnings: TSimpleErrors;

      procedure preprocess(lines: TTokenizedLines; incPath: string);
      function hasErrors(): boolean;
    private
      work: TTokenizedLines;
      defines: TPPDefines;
      ifstack: array of TIFV;
      includePath: string;
      procedure processInclude(tline: TTokenizedLine);
      procedure addLine(tline: TTokenizedLine);
      procedure addIncludedLine(tline: TTokenizedLine);

      procedure addError(filename:string; line:integer; message: string);
      procedure addWarning(filename:string; line:integer; message: string);

      procedure pushIF(val: TIFV);
      function  peekIF(): TIFV;
      function  popIF(): TIFV;

      function  isDefined(symbol: string): boolean;
      procedure addDefine(symbol: string; value: TToken; src: TTokenizedLine);
      function  getDefined(symbol: string): TToken;
    protected
      procedure preprocess(lines: TTokenizedLines; incPath: string; def: TPPDefines);
  end;

implementation

procedure CPreprocessor.preprocess(lines: TTokenizedLines; incPath: string);
var
  rerun: boolean = true;
  tline: TTokenizedLine;
  directive: string;
  I: integer;
begin
  includePath:=incPath;
  preprocessed:=lines;
  SetLength(ifstack, Length(ifstack)+1);
  ifstack[0]:=ifvTrue;
  while rerun do begin
    rerun := false;
    for tline in preprocessed do begin
        if length(preprocessed)>100000 then begin
           addError(tline.sourceFile, tline.lineNumber, 'Too many lines in source');
           exit;
        end;
        if (tline.tokens[0].tokenType=ttSymbol) and (tline.tokens[0].strVal[1]='#') then begin
           directive := lowercase(tline.tokens[0].strVal);
           { #endif }
           if directive = '#endif' then begin
              if Length(ifstack) = 1 then begin
                 addError(tline.sourceFile, tline.lineNumber, 'Unmatched #endif');
                 exit;
              end;
              popIF();
              continue;
           end;

           { #ifdef, #ifndef }
           if (directive = '#ifdef') or (directive = '#ifndef') then begin
              if (Length(tline.tokens) <> 2) or (tline.tokens[1].tokenType <> ttSymbol) then begin
                 addError(tline.sourceFile, tline.lineNumber, 'Invalid '+directive+' format');
                 exit;
              end;
              if peekIF() = ifvTrue then begin //a previous #if succeded, check this one
                 if directive = '#ifdef' then begin
                    if isDefined(tline.tokens[1].strVal) then begin
                       pushIF(ifvTrue);
                    end else begin
                       pushIF(ifvFalse);
                    end;
                 end else begin
                    if isDefined(tline.tokens[1].strVal) then begin
                       pushIF(ifvFalse);
                    end else begin
                       pushIF(ifvTrue);
                    end;
                 end;
              end else begin //a previous #if failed, don't eval this
                 pushIF(ifvIgnore);
              end;
              continue;
           end;

           { #else }
           if directive = '#else' then begin
              if Length(ifstack) = 1 then begin
                 addError(tline.sourceFile, tline.lineNumber, 'Unmatched #else');
                 exit;
              end;
              if peekIF() = ifvIgnore then continue;
              if peekIF() = ifvTrue then begin
                 popIF();
                 pushIF(ifvFalse);
              end else begin
                 popIF();
                 pushIF(ifvTrue);
              end;
              continue;
           end;

           if not (peekIF() = ifvTrue) then Continue;

           { #define }
           if directive = '#define' then begin
              i := Length(tline.tokens);
              if ((i<>2) and (i<>3)) or (tline.tokens[1].tokenType <> ttSymbol) then begin
                 addError(tline.sourceFile, tline.lineNumber, 'Invalid #define format');
                 exit;
              end;
              if i = 3 then begin
                 addDefine(tline.tokens[1].strVal, tline.tokens[2], tline);
              end else begin
                 addDefine(tline.tokens[1].strVal, defaultDefineValue, tline);
              end;
              Continue;
           end;

           { #include }
           if directive = '#include' then begin
              processInclude(tline);
              rerun:=true;
              Continue;
           end;

           addError(tline.sourceFile, tline.lineNumber, 'invalid preprocessor directive: ' + tline.tokens[0].strVal);
           exit;
        end;

        if (peekIF() = ifvTrue) then addLine(tline);
    end;
    preprocessed:=work;
    SetLength(work, 0);
  end;
  if Length(ifstack) <> 1 then
     addError(tline.sourceFile, tline.lineNumber, 'unmatched #if[n]def/#endif');
end;

procedure CPreprocessor.preprocess(lines: TTokenizedLines; incPath: string; def: TPPDefines);
begin
  defines := def;
  preprocess(lines, incPath);
end;

procedure CPreprocessor.processInclude(tline: TTokenizedLine);
var
   included: TStringList;
   tokenizer: CTokenizer;
   preprocessor: CPreprocessor;
   newtLine: TTokenizedLine;
   message: TSimpleError;
begin
  if length(tline.tokens) <> 2 then begin
     addError(tline.sourceFile, tline.lineNumber, 'invalid #include format (#include "filename")');
     exit;
  end;
  if tline.tokens[1].tokenType <> ttString then begin
     addError(tline.sourceFile, tline.lineNumber, 'invalid #include format, filename should be a string.');
     exit;
  end;
  included := TStringList.Create();
  try
     included.LoadFromFile(includePath + tline.tokens[1].strVal);
  except
     addError(tline.sourceFile, tline.lineNumber, 'include file not found.');
     FreeAndNil(included);
     exit;
  end;
  tokenizer := CTokenizer.Create();
  preprocessor := CPreprocessor.Create();
  tokenizer.tokenize(included.Text, tline.tokens[1].strVal);
  if tokenizer.hasErrors() then begin
     addError(tline.sourceFile, tline.lineNumber, 'errors in included file'+inttostr(high(tokenizer.errors)));
     for message in tokenizer.errors do
         addError(message.sourceFile, message.line, message.message);
  end else begin
      preprocessor := CPreprocessor.Create();
      preprocessor.preprocess(tokenizer.tokenized, includePath, defines);
      if preprocessor.hasErrors() then begin
         for message in preprocessor.errors do
            addError(message.sourceFile, message.line, message.message);
      end else begin
          for message in preprocessor.warnings do
             addWarning(message.sourceFile, message.line, message.message);
          defines := preprocessor.defines;
          for newtline in preprocessor.preprocessed do
             addIncludedLine(newtline);
      end;
  end;
  FreeAndNil(preprocessor);
  FreeAndNil(tokenizer);
  FreeAndNil(included);
end;

function CPreprocessor.popIF(): TIFV;
begin
  result := ifstack[High(ifstack)];
  SetLength(ifstack, Length(ifstack)-1);
  exit(result);
end;


function CPreprocessor.peekIF(): TIFV;
begin
  exit(ifstack[High(ifstack)]);
end;

procedure CPreprocessor.pushIF(val: TIFV);
begin
     SetLength(ifstack, Length(ifstack)+1);
     ifstack[High(ifstack)] := val;
end;

procedure CPreprocessor.addDefine(symbol: string; value: TToken; src: TTokenizedLine);
var
   i: integer;
   warn: string;
begin
     if isDefined(symbol) then begin
        warn := '"'+symbol+'" redefined, previous define: ';
        for i:=low(defines) to high(defines) do begin
            if defines[i].name = symbol then begin
               defines[i].value:=value;
               warn += defines[i].src.sourceFile+':'+inttostr(defines[i].src.lineNumber);
               addWarning(src.sourceFile, src.lineNumber, warn);
               exit;
            end;
        end;
     end;
     i := Length(defines);
     SetLength(defines, i+1);
     defines[i].name:=symbol;
     defines[i].value:=value;
     defines[i].src:=src;
end;

function CPreprocessor.isDefined(symbol: string): boolean;
var
   define: TPPDefine;
begin
     for define in defines do begin
         if define.name = symbol then begin
            exit(true);
         end;
     end;
     exit(false);
end;

function CPreprocessor.getDefined(symbol: string): TToken;
var
   define: TPPDefine;
begin
     for define in defines do begin
         if define.name = symbol then begin
            exit(define.value);
         end;
     end;
end;

procedure CPreprocessor.addIncludedLine(tline: TTokenizedLine);
begin
     SetLength(work, length(work)+1);
     work[high(work)] := tline;
end;

procedure CPreprocessor.addLine(tline: TTokenizedLine);
var
   n: integer;
   i: integer;
begin
     n := Length(work);
     SetLength(work, n+1);
     work[n] := tline;
     for i:=low(work[n].tokens) to high(work[n].tokens) do begin
         if (work[n].tokens[i].tokenType = ttSymbol) and (isDefined(work[n].tokens[i].strVal)) then begin
            work[n].tokens[i] := getDefined(work[n].tokens[i].strVal);
         end;
     end;
end;

procedure CPreprocessor.addError(filename:string; line:integer; message: string);
var
   n: integer;
begin
     n := Length(errors);
     SetLength(errors, n+1);
     errors[n].message:=message;
     errors[n].line:=line;
     errors[n].sourceFile:=filename;
end;

function CPreprocessor.hasErrors(): boolean;
begin
  exit(Length(errors)>0);
end;

procedure CPreprocessor.addWarning(filename:string; line:integer; message: string);
var
   n: integer;
begin
     n := Length(warnings);
     SetLength(warnings, n+1);
     warnings[n].message:=message;
     warnings[n].line:=line;
     warnings[n].sourceFile:=filename;
end;

end.

