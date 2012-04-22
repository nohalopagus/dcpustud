unit DPreprocessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DTokenizer, DCPUtypes;
type
  TPreprocessorError = record
    message: string;
    line: integer;
    sourceFile: string;
  end;
  TPreprocessorErrors = array of TPreprocessorError;

  TPPDefine = record
    name: string;
    value: TToken;

    src: TTokenizedLine;
  end;
  TPPDefines = array of TPPDefine;

  { CPreprocessor }
  CPreprocessor = class
    public
      preprocessed: TTokenizedLines;
      errors: TPreprocessorErrors;
      warnings: TPreprocessorErrors;
      procedure preprocess(lines: TTokenizedLines);
    private
      work: TTokenizedLines;
      defines: TPPDefines;
      ifstack: array of boolean;
      procedure processInclude(tline: TTokenizedLine);
      procedure addLine(tline: TTokenizedLine);
      procedure addIncludedLine(tline: TTokenizedLine);

      procedure addError(filename:string; line:integer; message: string);
      procedure addWarning(filename:string; line:integer; message: string);

      procedure pushIF(val: Boolean);
      function  peekIF(): boolean;
      function  popIF(): boolean;

      function  isDefined(symbol: string): boolean;
      procedure addDefine(symbol: string; value: TToken; src: TTokenizedLine);
      function  getDefined(symbol: string): TToken;
    protected
      procedure preprocess(lines: TTokenizedLines; def: TPPDefines);
  end;

implementation

procedure CPreprocessor.preprocess(lines: TTokenizedLines);
var
  rerun: boolean = true;
  tline: TTokenizedLine;
  directive: string;
  I: integer;
  found: Boolean;
  defaultValue: TToken;
begin
  defaultValue.intVal := 1;
  defaultValue.length := 0;
  defaultValue.linePos:= 0;
  defaultValue.strVal := '1';
  defaultValue.tokenType:=ttSymbol;

  preprocessed:=lines;
  SetLength(ifstack, Length(ifstack)+1);
  ifstack[0]:=true;
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
                 addError(tline.sourceFile, tline.lineNumber, 'Invalid #ifdef format');
                 exit;
              end;
              if peekIF() then begin //a previous #if succeded, check this one
                 if directive = '#ifdef' then begin
                    pushIF(isDefined(tline.tokens[1].strVal));
                 end else begin
                    pushIF(not isDefined(tline.tokens[1].strVal));
                 end;
              end else begin //a previous #if failed, don't eval this
                 pushIF(false);
              end;
              continue;
           end;

           if not peekIF() then Continue;

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
                 addDefine(tline.tokens[1].strVal, defaultValue, tline);
              end;
              Continue;
           end;

           { #include }
           if directive = '#include' then begin
              processInclude(tline);
              rerun:=true;
              Continue;
           end;

           addError(tline.sourceFile, tline.lineNumber, 'Invalid preprocessor directive: ' + tline.tokens[0].strVal);
           exit;
        end;

        if peekIF() then addLine(tline);
    end;
    preprocessed:=work;
  end;
end;

procedure CPreprocessor.preprocess(lines: TTokenizedLines; def: TPPDefines);
begin
  defines := def;
  preprocess(lines);
end;

procedure CPreprocessor.processInclude(tline: TTokenizedLine);
var
   included: TStringList;
   tokenizer: CTokenizer;
   preprocessor: CPreprocessor;
   newtLine: TTokenizedLine;
   tError: TTokenizerError;
   nError: TPreprocessorError;
   nWarning: TPreprocessorError;
begin
  if length(tline.tokens) <> 2 then begin
     addError(tline.sourceFile, tline.lineNumber, 'Invalid #include format (#include "filename")');
     exit;
  end;
  if tline.tokens[1].tokenType <> ttString then begin
     addError(tline.sourceFile, tline.lineNumber, 'Invalid #include format, filename should be a string.');
     exit;
  end;
  included := TStringList.Create();
  try
     included.LoadFromFile(tline.tokens[1].strVal);
  except
     addError(tline.sourceFile, tline.lineNumber, 'Include file not found.');
     FreeAndNil(included);
     exit;
  end;

  tokenizer := CTokenizer.Create();
  preprocessor := CPreprocessor.Create();
  tokenizer.tokenize(included.Text, tline.tokens[1].strVal);
  if high(tokenizer.errors) <> 0 then begin
     for tError in tokenizer.errors do
         addError(tError.sourceFile, tError.line, tError.message);
  end else begin
      preprocessor := CPreprocessor.Create();
      preprocessor.preprocess(tokenizer.tokenized, defines);
      if high(preprocessor.errors) <> 0 then begin
         for nError in preprocessor.errors do
            addError(nError.sourceFile, nError.line, nError.message);
      end else begin
          for nWarning in preprocessor.warnings do
             addWarning(nWarning.sourceFile, nWarning.line, nWarning.message);
          for newtline in preprocessor.preprocessed do
             addIncludedLine(newtline);
      end;
  end;
  FreeAndNil(preprocessor);
  FreeAndNil(tokenizer);
  FreeAndNil(included);
end;

function CPreprocessor.popIF(): boolean;
begin
  result := ifstack[High(ifstack)];
  SetLength(ifstack, Length(ifstack)-1);
  exit(result);
end;


function CPreprocessor.peekIF(): boolean;
begin
  exit(ifstack[High(ifstack)]);
end;

procedure CPreprocessor.pushIF(val: Boolean);
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
