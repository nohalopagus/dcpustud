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
  { CPreprocessor }
  CPreprocessor = class
    public
      preprocessed: TTokenizedLines;
      errors: array of TPreprocessorError;
      procedure preprocess(lines: TTokenizedLines);
    private
      work: array of TTokenizedLine;
      procedure processInclude(tline: TTokenizedLine);
      procedure addLine(tline: TTokenizedLine);

      procedure addError(filename:string; line:integer; message: string);
  end;

implementation

procedure CPreprocessor.preprocess(lines: TTokenizedLines);
var
  rerun: boolean = true;
  tline: TTokenizedLine;
begin
  preprocessed:=lines;
  while rerun do begin
    rerun := false;
    for tline in preprocessed do begin
        if length(preprocessed)>100000 then begin
           addError('Too many lines in source', 0, 'pp');
           exit;
        end;
        if (tline.tokens[0].tokenType=ttSymbol) and (tline.tokens[0].strVal[1]='#') then begin
           if lowercase(tline.tokens[0].strVal)='#include' then begin
              processInclude(tline);
              rerun:=true;
              Continue;
           end else begin
               addError('Invalid preprocessor directive: ' + tline.tokens[0].strVal, tline.lineNumber, tline.sourceFile);
               exit;
           end;
        end;
        addLine(tline);
    end;
    preprocessed:=work;
  end;
end;

procedure CPreprocessor.processInclude(tline: TTokenizedLine);
var
   included: TStringList;
   tokenizer: CTokenizer;
   newtLine: TTokenizedLine;
   tError: TTokenizerError;
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
  tokenizer.tokenize(included.Text, tline.tokens[1].strVal);
  if high(tokenizer.errors) <> 0 then begin
     for tError in tokenizer.errors do
         addError(tError.sourceFile, tError.line, tError.message);
  end else begin
     for newtline in tokenizer.tokenized do
         addLine(newtline);
  end;
  FreeAndNil(tokenizer);
  FreeAndNil(included);
end;

procedure CPreprocessor.addLine(tline: TTokenizedLine);
begin
     SetLength(work, length(work)+1);
     work[high(work)] := tline;
end;

procedure CPreprocessor.addError(filename:string; line:integer; message: string);
begin
     SetLength(errors, Length(errors)+1);
     errors[High(errors)].message:=message;
     errors[High(errors)].line:=line;
     errors[High(errors)].sourceFile:=filename;
end;

end.

