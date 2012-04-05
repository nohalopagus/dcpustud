program dcpustud;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainUnit, DCPU16, lazhelppackage
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='DCPU-16 Studio';
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.

