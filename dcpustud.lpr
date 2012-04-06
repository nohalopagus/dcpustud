program dcpustud;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainUnit, DCPU16, lazhelppackage;

{$R *.res}
{$R fontdata.rc}

begin
  Application.Title:='DCPU-16 Studio';
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.

