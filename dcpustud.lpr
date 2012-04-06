program dcpustud;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainUnit, DCPU16, lazhelppackage, UserScreenUnit;

{$R *.res}
{$IFDEF WINDOWS}
{$R fontdata.rc}
{$ENDIF}

begin
  Application.Title:='DCPU-16 Studio';
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.CreateForm(TUserScreen, UserScreen);
  Application.Run;
end.
