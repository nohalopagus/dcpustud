unit UserScreenUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TUserScreen }

  TUserScreen = class(TForm)
    pbScr: TPaintBox;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure pbScrPaint(Sender: TObject);
  private
  public
  end;

var
  UserScreen: TUserScreen;

implementation

uses
  MainUnit, LCLType;

{$R *.lfm}

{ TUserScreen }

procedure TUserScreen.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_LEFT: Main.KeyWasTyped(#1);
    VK_RIGHT: Main.KeyWasTyped(#2);
    VK_UP: Main.KeyWasTyped(#3);
    VK_DOWN: Main.KeyWasTyped(#4);
    VK_ESCAPE: Main.KeyWasTyped(#27);
  end;
end;

procedure TUserScreen.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key in [#32..#127] then Main.KeyWasTyped(Key);
end;

procedure TUserScreen.pbScrPaint(Sender: TObject);
begin
  Main.pbScreenPaint(Sender);
end;

end.

