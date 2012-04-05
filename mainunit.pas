unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls, Menus, DCPU16, LazHelp, SynEdit, SynCompletion,
  SynHighlighterAny;

type

  { TMain }

  TMain = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    btReset: TButton;
    btAssemble: TButton;
    btSingleStep: TButton;
    cbRunning: TCheckBox;
    cbFollow: TCheckBox;
    cbCycleExact: TCheckBox;
    LazHelp1: TLazHelp;
    LazHelpWindowedViewer1: TLazHelpWindowedViewer;
    lbLastCycles: TLabel;
    lbNextInstructionState: TLabel;
    lbMemoryDumpLabel: TLabel;
    lbDisassemblyLabel: TLabel;
    lbCPUState: TLabel;
    lbMessages: TLabel;
    lbPC: TLabel;
    lbSP: TLabel;
    lbO: TLabel;
    lbAssembly: TLabel;
    lbJ: TLabel;
    lbRegisters: TLabel;
    lbA: TLabel;
    lbB: TLabel;
    lbC: TLabel;
    lbX: TLabel;
    lbY: TLabel;
    lbZ: TLabel;
    lbI: TLabel;
    lbDisassembly: TListBox;
    lbMemoryDump: TListBox;
    MainMenu1: TMainMenu;
    mCPUBar1: TMenuItem;
    mCPUSaveProgram: TMenuItem;
    mCPULoadProgram: TMenuItem;
    mFileNew: TMenuItem;
    mFileOpen: TMenuItem;
    mFileSave: TMenuItem;
    mFileSaveAs: TMenuItem;
    MenuItem5: TMenuItem;
    mFileQuit: TMenuItem;
    mCPU: TMenuItem;
    mCPUReset: TMenuItem;
    mCPUSingleStep: TMenuItem;
    mAssembly: TMenuItem;
    mAssemblyAssemble: TMenuItem;
    mHelpBar1: TMenuItem;
    mHelpContents: TMenuItem;
    mHelpAbout: TMenuItem;
    mHelp: TMenuItem;
    mFile: TMenuItem;
    mMessages: TMemo;
    odCode: TOpenDialog;
    odProgram: TOpenDialog;
    sdProgram: TSaveDialog;
    sdCode: TSaveDialog;
    seA: TSpinEdit;
    seJ: TSpinEdit;
    seO: TSpinEdit;
    seB: TSpinEdit;
    seC: TSpinEdit;
    seX: TSpinEdit;
    seY: TSpinEdit;
    seZ: TSpinEdit;
    seI: TSpinEdit;
    sePC: TSpinEdit;
    seSP: TSpinEdit;
    mCode: TSynEdit;
    sasAssembly: TSynAnySyn;
    StringListLazHelpProvider1: TStringListLazHelpProvider;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure btAssembleClick(Sender: TObject);
    procedure btResetClick(Sender: TObject);
    procedure btSingleStepClick(Sender: TObject);
    procedure cbCycleExactChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure lbDisassemblyDblClick(Sender: TObject);
    procedure mAssemblyAssembleClick(Sender: TObject);
    procedure mCPULoadProgramClick(Sender: TObject);
    procedure mCPUResetClick(Sender: TObject);
    procedure mCPUSaveProgramClick(Sender: TObject);
    procedure mCPUSingleStepClick(Sender: TObject);
    procedure mFileNewClick(Sender: TObject);
    procedure mFileOpenClick(Sender: TObject);
    procedure mFileQuitClick(Sender: TObject);
    procedure mFileSaveAsClick(Sender: TObject);
    procedure mFileSaveClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure mHelpContentsClick(Sender: TObject);
    procedure seAChange(Sender: TObject);
    procedure seBChange(Sender: TObject);
    procedure seCChange(Sender: TObject);
    procedure seIChange(Sender: TObject);
    procedure seJChange(Sender: TObject);
    procedure seOChange(Sender: TObject);
    procedure sePCChange(Sender: TObject);
    procedure seSPChange(Sender: TObject);
    procedure seXChange(Sender: TObject);
    procedure seYChange(Sender: TObject);
    procedure seZChange(Sender: TObject);
  private
    FCPU: TCPU;
    FFileName: string;
    PrevRegValues: array [TCPURegister] of Word;
    SpinEditByReg: array [TCPURegister] of TSpinEdit;
    InstructionAddresses: array [TMemoryAddress] of Integer;
    LastKnownProgramSize: Integer;
    procedure OnMemoryChange(ASender: TObject; MemoryAddress: TMemoryAddress; var MemoryValue: Word);
    procedure OnRegisterChange(ASender: TObject; CPURegister: TCPURegister; var RegisterValue: Word);
    procedure DisassembleFrom(Address, EndAddress: TMemoryAddress);
    procedure SetFileName(const AValue: string);
    procedure SingleStep;
    procedure Reset;
    function ConfirmOk: Boolean;
  public
    property CPU: TCPU read FCPU;
    property FileName: string read FFileName write SetFileName;
  end; 

var
  Main: TMain;

procedure WriteMessage(AMsg: string);

implementation

procedure WriteMessage(AMsg: string);
begin
  if Assigned(Main) then begin
    Main.mMessages.Lines.Add(AMsg);
    Application.ProcessMessages;
  end;
end;

{$R *.lfm}

{ TMain }

procedure TMain.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  WriteMessage('Welcome');
  SpinEditByReg[crA]:=seA;
  SpinEditByReg[crB]:=seB;
  SpinEditByReg[crC]:=seC;
  SpinEditByReg[crX]:=seX;
  SpinEditByReg[crY]:=seY;
  SpinEditByReg[crZ]:=seZ;
  SpinEditByReg[crI]:=seI;
  SpinEditByReg[crJ]:=seJ;
  SpinEditByReg[crPC]:=sePC;
  SpinEditByReg[crSP]:=seSP;
  SpinEditByReg[crO]:=seO;
  {$IFDEF WINDOWS}
  lbDisassembly.Font.Name:='FixedSys';
  lbMemoryDump.Font.Name:='FixedSys';
  mMessages.Font.Name:='FixedSys';
  mCode.Font.Name:='FixedSys';
  mCode.ExtraCharSpacing:=-1;
  mCode.ExtraLineSpacing:=0;
  {$ELSE}
  lbDisassembly.Font.Name:=mCode.Font.Name;
  lbMemoryDump.Font.Name:=mCode.Font.Name;
  mMessages.Font.Name:=mCode.Font.Name;
  {$ENDIF}
  for I:=0 to $FFFF do begin
    lbMemoryDump.Items.Add(HexStr(I, 4) + ':');
  end;
  FCPU:=TCPU.Create(Self);
  CPU.OnMemoryChange:=@OnMemoryChange;
  CPU.OnRegisterChange:=@OnRegisterChange;
  LastKnownProgramSize:=$FFFF;
  Reset;
  DisassembleFrom(0, $FFFF);
  lbDisassembly.ItemIndex:=0;
end;

procedure TMain.lbDisassemblyDblClick(Sender: TObject);
begin
  if (lbDisassembly.ItemIndex >= 0) and (lbDisassembly.ItemIndex <= High(TMemoryAddress)) then
    CPU.CPURegister[crPC]:=lbDisassembly.ItemIndex;
end;

procedure TMain.mAssemblyAssembleClick(Sender: TObject);
begin
  btAssembleClick(Sender);
end;

procedure TMain.mCPULoadProgramClick(Sender: TObject);
var
  Size: Integer;
begin
  if odProgram.Execute then begin
    try
      Reset;
      Size:=CPU.LoadProgramFromFile(odProgram.FileName);
      WriteMessage('Loaded ' + IntToStr(Size) + ' words of program code');
      DisassembleFrom(0, Size);
    except
      MessageDlg('Error', 'Failed to load program code from the file ' + odProgram.FileName, mtError, [mbOK], 0);
      DisassembleFrom(0, High(TMemoryAddress));
    end;
  end;
end;

procedure TMain.mCPUResetClick(Sender: TObject);
begin
  Reset;
end;

procedure TMain.mCPUSaveProgramClick(Sender: TObject);
begin
  if FileName='' then
    sdProgram.FileName:=''
  else
    sdProgram.FileName:=ExtractFileNameWithoutExt(FileName) + '.dcpu16';
  if sdProgram.Execute then begin
    try
      CPU.SaveProgramToFile(sdProgram.FileName, LastKnownProgramSize);
      WriteMessage('Wrote ' + IntToStr(LastKnownProgramSize) + ' words of program code');
    except
      MessageDlg('Error', 'Failed to save the program to file ' + sdProgram.FileName, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TMain.mCPUSingleStepClick(Sender: TObject);
begin
  SingleStep;
end;

procedure TMain.mFileNewClick(Sender: TObject);
begin
  if ConfirmOk then begin
    mCode.ClearAll;
    mCode.ClearUndo;
    mCode.Modified:=False;
    FileName:='';
    Reset;
  end;
end;

procedure TMain.mFileOpenClick(Sender: TObject);
begin
  if not ConfirmOk then Exit;
  odCode.FileName:=FileName;
  if odCode.Execute then begin
    try
      mCode.Lines.LoadFromFile(odCode.FileName);
      FileName:=odCode.FileName;
      mCode.Modified:=False;
      Reset;
    except
      MessageDlg('Error', 'Failed to open file ' + odCode.FileName, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TMain.mFileQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMain.mFileSaveAsClick(Sender: TObject);
begin
  sdCode.FileName:=FileName;
  if sdCode.Execute then begin
    FileName:=sdCode.FileName;
    mFileSaveClick(Sender);
  end;
end;

procedure TMain.mFileSaveClick(Sender: TObject);
begin
  if FileName='' then begin
    mFileSaveAsClick(Sender);
    Exit;
  end;
  try
    mCode.Lines.SaveToFile(sdCode.FileName);
    FileName:=sdCode.FileName;
    mCode.MarkTextAsSaved;
    mCode.Modified:=False;
    Reset;
  except
    MessageDlg('Error', 'Failed to save file ' + FileName, mtError, [mbOK], 0);
  end;
end;

procedure TMain.mHelpAboutClick(Sender: TObject);
begin
  ShowMessage('DCPU-16 Studio version 20120405' + LineEnding + 'Copyright (C) 2012 by Kostas Michalopoulos' + LineEnding + LineEnding + 'Made using FreePascal, Lazarus and the SynEdit editor component.');
end;

procedure TMain.mHelpContentsClick(Sender: TObject);
begin
  LazHelpWindowedViewer1.ShowHelp;
end;

procedure TMain.seAChange(Sender: TObject);
begin
  CPU.CPURegister[crA]:=seA.Value;
end;

procedure TMain.seBChange(Sender: TObject);
begin
  CPU.CPURegister[crB]:=seB.Value;
end;

procedure TMain.seCChange(Sender: TObject);
begin
  CPU.CPURegister[crC]:=seC.Value;
end;

procedure TMain.seIChange(Sender: TObject);
begin
  CPU.CPURegister[crI]:=seI.Value;
end;

procedure TMain.seJChange(Sender: TObject);
begin
  CPU.CPURegister[crJ]:=seJ.Value;
end;

procedure TMain.seOChange(Sender: TObject);
begin
  CPU.CPURegister[crO]:=seO.Value;
end;

procedure TMain.sePCChange(Sender: TObject);
begin
  CPU.CPURegister[crPC]:=sePC.Value;
end;

procedure TMain.seSPChange(Sender: TObject);
begin
  CPU.CPURegister[crSP]:=seSP.Value;
end;

procedure TMain.seXChange(Sender: TObject);
begin
  CPU.CPURegister[crX]:=seX.Value;
end;

procedure TMain.seYChange(Sender: TObject);
begin
  CPU.CPURegister[crY]:=seY.Value;
end;

procedure TMain.seZChange(Sender: TObject);
begin
  CPU.CPURegister[crZ]:=seZ.Value;
end;

procedure TMain.btResetClick(Sender: TObject);
begin
  Reset;
end;

procedure TMain.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  Done:=False;
  if cbRunning.Checked then
    SingleStep
  else
    Sleep(1);
end;

procedure TMain.btAssembleClick(Sender: TObject);
var
  Ass: TAssembler;
  I: Integer;
  MemDump: string;
begin
  Ass:=TAssembler.Create;
  try
    WriteMessage('Assembling...');
    Ass.Assemble(mCode.Text);
    if Ass.Error then begin
      WriteMessage('Assembly error: ' + Ass.ErrorMessage);
      mCode.SelStart:=Ass.ErrorPos;
      mCode.SetFocus;
    end;
    Reset;
    MemDump:='Memory Dump:' + LineEnding + '  0000:';
    for I:=0 to Ass.Size - 1 do begin
      MemDump:=MemDump + ' ' + HexStr(Ass[I], 4);
      if (I and 7)=7 then
        MemDump:=MemDump + LineEnding + '  ' + HexStr(I + 1, 4) + ':';
      CPU[I]:=Ass[I];
    end;
    WriteMessage(MemDump);
    WriteMessage('Symbol Map:');
    for I:=0 to Ass.SymbolCount - 1 do
      WriteMessage('  ' + HexStr(Ass.Symbols[I].Address, 4) + '  ' + Ass.Symbols[I].Name);
    DisassembleFrom(0, Ass.Size);
    LastKnownProgramSize:=Ass.Size;
  except
    WriteMessage('Assembly failed due to an internal error: ' + Exception(ExceptObject).Message);
  end;
  FreeAndNil(Ass);
end;

procedure TMain.btSingleStepClick(Sender: TObject);
begin
  SingleStep;
end;

procedure TMain.cbCycleExactChange(Sender: TObject);
begin
  CPU.CycleExact:=cbCycleExact.Checked;
end;

procedure TMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=ConfirmOk;
end;

procedure TMain.OnMemoryChange(ASender: TObject; MemoryAddress: TMemoryAddress;
  var MemoryValue: Word);
begin
  lbMemoryDump.Items[MemoryAddress]:='0x' + HexStr(MemoryAddress, 4) + ' (' + Format('%05d', [MemoryAddress]) + '): ' + HexStr(MemoryValue, 4) + ' (' + Format('%05d', [MemoryValue]) + ')';
  if cbFollow.Checked then lbMemoryDump.ItemIndex:=MemoryAddress;
end;

procedure TMain.OnRegisterChange(ASender: TObject; CPURegister: TCPURegister;
  var RegisterValue: Word);
begin
  SpinEditByReg[CPURegister].Value:=RegisterValue;
  SpinEditByReg[CPURegister].Color:=clYellow;
  if CPURegister=crPC then begin
    if cbFollow.Checked and (InstructionAddresses[RegisterValue] >= 0) and (InstructionAddresses[RegisterValue] <= High(TMemoryAddress)) then
      lbDisassembly.ItemIndex:=InstructionAddresses[RegisterValue];
    if CPU.SkipInstruction then
      lbNextInstructionState.Caption:='The next will instruction will be skipped'
    else
      lbNextInstructionState.Caption:='The next will instruction will be executed';
    lbLastCycles.Caption:='Last instruction cycles: ' + IntToStr(CPU.Cycles);
  end;
end;

procedure TMain.DisassembleFrom(Address, EndAddress: TMemoryAddress);
var
  I: Integer;
  StartAddress: TMemoryAddress;
begin
  for I:=0 to High(InstructionAddresses) do InstructionAddresses[I]:=-1;
  I:=0;
  while Address <= EndAddress do begin
    InstructionAddresses[Address]:=I;
    StartAddress:=Address;
    if lbDisassembly.Count <= I then
      lbDisassembly.Items.Add(HexStr(Address, 4) + ': ' + CPU.DisassembleInstructionAt(Address))
    else
      lbDisassembly.Items[I]:=HexStr(Address, 4) + ': ' + CPU.DisassembleInstructionAt(Address);
    Inc(I);
    if StartAddress >= Address then Break;
  end;
end;

procedure TMain.SetFileName(const AValue: string);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
  if FileName='' then
    Caption:='DCPU-16 Studio'
  else
    Caption:=ExtractFileName(FileName) + ' - DCPU-16 Studio';
end;

procedure TMain.SingleStep;
var
  Reg: TCPURegister;
begin
  for Reg:=crA to crO do
    SpinEditByReg[Reg].Color:=clDefault;
  try
    CPU.RunInstruction;
  except
    on EDCPU16Exception do begin
      cbRunning.Checked:=False;
      WriteMessage('DCPU-16 Exception: ' + Exception(ExceptObject).Message);
      MessageDlg('DCPU-16 Exception', Exception(ExceptObject).Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TMain.Reset;
var
  I: TCPURegister;
begin
  WriteMessage('Resetting');
  CPU.Reset;
  for I:=crA to crO do begin
    PrevRegValues[I]:=CPU.CPURegister[I];
    SpinEditByReg[I].Color:=clDefault;
  end;
  CPU[$0000]:=$7c01;
  CPU[$0001]:=$0030;
  CPU[$0002]:=$7de1;
  CPU[$0003]:=$1000;
  CPU[$0004]:=$0020;
  CPU[$0005]:=$7803;
  CPU[$0006]:=$1000;
  CPU[$0007]:=$c00d;
  CPU[$0008]:=$7dc1;
  CPU[$0009]:=$001a;
  CPU[$000A]:=$a861;
  CPU[$000B]:=$7c01;
  CPU[$000C]:=$2000;
  CPU[$000D]:=$2161;
  CPU[$000E]:=$2000;
  CPU[$000F]:=$8463;
  CPU[$0010]:=$806d;
  CPU[$0011]:=$7dc1;
  CPU[$0012]:=$000d;
  CPU[$0013]:=$9031;
  CPU[$0014]:=$7c10;
  CPU[$0015]:=$0018;
  CPU[$0016]:=$7dc1;
  CPU[$0017]:=$001a;
  CPU[$0018]:=$9037;
  CPU[$0019]:=$61c1;
  CPU[$001A]:=$7dc1;
  CPU[$001B]:=$001a;
  CPU[$001C]:=$0000;
  CPU[$001D]:=$0000;
  CPU[$001E]:=$0000;
  CPU[$001F]:=$0000;
  cbRunning.Checked:=False;
  if lbDisassembly.Items.Count > 0 then lbDisassembly.ItemIndex:=0;
  lbMemoryDump.ItemIndex:=0;
  LastKnownProgramSize:=$FFFF;
  WriteMessage('Reset done');
end;

function TMain.ConfirmOk: Boolean;
begin
  if not mCode.Modified then Exit(True);
  Result:=MessageDlg('Modified', 'The code has been modified. If you continue you will lose the modifications. Do you really want to continue and lose them?', mtConfirmation, mbYesNo, 0)=mrYes;
end;

end.
