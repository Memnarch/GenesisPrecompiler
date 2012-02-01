unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SiAuto, SmartInspect, Menus, StdCtrls, ComCtrls, GenesisCompiler,
  GenesisConsts, GenesisUnit,
  ActnList, AboutUnit;

type
  TForm1 = class(TForm)
    LogEdit: TRichEdit;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    ActionList1: TActionList;
    OpenAction: TAction;
    OpenDialog1: TOpenDialog;
    AboutAction: TAction;
    About1: TMenuItem;
    procedure OpenActionExecute(Sender: TObject);
    procedure AboutActionExecute(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    procedure HandleMessage(AUnit: TGenesisUnit; AMessage: string; Alevel: TMessageLevel);
  end;

var
  Form1: TForm1;

implementation
{$R *.dfm}

procedure TForm1.AboutActionExecute(Sender: TObject);
begin
  SiMain.EnterMethod(Self, 'AboutActionExecute');
  AboutForm.ShowModal();
  SiMain.LeaveMethod(Self, 'AboutActionExecute');
end;

procedure TForm1.HandleMessage(AUnit: TGenesisUnit; AMessage: string; Alevel: TMessageLevel);
begin
  SiMain.EnterMethod(Self, 'HandleMessage');
  LogEdit.SelStart := Length(LogEdit.Text);
  case Alevel of
    mlNormal:
    begin
      LogEdit.SelAttributes.Color := clBlack;
    end;

    mlError:
    begin
      LogEdit.SelAttributes.Color := clRed;
    end;

    mlSuccess:
    begin
      LogEdit.SelAttributes.Color := clGreen;
    end;
  end;
  SiMain.LogString('Message', AMessage);
  LogEdit.Lines.Add(AMessage);
  SiMain.LeaveMethod(Self, 'HandleMessage');
end;

procedure TForm1.OpenActionExecute(Sender: TObject);
var
  LGenesis: TGenesisCompiler;
begin
  SiMain.EnterMethod(Self, 'OpenActionExecute');
  if OpenDialog1.Execute() then
  begin
    LogEdit.Clear();
    LGenesis := TGenesisCompiler.Create();
    LGenesis.OnMessage := HandleMessage;
    LGenesis.BasePath := ExtractFilePath(OpenDialog1.FileName);
    LGenesis.CompileFileEx(OpenDialog1.FileName);
    LGenesis.Free();
  end;
  SiMain.LeaveMethod(Self, 'OpenActionExecute');
end;

initialization
  Si.Enabled := True;

end.
