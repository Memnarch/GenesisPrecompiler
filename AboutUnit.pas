unit AboutUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TAboutForm = class(TForm)
    lbTitle: TLabel;
    Label2: TLabel;
    lbProgrammer: TLabel;
    lbLastModifiedVersion: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  AboutForm: TAboutForm;

implementation

uses
  GenesisConsts;

{$R *.dfm}

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  lbTitle.Caption := CGenTitle;
  lbLastModifiedVersion.Caption := 'v'+CGenVersion + ' ' + CGenLastModified;
  lbProgrammer.Caption := CGenProgrammer;
  lbTitle.Left := (Width-lbTitle.Width) div 2;
  lbLastModifiedVersion.Left := (Width-lbLastModifiedVersion.Width) div 2;
  lbProgrammer.Left := (Width-lbProgrammer.Width) div 2;
end;

end.
