unit MessageHandler;

interface

uses
  Classes, Types, SysUtils, GenesisConsts, GenesisUnit, SiAuto, SmartInspect;

type
  TCompileMessageHandler = class
  public
    procedure DoMessage(AUnit:TGenesisUnit; AMessage: string; Alevel: TMessageLevel);
  end;

implementation

{ TCompileMessageHandler }

procedure TCompileMessageHandler.DoMessage(AUnit: TGenesisUnit; AMessage: string; Alevel: TMessageLevel);
var
  LMessage: string;
begin
  SiMain.EnterMethod(Self, 'DoMessage');
  LMessage := '';
  if Assigned(AUnit) then
  begin
    LMessage := AUnit.GenesisUnitName+':'+IntToStr(AUnit.GetCurrentLine)+':'+
      IntToStr(AUnit.GetCurrentPosInLine()) + ':';
  end
  else
  begin
    LMessage := 'compiler.system:0:0: ';
  end;
  if Alevel = mlError then
  begin
    Writeln(ErrOutput,LMessage + ' ' + AMessage);
  end;
  if Alevel = mlWarning then
  begin
    Writeln(LMessage + ' warning: ' + AMessage);
  end;
//  else
//  begin
    Writeln(AMessage);
//  end;
  SiMain.LogWarning(AMessage);
  SiMain.LeaveMethod(Self, 'DoMessage');
end;

end.
