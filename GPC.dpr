program GPC;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  GenesisCompiler in 'GenesisCompiler.pas',
  GenesisUnit in 'GenesisUnit.pas',
  GenesisClass in 'GenesisClass.pas',
  GenesisConsts in 'GenesisConsts.pas',
  AboutUnit in 'AboutUnit.pas' {AboutForm},
  LiteCCompiler in 'LiteCCompiler.pas',
  LLCErrorConverter in 'LLCErrorConverter.pas',
  GenSourceObject in 'GenSourceObject.pas',
  ExecuteLib in 'ExecuteLib.pas';

{$R *.res}

begin
//  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Genesis Prcompiler';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.Run;
end.
