program GPCC;

{$APPTYPE CONSOLE}

uses
  classes,
  types,
  Windows,
  SysUtils,
  StrUtils,
  GenesisCompiler,
  GenesisConsts,
  LiteCCompiler,
  SiAuto, SmartInspect,
  Shellapi,
  LLCErrorConverter,
  MessageHandler in 'MessageHandler.pas';

var
  i: Integer;
  LFile, LPath: string;
  LExec, LError: string;
  LCompiler: TGenesisCompiler;
  LHandler: TCompileMessageHandler;
  LCommands, LLCErrors, LLLCGenErrors: TStringList;
begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    LLCErrors := TSTringList.Create();
    if (ParamCount = 0) then
    begin
      Writeln('Genesis Precompiler V0.4 28th January 2012');
      WriteLn('by Rackscha alias Memnarch');
      WriteLn('');
      WriteLn('Current Path to Acknex: ' + GetAcknexPath());
      WriteLn('');
      WriteLn('You can use the following parameters:');
      WriteLn('');
      WriteLn('-c <FileName>  Specifies the .cxx file you want to compile');
      WriteLn('-o <ExeName> <FileName>  Saves the compiler with the name <ExeName> and writes');
      WriteLn('   a config.txt which contains the file <filename>');
      WriteLn('-run runs the Acknex.exe with the .c file noted in config.txt');
      WriteLn('');
      WriteLn('Currently only ONE parameter at a time');
      WriteLn('');
      WriteLn('Press Enter to exit');
      ReadLn(LExec);
      Exit;
    end;
    for i := 0 to ParamCount do
    begin
      SiMain.LogColored($0000dd, ParamStr(i));
    end;

    for i := 0 to ParamCount do
    begin
      Writeln(ParamStr(i));
      if SameText('-run', ParamStr(i)) then
      begin
        if FileExists(ExtractFilePath(ParamStr(0)) + '\config.txt') then
        begin
          LCommands := TStringList.Create();
          LCommands.LoadFromFile(ExtractFilePath(ParamStr(0)) + '\config.txt');
          Writeln('Running acknex.exe');
          CompileLiteC(Trim(LCommands.Text), '', LLCErrors);
          LCommands.Free();
        end;
        Break;
      end;
      if SameText(ParamStr(i), '-c') then
      begin
        LHandler := TCompileMessageHandler.Create();
        LCompiler := TGenesisCompiler.Create();
        LCompiler.OnMessage := LHandler.DoMessage;
        LFile := ParamStr(i+1);
        LCompiler.CompileFileEx(LFile);
        if LCompiler.Errors = 0 then
        begin
          LLLCGenErrors := TStringList.Create;
          CompileLiteC(LFile, ' -cc -eq -diag', LLCErrors);
          LLCToGenesisError(LCompiler, LLCErrors, LLLCGenErrors);
          for LError in LLLCGenErrors do
          begin
            Writeln(ErrOutput, LError);
          end;
          LLLCGenErrors.Free;
        end;
        ExitCode := LCompiler.Errors;
        LCompiler.Free;
        LHandler.Free;
        Break;
      end
      else
      begin
        if SameText('-o', ParamStr(i)) then
        begin
          CopyFile(PChar(ParamStr(0)), PChar(ParamStr(i+1)), False);
          LCommands := TStringList.Create;
          LFile := ChangeFileExt(ExtractFileName(ParamStr(i+2)), '.c');
          LCommands.Add(GetCurrentDir() + '\' + LFile);
          LCommands.SaveToFile(ExtractFilePath(ParamStr(i+1)) + '\config.txt');
          LCommands.Free();
        end;
      end;
    end;
  LLCErrors.Free();
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
