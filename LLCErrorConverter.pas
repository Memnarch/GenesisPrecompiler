unit LLCErrorConverter;

interface

uses
  Classes, Types, Windows, SysUtils, GenesisCompiler, SiAuto, SmartInspect;

  procedure LLCToGenesisError(ACompiler: TGenesisCompiler; AInput, AOutPut: TStrings);
  procedure MergerErrorFiles(AList: TStrings; AErrPath, ALogPath: string);

implementation

uses
  StrUtils, GenesisUnit;

procedure SplitLCError(AInput: string; var AFile, AMessage, AErrObject: string; var ALine: Integer);
var
  LPos, LPosB: Integer;
begin
  LPos := Pos('''', AInput);
  LPosB := Pos(':', AInput);
  AFile := '';
  AMessage := '';
  ALine := 0;
  if (LPos >= 1) and (LPos < LPosB) then
  begin
    LPosB := PosEx('''', AInput, LPos+1);
    AFile := Copy(AInput, LPos+1, LPosB-LPos);
  end;
  LPos := Pos('line', AInput);
  if LPos >= 1 then
  begin
    LPosB := PosEx(':', AInput, LPos+1);
    ALine := STrToInt(Trim(Copy(AInput, LPos+5, LPosB-(LPos+5))));
    AMessage := Copy(AInput, LPosB + 1, Length(AInput));
    LPos := PosEx('''', AInput, LPosB);
    if LPos > LPosB then
    begin
      LPosB := PosEx('''', AInput, LPos+1);
      AErrObject := Copy(AInput, LPos+1, LPosB-LPos-1);
    end;
  end;

end;

procedure LLCToGenesisError(ACompiler: TGenesisCompiler; AInput, AOutPut: TStrings);
var
  LFile, LMessage, LLCError, LErrObject: string;
  LLine, LOld: Integer;
begin
  for LLCError in AInput do
  begin
    SplitLCError(LLCError, LFile, LMessage, LErrObject, LLine);
    if LFile = '' then
    begin
      LFile := ACompiler.UnitList.Items[1].GenesisUnitName;
    end
    else
    begin
      LFile := ChangeFileExt(LFile, '.cxx');
    end;
    LOld := LLine;
    LLine := ACompiler.GetLineForLCLine(LFile, LErrObject, LLine);
    AOutput.Add(LFile+':'+IntToSTr(LLine) + ':0: LiteCError: ' + Trim(LMessage));
  end;
end;

procedure MergerErrorFiles(AList: TStrings; AErrPath, ALogPath: string);
var
  LLog, LErr: TStringList;
  LLogStart, i: Integer;
begin
  SiMain.EnterMethod('MergerErrorFiles');
  LLogStart := 0;
  SiMain.LogString('ErrPath', AErrPath);
  SiMain.LogString('LogPath', ALogPath);
  if FileExists(ALogPath + '\acklog.txt') and (FileExists(AErrPath + '\ackerr.txt')) then
  begin
    LErr := TStringList.Create();
    LLog := TStringList.Create();
    LErr.LoadFromFile(AErrPath + '\ackerr.txt');
    LLog.LoadFromFile(ALogPath + '\acklog.txt');
    SiMain.LogStringList('errlist', LErr);
    SiMain.LogStringList('log', LLog);
    for i := 0 to LLog.Count - 1 do
    begin
      if Pos('Compiling', LLog.Strings[i]) = 1 then
      begin
        LLogStart := i+2;
      end;
    end;
    SiMain.LogInteger('LogStart', LLogStart);
    for i := 0 to LErr.Count - 1 do
    begin
      AList.Add(LErr.Strings[i] + ' ' + LLog.Strings[LLogStart]);
      Inc(LLogStart, 2);
    end;
    SiMAin.LogStringList('output', AList);
    DeleteFile(AErrPath + '\ackerr.txt');
    DeleteFile(ALogPath + '\acklog.txt')
  end
  else
  begin
    SiMain.LogWarning('ackerr.txt or acklog.txt does not exists');
  end;
  SiMain.LeaveMethod('MergerErrorFiles');
end;

end.
