unit LiteCCompiler;

interface

uses
  Classes, Types, Windows, SysUtils, ShellApi, Registry;

procedure CompileLiteC(AFile, AOptions: string; AErrors: TStrings);
function GetAcknexPath(): string;

implementation

uses
  LLCErrorConverter, ExecuteLib;

procedure RemoveNonErrorLines(AList: TStrings);
var
  LTemp: TSTringLIst;
  i: Integer;
begin
  LTemp := TStringList.Create();
  LTemp.Assign(AList);
  AList.Clear();
  for i := 0 to LTemp.Count -1 do
  begin
    if Pos('Error in line', LTemp.Strings[i]) = 1 then
    begin
      AList.Add(LTemp.Strings[i] + ' ' + LTemp.Strings[i+1]);
    end;
  end;
end;

procedure CompileLiteC(AFile, AOptions: string; AErrors: TStrings);
var
  LPath, LFile, LExec: string;
  LInfo: ShellExecuteInfo;
begin
  LPath := ExtractFilePath(AFile);
  LFile := ExtractFileName(AFile);
  LFile := ChangeFileExt(LFile, '.c');
  LFile := '"' + LPath+LFile + '"';
  Writeln(LFile);
  LExec := GetAcknexPath();
  Writeln(LExec);
  ZeroMemory(@LInfo, SizeOf(ShellExecuteInfo));
  LInfo.cbSize := SizeOf(ShellExecuteInfo);
  LInfo.lpFile := PChar(LExec);
  LInfo.lpDirectory := PChar(ExtractFilePath(LExec));
  LInfo.lpParameters := PChar(LFile + ' ' + AOptions);
  LInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
  ShellExecuteEx(@LInfo);
  WaitForSingleObject(LInfo.hProcess, INFINITE);
  MergerErrorFiles(AErrors, LPath, ExtractFilePath(LExec));
end;

function GetAcknexPath(): string;
var
  LReg: TRegistry;
  LNames: TSTringList;
  LName: string;
begin
  Result := '';
  LReg := TRegistry.Create();
  LReg.RootKey := HKEY_CURRENT_USER;
  //HKEY_CURRENT_USER\Software\Classes\Local Settings\Software\Microsoft\Windows\Shell\MuiCache
  //LReg.Access := KEY_WOW64_64KEY;
  if LReg.OpenKeyReadOnly('\Software\Classes\Local Settings\Software\Microsoft\Windows\Shell\MuiCache') then
  begin
    LNames := TStringList.Create;
    LReg.GetValueNames(LNames);
    for LName in LNames do
    begin
      if SameText(ExtractFileName(LName), 'acknex.exe') then
      begin
        Result := LName;
        Break;
      end;
    end;
    LReg.CloseKey;
    LNames.Free;
  end
  else
  begin
    raise Exception.Create('could not access MuiCache');
  end;
  LReg.Free;
end;

end.
