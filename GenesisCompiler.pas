unit GenesisCompiler;

interface
uses
  Classes, Types, SysUtils, Windows, Generics.Collections, GenesisUnit, GenesisClass, GenSourceObject,
  GenesisConsts, SiAuto, SmartInspect;

type

  TCompilerRuleSet = set of (crsAllowMethod, crsAllowMethodDummy);

  TCompileEvent = procedure(AUnit: TGenesisUnit; AMessage: string; ALevel: TMessageLevel) of Object;
  TCompilerObjectHandler = function(AUnit: TGenesisUnit; AObject: TObject; var AErrorMessage: string;
                            AAccessLevel: TAccessLevel = alPublic): Boolean of object;
  TParseProcedure = procedure(AUnit: TGenesisUnit; AParent: TGenSourceObject) of object;

  TCompilerObject = class(TObject)
  private
    FName: string;
    FObjectHandler: TCompilerObjectHandler;
  public
    property Name: string read FName write FName;
    property ObjectHandler: TCompilerObjectHandler read FObjectHandler write FObjectHandler;
  end;

  TGenesisCompiler = class(TObject)
  private
    FDataTypes: TStringList;
    FObjectHandlers: TObjectList<TCompilerObject>;
    FUnitList: TObjectList<TGenesisUnit>;
    FOnMessage: TCompileEvent;
    FExpectedChars: TAnsiCharSet;
    FBasePath: string;
    FBaseUnit: string;
    FBaseClass: string;
    FSystemUnit: string;
    FBaseCompiled: Boolean;
    FErrors: Integer;
    FCurrentUnit: TGenesisUnit;
    procedure SetupBasicDataTypes();
    procedure SetupBasicObjectHandlers();
    procedure CompileLog(AMessage: string; ALevel: TMessageLevel = mlNormal);
    procedure SetOnMessage(const Value: TCompileEvent);
    procedure ExpectDataType(AWord: string);
    procedure ExpectClassType(AClass: string; AMessage: string = '');
    procedure ExpectSameIdentifier(AName, ANameB: string; AMessage: string = '');
    function GetClosedTextBlock(AUnit: TGenesisUnit; AOpenChar, ACloseChar: Char): string;
    function IsDataType(AWord: string; ACaseSensitive: Boolean = True): boolean;
    function IsClass(AWord: string; ACaseSensitive: Boolean = True): boolean;
    function GetObjectHandler(AWord: String): TCompilerObject;
    function HandleClassObject(AUnit: TGenesisUnit; AObject: TObject; var AErrorMessage: string; AAccessLevel: TAccessLevel = alPublic): Boolean;
    function HandleIncludeObject(AUnit: TGenesisUnit; AObject: TObject; var AErrorMessage: string; AAccessLevel: TAccessLevel = alPublic): Boolean;
    function HandleDataTypeObject(AUnit: TGenesisUnit; AObject: TObject; var AErrorMessage: string; AAccessLevel: TAccessLevel = alPublic): Boolean;
    function HandleVirtualMethodObject(AUnit: TGenesisUnit; AObject: TObject; var AErrorMessage: string; AAccessLevel: TAccessLevel = alPublic): Boolean;
    function HandleTypeDefObject(AUnit: TGenesisUnit; AObject: TObject; var AErrorMessage: string; AAccessLevel: TAccessLevel = alPublic): Boolean;
    function HandleEnumObject(AUnit: TGenesisUnit; AObject: TObject; var AErrorMessage: string; AAccessLevel: TAccessLevel = alPublic): Boolean;
    function ProcessFieldVarDeclaration(AUnit: TGenesisUnit; AClass: TGenSourceObject;
      AFirstName, ADataType, APostChars: string; var AErrorMessage: string; AExpectDelimiter: Boolean = True;
      AAccessLevel: TAccessLevel = alPrivate): Boolean;
    function ProcessMethodDummyDeclaration(AUnit: TGenesisUnit; AClass: TGenSourceObject; AName,
      ADataType, APostChars: string; AIsVirtual, AIsAbstract: Boolean; var AErrorMessage: string; AAccessLevel: TAccessLevel = alPrivate): Boolean;
    function ProcessMethodSource(AUnit: TGenesisUnit; AParent: TGenSourceObject): string;
    function ProcessForEach(AUnit: TGenesisUnit; AParent: TGenSourceObject): string;
    function ProcessNewCall(AUnit: TGenesisUnit; AFrom: TGenSourceObject): string;
    function GetUnitByName(AName: string): TGenesisUnit;
    function GetClassByName(AName: string): TGenesisClass;
    function GetGlobalElement(AIdentifier: string; AType: TClass): TGenSourceObject;
    function GetSimilarType(AName: string): string;
    function GetSimilarClass(AName: string): string;
    function ResolveIdentifier(AUnit:TGenesisUnit; AParent: TGenSourceObject; AIdentifier: string; var ACallString: string;
      var AMethodDummy: TGenMethodDummyDeclaration): Boolean;
    procedure ProcessValueDeclaration(AUnit: TGenesisUnit;
      var ADataType, APostChars, AName: string);
    procedure ErrorUndeclaredIdentifier(AIdentifier: string);
    procedure ProcessConDestructorDeclaration(AUnit: TGenesisUnit; AParent: TGenSourceObject);
    procedure ProcessStructDeclaration(AUnit: TGenesisUnit; AParent: TGenSourceObject);
    procedure ProcessMethodFunctionDeclaration(AUnit: TGenesisUnit; ATypeName, APostChars, AIdentifier: string);
    procedure ProcessFunctionParameters(AUnit: TGenesisUnit; AFrom: TGenSourceObject);
    procedure SetExpectedChars(const Value: TAnsiCharSet);
    procedure CompileBase();
    procedure AccessError(AIdentifier: string; AAccessLevel: TAccessLevel);
    function RemoveOneLineComments(ASource: string): string;
    function RemoveMultiLineComments(ASource: string): string;
    function RemoveAllComments(ASource: string): string;
  public
    constructor Create();
    destructor Destroy(); override;
    function GetLineForLCLine(AUnit, AErrObject: string; ALine: Integer): Integer;
    procedure CompileFileEx(AFileName: String; ACatchException: Boolean = True);
    procedure CompileUnit(AUnit: TGenesisUnit; ACatchException: Boolean = True);
    property OnMessage: TCompileEvent read FOnMessage write SetOnMessage;
    property ExpectedChars: TAnsiCharSet read FExpectedChars write SetExpectedChars;
    property BasePath: string read FBasePath write FBasePath;
    property BaseClass: string read FBaseClass write FBaseClass;
    property BaseUnit: string read FBaseUnit write FBaseUnit;
    property SystemUnit: string read FSystemUnit write FSystemUnit;
    property BaseCompiled: Boolean read FBaseCompiled;
    property Errors: Integer read FErrors;
    property CurrentUnit: TGenesisUnit read FCurrentUnit;
    property UnitList:TObjectList<TGenesisUnit> read FUnitList;
  end;




implementation
uses
  StrUtils;

{ TGenesisCompiler }



procedure TGenesisCompiler.AccessError(AIdentifier: string;
  AAccessLevel: TAccessLevel);
var
  LMessage: string;
begin
  LMessage := 'Can not access ';
  case AAccessLevel of
    alPrivate:
    begin
      LMessage := LMessage + 'private ';
    end;
    alProtected:
    begin
      LMessage := LMessage + 'protected ';
    end;
    alPublic:
    begin
      LMessage := LMessage + 'public ';
    end;
  end;
  LMessage := LMessage + 'member ' + QuotedStr(AIdentifier);
  CompileLog(LMessage, mlError);
end;

procedure TGenesisCompiler.CompileBase;
var
  LUnit: TGenesisUnit;
  LOldPath: string;
begin
  SiMain.EnterMethod(Self, 'CompileBase');
  FBaseCompiled := True;
  if FileExists(FBasePath + FSystemUnit) then
  begin
    SiMain.LogString('compiling from', FBasePath+FSystemUnit);
    CompileFileEx(FBasePath + FSystemUnit, False);
    FBaseCompiled := True;
  end
  else
  begin
    if FileExists(ExtractFilePath(ParamStr(0)) +  FSystemUnit) then
    begin
      SiMain.LogString('compiling from', ExtractFilePath(ParamStr(0)) + FSystemUnit);
      LOldPath := FBasePath;
      FBasePath := ExtractFilePath(ParamStr(0));
      CompileFileEx(ExtractFilePath(ParamStr(0)) + FSystemUnit, False);
      FBasePath := LOldPath;
      for LUnit in FUnitList do
      begin
        CopyFile(PChar(ExtractFilePath(ParamStr(0)) + ChangeFileExt(LUnit.GenesisUnitName, '.c')),
          PChar(FBasePath + ChangeFileExt(LUnit.GenesisUnitName, '.c')), False);
      end;
      FBaseCompiled := True;
    end
    else
    begin
      raise Exception.Create('Base unit "' + FSystemUnit + '" not found');
    end;
  end;
  SiMain.LeaveMethod(Self, 'CompileBase');
end;

procedure TGenesisCompiler.CompileFileEx(AFileName: String; ACatchException: Boolean = true);
var
  LStringList: TStringList;
  LUnit, LExistingUnit: TGenesisUnit;
  LName: string;
begin
  SiMain.EnterMethod(Self, 'CompileFileEx');
  LStringList := TStringList.Create();
  LUnit := TGenesisUnit.Create();
  try
    if (not FBaseCompiled) and (not SameText(ExtractFileName(AFileName), FSystemUnit)) then
    begin
      CompileBase();
      if not FBaseCompiled then
      begin
        exit;
      end;
    end;


    LUnit.GenesisUnitName := ExtractFileName(AFileName);
    LExistingUnit := GetUnitByName(LUnit.GenesisUnitName);
    if Assigned(LExistingUnit) then
    begin
      if LExistingUnit.UnitState = usCompiling then
      begin
        raise Exception.Create('Recursive include of "' + LUnit.GenesisUnitName + '"');
      end
      else
      begin
        exit;
      end;
    end;
    LStringList.LoadFromFile(AFileName);
    LUnit.SourceCode :=RemoveAllComments(LStringList.Text);
    FUnitList.Add(LUnit);
    CompileUnit(LUnit, ACatchException);
    if LUnit.RequiresBaseInclude then
    begin
      if not SameText(LUnit.GenesisUnitName, FBaseUnit) then
      begin
        LUnit.AddIncludeAtTop(ChangeFileExt(FBaseUnit, '.c'));
      end;
    end;
    LUnit.Finalize();
    LStringList.Text := LUnit.GetLiteCSource();
    LName := StringReplace(LUnit.GenesisUnitName, CExtension, '', [rfReplaceAll, rfIgnoreCase]);
    LStringList.SaveToFile(ExtractFilePath(AFileName) + LName + '.c');
  except
    on E: Exception do
    begin
      if ACatchException then
      begin
        CompileLog(E.Message, mlError);
      end
      else
      begin
        raise;
      end;
    end;
  end;
  LStringList.Free();
  SiMain.LeaveMethod(Self, 'CompileFileEx');
end;

procedure TGenesisCompiler.CompileLog(AMessage: string; ALevel: TMessageLevel);
begin
  SiMain.EnterMethod(Self, 'CompileLog');
  if Assigned(FOnMessage) then
  begin
    FOnMessage(FCurrentUnit,AMessage, ALevel);
  end;
  if ALevel = mlError then
  begin
    Inc(FErrors);
  end;
  SiMain.LeaveMethod(Self, 'CompileLog');
end;

procedure TGenesisCompiler.CompileUnit(AUnit: TGenesisUnit; ACatchException: Boolean = True);
var
  LChar: Char;
  LWord, LErrorMessage: string;
  FSucces: Boolean;
  LCompilerObject: TCompilerObject;
  LPreviousUnit: TGenesisUnit;
begin
  SiMain.EnterMethod(Self, 'CompileUnit');
  CompileLog('Compiling Unit: ' + AUnit.GenesisUnitName);
  LPreviousUnit := CurrentUnit;
  FCurrentUnit := AUnit;
  FSucces := True;
  SiMain.LogInteger('SourceLength', Length(AUnit.SourceCode));
  try
    while not AUnit.IsEOF do
    begin
      FExpectedChars := CAlphaChars + CNumericChars + CUnderScore + ['#'];
      LChar := AUnit.GetNextVisibleChar();
      if CharInSet(LChar, FExpectedChars) then
      begin
        AUnit.Position := AUnit.Position - 1;
        LWord := AUnit.GetNextWord;
        LCompilerObject := GetObjectHandler(LWord);
        if Assigned(LCompilerObject) then
        begin
          if not LCompilerObject.FObjectHandler(AUnit, nil, LErrorMessage) then
          begin
            CompileLog(LErrorMessage, mlError);
            FSucces := False;
            Break;
          end;
        end
        else
        begin
          ExpectDataType(LWord);
          AUnit.Position := AUnit.Position - Length(LWord);
          if not HandleDataTypeObject(AUnit, nil, LErrorMessage) then
          begin
            CompileLog(LErrorMessage, mlError);
            FSucces := False;
            Break;
          end;
        end;
      end
      else
      begin
        CompileLog('Unexpected Char "' + string(LChar) + '"' , mlError);
        AUnit.Position := AUnit.Position - 1;
        FSucces := False;
        break;
      end;
      SiMain.LogCardinal('Position', AUnit.Position);
    end;
  except
    on E: Exception do
    begin
      FSucces := False;
      if ACatchException then
      begin
        CompileLog(E.Message, mlError);
      end
      else
      begin
        raise;
      end;
    end;
  end;


  if FSucces then
  begin
    CompileLog('Finished: ' + AUnit.GenesisUnitName, mlSuccess);
    AUnit.UnitState := usFinished;
  end
  else
  begin
    CompileLog('Error at Line: ' + IntToStr(AUnit.GetCurrentLine()) + ' Position: ' + IntToSTr(AUnit.GetCurrentPosInLine()) +
      ' in Unit ' + AUnit.GenesisUnitName, mlNormal);
    SiMain.LogString('SourceLenght', IntToSTr(Length(AUnit.SourceCode)));
    CompileLog(IntToStr(AUnit.Position), mlNormal);
    CompileLog('Compiling aborted', mlNormal);
  end;
  FCurrentUnit := LPreviousUnit;
  SiMain.LeaveMethod(Self, 'CompileUnit');
end;

constructor TGenesisCompiler.Create;
begin
  FUnitList := TObjectList<TGenesisUnit>.Create();
  FDataTypes := TStringList.Create();
  FObjectHandlers := TObjectList<TCompilerObject>.Create();
  SetupBasicDataTypes();
  SetupBasicObjectHandlers();
  FSystemUnit := 'GenSystem.cxx';
  FBaseClass := 'CBaseClass';
  FBaseUnit := 'BaseClass.cxx';
  FErrors := 0;
end;

destructor TGenesisCompiler.Destroy;
begin
  FUnitList.Free();
  FObjectHandlers.Free();
  FDataTypes.Free();
  inherited;
end;

procedure TGenesisCompiler.ErrorUndeclaredIdentifier(AIdentifier: string);
begin
  CompileLog('Undeclared identifier ' + QuotedStr(AIdentifier), mlError);
end;

procedure TGenesisCompiler.ExpectClassType(AClass, AMessage: string);
var
  LClass: string;
begin
  if not IsClass(AClass) then
  begin
    LClass := GetSimilarClass(AClass);
    if LClass <> '' then
    begin
      CompileLog('Expected Classtype but found "' + AClass +
        '". Did you mean "' + LClass + '"?', mlError);
    end
    else
    begin
      if AMessage = '' then
      begin
        raise Exception.Create('Expected Classtype but found "' + AClass + '"');
      end
      else
      begin
        raise Exception.Create(AMessage);
      end;
    end;
  end;
end;

procedure TGenesisCompiler.ExpectDataType(AWord: string);
var
  LHint, LType: string;
begin
  if not IsDataType(AWord) then
  begin
    LType := GetSimilarType(AWord);
    if LType <> '' then
    begin
      LHint := 'Did you mean "' + LType + '"?';
      CompileLog('Expected Datatype but found "' + AWord + '". ' + LHint, mlError);
    end
    else
    begin
      raise Exception.Create('Expected Datatype but found "' + AWord + '"');
    end;
  end;
end;

procedure TGenesisCompiler.ExpectSameIdentifier(AName, ANameB,
  AMessage: string);
begin
  if not SameStr(AName, ANameB) then
  begin
    if AMessage <> '' then
    begin
      raise Exception.Create(AMessage);
    end
    else
    begin
      raise Exception.Create('Identifier ' + QuotedStr(AName) + ' expected, but found ' + QuotedStr(ANameB));
    end;
  end;
end;

function TGenesisCompiler.GetClassByName(AName: string): TGenesisClass;
var
  LUnit: TGenesisUnit;
  LClass: TGenesisClass;
begin
  Result := nil;
  for LUnit in FUnitList do
  begin
    LClass := LUnit.GetClassByName(AName);
    if Assigned(LClass) then
    begin
      Result := LClass;
      Break;
    end;
  end;
end;

function TGenesisCompiler.GetClosedTextBlock(AUnit: TGenesisUnit; AOpenChar,
  ACloseChar: Char): string;
var
  LCount: Integer;
  LChar: Char;
begin
  SiMain.EnterMethod(Self, 'GetClosedTextBlock');
  Result := '';
  AUnit.ExpectChar(AOpenChar);
  AUnit.DropNextVisibleChar();
  LCount := 1;
  while (LCount > 0) and not (AUnit.IsEOF) do
  begin
    LChar := AUnit.SourceCode[AUnit.Position];
    AUnit.Position := AUnit.Position + 1;
    if LChar = ACloseChar then
    begin
      Dec(LCount);
      if LCount = 0 then
      begin
        Break;
      end;
    end
    else
    begin
      if LChar = AOpenChar then
      begin
        Inc(LCount);
      end;
    end;
     Result := Result + LChar;
  end;
  SiMain.LogString('Result', Result);
  SiMain.LeaveMethod(Self, 'GetClosedTextBlock');
end;

function TGenesisCompiler.GetGlobalElement(AIdentifier: string;
  AType: TClass): TGenSourceObject;
var
  LUnit: TGenesisUnit;
begin
  for LUnit in FUnitList do
  begin
    Result := LUnit.GetElement(AIdentifier, AType);
    if Assigned(Result) then
    begin
      Break;
    end;
  end;
end;

function TGenesisCompiler.GetLineForLCLine(AUnit, AErrObject: string;
  ALine: Integer): Integer;
var
  LUnit: TGenesisUnit;
begin
  Result := 0;
  LUnit := GetUnitByName(AUnit);
  if Assigned(LUnit) then
  begin
    Result := LUnit.GetGenLineForLCLine(AErrObject, ALine);
  end;
end;

function TGenesisCompiler.GetObjectHandler(AWord: String): TCompilerObject;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FObjectHandlers.Count - 1 do
  begin
    if FObjectHandlers.Items[i].Name = AWord then
    begin
      Result := FObjectHandlers.Items[i];
      break;
    end;
  end;
end;

function TGenesisCompiler.GetSimilarClass(AName: string): string;
var
  LClass: TGenesisClass;
  LUnit: TGenesisUnit;
  LCount: Integer;
  LPart: string;
  LElement: TGenSourceObject;
begin
  Result := '';
  LClass := GetClassByName(AName);
  if Assigned(LClass) then
  begin
    Result := LClass.Identifier;
  end;
  if Result = '' then
  begin
    for LUnit in FUnitList do
    begin
      for LElement in LUnit.Elements do
      begin
        if LElement.ClassType = TGenesisClass then
        begin
          LClass := TGenesisClass(LElement);
          LCount := Length(AName);
          while LCount > 2 do
          begin
            LPart := Copy(AName, 1, LCount);
            if Pos(LowerCase(LPart), LowerCase(LClass.Identifier)) = 1 then
            begin
              Result := LClass.Identifier;
              Break;
            end;
            Dec(LCount);
          end;
          if Result <> '' then
          begin
            Break;
          end;
        end;
      end;
      if Result <> '' then
      begin
        Break;
      end;
    end;
  end;
end;

function TGenesisCompiler.GetSimilarType(AName: string): string;
var
  LWord, LPart: string;
  LCount: Integer;
begin
  Result := '';
  for LWord in FDataTypes do
  begin
    if SameText(AName, LWord) then
    begin
      Result := LWord;
      Break;
    end;
  end;
  if Result = '' then
  begin
    LCount := Length(AName);
    while LCount > 2 do
    begin
      LPart := Copy(AName, 1, LCount);
      for LWord in FDataTypes do
      begin
        if Pos(LowerCase(LPart), LowerCase(LWord)) = 1 then
        begin
          Result := LWord;
          Break;
        end;
      end;
      if Result <> '' then
      begin
        Break;
      end;
      Dec(LCount);
    end;
  end;
end;

function TGenesisCompiler.GetUnitByName(AName: string): TGenesisUnit;
var
  LUnit: TGenesisUnit;
begin
  Result := nil;
  for LUnit in FUnitList do
  begin
    if SameText(LUnit.GenesisUnitName, AName) then
    begin
      Result := LUnit;
      Break;
    end;
  end;
end;

function TGenesisCompiler.HandleClassObject(AUnit: TGenesisUnit;
  AObject: TObject; var AErrorMessage: string; AAccessLevel: TAccessLevel = alPublic): Boolean;
var
  LClass: TGenesisClass;
  LChar: Char;
  LWord: string;
  LDataType, LPostChars: string;
  LBracketCount: Integer;
  LCompilerObject: TCompilerObject;
  LHasDestructorChar: Boolean;
  LAccessLevel: TAccessLevel;
begin
  SiMain.EnterMethod(Self, 'HandleClassObject');
  Result := False;
  LChar := ' ';
  LAccessLevel := alPrivate;
  if AObject = nil then
  begin
    FExpectedChars := CAlphaChars + CUnderScore;
    AUnit.ExpectCharSet(FExpectedChars);
    LWord := AUnit.GetNextWord();
    LClass := TGenesisClass.Create;
    LClass.Identifier := LWord;
    LClass.Parent := GetClassByName(FBaseClass);//in case we have no baseclass later, set it to base
    AUnit.AddElement(LClass);
    FDataTypes.Add(LClass.Identifier);

    if AUnit.IsNextVisibleChar(':') then //we expect inheritance
    begin
      AUnit.DropNextVisibleChar();
      AUnit.ExpectCharSet(FExpectedChars);
      LWord := AUnit.GetNextWord();
      AUnit.ExpectClassAccessSpecifier(LWord);
      AUnit.ExpectCharSet(FExpectedChars);
      LWord := AUnit.GetNextWord();
      ExpectClassType(LWord);
      LClass.Parent := GetClassByName(LWord);
    end;

    AUnit.ExpectChar('{');
    AUnit.DropNextVisibleChar();
    LBracketCount := 1;
    FExpectedChars := CAlphaChars + CUnderScore;
    while LBracketCount > 0 do
    begin
      if AUnit.IsNextVisibleChar('{') then
      begin
        Inc(LBracketCount);
        AUnit.DropNextVisibleChar();
      end;
      if AUnit.IsNextVisibleChar('}') then
      begin
        Dec(LBracketCount);
        AUnit.DropNextVisibleChar();
      end;
      if LBracketCount > 1 then
      begin
        Break;
      end;
      if LBracketCount = 1 then
      begin
        AUnit.ExpectCharSet(FExpectedChars+['~']);
        LHasDestructorChar := False;
        if AUnit.IsNextVisibleChar('~') then
        begin
          AUnit.DropNextVisibleChar();
          LHasDestructorChar := True;
        end;
        AUnit.ExpectCharSet(FExpectedChars);
        LWord := AUnit.GetNextWord();

        while(SameText('private', LWord) or SameText('public', LWord) or SameText('protected', LWord))do
        begin
          if SameText('private', LWord) then
          begin
            LAccessLevel := alPrivate;
          end;
          if SameText('protected', LWord) then
          begin
            LAccessLevel := alProtected;
          end;
          if SameText('public', LWord) then
          begin
            LAccessLevel := alPublic;
          end;
          AUnit.ExpectChar(':', 'Expected ":" after ' + LWord + ' statement');
          AUnit.DropNextVisibleChar();
          AUnit.ExpectCharSet(FExpectedChars);
          LWord := AUnit.GetNextWord();
        end;

        LCompilerObject := GetObjectHandler(LWord);
        if Assigned(LCompilerObject) then
        begin
          if not LCompilerObject.FObjectHandler(AUnit, LClass, AErrorMessage, LAccessLevel) then
          begin
            Break;
          end;
        end
        else
        begin
//          while(SameText('private', LWord) or SameText('public', LWord) or SameText('protected', LWord))do
//          begin
//            if SameText('private', LWord) then
//            begin
//              LAccessLevel := alPrivate;
//            end;
//            if SameText('protected', LWord) then
//            begin
//              LAccessLevel := alProtected;
//            end;
//            if SameText('public', LWord) then
//            begin
//              LAccessLevel := alPublic;
//            end;
//            AUnit.ExpectChar(':', 'Expected ":" after ' + LWord + ' statement');
//            Continue;
//            LWord := AUnit.GetNextWord();
//          end;
          ExpectDataType(LWord);
          LDataType := LWord;
          LPostChars := AUnit.GetCharsInRow('*');
          SiMain.LogString('DataType', LDataType);
          if SameText(LDataType, LClass.Identifier) then  //if its a constructor/destructor
          begin
            LWord := LDataType;
          end
          else
          begin
            AUnit.ExpectCharSet(FExpectedChars);
            LWord := AUnit.GetNextWord();
          end;
          if AUnit.IsNextVisibleChar(',') or AUnit.IsNextVisibleChar(';') then
          begin
            //process as datatype
            if not ProcessFieldVarDeclaration(AUnit, LClass, LWord, LDataType, LPostChars, AErrorMessage,
              True, LAccessLevel) then
            begin
              Break;
            end;
          end
          else
          begin
            //process as method dummy
            if LHasDestructorChar then
            begin
              LWord := 'Destructor';
              LDataType := 'void';
              LPostChars := '';
            end
            else
            begin
              if SameText(LWord, LClass.Identifier) then
              begin
                LWord := 'Constructor';
                LDataType := 'void';
                LPostChars := '';
              end;
            end;
            AUnit.ExpectChar('(');
            if not ProcessMethodDummyDeclaration(AUnit, LClass, LWord, LDataType, LPostChars,
              LHasDestructorChar, false, AErrorMessage, LAccessLevel) then
            begin
              Break;
            end;
          end;
        end;
      end;
    end;
    if LBracketCount = 0 then
    begin
      AUnit.ExpectChar(';');
      AUnit.DropNextVisibleChar();
      if not Assigned(LClass.Parent) then
      begin
        CompileLog('No Parent class for ' + QuotedStr(LClass.Identifier), mlNormal);
      end
      else
      begin
        CompileLog('Baseclass: ' + LClass.Parent.Identifier);
      end;
      Result := True;
    end
    else
    begin
      if LChar = '{' then
      begin
        AErrorMessage := 'Found "{" but subscope not allowed in Class' + IntToStr(LBracketCount);
      end
      else
      begin
        if Length(AErrorMessage) = 0 then
        begin
          AErrorMessage := 'Unexpected Error on Char "' + LChar + '"';
        end;
      end;
    end;
  end
  else
  begin
    AErrorMessage := 'class declaration not allowed here';
  end;
  SiMain.LeaveMethod(Self, 'HandleClassObject');
end;

function TGenesisCompiler.HandleDataTypeObject(AUnit: TGenesisUnit;
  AObject: TObject; var AErrorMessage: string; AAccessLevel: TAccessLevel = alPublic): Boolean;
var
  LTypeName, LPostChars, LWord: string;
  LVar: TGenVarDeclaration;
begin
  SiMain.EnterMethod(Self, 'HandleDataTypeObject');
  FExpectedChars := CAlphaChars+CNumericChars+CUnderScore;
  AUnit.ExpectCharSet(FExpectedChars);
  LTypeName := AUnit.GetNextWord();
  ExpectDataType(LTypeName);
  LPostChars := AUnit.GetCharsInRow('*');
  if IsClass(LTypeName) and (LPostChars = '') then
  begin
    ProcessConDestructorDeclaration(AUnit, GetClassByName(LTypeName));
    Result := True;
    exit;//if con/destructor we are finished here. subroutine handled it
  end;
  AUnit.ExpectCharSet(FExpectedChars);
  LWord := AUnit.GetNextWord();
  if (AUnit.IsNextVisibleChar(';') or AUnit.IsNextVisibleChar('=')) then
  begin // its a normal global var
    AUnit.ExpectChar(';');
    AUnit.DropNextVisibleChar();
    LVar := TGenVarDeclaration.Create();
    LVar.GenType.Identifier := LTypeName;
    LVar.GenType.PostChars := LPostChars;
    LVar.Identifier := LWord;
    AUnit.AddElement(LVar);
  end
  else
  begin
    ProcessMethodFunctionDeclaration(AUnit, LTypeName, LPostChars, LWord);
  end;
  Result := True;
  SiMain.LeaveMethod(Self, 'HandleDataTypeObject');
end;

function TGenesisCompiler.HandleEnumObject(AUnit: TGenesisUnit;
  AObject: TObject; var AErrorMessage: string; AAccessLevel: TAccessLevel = alPublic): Boolean;
var
  LEnum: TGenEnumDeclaration;
  LElement: TGenType;
begin
  SiMain.EnterMethod(Self, 'HandleEnumObject');
  FExpectedChars := CAlphaChars + CNumericChars + CUnderScore;
  LEnum := TGenEnumDeclaration.Create();
  AUnit.AddElement(LEnum);
  AUnit.ExpectCharSet(FExpectedChars);
  LEnum.Identifier := AUnit.GetNextWord();
  AUnit.ExpectChar('=');
  AUnit.DropNextVisibleChar();
  AUnit.ExpectChar('{');
  AUnit.DropNextVisibleChar();
  while not AUnit.IsNextVisibleChar('}') do
  begin
    AUnit.ExpectCharSet(FExpectedChars);
    LElement := TGenType.Create();
    LElement.Identifier := AUnit.GetNextWord();
    LEnum.AddElement(LElement);
    if not AUnit.IsNextVisibleChar('}') then
    begin
      AUnit.ExpectChar(',');
      AUnit.DropNextVisibleChar();
    end;
  end;
  AUnit.DropNextVisibleChar();
  AUnit.ExpectChar(';');
  AUnit.DropNextVisibleChar();
  FDataTypes.Add(LENum.Identifier);
  Result := True;
  SiMain.LeaveMethod(Self, 'HandleEnumObject');
end;

function TGenesisCompiler.HandleIncludeObject(AUnit: TGenesisUnit;
  AObject: TObject; var AErrorMessage: string; AAccessLevel: TAccessLevel = alPublic): Boolean;
var
  LFile, LFileToCompile: string;
begin
  SiMain.EnterMethod(Self, 'HandleIncludeObject');
  AUnit.Position := AUnit.Position - Length('include') - 1;
  AUnit.ExpectChar('#', 'include statement has to start with #');
  AUnit.Position := AUnit.Position + Length('include') + 1;
  LFile := GetClosedTextBlock(AUnit, '<', '>');

  LFileToCompile := StringReplace(LFile, '"', '', [rfReplaceAll, rfIgnoreCase]);
  SiMain.LogMessage(LFileToCompile);
  if SameText(ExtractFileExt(LFileToCompile), CExtension) then
  begin
    CompileFileEx(BasePath + LFileToCompile, True);
    LFileToCompile := ChangeFileExt(LFileToCompile, '.c');
    if LFile[1] = '"' then
    begin
      LFileToCompile := '"' + LFileToCompile + '"'
    end;
    LFile := LFileToCompile;
  end;

  AUnit.AddInclude(LFile);
  AUnit.ExpectChar(';');
  AUnit.DropNextVisibleChar();
  Result := True;
  SiMain.LeaveMethod(Self, 'HandleIncludeObject');
end;

function TGenesisCompiler.HandleTypeDefObject(AUnit: TGenesisUnit;
  AObject: TObject; var AErrorMessage: string; AAccessLevel: TAccessLevel = alPublic): Boolean;
var
  LWord: string;
begin
  FExpectedChars := CAlphaChars + CNumericChars + CUnderScore;
  AUnit.ExpectCharSet(FExpectedChars);
  LWord := AUnit.GetNextWord();
  ExpectSameIdentifier('struct', LWord);
  ProcessStructDeclaration(AUnit, AUnit);
  Result := True;
end;

function TGenesisCompiler.HandleVirtualMethodObject(AUnit: TGenesisUnit;
  AObject: TObject; var AErrorMessage: string; AAccessLevel: TAccessLevel = alPublic): Boolean;
var
  LWord, LDataType, LPostChars: string;
  LClass: TGenesisClass;
  LIsAbstract: Boolean;
begin
  Result := False;
  LIsAbstract := False;
  Assert(AObject is TGenesisClass, 'virtual declaration not allowed outside of class');
  LClass := TGenesisClass(AObject);
  FExpectedChars := CAlphaChars + CNumericChars + CUnderScore;
  AUnit.ExpectCharSet(FExpectedChars);
  LWord := AUnit.GetNextWord();
  if LWord = 'abstract' then
  begin
    LIsAbstract := True;
    AUnit.ExpectCharSet(FExpectedChars);
    LWord := AUnit.GetNextWord();
  end;
  ExpectDataType(LWord);
  LDataType := LWord;
  LPostChars := AUnit.GetCharsInRow('*');
  AUnit.ExpectCharSet(FExpectedChars);
  LWord := AUnit.GetNextWord();
  AUnit.ExpectChar('(');
  ProcessMethodDummyDeclaration(AUnit, LClass, LWord, LDataType, LPostChars, True, LIsAbstract, AErrorMessage, AAccessLevel);
  if AErrorMessage = '' then
  begin
    Result := True;
  end;
end;

function TGenesisCompiler.IsClass(AWord: string; ACaseSensitive: Boolean = True): boolean;
var
  LClass: TGenesisClass;
begin
  LClass := GetClassByName(AWord);
  Result := Assigned(LClass);
  if Result and ACaseSensitive then
  begin
    Result := SameStr(AWord, LClass.Identifier);
  end;
end;

function TGenesisCompiler.IsDataType(AWord: string; ACaseSensitive: Boolean = True): boolean;
var
  LString: string;
begin
  Result := False;
  for LString in FDataTypes do
  begin
    if ACaseSensitive then
    begin
      if SameStr(AWord, LString) then
      begin
        Result := True;
        Break;
      end;
    end
    else
    begin
      if SameText(AWord, LString) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

procedure TGenesisCompiler.ProcessFunctionParameters(AUnit: TGenesisUnit;
  AFrom: TGenSourceObject);
var
  LDataType, LPostChars, LName: string;
  LVarDec: TGenVarDeclaration;
begin
  SiMain.EnterMethod(Self, 'ProcessFunctionParameters');
  if (AFrom is TGenMethodDeclaration)
    and (TGenMethodDeclaration(AFrom).NeedsThis)
    and Assigned(AFrom.Parent) then
  begin
    TGenMethodDeclaration(AFrom).AddVarDec('this', AFrom.Parent.Identifier, '*');
  end;
  AUnit.ExpectChar('(');
  AUnit.DropNextVisibleChar();
  while not AUnit.IsNextVisibleChar(')') do
  begin
    ProcessValueDeclaration(AUnit, LDataType,LPostChars, LName);
    LVarDec := TGenVarDeclaration.Create();
    LVarDec.Identifier := LName;
    LVarDec.GenType.Identifier := LDataType;
    LVarDec.GenType.PostChars := LPostChars;
    AFrom.AddElement(LVarDec);
    if AUnit.IsNextVisibleChar(',') then
    begin
      AUnit.DropNextVisibleChar();
    end;
  end;
  AUnit.ExpectChar(')');
  AUnit.DropNextVisibleChar();
  SiMain.LeaveMethod(Self, 'ProcessFunctionParameters');
end;


function TGenesisCompiler.ProcessMethodDummyDeclaration(AUnit: TGenesisUnit;
  AClass: TGenSourceObject; AName, ADataType, APostChars: string; AIsVirtual, AIsAbstract: Boolean; var AErrorMessage: string;
  AAccessLevel: TAccessLevel = alPrivate): Boolean;
var
  LChar: Char;
  LMethodName, LWord, LDataType, LPostChars: string;
  LMethodDummy: TGenMethodDummyDeclaration;
  LVarDec: TGenVarDeclaration;
begin
  SiMain.EnterMethod(Self, 'ProcessMethodDummyDeclaration');
  //Result := False;
  AUnit.GetNextVisibleChar(); //drop '('
  LMethodName := AName;
  LMethodDummy := TGenMethodDummyDeclaration.Create();
  LMethodDummy.Identifier := LMethodName;
  LMethodDummy.GenType.Identifier := ADataType;
  LMethodDummy.GenType.PostChars := APostChars;
  LMethodDummy.IsVirtual := AIsVirtual;
  LMethodDummy.IsAbstract := AIsAbstract;
  LMethodDummy.NeedsThis := (AClass is TGenesisClass);
  LMethodDummy.NeedsPrefix := False;
  LMethodDummy.AccessLevel := AAccessLevel;
  if AClass is TGenesisClass then //add class identifier because class methods ALWAYS need a this parameter
  begin
    LVarDec := TGenVarDeclaration.Create();
    LVarDec.Identifier := '';
    LVarDec.GenType.Identifier := AClass.Identifier;
    LVarDec.GenType.PostChars := '';
    LMethodDummy.AddElement(LVarDec);
  end;
  while not AUnit.IsNextVisibleChar(')') do
  begin
    LChar := AUnit.GetNextVisibleChar();
    if CharInSet(LChar, FExpectedChars) then
    begin
      AUnit.Position := AUnit.Position - 1;
      LWord := AUnit.GetNextWord();
      if IsDataType(LWord) then
      begin
        LDataType := LWord;
        LPostChars := AUnit.GetCharsInRow('*');
        LChar := AUnit.GetNextVisibleChar();
        if CharInSet(LChar, FExpectedChars + [',', ')']) then
        begin
          if CharInSet(LChar, FExpectedChars) then //if no name for parameter we get an ',' instantly
          begin
            AUnit.Position := AUnit.Position - 1;
            LWord := AUnit.GetNextWord();
            LMethodDummy.AddVarDec(LWord, LDataType, LPostChars);
          end
          else
          begin
            LMethodDummy.AddVarDec('NULL', LDataType, LPostChars);
            if LChar = ')' then
            begin
//                                  AUnit.GetNextVisibleChar(); //drop ')'
              Break;
            end;
          end;
          if AUnit.IsNextVisibleChar(',') then
          begin
            AUnit.GetNextVisibleChar(); // drop ',' and move on
          end;
        end
        else
        begin
          AErrorMessage := 'Unexpected Char "' + LChar + '"';
          //Result := False;
          Break;
        end;
      end
      else
      begin
        AErrorMessage := 'Unknown KeyWord "' + LWord + '"';
        //Result := False;
        Break;
      end;
    end
    else
    begin
      AErrorMessage := 'Unexpected Char "' + LChar + '"';
      //Result := False;
      Break;
    end;
  end;
  if (AClass is TGenesisClass) and (TGenesisClass(AClass).ParentImplementsMethod(LMethodDummy.Identifier)) then
  begin
    LMethodDummy.IsOverriding := True;
  end;
  LMethodDummy.Parent := AClass;
  AClass.AddElement(LMethodDummy);
  Result := True;
  if AUnit.IsNextVisibleChar(')') then
  begin
    AUnit.GetNextVisibleChar(); //drop ')'
  end;
  if AUnit.IsNextVisibleChar(';') then
  begin
    AUnit.GetNextVisibleChar(); //drop ';'
  end
  else
  begin
    AErrorMessage := 'Expected ";" but found "' + AUnit.GetNextVisibleChar() + '"';
    Result := False;
  end;
  if Length(AErrorMessage) > 0 then
  begin
    SiMain.LogException(AErrorMessage);
  end;
  SiMain.LeaveMethod(Self, 'ProcessMethodDummyDeclaration');
end;

procedure TGenesisCompiler.ProcessMethodFunctionDeclaration(AUnit: TGenesisUnit; ATypeName, APostChars, AIdentifier: string);
var
  LMethod: TGenMethodDeclaration;
begin
  SiMain.EnterMethod(Self, 'ProcessMethodFunctionDeclaration');
  LMethod := TGenMethodDeclaration.Create();
  LMethod.GenesisLineStart := AUnit.GetCurrentLine();
  AUnit.AddElement(LMethod);

  FExpectedChars := CAlphaChars + CUnderScore;

  LMethod.GenType.Identifier := ATypeName;
  LMethod.GenType.PostChars := APostChars;
  LMethod.Identifier := AIdentifier;

  if IsClass(AIdentifier) then
  begin
    LMethod.Parent := AUnit.GetClassByName(AIdentifier);
    AUnit.ExpectChar(':');
    AUnit.DropNextVisibleChar();
    AUnit.ExpectChar(':');
    AUnit.DropNextVisibleChar();

    AUnit.ExpectCharSet(FExpectedChars);
    LMethod.Identifier := AUnit.GetNextWord();
    LMethod.NeedsThis := True;
    LMethod.NeedsPrefix := True;
  end
  else
  begin
    LMethod.NeedsPrefix := False;
    LMethod.NeedsThis := False;
  end;

  ProcessFunctionParameters(AUnit, LMethod);
  if not AUnit.IsNextVisibleChar(';') then
  begin
    LMethod.Source := ProcessMethodSource(AUnit, LMethod); //GetClosedTextBlock(AUnit, '{', '}');
  end
  else
  begin
    AUnit.DropNextVisibleChar();
  end;
  LMethod.GenesisLineEnd := AUnit.GetCurrentLine()-1;
  SiMain.LeaveMethod(Self, 'ProcessMethodFunctionDeclaration');
end;

function TGenesisCompiler.ProcessMethodSource(AUnit: TGenesisUnit;
  AParent: TGenSourceObject): string;
var
  LBracketCount, LStartPos, LEndPos: Integer;
  LWord, LNextWord, LIdentifier, LPostChars, LCallString, LMessage: string;
  LHasValueReturn, LHasVoidReturn, LReturnIsUnsafe: Boolean;
  LMethod: TGenMethodDeclaration;
  LMethodDummy: TGenMethodDummyDeclaration;
  //LVarDec: TGenVarDeclaration;
  LClass: TGenesisClass;
begin
  SiMain.EnterMethod(Self, 'ProcessMethodSource');
  FExpectedChars := CAlphaChars + CUnderScore;
  AUnit.ExpectChar('{');
  AUnit.DropNextVisibleChar();
  LBracketCount := 1;
  LStartPos := AUnit.Position;
  LEndPos := AUnit.Position;
  LHasValueReturn := False;
  LHasVoidReturn := False;
  LReturnIsUnsafe := True;
  while LBracketCount >= 1 do
  begin

    if AUnit.IsNextVisibleChar('{') then
    begin
      Inc(LBracketCount);
      AUnit.DropNextVisibleChar();
      LEndPos := AUnit.Position;
      Result := Result + Copy(AUnit.SourceCode, LStartPos, LEndPos-LStartPos);
      LStartPos := LEndPos + 1;
      Continue;
    end;

    if AUnit.IsNextVisibleChar('}') then
    begin
      Dec(LBracketCount);
      AUnit.DropNextVisibleChar();
      LEndPos := AUnit.Position;
      if LBracketCount = 0 then
      begin
        LEndPos := LEndPos - 1; //do not include last  '}'
      end;
      Result := Result + Copy(AUnit.SourceCode, LStartPos, LEndPos-LStartPos);
      LStartPos := LEndPos + 1;
      Continue;
    end;

    if AUnit.IsNextVisibleChar('"') then
    begin
      Result := Result + '"' + GetClosedTextBlock(AUnit, '"', '"') + '"';
      Continue;
    end;

    if AUnit.IsNextVisibleCharSet(CAlphaChars+CUnderScore) then
    begin
      LStartPos := AUnit.Position;
      LWord := AUnit.GetNextWord();
      LEndPos := AUnit.Position;
      Result := Result + Copy(AUnit.SourceCode, LStartPos, LEndPos-LStartPos);
      LStartPos := LEndPos + 1;

      LStartPos := AUnit.Position;

      if Assigned(AParent) then
      begin
        if (not Assigned(AParent.GetElement(LWord, TGenSourceObject))) and (Assigned(AParent.Parent)) then
        begin
          LClass := GetClassByName(AParent.Parent.Identifier);
          if Assigned(LClass) and Assigned(LClass.GetMember(LWord)) then
          begin
            AUnit.Position := AUnit.Position - Length(LWord);
            LStartPos := AUnit.Position;
            Result := Copy(Result, 1, Length(Result) - Length(LWord));//removing the current item again from the output
            AUnit.InsertSource('this.', AUnit.Position);
            Continue;
          end;
        end;
      end;

      if LWord = 'for' then
      begin
        if AUnit.IsNextVisibleCharSet(CAlphaChars+CUnderScore) then
        begin
          LNextWord := AUnit.GetNextWord();
          if LNextWord = 'each' then
          begin
            Result := Copy(Result, 1, Length(Result) - Length(LWord)); //removing "for" from result
            Result := Result + ProcessForEach(AUnit, AParent);
            Continue;
          end;
        end;
      end;

      if LWord = 'new' then
      begin
        Result := Copy(Result, 1, Length(Result) - Length(LWord)); //removing "new" from result
        Result := Result + ProcessNewCall(AUnit, AParent);
        Continue;
      end;

      if IsClass(LWord) and AUnit.IsNextVisibleChar(':') then
      begin
        if not (AParent.Parent is TGenesisClass) then
        begin
          CompileLog('You can not do an inherited call in a normal function', mlError);
        end;
        SiMain.LogString('Class', LWord);
        SiMain.LogString('parentclass',AParent.Parent.Parent.Identifier);
        if not SameStr(AParent.Parent.Parent.Identifier, LWord) then
        begin
          CompileLog(QuotedStr(LWord)  + ' is not derived from ' + QuotedStr(AParent.Parent.Identifier), mlError);
        end;
        AUnit.DropNextVisibleChar();//dropping ':'
        AUnit.ExpectChar(':');
        AUnit.DropNextVisibleChar();
        AUnit.ExpectCharSet(FExpectedChars);
        LIdentifier := AUnit.GetNextWord();
        LClass := GetClassByName(LWord);
        if not LClass.HasMethodDummy(LIdentifier) then
        begin
          CompileLog(QuotedStr(LIdentifier) + ' is not a method of class ' + QuotedStr(LClass.Identifier), mlError);
        end;
        AUnit.ExpectChar('(');
        AUnit.DropNextVisibleChar();//dropping '(' we will ad it manually
        Result := GenesisPrefix + LClass.Identifier+LIdentifier + '(this';
        if not AUnit.IsNextVisibleChar(')') then
        begin
          Result := Result + ', ';
        end;
        Continue;
      end;

      if IsDataType(LWord) then
      begin
        LPostChars := AUnit.GetCharsInRow('*');
        if AUnit.IsNextVisibleCharSet(FExpectedChars) then
        begin
          LIdentifier := AUnit.GetNextWord();
          ProcessFieldVarDeclaration(AUnit, AParent, LIdentifier, LWord, LPostChars, LMessage, False, alPublic);
          LEndPos := AUnit.Position;
          Result := Result + Copy(AUnit.SourceCode, LStartPos, LEndPos-LStartPos);
          LStartPos := LEndPos + 1;
          continue;
        end;
      end;

      if ResolveIdentifier(AUnit, AParent, LWord,LCallString, LMethodDummy) then
      begin
        LEndPos := AUnit.Position;
        Result := Result + Copy(AUnit.SourceCode, LStartPos, LEndPos-LStartPos);
        LStartPos := LEndPos + 1;
        if AUnit.IsNextVisibleChar('(') then
        begin
          Result := Result + '(';
          AUnit.DropNextVisibleChar();
          Result := Result + LCallString;
          if not AUnit.IsNextVisibleChar(')') then
          begin
            Result := Result + ',';
          end;
        end;
      end
      else
      begin
        LEndPos := AUnit.Position;
        Result := Result + Copy(AUnit.SourceCode, LStartPos, LEndPos-LStartPos);
        LStartPos := LEndPos + 1;
      end;

      if LWord = 'return' then
      begin
        if AUnit.IsNextVisibleChar('(') then
        begin
          LHasValueReturn := True;
        end
        else
        begin
          LHasVoidReturn := True;
        end;
        if LBracketCount = 1 then
        begin
          LReturnIsUnsafe := False;
        end;
      end;

      continue;
    end;
    LStartPos := AUnit.Position;
    AUnit.DropNextVisibleChar();
    LEndPos := AUnit.Position;
    Result := Result + Copy(AUnit.SourceCode, LStartPos, LEndPos-LStartPos);
    LStartPos := LEndPos + 1;
  end;
  //output messages if required return is missing or unsafe
  if AParent is TGenMethodDeclaration then
  begin
    LMethod := TGenMethodDeclaration(AParent);
    if LHasValueReturn or LHasVoidReturn then
    begin
      if (LReturnIsUnsafe or LHasVoidReturn) and (LMethod.GenType.Identifier <> 'void') then
      begin
        if LHasVoidReturn and (not LHasValueReturn) then
        begin
          CompileLog('function ' + QuotedStr(LMethod.Identifier) + ' of type ' + QuotedStr(LMethod.GenType.Identifier) +
          ' does not return any value', mlWarning);
        end
        else
        begin
          CompileLog('function ' + QuotedStr(LMethod.Identifier) + ' might not return a value for certain conditions',
            mlWarning);
        end;
      end;
      if LHasValueReturn and (LMethod.GenType.Identifier = 'void') then
      begin
        CompileLog('function ' + QuotedStr(LMethod.Identifier) + ' of type ' + QuotedStr('void') +
          ' provides unexpected result', mlWarning);
      end;
      if LHasVoidReturn and (LMethod.GenType.Identifier = 'void') then
      begin
        CompileLog('function ' + QuotedStr(LMethod.Identifier) + ' provides return-statement without a value, but ' +
          'requires a return-value of type ' + QuotedStr(LMethod.GenType.Identifier), mlWarning);
      end;
    end
    else
    begin
      if LMethod.GenType.Identifier <> 'void' then
      begin
        CompileLog('function ' + QuotedStr(LMethod.Identifier) + ' of type ' + QuotedStr(LMethod.GenType.Identifier) +
          ' does not return any value', mlWarning);
      end;
    end;
  end;
  SiMain.LeaveMethod(Self, 'ProcessMethodSource');
end;

function TGenesisCompiler.ProcessNewCall(AUnit: TGenesisUnit;
  AFrom: TGenSourceObject): string;
var
  LClassIdentifier: string;
begin
  LClassIdentifier := AUnit.GetNextIdentifier();
  ExpectClassType(LClassIdentifier);
  Result := '_' + LClassIdentifier;
  AUnit.ExpectChar('(');
end;

procedure TGenesisCompiler.ProcessStructDeclaration(AUnit: TGenesisUnit;
  AParent: TGenSourceObject);
var
  LIdentifier: string;
  LStruct: TGenStructDeclaration;
  LDataType, LPostChars, LErr: string;
begin
  FExpectedChars := CAlphaChars + CNumericChars + CUnderScore;
  LStruct := TGenStructDeclaration.Create();
  AUnit.AddElement(LStruct);
  AUnit.ExpectCharSet(FExpectedChars);
  LIdentifier := AUnit.GetNextWord();
  LStruct.Identifier := LIdentifier;
  AUnit.ExpectChar('{');
  AUnit.DropNextVisibleChar();
  while not AUnit.IsNextVisibleChar('}') do
  begin
    ProcessValueDeclaration(AUnit, LDataType, LPostChars, LIdentifier);
    if not AUnit.IsNextVisibleChar('(') then
    begin
      if not ProcessFieldVarDeclaration(AUnit, LStruct, LIdentifier, LDataType, LPostChars, LErr) then
      begin
        raise Exception.Create(LErr);
      end;
    end
    else
    begin
      if not ProcessMethodDummyDeclaration(AUnit, LStruct, LIdentifier, LDataType, LPostChars, False, false, LErr) then
      begin
        raise Exception.Create(LErr);
      end;
    end;
  end;
  AUnit.DropNextVisibleChar();
  AUnit.ExpectChar(';');
  FDataTypes.Add(LStruct.Identifier);
  AUnit.DropNextVisibleChar();
end;

procedure TGenesisCompiler.ProcessValueDeclaration(AUnit: TGenesisUnit;
  var ADataType, APostChars, AName: string);
var
  LWord, LDataType: string;
begin
  SiMain.EnterMethod(Self, 'ProcessValueDeclaration');
  FExpectedChars := CAlphaChars + CNumericChars + CUnderScore;
  AUnit.ExpectCharSet(FExpectedChars);
  LWord := AUnit.GetNextWord();
  ExpectDataType(LWord);
  LDataType := LWord;
  APostChars := AUnit.GetCharsInRow('*');
  AUnit.ExpectCharSet(FExpectedChars);
  LWord := AUnit.GetNextWord();
  ADataType := LDataType;
  AName := LWord;
  SiMain.LeaveMethod(Self, 'ProcessValueDeclaration');
end;

function TGenesisCompiler.RemoveAllComments(ASource: string): string;
begin
  Result := RemoveMultiLineComments(RemoveOneLineComments(ASource));
end;

function TGenesisCompiler.RemoveMultiLineComments(ASource: string): string;
var
  i, k: Cardinal;
  LInString: Boolean;
begin
  SiMain.EnterMethod(Self, 'RemoveMultiLineComments');
  Result := '';
  LInString := False;
  for i := 1 to Length(ASource)-1 do
  begin
    if ASource[i] = '"' then
    begin
      LInString := not LInString;
    end;
    if (ASource[i] = '/') and (ASource[i+1] = '*') and (not LInString)then
    begin
      for k := i to Length(ASource)-1 do
      begin
        if (ASource[k] = '*') and (ASource[k+1]='/') then
        begin
          ASource[k] := ' ';
          ASource[k+1] := ' ';
          Break;
        end;
        if (ASource[k] <> #13) and (ASource[k] <> #10) then
        begin
          ASource[k] := ' ';
        end;
      end;
    end;
    Result := Result + ASource[i];
  end;
  SiMain.LogText('Result', Result);
  SiMain.LeaveMethod(Self, 'RemoveMultiLineComments');
end;

function TGenesisCompiler.RemoveOneLineComments(ASource: string): string;
var
  i, k: Cardinal;
  LInString: Boolean;
begin
  SiMain.EnterMethod(Self, 'RemoveOneLineComments');
  Result := '';
  LInString := False;
  for i := 1 to Length(ASource)-1 do
  begin
    if ASource[i] = '"' then
    begin
      LInString := not LInString;
    end;
    if (ASource[i] = '/') and (ASource[i+1] = '/') and (not LInString)then
    begin
      for k := i to Length(ASource)-1 do
      begin
        if (ASource[k] = #13) and (ASource[k+1]=#10) then
        begin
          Break;
        end;
        ASource[k] := ' ';
      end;
    end;
    Result := Result + ASource[i];
  end;
  SiMain.LogText('Result', Result);
  SiMain.LeaveMethod(Self, 'RemoveOneLineComments');
end;

function TGenesisCompiler.ResolveIdentifier(AUnit:TGenesisUnit; AParent:TGenSourceObject; AIdentifier: string;
  var ACallString: string; var AMethodDummy: TGenMethodDummyDeclaration): Boolean;
var
  LWord: string;
  LElement: TGenSourceObject;
  LClass: TGenesisClass;
  LAccessLevel: TAccessLevel;
begin
  Result := False;
  if Assigned(AParent) and (AParent is TGenMethodDeclaration) then
  begin
    LElement := AParent.GetElement(AIdentifier, TGenVarDeclaration);
  end;
  if not Assigned(LElement) then
  begin
    LElement := AUnit.GetElement(AIdentifier, TGenVarDeclaration);
  end;

  if Assigned(LElement) then
  begin
    LClass := GetClassByName(TGenVarDeclaration(LElement).GenType.Identifier);
    if not Assigned(LClass) then
    begin
      exit;
    end;
    LAccessLevel := alPublic;
    if (Assigned(AParent) and Assigned(AParent.Parent)) and (LClass.Identifier = AParent.Parent.Identifier) then
    begin
      LAccessLevel := alPrivate;
    end;
    ACallString := LElement.Identifier;
    AUnit.GetCharsInRow(')');
    if AUnit.IsNextVisibleChar('.') then
    begin
      AUnit.DropNextVisibleChar();
      while Assigned(LElement) and Assigned(LClass) do
      begin
        if AUnit.IsNextVisibleCharSet(CAlphaChars+CUnderScore) then
        begin
          LWord := AUnit.GetNextWord();
          LElement := LClass.GetElement(LWord, TGenSourceObject);
          if Assigned(LElement) and (LElement is TGenVarDeclaration)then
          begin
            if LAccessLevel > LElement.AccessLevel then
            begin
              AccessError(LElement.Identifier, LElement.AccessLevel);
            end;
            LClass := GetClassByName(TGenVarDeclaration(LElement).GenType.Identifier);
            if not Assigned(LClass) then
            begin
              Break;
            end;
            LAccessLevel := alPublic;
            if (Assigned(AParent) and Assigned(AParent.Parent)) and (LClass.Identifier = AParent.Parent.Identifier) then
            begin
              LAccessLevel := alPrivate;
            end;
            ACallString := ACallString + '.' + LElement.Identifier;
            AUnit.GetCharsInRow(')');
            if not AUnit.IsNextVisibleChar('.') then
            begin
              Break;
            end;
            AUnit.DropNextVisibleChar();
          end
          else
          begin
            if not Assigned(LElement) then
            begin
              LElement := LClass.GetMember(LWord);
              if not Assigned(LElement) then
              begin
                CompileLog(QuotedStr(LWord) + ' is not a member of ' + QuotedStr(LClass.Identifier), mlError);
              end
              else
              begin
                if LAccessLevel > LElement.AccessLevel then
                begin
                  AccessError(LElement.Identifier, LElement.AccessLevel);
                end;
              end;
            end;
          end;
        end
        else
        begin
          Break;
        end;
      end;
    end;
    if Assigned(LElement) and (Assigned(LClass)) then
    begin
//      CompileLog(LElement.Identifier, mlWarning);
      //get local acceslevel
      LAccessLevel := alPublic;
      if (Assigned(AParent) and Assigned(AParent.Parent)) and (LClass.Identifier = LElement.Parent.Identifier) then
      begin
        LAccessLevel := alPrivate;
      end;
      if Assigned(LClass.GetMember(LWord)) then
      begin
        LAccessLevel := alProtected;
      end;

      if LAccessLevel > LElement.AccessLevel then
      begin
        AccessError(LElement.Identifier, LElement.AccessLevel);
      end;
    end;
    if Assigned(LElement) and (LElement is TGenMethodDummyDeclaration)  then
    begin
      AMethodDummy := TGenMethodDummyDeclaration(LElement);
      Result := True;
    end;
  end;
end;

procedure TGenesisCompiler.ProcessConDestructorDeclaration(AUnit: TGenesisUnit;
  AParent: TGenSourceObject);
var
  LWord: string;
  LIsDestructor: Boolean;
  LMethod: TGenMethodDeclaration;
begin
  SiMain.EnterMethod(Self, 'ProcessConDestructorDeclaration');
  FExpectedChars := CAlphaChars+CNumericChars+CUnderScore;
  LMethod := TGenMethodDeclaration.Create();
  LMethod.NeedsThis := True;
  LMethod.NeedsPrefix := True;
  LMethod.Parent := AParent;
  AUnit.AddElement(LMethod);
  LIsDestructor := False;
  AUnit.ExpectChar(':');
  AUnit.DropNextVisibleChar();
  AUnit.ExpectChar(':');
  AUnit.DropNextVisibleChar();
  if AUnit.IsNextVisibleChar('~') then
  begin
    LIsDestructor := True;
    AUnit.DropNextVisibleChar();
    LMethod.Identifier := 'Destructor';
  end
  else
  begin
    LMethod.Identifier := 'Constructor';
  end;
  AUnit.ExpectCharSet(FExpectedChars);
  LWord := AUnit.GetNextWord();
  ExpectSameIdentifier(AParent.Identifier, LWord);
  LMethod.GenType.Identifier := 'void';
  LMethod.GenesisLineStart := AUnit.GetCurrentLine();
  ProcessFunctionParameters(AUnit, LMethod);
  AUnit.ExpectChar('{');
  LMethod.Source := ProcessMethodSource(AUnit, LMethod); //GetClosedTextBlock(AUnit, '{', '}');
  LMethod.GenesisLineEnd := AUnit.GetCurrentLine() - 1;
  SiMain.LeaveMethod(Self, 'ProcessConDestructorDeclaration');
end;

function TGenesisCompiler.ProcessFieldVarDeclaration(AUnit: TGenesisUnit;
  AClass: TGenSourceObject; AFirstName, ADataType, APostChars: string; var AErrorMessage: string;
    AExpectDelimiter: Boolean = True; AAccessLevel: TAccessLevel = alPrivate): Boolean;
var
  LChar: Char;
  LWord: string;
  LVarDec: TGenVarDeclaration;
begin
  SiMain.EnterMethod(Self, 'ProcessMethodDeclarationEx');
  Result := True;
  LVarDec := TGenVarDeclaration.Create();
  LVarDec.Identifier := AFirstName;
  LVarDec.GenType.Identifier := ADataType;
  LVarDec.GenType.PostChars := APostChars;
  LVarDec.AccessLevel := AAccessLevel;
  LVarDec.Enabled := AExpectDelimiter;//if no delimiter expected we dont parse correctly but to find local vars only
  LVarDec.Parent := AClass;
  AClass.AddElement(LVarDec);
  SiMain.LogMessage(AFirstName + '=' + ADataType);
  while AUnit.IsNextVisibleChar(',') do
  begin
    AUnit.GetNextVisibleChar();//drop ',' char and move on
    LChar := AUnit.GetNextVisibleChar();
    if CharInSet(LChar, FExpectedChars) then
    begin
      AUnit.Position := AUnit.Position - 1;
      LWord := AUnit.GetNextWord();
      LVarDec := TGenVarDeclaration.Create();
      LVarDec.Identifier := LWord;
      LVarDec.GenType.Identifier := ADataType;
      LVarDec.GenType.PostChars := APostChars;
      LVarDec.Enabled := AExpectDelimiter;
      LVarDec.AccessLevel := AAccessLevel;
      LVarDec.Parent := AClass;
      AClass.AddElement(LVarDec);
      SiMain.LogMessage(LWord + '=' + ADataType);
    end
    else
    begin
      if LChar <> ';' then
      begin
        AErrorMessage := 'Unexpected char "' + LChar + '"';
        Result := False;
        Break;
      end;
    end;
  end;
  if AExpectDelimiter then
  begin
    if not AUnit.IsNextVisibleChar(';') then
    begin
      LChar := AUnit.GetNextVisibleChar();
      AErrorMessage := 'Expected ";" but found "' + LChar + '"';
      Result := False;
    end
    else
    begin
      AUnit.GetNextVisibleChar(); //drop ';' and move on
    end;
  end;
  SiMain.LeaveMethod(Self, 'ProcessMethodDeclarationEx');
end;

function TGenesisCompiler.ProcessForEach(AUnit: TGenesisUnit;
  AParent: TGenSourceObject): string;
var
  LVarIdentifier, LListIdentifier, LWord: string;
  LVarElement: TGenVarDeclaration;
  LListElement: TGenVarDeclaration;
  LClass: TGenesisClass;
begin
  AUnit.ExpectChar('(');
  AUnit.DropNextVisibleChar();
  AUnit.ExpectCharSet(CAlphaChars+CUnderScore);
  LVarIdentifier := AUnit.GetNextWord();
  LVarElement := AParent.GetElement(LVarIdentifier, TGenVarDeclaration) as TGenVarDeclaration;
  if not Assigned(LVarElement) then
  begin
    LVarElement := GetGlobalElement(LVarIdentifier, TGenVarDeclaration) as TGenVarDeclaration;
    if not assigned(LVarElement) then
    begin
      ErrorUndeclaredIdentifier(LVarIdentifier);
      exit;
    end;
  end;

  AUnit.ExpectCharSet(CAlphaChars+CUnderScore);
  LWord := AUnit.GetNextWord();
  ExpectSameIdentifier(LWord, 'in');
  AUnit.ExpectCharSet(CAlphaChars+CUnderScore);
  LListIdentifier := AUnit.GetNextWord();

  LListElement := AParent.GetElement(LListIdentifier, TGenVarDeclaration) as TGenVarDeclaration;
  if not Assigned(LListElement) then
  begin
    LListElement := GetGlobalElement(LListIdentifier, TGenVarDeclaration) as TGenVarDeclaration;
    if not assigned(LListElement) then
    begin
      ErrorUndeclaredIdentifier(LListIdentifier);
      exit;
    end;
  end;
  ExpectClassType(LListElement.GenType.Identifier);
  LClass := GetClassByName(LListElement.GenType.Identifier);
  if not LClass.GenInheritsFrom('CForEachObject') then
  begin
    CompileLog(QuotedStr(LListElement.Identifier) + ' is not derived from ' + QuotedStr('CForEachObject'), mlError);
  end;

  AUnit.ExpectChar(')');
  AUnit.DropNextVisibleChar();
  AUnit.ExpectChar('{');
  Result := LListIdentifier+'.FEInit('+LListIdentifier+'); while('+LListIdentifier+'.FEHasMoreElements(' +
    LListIdentifier+') > 0)' + sLineBreak +
    '{ ' + LVarIdentifier + ' = ' + LListIdentifier + '.FENextElement(' + LListIdentifier + ');' + sLineBreak;
  Result := Result + ProcessMethodSource(AUnit, AParent) + '}' + sLineBreak;
end;

procedure TGenesisCompiler.SetExpectedChars(const Value: TAnsiCharSet);
begin
  FExpectedChars := Value;
end;

procedure TGenesisCompiler.SetOnMessage(const Value: TCompileEvent);
begin
  FOnMessage := Value;
end;

procedure TGenesisCompiler.SetupBasicDataTypes;
begin
  SiMain.EnterMethod(Self, 'SetupBasicDataTypes');
  FDataTypes.Add('void');
  FDataTypes.Add('int');
  FDataTypes.Add('STRING');
  FDataTypes.Add('VECTOR');
  FDataTypes.Add('var');
  FDataTypes.Add('long');
  FDataTypes.Add('short');
  FDataTypes.Add('char');
  SiMain.LeaveMethod(Self, 'SetupBasicDataTypes');
end;

procedure TGenesisCompiler.SetupBasicObjectHandlers;
var
  LObject: TCompilerObject;
begin
  SiMain.EnterMethod(Self, 'SetupBasicObjectHandlers');
  LObject := TCompilerObject.Create();
  LObject.Name := 'class';
  LObject.ObjectHandler := Self.HandleClassObject;
  FObjectHandlers.Add(LObject);

  LObject := TCompilerObject.Create();
  LObject.Name := 'include';
  LObject.ObjectHandler := Self.HandleIncludeObject;
  FObjectHandlers.Add(LObject);

  LObject := TCompilerObject.Create();
  LObject.Name := 'virtual';
  LObject.ObjectHandler := Self.HandleVirtualMethodObject;
  FObjectHandlers.Add(LObject);

  LObject := TCompilerObject.Create();
  LObject.Name := 'typedef';
  LObject.ObjectHandler := Self.HandleTypeDefObject;
  FObjectHandlers.Add(LObject);
  LObject := TCompilerObject.Create();
  LObject.Name := 'enum';
  LObject.ObjectHandler := Self.HandleEnumObject;
  FObjectHandlers.Add(LObject);
  SiMain.LeaveMethod(Self, 'SetupBasicObjectHandlers');
end;

initialization
  Si.Enabled := True;

end.
