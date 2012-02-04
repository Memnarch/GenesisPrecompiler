unit GenesisUnit;

interface
uses
  Classes, Types, SysUtils, Windows, Generics.Collections,GenesisClass,
  GenesisConsts, GenSourceObject,
  SiAuto, SmartInspect;

type
  TUnitState = (usCompiling, usFinished);

  TGenesisUnit = class(TGenSourceObject)
  private
    FGenesisUnitName: String;
    FSourceCode: string;
    FPosition: Integer;
    FUnitState: TUnitState;
    procedure BuildMethodBindFunctions();
    procedure BuildConstructor();
    procedure BuildDestructor();
    function GetEOF: Boolean;
    procedure SetSourceCode(const Value: string);
  public
    constructor Create(); reintroduce;
    destructor Destroy(); override;
    property GenesisUnitName: String read FGenesisUnitName write FGenesisUnitName;
    procedure Finalize();
    procedure ExpectChar(AChar: Char; AMessage: string = '');
    procedure ExpectCharSet(ACharSet: TAnsiCharSet; AMessage: string = '');
    procedure ExpectClassType(AClass: string; AMessage: string = '');
    procedure ExpectClassAccessSpecifier(AWord: string);
    procedure AddInclude(AFile: string);
    procedure AddIncludeAtTop(AFile: string);
    procedure DropNextVisibleChar();
    procedure InsertSource(ASource: string; APosition: Integer);
    function GetNextWord(): string;
    function GetNextIdentifier(): string;
    function GetNextVisibleChar(): Char;
    function IsNextVisibleChar(AChar: Char): Boolean;
    function IsNextVisibleCharSet(ASet: TAnsiCharSet): Boolean;
    function GetLiteCSource(): String; override;
    function IsClass(AWord: String): Boolean;
    function GetCharsInRow(AChar: Char): string;
    function GetCurrentLine(): Integer;
    function GetCurrentPosInLine(): Integer;
    function GetClassByName(AName: string): TGenesisClass;
    function GetMethodByName(AName: string): TGenMethodDeclaration;
    function RequiresBaseInclude(): Boolean;
    function GetLineCountOfString(ASource: string): Integer;
    function GetGenLineForLCLine(AErrObject: string; ALine: Integer): Integer;
    property SourceCode: string read FSourceCode write SetSourceCode;
    property Position: Integer read FPosition write FPosition;
    property IsEOF: Boolean read GetEOF;
    property UnitState: TUnitState read FUnitState write FUnitState;
  end;

implementation
{ TGenesisUnit }

uses
  StrUtils;

procedure TGenesisUnit.AddInclude(AFile: string);
var
  LInclude: TGenInclude;
begin
  LInclude := TGenInclude.Create();
  LInclude.Identifier := AFile;
  AddElement(LInclude);
end;

procedure TGenesisUnit.AddIncludeAtTop(AFile: string);
var
  LInclude: TGenInclude;
begin
  LInclude := TGenInclude.Create();
  LInclude.Identifier := AFile;
  AddElementAtTop(LInclude);
end;

procedure TGenesisUnit.BuildConstructor;
var
  LClass: TGenesisClass;
  LElement, LElementB: TGenSourceObject;
  LGenType: TGenType;
  LConstructor, LMethod: TGenMethodDeclaration;
  i: Integer;
begin
  for LElement in Elements do
  begin
    if LElement.ClassType = TGenesisClass then
    begin
      LClass := TGenesisClass(LElement);

      LConstructor := TGenMethodDeclaration.Create();
      LConstructor.NeedsPrefix := False;
      LConstructor.NeedsThis := False;
      LConstructor.GenType.Identifier := LClass.Identifier;
      LConstructor.GenType.PostChars := '*';
      LConstructor.Parent := nil;//LClass;
      LConstructor.Identifier := '_' + LClass.Identifier;
      LConstructor.Source := LClass.Identifier + '* LObject;' + sLineBreak +
        'LObject = sys_malloc(sizeof(' + LClass.Identifier + '));' + sLineBreak +
        GenesisPrefix + LClass.Identifier + 'BindMethods(LObject);' + sLineBreak +
        'LObject.ClassName = "' + LClass.Identifier + '";' + sLineBreak;
      if Assigned(LClass.Parent) then
      begin
        LConstructor.Source := LConstructor.Source + 'LObject.ClassParentName = "' +
          LClass.Parent.Identifier +'";' + sLineBreak;
      end
      else
      begin
        LConstructor.Source := LConstructor.Source + 'LObject.ClassParentName = "";' + sLineBreak;
      end;

      if LClass.HasMethodDummy('Constructor') then
      begin
        LConstructor.Source := LConstructor.Source + 'LObject.Constructor(LObject';
        LMethod := GetMethodByName('Constructor');
        if Assigned(LMethod) then
        begin
          for LElementB in LMethod.Elements do
          begin
            if TGenType(LElementB).Identifier <> 'this' then
            begin
              LGenType := TGenType.Create();
              LGenType.Identifier := TGenType(LElementB).Identifier;
              LGenType.PostChars := TGenType(LElementB).PostChars;
              LConstructor.AddElement(LGenType);
            end;
          end;
          //LConstructor.ParameterList.Assign(LMethod.ParameterList);
          for i := 0 to LMethod.Elements.Count - 1 do
          begin
            if LMethod.Elements.Items[i].Identifier <> 'this' then
            begin
              LConstructor.Source := LConstructor.Source + ', ' + LMethod.Elements.Items[i].GetLiteCSource();
            end;
          end;

        end;
        LConstructor.Source := LConstructor.Source + ');' + sLineBreak;
      end;
      LConstructor.Source := LConstructor.Source + 'return(LObject);' + sLineBreak;

      Self.AddElement(LConstructor);
    end;
  end;
end;

procedure TGenesisUnit.BuildDestructor;
//var
//  LClass: TGenesisClass;
//  LDestructor: TGenesisMethod;
begin
//  SiMain.EnterMethod(Self, 'BuildDestructor');
//  for LClass in FClassList do
//  begin
//    LDestructor := TGenesisMethod.Create();
//    LDestructor.NeedsPrefix := False;
//    LDestructor.GenesisMethodName := 'Free';
//    LDestructor.MethodParentClass := LClass;
//    LDestructor.DataType := 'void';
//    LDestructor.SourceCode := '';
//    if LClass.HasMethodDummy('Destructor') then
//    begin
//      LDestructor.SourceCode := 'This.Destructor(This);' + sLineBreak;
//    end;
//    LDestructor.SourceCode := LDestructor.SourceCode + 'sys_free(This);' +  sLineBreak;
//    FMethodList.Add(LDestructor);
//  end;
//  SiMain.LeaveMethod(Self, 'BuildDestructor');
end;

procedure TGenesisUnit.BuildMethodBindFunctions;
var
  LClass: TGenesisClass;
  LMethod: TGenMethodDeclaration;
  LBindMethod: TGenMethodDeclaration;
  LElement, LElementB: TGenSourceObject;
begin
  for LElement in Elements do
  begin
    if LElement.ClassType = TGenesisClass then
    begin
      LClass := TGenesisClass(LElement);
      LBindMethod := TGenMethodDeclaration.Create();
      LBindMethod.GenType.Identifier := 'void';
      LBindMethod.Identifier := 'BindMethods';
      LBindMethod.NeedsThis := True;
      LBindMethod.NeedsPrefix := True;
      LBindMethod.Parent := LClass;
      LBindMethod.AddVarDec('this', LClass.Identifier, '*');
      if Assigned(LClass.Parent) then
      begin
        LBindMethod.Source := LBindMethod.Source + GenesisPrefix + LClass.Parent.Identifier +
          LBindMethod.Identifier + '(this);' + sLineBreak;
      end;
      //LBindMethod.SourceCode := LBindMethod.SourceCode;
      for LElementB in Elements do
      begin
        if LElementB.ClassType = TGenMethodDeclaration then
        begin
          LMethod := TGenMethodDeclaration(LElementB);
          if Assigned(LMethod.Parent)
            and (LMethod.Parent.Identifier = LClass.Identifier)
            and (LClass.HasMethodDummy(LMethod.Identifier)) then
          begin
            LBindMethod.Source := LBindMethod.Source + 'this.' +
              LMethod.Identifier + ' = ' + GenesisPrefix + LMethod.Parent.Identifier +
              LMethod.Identifier + ';' + sLineBreak;
          end;
        end;
      end;
      //LBindMethod.SourceCode := LBindMethod.SourceCode;
      Self.AddElement(LBindMethod);
    end;
  end;
end;

constructor TGenesisUnit.Create;
begin
  inherited;
  FUnitState := usCompiling;
end;

destructor TGenesisUnit.Destroy;
begin
  inherited;
end;

procedure TGenesisUnit.DropNextVisibleChar;
begin
  GetNextVisibleChar();
end;

procedure TGenesisUnit.ExpectChar(AChar: Char; AMessage: string = '');
begin
  if not IsNextVisibleChar(AChar) then
  begin
    if AMessage = '' then
    begin
      raise Exception.Create('Unexpected Char "' + GetNextVisibleChar() + '" expected "' + AChar + '"');
    end
    else
    begin
      raise Exception.Create(AMessage);
    end;
  end;
end;

procedure TGenesisUnit.ExpectCharSet(ACharSet: TAnsiCharSet; AMessage: string = '');
var
  LChar: Char;
begin
  LChar := GetNextVisibleChar();
  Position := Position -1;
  if not CharInSet(LChar, ACharSet) then
  begin
    if AMessage = '' then
    begin
      raise Exception.Create('Unexpected Char "' + LChar + '"');
    end
    else
    begin
      raise Exception.Create(AMessage);
    end;
  end;
end;

procedure TGenesisUnit.ExpectClassAccessSpecifier(AWord: string);
begin
  if not (SameText(AWord, 'private') or SameText(AWord, 'public') or SameText(AWord, 'protected')) then
  begin
    raise Exception.Create('Expected Class access specifier, but found ' + QuotedStr(AWord));
  end;
end;

procedure TGenesisUnit.ExpectClassType(AClass: string; AMessage: string = '');
begin
  if not IsClass(AClass) then
  begin
    if AMessage = '' then
    begin
      raise Exception.Create('Expected Classtype of current unit but found "' + AClass + '"');
    end
    else
    begin
      raise Exception.Create(AMessage);
    end;
  end;
end;

procedure TGenesisUnit.Finalize;
begin
  BuildMethodBindFunctions();
  BuildConstructor();
  //BuildDestructor();
end;

function TGenesisUnit.GetCharsInRow(AChar: Char): string;
begin
  Result := '';
  while(IsNextVisibleChar(AChar)) do
  begin
    Result := Result + GetNextVisibleChar();
  end;
end;

function TGenesisUnit.GetClassByName(AName: string): TGenesisClass;
var
  LElement: TGenSourceObject;
begin
  Result := nil;
  LElement := GetElement(AName, TGenesisClass);
  if Assigned(LElement) then
  begin
    Result := LElement as TGenesisClass;
  end;
end;

function TGenesisUnit.GetCurrentLine: Integer;
var
  LPos: Integer;
begin
  Result := 1;
  LPos := 1;
  while (LPos < Position) and (LPos >= 1) do
  begin
    LPos := PosEx(sLineBreak, SourceCode, LPos+2);
    if LPos < Position then
    begin
      Inc(Result);
    end;
  end;
end;

function TGenesisUnit.GetCurrentPosInLine: Integer;
var
  LMaxLine, LPos, i: Integer;
  LLines: TStringList;
begin
  LMaxLine := GetCurrentLine();
  LLines := TStringList.Create();
  LLines.Text := FSourceCode;
  Result := 1;
  LPos := 0;
  if LLines.Count >= LMaxLine then
  begin
    for i := 0 to LMaxLine - 2 do
    begin
      LPos := LPos + Length(LLines.Strings[i]);
    end;
    Result := Position - LPos - (LMaxLine*2);//including SLinebreake which is removed by stringlist
  end;
end;

function TGenesisUnit.GetEOF: Boolean;
begin
  Result := False;
  if (FPosition > Length(FSourceCode) - 1) then
  begin
    Result := True;
  end;
end;

function TGenesisUnit.GetGenLineForLCLine(AErrObject: string; ALine: Integer): Integer;
var
  LMethod: TGenMethodDeclaration;
  i, k, LFixLine: Integer;
  LLines: TStringList;
begin
  Result := 1;
  for i := ELements.Count - 1 downto 0 do
  begin
    if ELements.Items[i].ClassType = TGenMethodDeclaration then
    begin
      LMethod := TGenMethodDeclaration(Elements.Items[i]);
      if LMethod.LiteCLineStart <= ALine then
      begin
        SiMain.LogString('Method', LMethod.Identifier);
        SiMain.LogInteger('GenStart', LMethod.GenesisLineStart);
        SiMain.LogInteger('GenEnd', LMethod.GenesisLineEnd);
        SiMain.LogInteger('LCStart', LMethod.LiteCLineStart);
        SIMain.LogInteger('LCEnd', LMethod.LiteCLineEnd);
        Result := ALine - (LMethod.LiteCLineStart - LMethod.GenesisLineStart);
        Result := Result -
          ((LMethod.LiteCLineEnd - LMethod.LiteCLineStart) - (LMethod.GenesisLineEnd-LMethod.GenesisLineStart));
        if (AErrObject <> '') then
        begin
          LFixLine := ALine;
          LLines := TStringList.Create;
          LLines.Text := FSourceCode;
          for k :=  ALine downto 0 do        //used Result-1 before
          begin
            if Pos(AErrObject, LLines.Strings[k]) >= 1 then
            begin
              LFixLine := k+1;
              Break;
            end;
          end;
          Result := Result - (Result-LFixLine);
          LLines.Free;
        end;
        Break;
      end;
    end;
  end;
end;

function TGenesisUnit.GetLineCountOfString(ASource: string): Integer;
var
//  LPos: Integer;
  LList: TStringList;
begin
  Result := 1;
  LList := TStringList.Create();
  LLIst.Text := ASource;
  Result := LList.Count;
  LList.Free;
//  LPos := 1;
//  LPos := PosEx(sLineBreak, ASource, LPos);
//  while(LPos >= 1) do
//  begin
//    Inc(Result);
//    LPos := PosEx(sLineBreak, ASource, LPos+1);
//  end;
end;

function TGenesisUnit.GetLiteCSource: String;
var
  LELement: TGenSourceObject;
begin
  Result := '#ifndef ' + GenesisPrefix + ChangeFileExt(FGenesisUnitName, '_class') +
    sLineBreak + '#define ' + GenesisPrefix + ChangeFileExt(FGenesisUnitName, '_class') + sLineBreak;
  for LElement in Elements do
  begin
    if LELement.ClassType = TGenMethodDeclaration then
    begin
      Result := Result + sLineBreak + sLineBreak;
      TGenMethodDeclaration(LElement).LiteCLineStart := GetLineCountOfString(Result);
      Result := Result + TGenMethodDeclaration(LElement).GetLiteCSource;
      TGenMethodDeclaration(LElement).LiteCLineEnd := GetLineCountOfString(Result);
      Result := Result + sLineBreak;
    end
    else
    begin
      Result := Result + LELement.GetLiteCSource();
    end;


  end;
  Result := Result+ sLineBreak + '#endif';
end;

function TGenesisUnit.GetMethodByName(AName: string): TGenMethodDeclaration;
begin
  Result := TGenMethodDeclaration(GetElement(AName, TGenMethodDeclaration));
end;

function TGenesisUnit.GetNextIdentifier: string;
begin
  ExpectCharSet(CAlphaChars+CUnderScore);
  Result := GetNextWord();
end;

function TGenesisUnit.GetNextVisibleChar: Char;
var
  i: Integer;
  LRestrictedChars: TAnsiCharSet;
begin
  Result := Char(1);
  LRestrictedChars := [Char(10), Char(13), ' '];
  for i := FPosition to Length(FSourceCode) do
  begin
    if not CharInSet(FSourceCode[i], LRestrictedChars) then
    begin
      Result := FSourceCode[i];
      break;
    end;
  end;
  FPosition := i + 1;
end;

function TGenesisUnit.GetNextWord: string;
var
  i: Integer;
begin
  Result := '';
  for i := FPosition to Length(FSourceCode) do
  begin
    if not CharInSet(FSourceCode[i], CWordDelimiters) then
    begin
      Result := Result + FSourceCode[i];
    end
    else
    begin
      if not (Result = '')  then
      begin
        break;
      end;
    end;
  end;
  FPosition := i;
end;

procedure TGenesisUnit.InsertSource(ASource: string; APosition: Integer);
begin
  Insert(ASource, FSourceCode, APosition);
end;

function TGenesisUnit.IsClass(AWord: String): Boolean;
begin
  Result := Assigned(GetElement(AWord, TGenesisClass));
end;

function TGenesisUnit.IsNextVisibleChar(AChar: Char): Boolean;
var
  LPosition: Cardinal;
begin
  LPosition := FPosition;
  Result := False;
  if GetNextVisibleChar() = AChar then
  begin
    Result := True;
  end;
  FPosition := LPosition;
end;

function TGenesisUnit.IsNextVisibleCharSet(ASet: TAnsiCharSet): Boolean;
var
  LPosition: Cardinal;
begin
  LPosition := FPosition;
  Result := False;
  if  CharInSet(GetNextVisibleChar(), ASet) then
  begin
    Result := True;
  end;
  FPosition := LPosition;
end;

function TGenesisUnit.RequiresBaseInclude: Boolean;
var
  LClass: TGenesisClass;
  LElement: TGenSourceObject;
begin
  Result := False;
  for LElement in Elements do
  begin
    if LElement.ClassType = TGenesisClass then
    begin
      LClass := TGenesisClass(LElement);
      if Assigned(LClass.Parent) and (LClass.Parent.Identifier = 'CBaseClass')
        and (not Assigned(GetElement('BaseClass.c', TGenInclude))) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

procedure TGenesisUnit.SetSourceCode(const Value: string);
begin
  FSourceCode := TrimRight(Value);
  FSourceCode := StringReplace(FSourceCode, Char(9),'  ', [rfReplaceAll]);//remove tab and replace with double space
  FPosition := 1;
end;

end.
