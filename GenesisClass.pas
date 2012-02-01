unit GenesisClass;

interface
uses
  Classes, Types, SysUtils, Generics.Collections,
  GenSourceObject, SiAuto, SmartInspect;

type
  TGenesisClass = class(TGenSourceObject)
  private
    FOverridenMethods: TStringList;
    function GetBodyLiteCSource(): string;
    procedure AddOverridingMethods(AList: TStrings);
  public
    constructor Create(); reintroduce;
    destructor Destroy(); override;
    function GetMember(AIdentifier: string): TGenSourceObject;
    function GetLiteCSource(): string; override;
    function GetParentLiteCSource(): string;
    function HasMethodDummy(AName: string): boolean;
    function ParentImplementsMethod(AName: string): Boolean;
    function GetMethodDummyByName(AName: string): TGenMethodDummyDeclaration;
    function GetFirstParentImplementorOfMethod(AName: string; AIsVirtualOrAbstract: Boolean): TGenesisClass;
    property OverridenMethods: TStringList read FOverridenMethods;
  end;

implementation
uses
  StrUtils, GenesisConsts;
{ TGenesisClass }

procedure TGenesisClass.AddOverridingMethods(AList: TStrings);
var
  LDummy: TGenSourceObject;
begin
  for LDummy in  Elements do
  begin
    if (LDummy.ClassType = TGenMethodDummyDeclaration) and TGenMethodDummyDeclaration(LDummy).IsOverriding then
    begin
      AList.Add(LDummy.Identifier);
    end;
  end;
end;

constructor TGenesisClass.Create;
begin
  inherited;
  FOverridenMethods := TStringList.Create();
end;

destructor TGenesisClass.Destroy;
begin
  FOverridenMethods.Free;
  inherited;
end;

function TGenesisClass.GetBodyLiteCSource(): string;
var
  i: Integer;
  LName: string;
  LIsOverriden, LParentIsVirtual: Boolean;
  LMethodDummy, LParentMethodDummy: TGenMethodDummyDeclaration;
  LParentClass: TGenesisClass;
  LElement: TGenSourceObject;
begin
  Result := '';
  if Assigned(Parent) and (Parent.Identifier <> '') then
  begin
    LName := Parent.Identifier;
  end
  else
  begin
    LName := Identifier;
  end;

  for LElement in Elements do
  begin
    if LElement.ClassType = TGenVarDeclaration then
    begin
      Result := Result + LElement.GetLiteCSource();
    end
    else
    begin
      LMethodDummy := TGenMethodDummyDeclaration(LElement);
      LParentIsVirtual := False;
      LIsOverriden := (FOverridenMethods.IndexOf(LMethodDummy.Identifier)>=0);
      LParentClass := GetFirstParentImplementorOfMethod(LMethodDummy.Identifier, True);
      if Assigned(LParentClass) then
      begin
        LParentMethodDummy := LParentClass.GetMethodDummyByName(LMethodDummy.Identifier);
        LParentIsVirtual := LParentMethodDummy.IsVirtual;
      end;
      if not LParentIsVirtual then
      begin
        LMethodDummy.DoOverride := LIsOverriden;
        Result := Result + LMethodDummy.GetLiteCSource(); //FMethodDummyList.Strings[i] + ';' + sLineBreak;
      end;
    end;
  end;
end;

function TGenesisClass.GetFirstParentImplementorOfMethod(
  AName: string; AIsVirtualOrAbstract: Boolean): TGenesisClass;
var
  LMethodDummy: TGenMethodDummyDeclaration;
begin
  Result := nil;
  if Assigned(Parent) then
  begin
    if TGenesisClass(Parent).HasMethodDummy(AName) then
    begin
      LMethodDummy := TGenesisClass(Parent).GetMethodDummyByName(AName);
      if (LMethodDummy.IsVirtual or LMethodDummy.IsAbstract)
        or((not AIsVirtualOrAbstract))
      then
      begin
        Result := TGenesisClass(Parent);
      end
      else
      begin
        Result := TGenesisClass(Parent).GetFirstParentImplementorOfMethod(AName, AIsVirtualOrAbstract)
      end;
    end
    else
    begin
      Result := TGenesisClass(Parent).GetFirstParentImplementorOfMethod(AName, AIsVirtualOrAbstract);
    end;
  end;
end;

function TGenesisClass.GetLiteCSource: string;
var
  LOverridingMethods: TSTringList;
begin
  LOverridingMethods := TStringList.Create();
  Result := '';
  if Assigned(Parent) then
  begin
    Result := '//is derived from: ' + Parent.Identifier + sLineBreak;
  end;
  Result := Result + 'typedef struct ' + Identifier + '{' + sLineBreak;
  AddOverridingMethods(LOverridingMethods);
  OverridenMethods.Assign(LOverridingMethods);
  Result := Result + GetParentLiteCSource();
  LOverridingMethods.Clear();
  FOverridenMethods.Assign(LOverridingMethods);
  Result := Result + GetBodyLiteCSource();
  Result := Result + '}' + Identifier + ';' + sLineBreak;
  LOverridingMethods.Free();
end;

function TGenesisClass.GetMember(AIdentifier: string): TGenSourceObject;
begin
  Result := GetElement(AIdentifier, TGenSourceObject);
  if (not Assigned(Result)) and (Assigned(Parent)) and (Parent is TGenesisClass) then
  begin
    Result := TGenesisClass(Parent).GetMember(AIdentifier);
  end;
end;

function TGenesisClass.GetMethodDummyByName(AName: string): TGenMethodDummyDeclaration;
var
  LDummy: TGenMethodDummyDeclaration;
begin
  Result := TGenMethodDummyDeclaration(GetElement(AName, TGenMethodDummyDeclaration));
end;

function TGenesisClass.GetParentLiteCSource(): string;
var
  LName: string;
begin
  Result := '';
  if not Assigned(Parent) then
  begin
    LName := Identifier;
  end
  else
  begin
    LName := Parent.Identifier;
  end;
  if Assigned(Parent) then
  begin
    if Parent is TGenesisClass then
    begin
      TGenesisClass(Parent).OverridenMethods.Assign(OverridenMethods);
      Result := Result + TGenesisClass(Parent).GetParentLiteCSource();
      Result := Result + TGenesisClass(Parent).GetBodyLiteCSource();
    end;
  end;
end;

function TGenesisClass.HasMethodDummy(AName: string): boolean;
begin
  Result := Assigned(GetElement(AName, TGenMethodDummyDeclaration));
end;

function TGenesisClass.ParentImplementsMethod(AName: string): Boolean;
var
  LDummy: TGenMethodDummyDeclaration;
begin
  Result := False;
  if Assigned(Parent) then
  begin
    Result := Assigned(Parent.GetElement(AName, TGenMethodDummyDeclaration));
    if not Result then
    begin
      Result := TGenesisClass(Parent).ParentImplementsMethod(AName);
    end;
  end;
end;

end.
