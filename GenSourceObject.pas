unit GenSourceObject;

interface

uses
  Classes, Types, Generics.Collections, GenesisConsts, SiAuto, SmartInspect;

type
  TGenSourceObject = class(TObject)
  private
    FElements: TObjectList<TGenSourceObject>;
    FIdentifier: string;
    FParent: TGenSourceObject;
    FEnabled: Boolean;
    FAccessLevel: TAccessLevel;
  public
    constructor Create();
    destructor Destroy(); override;
    function GetLiteCSource(): string; virtual;
    function GetElement(AIdentifier: string; AType: TClass): TGenSourceObject;
    procedure AddElement(AOject: TGenSourceObject);
    procedure AddElementAtTop(AObject: TGenSourceObject);
    property Identifier: string read FIdentifier write FIdentifier;
    property Parent: TGenSourceObject read FParent write FParent;
    property Elements: TObjectList<TGenSourceObject> read FElements;
    property Enabled: Boolean read FEnabled write FEnabled;
    property AccessLevel: TAccessLevel read FAccessLevel write FAccessLevel;
  end;

  TGenIdentifier = class(TGenSourceObject)
  public
    function GetLiteCSource(): string; override;
  end;


  TGenType = class(TGenIdentifier)
  private
    FPostChars: string;
  published
    function GetLiteCSource(): string; override;
    property PostChars: string read FPostChars write FPostChars;
  end;

  TGenInclude = class(TGenSourceObject)
  public
    function GetLiteCSource(): string; override;
  end;

  TGenSourceSnippet = class(TGenSourceObject)
  private
    FSource: string;
  public
    function GetLiteCSource(): string; override;
    property Source: string read FSource write FSource;
  end;

  TGenVarDeclaration = class(TGenSourceObject)
  private
    FGenType: TGenType;
    FNoDelimiter: Boolean;
  public
    constructor Create(); reintroduce;
    destructor Destroy(); override;
    function GetLiteCSource(): string; override;
    property GenType: TGenType read FGenType;
    property NoDelimiter: Boolean read FNoDelimiter write FNoDelimiter;
  end;

  TGenMethodDummyDeclaration = class(TGenVarDeclaration)
  private
    FIsAbstract: Boolean;
    FDoOverride: Boolean;
    FIsOverriding: Boolean;
    FIsVirtual: Boolean;
    FNeedsPrefix: Boolean;
    FNeedsThis: Boolean;
    FOverridingParentIdentifier: string;
  public
    function GetLiteCSource(): string; override;
    procedure AddVarDec(AIdentifier, AType, APostChars: string);
    property IsOverriding: Boolean read FIsOverriding write FIsOverriding;
    property DoOverride: Boolean read FDoOverride write FDoOverride;
    property IsVirtual: Boolean read FIsVirtual write FIsVirtual;
    property IsAbstract: Boolean read FIsAbstract write FIsAbstract;
    property NeedsThis: Boolean read FNeedsThis write FNeedsThis;
    property NeedsPrefix: Boolean read FNeedsPrefix write FNeedsPrefix;
    property OverridingParentIdentifier: string read FOverridingParentIdentifier write FOverridingParentIdentifier;
  end;

  TGenMethodDeclaration = class(TGenMethodDummyDeclaration)
  private
    FSource: string;
    FLiteCLineStart: Integer;
    FGenesisLineStart: Integer;
    FLiteCLineEnd: Integer;
    FGenesisLineEnd: Integer;
  public
    function GetLiteCSource(): string; override;
    property Source: string read FSource write FSource;
    property GenesisLineStart: Integer read FGenesisLineStart write FGenesisLineStart;
    property GenesisLineEnd: Integer read FGenesisLineEnd write FGenesisLineEnd;
    property LiteCLineStart: Integer read FLiteCLineStart write FLiteCLineStart;
    property LiteCLineEnd: Integer read FLiteCLineEnd write FLiteCLineEnd;
  end;

  TGenStructDeclaration = class(TGenSourceObject)
  public
    function GetLiteCSource(): string; override;
  end;

  TGenEnumDeclaration = class(TGenSourceObject)
  public
    function GetLiteCSource(): string; override;
  end;

implementation

uses
  SysUtils, GenesisClass;

{ TGenSourceObject }

procedure TGenSourceObject.AddElement(AOject: TGenSourceObject);
begin
  FElements.Add(AOject);
end;

procedure TGenSourceObject.AddElementAtTop(AObject: TGenSourceObject);
begin
  FElements.Insert(0, AObject);
end;

constructor TGenSourceObject.Create;
begin
  FElements := TObjectList<TGenSourceObject>.Create();
  FEnabled := True;
  FAccessLevel := alPublic;
end;

destructor TGenSourceObject.Destroy;
begin
  FElements.Free;
  inherited;
end;

function TGenSourceObject.GetElement(AIdentifier: string;
  AType: TClass): TGenSourceObject;
var
  LElement: TGenSourceObject;
begin
  Result := nil;
  for LElement in FElements do
  begin
    if SameStr(LElement.Identifier, AIdentifier)
      and ((LElement.ClassType = AType) or (LElement.InheritsFrom(AType))) then
    begin
      Result := LElement;
      Break;
    end;
  end;
end;

function TGenSourceObject.GetLiteCSource: string;
var
  LElement: TGenSourceObject;
begin
  for LElement in FElements do
  begin
    Result := Result + LElement.GetLiteCSource();
  end;
end;

{ TGenType }

function TGenType.GetLiteCSource: string;
begin
  Result := Identifier + FPostChars;
end;

{ TGenIdentifier }

function TGenIdentifier.GetLiteCSource: string;
begin
  Result := FIdentifier;
end;

{ TGenSourceSnippet }

function TGenSourceSnippet.GetLiteCSource: string;
begin
  Result := FSource;
end;

{ TGenVarDeclaration }

constructor TGenVarDeclaration.Create;
begin
  inherited;
  FGenType := TGenType.Create();
end;

destructor TGenVarDeclaration.Destroy;
begin
  FGenType.Free;
  inherited;
end;

function TGenVarDeclaration.GetLiteCSource: string;
begin
  Result := FGenType.GetLiteCSource() + ' ' + Identifier;
  if not NoDelimiter then
  begin
    Result := Result + ';' + sLineBreak;
  end;
end;

{ TGenMethodDummyDeclaration }

procedure TGenMethodDummyDeclaration.AddVarDec(AIdentifier, AType, APostChars: string);
var
  LVarDec: TGenVarDeclaration;
begin
  LVarDec := TGenVarDeclaration.Create();
  LVarDec.Identifier := AIdentifier;
  LVarDec.GenType.Identifier := AType;
  LVarDec.GenType.PostChars := APostChars;
  AddElement(LVarDec);
end;

function TGenMethodDummyDeclaration.GetLiteCSource: string;
var
  LElement: TGenSourceObject;
  LIsNotFirst: Boolean;
  i: Integer;
  LParentIdentifier: string;
begin
  Result := GenType.GetLiteCSource() + ' ';
  if DoOverride and (not IsVirtual) then
  begin
    Result := Result +'zzOverriden';
  end;
  if (NeedsPrefix) then
  begin
    Result := Result +  GenesisPrefix;
  end;
  if (Assigned(Parent)) and NeedsPrefix then
  begin
    Result := Result + Parent.Identifier;
  end;
  Result := Result + Identifier + '(';
//  if NeedsThis and (Assigned(Parent)) and (Parent is TGenesisClass) then
//  begin
//    Result := Result + Parent.Identifier + '* this';
//  end;
  LIsNotFirst := False;
  for i := 0 to Elements.Count - 1 do
  begin
    LElement := Elements.Items[i];
    if (LElement is TGenVarDeclaration) and (LElement.Enabled) then
    begin
      if LIsNotFirst then
      begin
        Result := Result + ', ';
      end;
      if (not LIsNotFirst) and (OverridingParentIdentifier <> '') then
      begin
        Result := Result +OverridingParentIdentifier; //simply place void here if overriden and ignore original this type
      end
      else
      begin
         TGenVarDeclaration(LElement).NoDelimiter := True;
        Result := Result + LElement.GetLiteCSource();
      end;
      LIsNotFirst := True;


//      if i < Elements.Count - 1 then
//      begin
//        Result := Result + ', ';
//      end;
    end;
  end;
  Result := Result + ')';
  if not NoDelimiter then
  begin
    Result := Result + ';' + sLineBreak;
  end;
end;

{ TGenMethodDeclaration }

function TGenMethodDeclaration.GetLiteCSource: string;
begin
  NoDelimiter := True;
  Result := inherited;
  if Source <> '' then
  begin
    Result := Result + sLineBreak + '{' + sLineBreak;
    Result := Result + Source + '}';
  end
  else
  begin
    Result := Result + ';' + sLineBreak;
  end;
end;

{ TGenInclude }

function TGenInclude.GetLiteCSource: string;
begin
  Result := '#include<' + Identifier + '>;' + sLineBreak;
end;

{ TGenStructDeclaration }

function TGenStructDeclaration.GetLiteCSource: string;
begin
  Result := 'typedef struct ' + Identifier + sLineBreak;
  Result := Result + '{' + sLineBreak;
  Result := Result + inherited;
  Result := Result + '}'+Identifier+';' + sLineBreak;
end;

{ TGenEnumDeclaration }

function TGenEnumDeclaration.GetLiteCSource: string;
var
  LElement: TGenSourceObject;
  LNum: Integer;
begin
  LNum := 0;
  Result := '#define ' + Identifier + ' int' + sLineBreak;
  for LElement in Elements do
  begin
    Result := Result + '#define ' + LElement.Identifier + ' ' + IntToStr(LNum) + sLineBreak;
    Inc(LNum);
  end;
end;

end.
