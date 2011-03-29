{-----------------------------------------------------------------------------
 Unit Name: uSynWebCss
 Author:    Krystian
 Date:      29-Mar-2011
 Purpose:
 History:
-----------------------------------------------------------------------------}

unit uSynWebCss;

interface

uses
  SysUtils, Classes, xmlSynWeb;

type

{ TSynWebCssPropertyValues }

  TSynWebCssPropertyValues = class(TStringList)
  private
    FFlags: Cardinal;
    
  public
    constructor Create;

    property Flags: Cardinal read FFlags;
  end;

{ TSynWebCssPropertiesList }

  TSynWebCssPropertiesList = class(TStringList)
  private
    function GetValues(AIndex: Integer): TSynWebCssPropertyValues;

  protected
    procedure InsertItem(Index: Integer; const S: string; AObject: TObject); override;

  public
    constructor Create;       
    destructor Destroy; override;

    property Values[AIndex: Integer]: TSynWebCssPropertyValues read GetValues;
  end;

{ TSynWebCssProperties }

  TSynWebCssProperties = class(TObject)
  private
    FProperties: TSynWebCssPropertiesList;

    procedure DoLoadFrom(AXml: IXMLCssType);

  public
    constructor Create(AXml: IXMLCssType);
    destructor Destroy; override;

    function GetPropertyIndex(const APropertyName: String): Integer;
    function GetPropertyFlags(APropertyIndex: Integer; var AFlags: Cardinal): Boolean;

    function GetValidPropertyValue(APropertyIndex: Integer; const AValue: String;
      var AIsValid: Boolean): Boolean;

    property List: TSynWebCssPropertiesList read FProperties;
  end;

implementation

{ TSynWebCssPropertyValues }

constructor TSynWebCssPropertyValues.Create;
begin
  inherited Create;

  Sorted := True;
end;

{ TSynWebCssPropertiesList }

constructor TSynWebCssPropertiesList.Create;
begin
  inherited Create;

  Sorted := True;
end;

destructor TSynWebCssPropertiesList.Destroy;
var
  lIdx: Integer;
begin
  for lIdx := 0 to Count - 1 do
    Objects[lIdx].Free;

  inherited Destroy;
end;

function TSynWebCssPropertiesList.GetValues(
  AIndex: Integer): TSynWebCssPropertyValues;
begin
  Result := TSynWebCssPropertyValues(Objects[AIndex]);
end;

procedure TSynWebCssPropertiesList.InsertItem(Index: Integer; const S: string;
  AObject: TObject);
begin
  if AObject = nil then
    AObject := TSynWebCssPropertyValues.Create;

  inherited InsertItem(Index, S, AObject);
end;

{ TSynWebCssProperties }

constructor TSynWebCssProperties.Create(AXml: IXMLCssType);
begin              
  inherited Create;
  
  FProperties := TSynWebCssPropertiesList.Create;

  DoLoadFrom(AXml);
end;

destructor TSynWebCssProperties.Destroy;
begin
  FProperties.Free;

  inherited Destroy;
end;

function TSynWebCssProperties.GetPropertyFlags(
  APropertyIndex: Integer; var AFlags: Cardinal): Boolean;
begin
  Result := (APropertyIndex >= 0) and (APropertyIndex < List.Count);
  if Result then
    AFlags := List.Values[APropertyIndex].Flags;
end;

function TSynWebCssProperties.GetPropertyIndex(const APropertyName: String): Integer;
begin
  Result := FProperties.IndexOf(APropertyName);
end;

function TSynWebCssProperties.GetValidPropertyValue(APropertyIndex: Integer;
  const AValue: String; var AIsValid: Boolean): Boolean;
begin
  Result := (APropertyIndex >= 0) and (APropertyIndex < List.Count);
  if Result then
    AIsValid := TSynWebCssPropertyValues(FProperties.Objects[APropertyIndex]).IndexOf(AValue) > -1;
end;

procedure TSynWebCssProperties.DoLoadFrom(AXml: IXMLCssType);
var
  lIdx, lValIdx: Integer;
  lVal: TSynWebCssPropertyValues;
  lProp: IXMLCssPropertyType;
begin
  if AXml = nil then
    Exit;

  for lIdx := 0 to AXml.CssProperties.Count - 1 do
  begin
    lProp := AXml.CssProperties[lIdx];

    lVal := List.Values[List.Add(lProp.Name)];
    lVal.FFlags := lProp.Flags;

    for lValIdx := 0 to lProp.CssValues.Count - 1 do
      lVal.Add(lProp.CssValues[lValIdx]);
  end;
end;

end.

