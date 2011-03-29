
{**************************************************************}
{                                                              }
{                       XML Data Binding                       }
{                                                              }
{         Generated on: 3/29/2011 2:45:53 PM                   }
{       Generated from: \\tsclient\D\css.xml                   }
{   Settings stored in: P:\synweb\CSS\PropertyEditor\css.xdb   }
{                                                              }
{**************************************************************}

unit xmlSynWeb;

interface

uses xmldom, XMLDoc, XMLIntf;

type

{ Forward Decls }

  IXMLSynWebType = interface;
  IXMLCssType = interface;
  IXMLCssPropertiesType = interface;
  IXMLCssPropertyType = interface;
  IXMLCssValuesType = interface;

{ IXMLSynWebType }

  IXMLSynWebType = interface(IXMLNode)
    ['{97969E0C-D6EA-44DD-A8EB-3867BA0A242B}']
    { Property Accessors }
    function Get_Css: IXMLCssType;
    { Methods & Properties }
    property Css: IXMLCssType read Get_Css;
  end;

{ IXMLCssType }

  IXMLCssType = interface(IXMLNode)
    ['{1DE26DD6-893C-4ABE-993B-C10D0220D427}']
    { Property Accessors }
    function Get_CssProperties: IXMLCssPropertiesType;
    { Methods & Properties }
    property CssProperties: IXMLCssPropertiesType read Get_CssProperties;
  end;

{ IXMLCssPropertiesType }

  IXMLCssPropertiesType = interface(IXMLNodeCollection)
    ['{9B370D0A-44BD-4802-B20E-A6ADF4919FCC}']
    { Property Accessors }
    function Get_CssProperty(Index: Integer): IXMLCssPropertyType;
    { Methods & Properties }
    function Add: IXMLCssPropertyType;
    function Insert(const Index: Integer): IXMLCssPropertyType;
    property CssProperty[Index: Integer]: IXMLCssPropertyType read Get_CssProperty; default;
  end;

{ IXMLCssPropertyType }

  IXMLCssPropertyType = interface(IXMLNode)
    ['{F5084E05-2636-4D07-AFDB-CE0ABDEAE91E}']
    { Property Accessors }
    function Get_Name: WideString;
    function Get_Flags: Cardinal;
    function Get_CssValues: IXMLCssValuesType;
    procedure Set_Name(Value: WideString);
    procedure Set_Flags(Value: Cardinal);
    { Methods & Properties }
    property Name: WideString read Get_Name write Set_Name;
    property Flags: Cardinal read Get_Flags write Set_Flags;
    property CssValues: IXMLCssValuesType read Get_CssValues;
  end;

{ IXMLCssValuesType }

  IXMLCssValuesType = interface(IXMLNodeCollection)
    ['{0D50E3EF-4D53-4FE4-9B98-6A15EFAD6D56}']
    { Property Accessors }
    function Get_CssValue(Index: Integer): WideString;
    { Methods & Properties }
    function Add(const CssValue: WideString): IXMLNode;
    function Insert(const Index: Integer; const CssValue: WideString): IXMLNode;
    property CssValue[Index: Integer]: WideString read Get_CssValue; default;
  end;

{ Forward Decls }

  TXMLSynWebType = class;
  TXMLCssType = class;
  TXMLCssPropertiesType = class;
  TXMLCssPropertyType = class;
  TXMLCssValuesType = class;

{ TXMLSynWebType }

  TXMLSynWebType = class(TXMLNode, IXMLSynWebType)
  protected
    { IXMLSynWebType }
    function Get_Css: IXMLCssType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLCssType }

  TXMLCssType = class(TXMLNode, IXMLCssType)
  protected
    { IXMLCssType }
    function Get_CssProperties: IXMLCssPropertiesType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLCssPropertiesType }

  TXMLCssPropertiesType = class(TXMLNodeCollection, IXMLCssPropertiesType)
  protected
    { IXMLCssPropertiesType }
    function Get_CssProperty(Index: Integer): IXMLCssPropertyType;
    function Add: IXMLCssPropertyType;
    function Insert(const Index: Integer): IXMLCssPropertyType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLCssPropertyType }

  TXMLCssPropertyType = class(TXMLNode, IXMLCssPropertyType)
  protected
    { IXMLCssPropertyType }
    function Get_Name: WideString;
    function Get_Flags: Cardinal;
    function Get_CssValues: IXMLCssValuesType;
    procedure Set_Name(Value: WideString);
    procedure Set_Flags(Value: Cardinal);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLCssValuesType }

  TXMLCssValuesType = class(TXMLNodeCollection, IXMLCssValuesType)
  protected
    { IXMLCssValuesType }
    function Get_CssValue(Index: Integer): WideString;
    function Add(const CssValue: WideString): IXMLNode;
    function Insert(const Index: Integer; const CssValue: WideString): IXMLNode;
  public
    procedure AfterConstruction; override;
  end;

{ Global Functions }

function GetsynWeb(Doc: IXMLDocument): IXMLSynWebType;
function LoadsynWeb(const FileName: WideString): IXMLSynWebType;
function NewsynWeb: IXMLSynWebType;

const
  TargetNamespace = '';

implementation

{ Global Functions }

function GetsynWeb(Doc: IXMLDocument): IXMLSynWebType;
begin
  Result := Doc.GetDocBinding('synWeb', TXMLSynWebType, TargetNamespace) as IXMLSynWebType;
end;

function LoadsynWeb(const FileName: WideString): IXMLSynWebType;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('synWeb', TXMLSynWebType, TargetNamespace) as IXMLSynWebType;
end;

function NewsynWeb: IXMLSynWebType;
begin
  Result := NewXMLDocument.GetDocBinding('synWeb', TXMLSynWebType, TargetNamespace) as IXMLSynWebType;
end;

{ TXMLSynWebType }

procedure TXMLSynWebType.AfterConstruction;
begin
  RegisterChildNode('css', TXMLCssType);
  inherited;
end;

function TXMLSynWebType.Get_Css: IXMLCssType;
begin
  Result := ChildNodes['css'] as IXMLCssType;
end;

{ TXMLCssType }

procedure TXMLCssType.AfterConstruction;
begin
  RegisterChildNode('cssProperties', TXMLCssPropertiesType);
  inherited;
end;

function TXMLCssType.Get_CssProperties: IXMLCssPropertiesType;
begin
  Result := ChildNodes['cssProperties'] as IXMLCssPropertiesType;
end;

{ TXMLCssPropertiesType }

procedure TXMLCssPropertiesType.AfterConstruction;
begin
  RegisterChildNode('cssProperty', TXMLCssPropertyType);
  ItemTag := 'cssProperty';
  ItemInterface := IXMLCssPropertyType;
  inherited;
end;

function TXMLCssPropertiesType.Get_CssProperty(Index: Integer): IXMLCssPropertyType;
begin
  Result := List[Index] as IXMLCssPropertyType;
end;

function TXMLCssPropertiesType.Add: IXMLCssPropertyType;
begin
  Result := AddItem(-1) as IXMLCssPropertyType;
end;

function TXMLCssPropertiesType.Insert(const Index: Integer): IXMLCssPropertyType;
begin
  Result := AddItem(Index) as IXMLCssPropertyType;
end;

{ TXMLCssPropertyType }

procedure TXMLCssPropertyType.AfterConstruction;
begin
  RegisterChildNode('cssValues', TXMLCssValuesType);
  inherited;
end;

function TXMLCssPropertyType.Get_Name: WideString;
begin
  Result := AttributeNodes['name'].Text;
end;

procedure TXMLCssPropertyType.Set_Name(Value: WideString);
begin
  SetAttribute('name', Value);
end;

function TXMLCssPropertyType.Get_Flags: Cardinal;
begin
  Result := AttributeNodes['flags'].NodeValue;
end;

procedure TXMLCssPropertyType.Set_Flags(Value: Cardinal);
begin
  SetAttribute('flags', Value);
end;

function TXMLCssPropertyType.Get_CssValues: IXMLCssValuesType;
begin
  Result := ChildNodes['cssValues'] as IXMLCssValuesType;
end;

{ TXMLCssValuesType }

procedure TXMLCssValuesType.AfterConstruction;
begin
  ItemTag := 'cssValue';
  ItemInterface := IXMLNode;
  inherited;
end;

function TXMLCssValuesType.Get_CssValue(Index: Integer): WideString;
begin
  Result := List[Index].Text;
end;

function TXMLCssValuesType.Add(const CssValue: WideString): IXMLNode;
begin
  Result := AddItem(-1);
  Result.NodeValue := CssValue;
end;

function TXMLCssValuesType.Insert(const Index: Integer; const CssValue: WideString): IXMLNode;
begin
  Result := AddItem(Index);
  Result.NodeValue := CssValue;
end;

end.
