{-----------------------------------------------------------------------------
 Unit Name: uMainForm
 Author:    Krystian
 Date:      29-Mar-2011
 Purpose:
 History:
-----------------------------------------------------------------------------}

unit uMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, xmlSynWeb, SynEdit, SynEditHighlighter,
  SynHighlighterWeb, uSynWebCss;

const
  cXmlDefaultFilename = 'xmlSynWeb.xml';

type

{ TfrmMain }

  TfrmMain = class(TForm)
    pcMain: TPageControl;
    tsEditor: TTabSheet;
    tsPreview: TTabSheet;
    gbProperties: TGroupBox;
    lbProperties: TListBox;
    gbAddProperties: TGroupBox;
    btnAddProperties: TButton;
    mProperties: TMemo;
    panClient: TPanel;
    gbValues: TGroupBox;
    mValues: TMemo;
    gbFlags: TGroupBox;
    panFlags: TPanel;
    cbFlag31: TCheckBox;
    cbFlag30: TCheckBox;
    cbFlag29: TCheckBox;
    cbFlag28: TCheckBox;
    cbFlag27: TCheckBox;
    cbFlag26: TCheckBox;
    cbFlag24: TCheckBox;
    cbFlag23: TCheckBox;
    cbFlag22: TCheckBox;
    cbFlag21: TCheckBox;
    cbFlag20: TCheckBox;
    cbFlag19: TCheckBox;
    cbFlag18: TCheckBox;
    cbFlag17: TCheckBox;
    cbFlag16: TCheckBox;
    edtFlags: TEdit;
    lblFlags: TLabel;
    cbFlag25: TCheckBox;
    btnRemoveProperty: TButton;
    syn: TSynEdit;
    SynWebEngine: TSynWebEngine;
    SynWebHtmlSyn: TSynWebHtmlSyn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddPropertiesClick(Sender: TObject);
    procedure lbPropertiesClick(Sender: TObject);
    procedure cbFlagClick(Sender: TObject);
    procedure edtFlagsChange(Sender: TObject);
    procedure mValuesChange(Sender: TObject);
    procedure btnRemovePropertyClick(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure SynWebEngineCssCheckVendorProperty(const AProperty: string;
      var AIsVendor: Boolean; var APropertyId: Integer);
    procedure SynWebEngineCssGetVendorPropertyFlags(APropertyId: Integer;
      var AFlags: Cardinal);
    procedure SynWebEngineCssCheckVendorValue(APropertyId: Integer;
      const AValue: string; var AIsValid: Boolean);
  private
    FXml: IXMLSynWebType;
    FXmlFile: String;

    FSampleFile: String;

    FSynWebCss: TSynWebCssProperties;

    function GetCurrentProperty(out AIdx: Integer): Boolean;

    procedure LoadChkFlags(AFlags: Cardinal);

    function GetFlags: Cardinal;
    procedure SetFlags(AValue: Cardinal);

    procedure LoadProeprty(AProperty: IXMLCssPropertyType);

    procedure UpdateValues(AValues: IXMLCssValuesType);

    function CurrentProperty: IXMLCssPropertyType;
  public
    procedure ReloadXml;
    procedure SaveXml;
  end;

var
  frmMain: TfrmMain;

implementation

uses XMLIntf;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FSampleFile := ChangeFileExt(Application.ExeName, '.htm');
  FXmlFile := ExtractFilePath(Application.ExeName) + cXmlDefaultFilename;

  if FileExists(FXmlFile) then
    FXml := xmlSynWeb.LoadsynWeb(FXmlFile)
  else
    FXml := xmlSynWeb.NewsynWeb;

  ReloadXml;

  if FileExists(FSampleFile) then
    syn.Lines.LoadFromFile(FSampleFile);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  SaveXml;

  syn.Lines.SaveToFile(FSampleFile);
end;

function TfrmMain.GetCurrentProperty(out AIdx: Integer): Boolean;
begin
  AIdx := lbProperties.ItemIndex;
  Result := AIdx > -1;
  if Result then
    AIdx := Integer(lbProperties.Items.Objects[AIdx]);
end;

function TfrmMain.GetFlags: Cardinal;
begin
  Result := Cardinal(StrToInt64Def(edtFlags.Text, 0));
end;

procedure TfrmMain.btnAddPropertiesClick(Sender: TObject);
var
  lIdx: Integer;
  lPropertyName: String;
  lProperty: IXMLCssPropertyType;
begin
  for lIdx := 0 to mProperties.Lines.Count - 1 do
  begin
    lPropertyName := Trim(mProperties.Lines[lIdx]);
    if Pos(' ', lPropertyName) > 0 then
    begin
      ShowMessage(Format('Invalid property "%s" name. Skipping', [lPropertyName]));
      Continue;
    end;

    if lbProperties.Items.IndexOf(lPropertyName) > -1 then
    begin
      ShowMessage(Format('Property "%s" already added. Skipping', [lPropertyName]));
      Continue;
    end;

    lProperty := FXml.Css.CssProperties.Add;
    lProperty.Name := lPropertyName;
    lProperty.Flags := GetFlags;

    UpdateValues(lProperty.CssValues);
  end;

  mProperties.Clear;

  ReloadXml;
end;

procedure TfrmMain.btnRemovePropertyClick(Sender: TObject);
var
  lIdx: Integer;
begin
  if not GetCurrentProperty(lIdx) then
    Exit;

  FXml.Css.CssProperties.Delete(lIdx);
  ReloadXml;
end;

procedure TfrmMain.cbFlagClick(Sender: TObject);
var
  lFlags: Cardinal;
  lChk: TCheckBox;
begin
  if not (Sender is TCheckBox) then
    Exit;

  lFlags := GetFlags;

  lChk := TCheckBox(Sender);
  if lChk.Checked then
    lFlags := lFlags or (1 shl lChk.Tag)
  else
    lFlags := lFlags and not (1 shl lChk.Tag);

  SetFlags(lFlags);
end;

procedure TfrmMain.lbPropertiesClick(Sender: TObject);
var
  lIdx: Integer;
begin
  if GetCurrentProperty(lIdx) then
    LoadProeprty(FXml.Css.CssProperties[lIdx]);
end;

procedure TfrmMain.LoadChkFlags(AFlags: Cardinal);
var
  lIdx: Integer;
  lChk: TCheckBox;
begin
  for lIdx := 0 to panFlags.ControlCount - 1 do
    if panFlags.Controls[lIdx] is TCheckBox then
    begin
      lChk := TCheckBox(panFlags.Controls[lIdx]);

      lChk.Checked := (AFlags and (1 shl lChk.Tag)) <> 0;
    end;
end;

procedure TfrmMain.LoadProeprty(AProperty: IXMLCssPropertyType);
var
  lIdx: Integer;
begin
  SetFlags(AProperty.Flags);

  mValues.OnChange := nil;
  try
    mValues.Lines.BeginUpdate;
    try
      mValues.Lines.Clear;

      for lIdx := 0 to AProperty.CssValues.Count - 1 do
        mValues.Lines.Add(AProperty.CssValues[lIdx]);
    finally
      mValues.Lines.EndUpdate;
    end;
  finally
    mValues.OnChange := mValuesChange;
  end;
end;

procedure TfrmMain.mValuesChange(Sender: TObject);
var
  lProp: IXMLCssPropertyType;
begin
  lProp := CurrentProperty;
  if lProp <> nil then
  begin
    UpdateValues(lProp.CssValues);
    SaveXml;
  end;
end;

procedure TfrmMain.pcMainChange(Sender: TObject);
begin
  syn.Lines.SaveToFile(FSampleFile);

  FreeAndNil(FSynWebCss);

  FSynWebCss := TSynWebCssProperties.Create(FXml.Css);
end;

function TfrmMain.CurrentProperty: IXMLCssPropertyType;
var
  lIdx: Integer;
begin
  if GetCurrentProperty(lIdx) then
    Result := FXml.Css.CssProperties[lIdx]
  else
    Result := nil;
end;

procedure TfrmMain.edtFlagsChange(Sender: TObject);
var
  lProp: IXMLCssPropertyType;
  lFlags: Cardinal;
begin
  lFlags := GetFlags;
  lProp := CurrentProperty;
  if lProp <> nil then
    if lProp.Flags <> lFlags then
    begin
      lProp.Flags := lFlags;
      SaveXml;
    end;

  LoadChkFlags(lFlags);
end;

procedure TfrmMain.ReloadXml;
var
  lIdx: Integer;
  lPrevIdx: Integer;
begin
  lbProperties.Items.BeginUpdate;
  try
    if not GetCurrentProperty(lPrevIdx) then
      lPrevIdx := -1;

    lbProperties.Items.Clear;
    
    for lIdx := 0 to FXml.Css.CssProperties.Count - 1 do
      lbProperties.Items.AddObject(FXml.Css.CssProperties[lIdx].Name, TObject(lIdx));
  finally
    lbProperties.Items.EndUpdate;
  end;

  for lIdx := 0 to lbProperties.Items.Count - 1 do
    if Integer(lbProperties.Items.Objects[lIdx]) = lPrevIdx then
      lbProperties.ItemIndex := lIdx;

  if lbProperties.ItemIndex = -1 then
    if lbProperties.Items.Count > 0 then
      lbProperties.ItemIndex := 0;

  lbPropertiesClick(nil);
end;

procedure TfrmMain.SaveXml;
begin
  FXml.OwnerDocument.SaveToFile(FXmlFile);
end;

procedure TfrmMain.SetFlags(AValue: Cardinal);
begin
  edtFlags.Text := '$' + IntToHex(Int64(AValue), 8);
end;

procedure TfrmMain.SynWebEngineCssCheckVendorProperty(const AProperty: string;
  var AIsVendor: Boolean; var APropertyId: Integer);
begin
  if FSynWebCss = nil then
    Exit;

  APropertyId := FSynWebCss.GetPropertyIndex(AProperty);
  if APropertyId > -1 then
    AIsVendor := True;
end;

procedure TfrmMain.SynWebEngineCssCheckVendorValue(APropertyId: Integer;
  const AValue: string; var AIsValid: Boolean);
begin
  if FSynWebCss = nil then
    Exit;

  FSynWebCss.GetValidPropertyValue(APropertyId, AValue, AIsValid);
end;

procedure TfrmMain.SynWebEngineCssGetVendorPropertyFlags(APropertyId: Integer;
  var AFlags: Cardinal);
begin
  if FSynWebCss = nil then
    Exit;

  FSynWebCss.GetPropertyFlags(APropertyId, AFlags);
end;

procedure TfrmMain.UpdateValues(AValues: IXMLCssValuesType);
var
  lIdx: Integer;
  lValue: String;
begin
  AValues.Clear;

  for lIdx := 0 to mValues.Lines.Count - 1 do
  begin
    lValue := Trim(mValues.Lines[lIdx]);

    if Pos(' ', lValue) > 0 then
      Continue;

    AValues.Add(lValue);
  end;
end;

end.

