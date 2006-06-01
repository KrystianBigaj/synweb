unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynEdit, SynHighlighterWeb, StdCtrls, SynEditHighlighter,
  ExtCtrls, SynEditOptionsDialog, SynEditExport, SynExportHTML, SynTokenMatch,
  SynHighlighterWebData, SynHighlighterWebMisc, SynEditTypes, SynUnicode;

type
  TForm1 = class(TForm)
    SynWebEngine1: TSynWebEngine;
    SynWebHtmlSyn1: TSynWebHtmlSyn;
    SynWebCSSSyn1: TSynWebCSSSyn;
    SynWebESSyn1: TSynWebESSyn;
    SynWebPHPCliSyn1: TSynWebPHPCliSyn;
    SynEdit1: TSynEdit;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Button1: TButton;
    Button2: TButton;
    CheckBox2: TCheckBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    SynExporterHTML1: TSynExporterHTML;
    SynEditOptionsDialog1: TSynEditOptionsDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SynEdit1StatusChange(Sender: TObject;
      Changes: TSynStatusChanges);
    procedure SynEdit1PaintTransient(Sender: TObject; Canvas: TCanvas;
      TransientType: TTransientType);
    procedure CheckBox2Click(Sender: TObject);
    procedure SynEdit1DropFiles(Sender: TObject; X, Y: Integer;
      AFiles: TWideStrings);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  s:String;
  i:TSynWebHtmlVersion;
  j:TSynWebCssVersion;
  k:TSynWebPhpVersion;
begin
  SynEdit1.Lines.SaveUnicode:=True;
  for i:=Low(TSynWebHtmlVersion) to High(TSynWebHtmlVersion) do
    ComboBox1.Items.Add(TSynWebHtmlVersionStr[i]);
  ComboBox1.ItemIndex:=Integer(shvXHtml10Transitional);

  for j:=Low(TSynWebCssVersion) to High(TSynWebCssVersion) do
    ComboBox2.Items.Add(TSynWebCssVersionStr[j]);
  ComboBox2.ItemIndex:=Integer(scvCss21);

  for k:=Low(TSynWebPhpVersion) to High(TSynWebPhpVersion) do
    ComboBox3.Items.Add(TSynWebPhpVersionStr[k]);
  ComboBox3.ItemIndex:=Integer(spvPHP5);

  s:=ChangeFileExt(Application.ExeName,'_sample.txt');
  if FileExists(s) then
    SynEdit1.Lines.LoadFromFile(s);
  CheckBox2Click(nil);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SynEdit1.Lines.SaveToFile(ChangeFileExt(Application.ExeName,'_sample.txt'));
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if SynEdit1.Highlighter<>nil then
    synEdit1.Highlighter.Enabled:=CheckBox1.Checked;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SynExporterHTML1.Highlighter:=SynEdit1.Highlighter;
  SynExporterHTML1.ExportAsText := TRUE;
  SynExporterHTML1.ExportAll(SynEdit1.Lines);
  SynExporterHTML1.SaveToFile('C:\demo.html');
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  SynWebEngine1.Options.PhpAspTags:=CheckBox3.Checked;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin                    
  SynWebEngine1.Options.PhpShortOpenTag:=CheckBox4.Checked;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  SynWebEngine1.Options.HtmlVersion:=TSynWebHtmlVersion(ComboBox1.ItemIndex);
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  SynWebEngine1.Options.CssVersion:=TSynWebCssVersion(ComboBox2.ItemIndex);
end;

procedure TForm1.ComboBox3Change(Sender: TObject);
begin
  SynWebEngine1.Options.PhpVersion:=TSynWebPhpVersion(ComboBox3.ItemIndex);
end;

procedure TForm1.ComboBox4Change(Sender: TObject);
begin
  case ComboBox4.ItemIndex of
  0:
    SynEdit1.Highlighter:=SynWebHtmlSyn1;
  1:
    SynEdit1.Highlighter:=SynWebCSSSyn1;
  2:
    SynEdit1.Highlighter:=SynWebESSyn1;
  3:
    SynEdit1.Highlighter:=SynWebPHPCliSyn1;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  t:TSynEditorOptionsContainer;
begin
  t:=TSynEditorOptionsContainer.create(SynEdit1);
  t.Assign(SynEdit1);
  if SynEditOptionsDialog1.Execute(t) then
    t.AssignTo(SynEdit1);
end;

procedure TForm1.SynEdit1StatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
var
  t:TSynWebHighlighterTypes;
begin
  if CheckBox2.Checked then
    if Changes-[scCaretX, scCaretY]<>Changes then
    begin           
      t:=SynWebUpdateActiveHighlighter(SynEdit1, TSynWebBase(SynEdit1.Highlighter));
      Label5.Caption:='';
      if shtHtml in t then
        Label5.Caption:=Label5.Caption+'HTML,';
      if shtCss in t then       
        Label5.Caption:=Label5.Caption+'CSS,';
      if shtES in t then          
        Label5.Caption:=Label5.Caption+'JS,';
      if t-[shtPhpInHtml, shtPhpInCss, shtPhpInES]<>t then
        Label5.Caption:=Label5.Caption+'PHP,';
    end;
end;

procedure TForm1.SynEdit1PaintTransient(Sender: TObject; Canvas: TCanvas;
  TransientType: TTransientType);
const
  PasTokens:array[0..2] of TSynTokenMatch=(
    (OpenToken: '('; CloseToken: ')'; TokenKind: Integer(stkPhpSymbol)),
    (OpenToken: '['; CloseToken: ']'; TokenKind: Integer(stkPhpSymbol)),
    (OpenToken: '{'; CloseToken: '}'; TokenKind: Integer(stkPhpSymbol)));
var
  Editor : TSynEdit;  
  Pix: TPoint;      
  Match: TSynTokenMatches;
  I: Integer;

  function CharToPixels(P: TBufferCoord): TPoint;
  begin
    Result:=Editor.RowColumnToPixels(Editor.BufferToDisplayPos(P));
  end;

begin                  
  Editor := TSynEdit(Sender);
  if Editor.SelAvail then
    Exit;

  I := SynEditGetMatchingTokenEx(Editor, Editor.CaretXY, PasTokens, Match);
  if I = 0 then
    Exit;

  Canvas.Brush.Style := bsSolid;
  Canvas.Font.Assign(Editor.Font);
  Canvas.Font.Style := Match.TokenAttri.Style;

  if (TransientType = ttBefore) then   
    Canvas.Brush.Color := Match.TokenAttri.Background
  else
    if Abs(I) = 2 then
      Canvas.Brush.Color := clAqua
    else                          
      Canvas.Brush.Color := clYellow;

  if Canvas.Font.Color = clNone then
    Canvas.Font.Color := Editor.Font.Color;
  if Canvas.Brush.Color = clNone then
    Canvas.Brush.Color := Editor.Color;

  if I <> -1 then
  begin
    Pix := CharToPixels(Match.OpenTokenPos);
    Canvas.TextOut(Pix.X, Pix.Y, Match.OpenToken);
  end;
  if I <> 1 then
  begin
    Pix := CharToPixels(Match.CloseTokenPos);
    Canvas.TextOut(Pix.X, Pix.Y, Match.CloseToken);
  end;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  TSynWebBase(SynEdit1.Highlighter).ActiveSwitchHighlighter:=CheckBox2.Checked;
  if CheckBox2.Checked then
    SynEdit1StatusChange(SynEdit1,[scCaretX, scCaretY]);
  //else
    SynEdit1.Repaint;
end;

procedure TForm1.SynEdit1DropFiles(Sender: TObject; X, Y: Integer;
  AFiles: TWideStrings);
begin
  if (AFiles.Count>0) and (FileExists(AFiles[0])) then
    SynEdit1.Lines.LoadFromFile(AFiles[0]);
end;

end.
