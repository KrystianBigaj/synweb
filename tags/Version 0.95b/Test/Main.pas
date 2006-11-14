unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynEdit, SynEditHighlighter, ComCtrls, StdCtrls, ExtCtrls,
  SynEditExport, SynExportHTML, SynEditOptionsDialog, SynEditTextBuffer,
  SynHighlighterWeb, SynHighlighterWebData, SynHighlighterWebMisc,
  SynEditTypes;

type
  TForm1 = class(TForm)
    SynEdit1: TSynEdit;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    ComboBox2: TComboBox;
    Label2: TLabel;
    SynExporterHTML1: TSynExporterHTML;
    Button1: TButton;
    SynEditOptionsDialog1: TSynEditOptionsDialog;
    Button2: TButton;
    CheckBox2: TCheckBox;
    Label3: TLabel;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    Label4: TLabel;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Edit1: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    SynWebSyn1: TSynWebSyn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SynEdit1DropFiles(Sender: TObject; X, Y: Integer;
      AFiles: TStrings);
    procedure CheckBox1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure SynEdit1StatusChange(Sender: TObject;
      Changes: TSynStatusChanges);
    procedure CheckBox2Click(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure SynEdit1PaintTransient(Sender: TObject; Canvas: TCanvas;
      TransientType: TTransientType);
  private
    { Private declarations }
  public
    ftLine1,ftLine2:TBufferCoord;
  end;

var
  Form1: TForm1;

implementation

uses Types;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  s:String;
  i:THtmlVersion;
  j:TCssVersion;
  k:TPhpVersion;
begin
  ftLine1.Char:=0;
  ftLine2.Line:=0;
  ftLine1.Char:=0;
  ftLine2.Line:=0;
  for i:=Low(THtmlVersion) to High(THtmlVersion) do
    ComboBox1.Items.Add(THtmlVersionStr[i]);
  ComboBox1.ItemIndex:=Integer(SynWebSyn1.HtmlVersion);

  for j:=Low(TCssVersion) to High(TCssVersion) do
    ComboBox2.Items.Add(TCssVersionStr[j]);
  ComboBox2.ItemIndex:=Integer(SynWebSyn1.CssVersion);

  for k:=Low(TPhpVersion) to High(TPhpVersion) do
    ComboBox3.Items.Add(TPhpVersionStr[k]);
  ComboBox3.ItemIndex:=Integer(SynWebSyn1.PhpVersion);

  ComboBox4.ItemIndex:=Integer(SynWebSyn1.HighlighterMode);

  CheckBox3.Checked:=SynWebSyn1.PhpAspTags;
  CheckBox4.Checked:=SynWebSyn1.PhpShortOpenTag;

  s:=ChangeFileExt(Application.ExeName,'_sample.txt');
  if FileExists(s) then
    SynEdit1.Lines.LoadFromFile(s);
  CheckBox2.Checked:=SynWebSyn1.ActiveHighlighter;
  CheckBox2Click(nil);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SynEdit1.Lines.SaveToFile(ChangeFileExt(Application.ExeName,'_sample.txt'));
end;

procedure TForm1.SynEdit1DropFiles(Sender: TObject; X, Y: Integer;
  AFiles: TStrings);
begin
  if (AFiles.Count>0) and (FileExists(AFiles[0])) then
    SynEdit1.Lines.LoadFromFile(AFiles[0]);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
//  SynWebSyn1.Enabled:=CheckBox1.Checked;
  if CheckBox1.Checked then
    synEdit1.Highlighter:=SynWebSyn1
  else
    SynEdit1.Highlighter:=nil;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  SynWebSyn1.HtmlVersion:=THtmlVersion(ComboBox1.ItemIndex);
//  SynEdit1.InvalidateLines;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SynExporterHTML1.ExportAsText := TRUE;
  SynExporterHTML1.ExportAll(SynEdit1.Lines);
  SynExporterHTML1.SaveToFile('C:\demo.html');
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

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  SynWebSyn1.CssVersion:=TCssVersion(ComboBox2.ItemIndex);
end;

procedure TForm1.SynEdit1StatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
var
  t:TSynHighlighterTypes;
begin
  if CheckBox2.Checked then
    if Changes-[scCaretX, scCaretY]<>Changes then
    begin              //, shtCss, shtES, shtPHP_inHtml, shtPHP_inCss, shtPHP_inES
      t:=SynWeb_UpdateActiveHighlighter(SynEdit1, SynWebSyn1);
      Label5.Caption:='';
      if shtHtml in t then
        Label5.Caption:=Label5.Caption+'HTML,';
      if shtCss in t then       
        Label5.Caption:=Label5.Caption+'CSS,';
      if shtES in t then          
        Label5.Caption:=Label5.Caption+'JS,';
      if t-[shtPHP_inHtml, shtPHP_inCss, shtPHP_inES]<>t then
        Label5.Caption:=Label5.Caption+'PHP,';
    end;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  SynWebSyn1.ActiveHighlighter:=CheckBox2.Checked;
  if CheckBox2.Checked then
    SynEdit1StatusChange(SynEdit1,[scCaretX, scCaretY]);
  //else
    SynEdit1.Repaint;
end;

procedure TForm1.ComboBox3Change(Sender: TObject);
begin
  SynWebSyn1.PhpVersion:=TPhpVersion(ComboBox3.ItemIndex);
end;

procedure TForm1.ComboBox4Change(Sender: TObject);
begin
  SynWebSyn1.HighlighterMode:=TSynHighlighterMode(ComboBox4.ItemIndex);
  CheckBox2Click(nil);
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  SynWebSyn1.PhpShortOpenTag:=CheckBox4.Checked;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  SynWebSyn1.PhpAspTags:=CheckBox3.Checked;
end;

procedure TForm1.SynEdit1PaintTransient(Sender: TObject; Canvas: TCanvas;
  TransientType: TTransientType);
const
  OpenTokens:array[0..3] of String=('{', '(', '[', 'match_me_open');
  CloseTokens:array[0..3] of String=('}', ')', ']', 'match_me_close');
  TokensID:array[0..3] of TtkTokenKind=(tkPhpSymbol,tkPhpSymbol,tkPhpSymbol, tkPhpIdentifier);
var
  B,b2:TBufferCoord;
  i,id:Integer;
  t1,t2:String;

  procedure DrawOpen;
  var
    p:TPoint;
  begin
    p:=SynEdit1.RowColumnToPixels(SynEdit1.BufferToDisplayPos(b2));
    Panel2.Left:=SynEdit1.Left+p.X+2;
    Panel2.Top:=SynEdit1.Top+p.Y+SynEdit1.LineHeight;
    Panel2.Width:=Length(t1)*SynEdit1.CharWidth;
    Panel2.Visible:=True;
  end;

  procedure DrawClose;  
  var
    p:TPoint;
  begin
    p:=SynEdit1.RowColumnToPixels(SynEdit1.BufferToDisplayPos(b));
    Panel3.Left:=SynEdit1.Left+p.X+2;
    Panel3.Top:=SynEdit1.Top+p.Y+SynEdit1.LineHeight;
    Panel3.Width:=Length(t2)*SynEdit1.CharWidth;
    Panel3.Visible:=True;
  end;

  procedure DrawOpenClose;
  begin
    DrawOpen;
    DrawClose;
  end;

begin
  Caption:='';
  if SynEdit1.SelAvail or (TransientType=ttBefore) then
    Exit;

  i:=SynWeb_FindMatchingToken(SynEdit1,SynWebSyn1,
    OpenTokens,CloseTokens,TokensID,
    SynEdit1.CaretXY,b,id);
  b2:=SynEdit1.CaretXY;

  case i of
  -2:
      begin
        Label6.Caption:=
          Format('Open token ("%s") found at [%d, %d].'+
                 'Close token ("%s") found at [%d, %d].',
                 [OpenTokens[id],b.Line,b.Char,CloseTokens[id],b2.Line,b2.Char]);      
        t1:=CloseTokens[id];
        t2:=OpenTokens[id];
        DrawOpenClose;
      end;
  -1:
      begin
        Label6.Caption:=
          Format('Open token ("%s") found at [%d, %d].'+
                 'Close token ("%s") didn''t match!',
                 [OpenTokens[id],b.Line,b.Char,CloseTokens[id]]);    
        t1:=CloseTokens[id];
        DrawOpen;
        Panel3.Visible:=False;
      end;
   0: begin
        Panel2.Visible:=False;
        Panel3.Visible:=False;
        Label6.Caption:='Token match not found!';
      end;
   1:
      begin
        Label6.Caption:=
          Format('Open token ("%s") found at [%d, %d].'+
                 'Close token ("%s") didn''t match!',
                 [OpenTokens[id],b2.Line,b2.Char,CloseTokens[id]]);
        t1:=OpenTokens[id];
        DrawOpen;
        Panel3.Visible:=False;
      end;
   2:
      begin
        Label6.Caption:=
          Format('Open token ("%s") found at [%d, %d].'+
                 'Close token ("%s") found at [%d, %d].',
                 [OpenTokens[id],b2.Line,b2.Char,CloseTokens[id],b.Line,b.Char]);
        t1:=OpenTokens[id];   
        t2:=CloseTokens[id];
        DrawOpenClose;
      end;
  end;
end;

end.
