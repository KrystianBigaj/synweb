unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, SynEdit, SynHighlighterWeb, StdCtrls, SynEditHighlighter,
  ExtCtrls, SynEditOptionsDialog, SynEditExport, SynExportHTML, SynTokenMatch,
  SynHighlighterWebData, SynHighlighterWebMisc, SynEditTypes,
  SynEditTextBuffer, SynCompletionProposal, StrUtils, Menus;

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
    Button3: TButton;
    SynWebWmlSyn1: TSynWebWmlSyn;
    Label6: TLabel;
    ComboBox5: TComboBox;
    SynWebXmlSyn1: TSynWebXmlSyn;
    scpDemo: TSynCompletionProposal;
    SynWebErrorTimer: TTimer;
    SynWebErrorList: TListBox;
    PopupMenu1: TPopupMenu;
    Reload1: TMenuItem;
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
    procedure Button3Click(Sender: TObject);
    procedure ComboBox5Change(Sender: TObject);
    procedure scpDemoExecute(Kind: SynCompletionType; Sender: TObject;
      var CurrentInput: WideString; var x, y: Integer; var CanExecute: Boolean);
    procedure SynEdit1Change(Sender: TObject);
    procedure SynWebErrorTimerTimer(Sender: TObject);
    procedure SynWebErrorListDblClick(Sender: TObject);
    procedure Reload1Click(Sender: TObject);
    procedure scpDemoAfterCodeCompletion(Sender: TObject;
      const Value: WideString; Shift: TShiftState; Index: Integer;
      EndToken: WideChar);
  private
    FPaintUpdating: Boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  s: String;
  i: TSynWebHtmlVersion;
  j: TSynWebCssVersion;
  k: TSynWebPhpVersion;
  w: TSynWebWmlVersion;
begin
  FPaintUpdating := False;
  for i:=Low(TSynWebHtmlVersion) to High(TSynWebHtmlVersion) do
    ComboBox1.Items.Add(TSynWebHtmlVersionStr[i]);
  ComboBox1.ItemIndex:=Integer(SynWebEngine1.Options.HtmlVersion);

  for j:=Low(TSynWebCssVersion) to High(TSynWebCssVersion) do
    ComboBox2.Items.Add(TSynWebCssVersionStr[j]);
  ComboBox2.ItemIndex:=Integer(SynWebEngine1.Options.CssVersion);

  for k:=Low(TSynWebPhpVersion) to High(TSynWebPhpVersion) do
    ComboBox3.Items.Add(TSynWebPhpVersionStr[k]);
  ComboBox3.ItemIndex:=Integer(SynWebEngine1.Options.PhpVersion);

  for w:=Low(TSynWebWmlVersion) to High(TSynWebWmlVersion) do
    ComboBox5.Items.Add(TSynWebWmlVersionStr[w]);
  ComboBox5.ItemIndex:=Integer(SynWebEngine1.Options.WmlVersion);

  ComboBox4.ItemIndex := 0;

  s:=ChangeFileExt(Application.ExeName,'_sample.txt');
  if FileExists(s) then
    SynEdit1.Lines.LoadFromFile(s)
  else
    Button3.Click;
  CheckBox2Click(nil);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SynEdit1.Lines.SaveToFile(ChangeFileExt(Application.ExeName,'_sample.txt'));
end;

procedure TForm1.Reload1Click(Sender: TObject);
begin
  SynWebErrorTimerTimer(nil);
end;

procedure TForm1.scpDemoAfterCodeCompletion(Sender: TObject;
  const Value: WideString; Shift: TShiftState; Index: Integer;
  EndToken: WideChar);

  function CaretBetween(AStr: String): Boolean;
  var
    i: Integer;
  begin
    i := Pos(AStr, Value);
    Result := i > 0;
    if Result then
      SynEdit1.CaretX := SynEdit1.CaretX - (Length(Value) - i);
  end;

begin
  CaretBetween('()') or CaretBetween('><') or CaretBetween('""') or CaretBetween(' ;');
end;

procedure TForm1.scpDemoExecute(Kind: SynCompletionType; Sender: TObject;
  var CurrentInput: WideString; var x, y: Integer; var CanExecute: Boolean);
begin
  SynWebFillCompletionProposal(SynEdit1, SynWebHtmlSyn1, scpDemo, CurrentInput);
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
  4:
    SynEdit1.Highlighter:=SynWebWmlSyn1;
  5:
    SynEdit1.Highlighter:=SynWebXmlSyn1;
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
  if Changes-[scCaretX, scCaretY]<>Changes then
  begin
    t := SynWebUpdateActiveHighlighter(SynEdit1, TSynWebBase(SynEdit1.Highlighter));
    Label5.Caption:='';
    if shtML in t then
      Label5.Caption:=Label5.Caption+'HTML/WML,';
    if shtCss in t then
      Label5.Caption:=Label5.Caption+'CSS,';
    if shtES in t then
      Label5.Caption:=Label5.Caption+'JS,';
    if t-[shtPhpInML, shtPhpInCss, shtPhpInES]<>t then
      Label5.Caption:=Label5.Caption+'PHP,';
  end;
  with SynEdit1, SynWebHtmlSyn1 do
  begin
    if SynEdit1.CaretY = 1 then
      ResetRange
    else
      SetRange(TSynEditStringList(Lines).Ranges[CaretY - 2]);
    SetLine(Lines[CaretY-1], CaretY-1);
    while not GetEol and (CaretX-1 >= GetTokenPos + Length(GetToken)) do
      Next;
    Caption := Format('%.8x, %s, %d, %d', [Integer(GetRange),
      GetToken, GetTagID, GetTagKind]);
  end;
end;

procedure TForm1.SynWebErrorListDblClick(Sender: TObject);
var
  i: Integer;
  pos: Longword;
  caret: TBufferCoord;
begin
  if SynWebErrorTimer.Enabled then
    SynWebErrorTimerTimer(nil);
  i := SynWebErrorList.ItemIndex;
  if i = -1 then
    Exit;

  pos := Longword(SynWebErrorList.Items.Objects[i]);
  caret.Char := pos and $00000FFF;
  caret.Line := pos shr 12;
  SynEdit1.CaretXY := caret;
  SynEdit1.SetFocus;
end;

procedure TForm1.SynWebErrorTimerTimer(Sender: TObject);
var
  i, idx, idxtop: Integer;
  hl: TSynWebBase;
  e: TCustomSynEdit;

  procedure AddError(S: WideString);
  var
    pos: Longword;
  begin
    s := Format(s, [hl.GetToken]);
    pos := hl.GetTokenPos + 1;
    if pos > $00000FFF then
      pos := $00000FFF;
      
    pos := pos or Longword((i + 1) shl 12);

    SynWebErrorList.AddItem(Format('[%4.d, %4.d] - %s',
      [i + 1 , hl.GetTokenPos + 1, s]),
      TObject(pos));
  end;

begin
  SynWebErrorTimer.Enabled := False;
  e := SynEdit1;
  SynWebErrorList.Items.BeginUpdate;
  idx := SynWebErrorList.ItemIndex;
  idxtop := SynWebErrorList.TopIndex;
  SynWebErrorList.Clear;

  if e.Highlighter is TSynWebBase then
  begin
    hl := TSynWebBase(e.Highlighter);
    hl.ResetRange;
    i := 0;
    while i < e.Lines.Count do
    begin
      hl.SetLine(e.Lines[i], i + 1);
      while not hl.GetEol do
      begin
        case hl.GetTokenID of
        stkMLTagNameUndef:
          AddError('HTML: Invalid tag "%s"');
        stkMLTagKeyUndef:
          AddError('HTML: Invalid attribute "%s"');
        stkMLError:
          AddError('HTML: Invalid token "%s"');

        stkCssSelectorUndef:
          AddError('CSS: Invalid selector "%s"');
        stkCssPropUndef:
          AddError('CSS: Invalid property "%s"');
        stkCssValUndef:
          AddError('CSS: Invalid value "%s"');
        stkCssError:
          AddError('CSS: Invalid token "%s"');

        stkEsError:
          AddError('JS: Invalid token "%s"');

        stkPhpError:
          AddError('PHP: Invalid token "%s"');
        end;
        hl.Next;
      end;
      Inc(i);
    end;
  end;
  SynWebErrorList.Visible := SynWebErrorList.Items.Count > 0;
  if idx < SynWebErrorList.Count then
  begin
    SynWebErrorList.ItemIndex := idx;
    SynWebErrorList.TopIndex := idxtop;
  end;

  SynWebErrorList.Items.EndUpdate;
end;

procedure TForm1.SynEdit1Change(Sender: TObject);
begin
  SynWebErrorTimer.Enabled := False;
  SynWebErrorTimer.Enabled := True;
end;

procedure TForm1.SynEdit1PaintTransient(Sender: TObject; Canvas: TCanvas;
  TransientType: TTransientType);
const
  Tokens:array[0..15] of TSynTokenMatch=(
    (OpenToken: '('; CloseToken: ')'; TokenKind: Integer(stkCssSymbol)),
    (OpenToken: '{'; CloseToken: '}'; TokenKind: Integer(stkCssSymbol)),
    (OpenToken: '['; CloseToken: ']'; TokenKind: Integer(stkCssSymbol)),
    (OpenToken: '('; CloseToken: ')'; TokenKind: Integer(stkEsSymbol)),
    (OpenToken: '{'; CloseToken: '}'; TokenKind: Integer(stkEsSymbol)),
    (OpenToken: '['; CloseToken: ']'; TokenKind: Integer(stkEsSymbol)),
    (OpenToken: '('; CloseToken: ')'; TokenKind: Integer(stkPhpSymbol)),
    (OpenToken: '['; CloseToken: ']'; TokenKind: Integer(stkPhpSymbol)),
    (OpenToken: '{'; CloseToken: '}'; TokenKind: Integer(stkPhpSymbol)),
    (OpenToken: '<'; CloseToken: '>'; TokenKind: Integer(stkMLTag)),
    (OpenToken: '<'; CloseToken: '/>'; TokenKind: Integer(stkMLTag)),
    (OpenToken: '</'; CloseToken: '>'; TokenKind: Integer(stkMLTag)),
    (OpenToken: '<!'; CloseToken: '>'; TokenKind: Integer(stkMLTag)),
    (OpenToken: '<![cdata['; CloseToken: ']]>'; TokenKind: Integer(stkMLTag)),
    (OpenToken: '<?'; CloseToken: '?>'; TokenKind: Integer(stkMLTag)),
    (OpenToken: '<%'; CloseToken: '%>'; TokenKind: Integer(stkMLTag)));
var
  Editor : TSynEdit;  
  Pix: TPoint;      
  Match: TSynTokenMatched;
  I: Integer;

  function CharToPixels(P: TBufferCoord): TPoint;
  begin
    Result:=Editor.RowColumnToPixels(Editor.BufferToDisplayPos(P));
  end;

  function TryMatch: Integer;
  begin
    Result := SynEditGetMatchingTagEx(Editor, Editor.CaretXY, Match);
    if Result = 0 then
      Result := SynEditGetMatchingTokenEx(Editor, Editor.CaretXY, Tokens, Match);
  end;

begin
  if FPaintUpdating then
    Exit;
  Editor := TSynEdit(Sender);
  if TransientType = ttBefore then
  begin
    I := TryMatch;
    if I = 0 then
      Exit;
    FPaintUpdating := True;
    if I <> -1 then
      Editor.InvalidateLines(Match.OpenTokenPos.Line, Match.OpenTokenPos.Line);
    if I <> 1 then
      Editor.InvalidateLines(Match.CloseTokenPos.Line, Match.CloseTokenPos.Line);
    FPaintUpdating := False;
    Exit;
  end;
  if Editor.SelAvail then
    Exit;
  I := TryMatch;
  if I = 0 then
    Exit;
  Canvas.Brush.Style := bsSolid;                           
  if Abs(I) = 2 then
    Canvas.Brush.Color := clAqua // matched color
  else
    Canvas.Brush.Color := clYellow; // unmatched color
  if I <> -1 then
  begin
    Pix := CharToPixels(Match.OpenTokenPos);
    Canvas.Font.Color := Editor.Font.Color;
    Canvas.Font.Style := Match.TokenAttri.Style;
    Canvas.TextOut(Pix.X, Pix.Y, Match.OpenToken);
  end;
  if I <> 1 then
  begin
    Pix := CharToPixels(Match.CloseTokenPos);    
    Canvas.Font.Color := Editor.Font.Color;
    Canvas.Font.Style := Match.TokenAttri.Style;  
    Canvas.TextOut(Pix.X, Pix.Y, Match.CloseToken);
  end;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  TSynWebBase(SynEdit1.Highlighter).ActiveHighlighterSwitch:=CheckBox2.Checked;
  if CheckBox2.Checked then
    SynEdit1StatusChange(SynEdit1,[scCaretX, scCaretY]);
  //else
    SynEdit1.Repaint;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  case ComboBox4.ItemIndex of
  0:
    SynEdit1.Text := TSynWebHtmlSyn.SynWebSample;
  1:
    SynEdit1.Text := TSynWebCssSyn.SynWebSample;
  2:
    SynEdit1.Text := TSynWebEsSyn.SynWebSample;
  3:
    SynEdit1.Text := TSynWebPhpCliSyn.SynWebSample;
  4:
    SynEdit1.Text := TSynWebWmlSyn.SynWebSample;
  end;
end;

procedure TForm1.ComboBox5Change(Sender: TObject);
begin
  SynWebEngine1.Options.WmlVersion:=TSynWebWmlVersion(ComboBox5.ItemIndex);
end;

end.
