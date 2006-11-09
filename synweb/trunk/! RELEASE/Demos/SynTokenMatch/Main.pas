unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, SynEdit, SynHighlighterPas, SynEditHighlighter,
  SynHighlighterCpp, SynTokenMatch, StdCtrls, SynEditTypes;

const
  PasTokens:array[0..14] of TSynTokenMatch=(
    (OpenToken: '('; CloseToken: ')'; TokenKind: Integer(SynHighlighterPas.tkSymbol)),
    (OpenToken: '['; CloseToken: ']'; TokenKind: Integer(SynHighlighterPas.tkSymbol)),
    (OpenToken: 'begin'; CloseToken: 'end'; TokenKind: Integer(SynHighlighterPas.tkKey)),
    (OpenToken: 'class'; CloseToken: 'end'; TokenKind: Integer(SynHighlighterPas.tkKey)),
    (OpenToken: 'interface'; CloseToken: 'end'; TokenKind: Integer(SynHighlighterPas.tkKey)),
    (OpenToken: 'record'; CloseToken: 'end'; TokenKind: Integer(SynHighlighterPas.tkKey)),
    (OpenToken: 'case'; CloseToken: 'end'; TokenKind: Integer(SynHighlighterPas.tkKey)),
    (OpenToken: 'asm'; CloseToken: 'end'; TokenKind: Integer(SynHighlighterPas.tkKey)),
    (OpenToken: 'try'; CloseToken: 'end'; TokenKind: Integer(SynHighlighterPas.tkKey)),
    (OpenToken: 'implementation'; CloseToken: 'end'; TokenKind: Integer(SynHighlighterPas.tkKey)),
    (OpenToken: 'package'; CloseToken: 'end'; TokenKind: Integer(SynHighlighterPas.tkKey)),
    (OpenToken: 'for'; CloseToken: 'do'; TokenKind: Integer(SynHighlighterPas.tkKey)),
    (OpenToken: 'repeat'; CloseToken: 'until'; TokenKind: Integer(SynHighlighterPas.tkKey)),
    (OpenToken: 'while'; CloseToken: 'do'; TokenKind: Integer(SynHighlighterPas.tkKey)),
    (OpenToken: 'if'; CloseToken: 'then'; TokenKind: Integer(SynHighlighterPas.tkKey))
    );

type
  TForm1 = class(TForm)
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    procedure SynEdit1PaintTransient(Sender: TObject; Canvas: TCanvas;
      TransientType: TTransientType);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public      
    FPaintUpdating: Boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.SynEdit1PaintTransient(Sender: TObject; Canvas: TCanvas;
  TransientType: TTransientType);
var
  Editor : TSynEdit;  
  Pix: TPoint;      
  Match: TSynTokenMatched;
  I: Integer;

  function CharToPixels(P: TBufferCoord): TPoint;
  begin
    Result:=Editor.RowColumnToPixels(Editor.BufferToDisplayPos(P));
  end;

begin
  if FPaintUpdating then
    Exit;
  Editor := TSynEdit(Sender);
  if TransientType = ttBefore then
  begin
    I := SynEditGetMatchingTokenEx(Editor, Editor.CaretXY, PasTokens, Match);
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
  I := SynEditGetMatchingTokenEx(Editor, Editor.CaretXY, PasTokens, Match);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  FPaintUpdating := False;
end;

end.
