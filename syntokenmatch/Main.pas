unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, SynEdit, SynHighlighterPas, SynEditHighlighter,
  SynHighlighterCpp, SynTokenMatch, StdCtrls, SynEditTypes;

const
  PasTokens:array[0..11] of TSynTokenMatch=(
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
    (OpenToken: 'repat'; CloseToken: 'until'; TokenKind: Integer(SynHighlighterPas.tkKey))
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
    FUpdating: Boolean;
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
  Match: TSynTokenMatches;
  I: Integer;

  function CharToPixels(P: TBufferCoord): TPoint;
  begin
    Result:=Editor.RowColumnToPixels(Editor.BufferToDisplayPos(P));
  end;

begin
  if FUpdating then
  begin
    FUpdating := False;
    Exit;
  end;
  Editor := TSynEdit(Sender);
  if TransientType = ttBefore then
  begin
    I := SynEditGetMatchingTokenEx(Editor, Editor.CaretXY, PasTokens, Match);
    if I = 0 then
      Exit;
    FUpdating := True;
    case Abs(I) of
    2:
      Editor.InvalidateLines(Match.OpenTokenPos.Line, Match.CloseTokenPos.Line);
    1:
      Editor.InvalidateLines(Match.OpenTokenPos.Line, Match.OpenTokenPos.Line);
    -1:
      Editor.InvalidateLines(Match.CloseTokenPos.Line, Match.CloseTokenPos.Line);
    end;
    Exit;
  end;
  if Editor.SelAvail then
    Exit;
  I := SynEditGetMatchingTokenEx(Editor, Editor.CaretXY, PasTokens, Match);
  if I = 0 then
    Exit;
  Canvas.Brush.Style := bsSolid;
  Canvas.Font.Style := Match.TokenAttri.Style;   // doesn't work, but why ????
  Canvas.Font.Color := Editor.Font.Color;
  if Abs(I) = 2 then
    Canvas.Brush.Color := clAqua // matched color
  else
    Canvas.Brush.Color := clYellow; // unmatched color
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  FUpdating := False;
end;

end.
