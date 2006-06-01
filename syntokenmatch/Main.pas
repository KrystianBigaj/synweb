unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, SynEdit, SynHighlighterPas, SynEditHighlighter,
  SynHighlighterCpp, SynTokenMatch, StdCtrls, SynEditTypes;

const
  PasTokens:array[0..2] of TSynTokenMatch=(
    (OpenToken: '('; CloseToken: ')'; TokenKind: Integer(SynHighlighterPas.tkSymbol)),
    (OpenToken: '['; CloseToken: ']'; TokenKind: Integer(SynHighlighterPas.tkSymbol)),
    (OpenToken: 'begin'; CloseToken: 'end'; TokenKind: Integer(SynHighlighterPas.tkKey))
    );

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    Label6: TLabel;
    procedure SynEdit1PaintTransient(Sender: TObject; Canvas: TCanvas;
      TransientType: TTransientType);
  private
    { Private declarations }
  public
    { Public declarations }
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
  Editor := TSynEdit(Sender);
  if Editor.SelAvail then
    Exit;

  I := SynEditGetMatchingTokenEx(Editor, Editor.CaretXY, PasTokens, Match);
  if I = 0 then
    Exit;

  Canvas.Brush.Style := bsSolid;
  Canvas.Font.Assign(Editor.Font);
//  Canvas.Font.Style := Match.TokenAttri.Style;
  Canvas.Font.Style := [fsBold];

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

end.
