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
  FGColor, BGColor: TColor;

  function CharToPixels(P: TBufferCoord): TPoint;
  begin
    Result:=Editor.RowColumnToPixels(Editor.BufferToDisplayPos(P));
  end;

  procedure UpdateColors(ALine: Integer); 
  var
    iAttri: TSynHighlighterAttributes;
    b: Boolean;
  begin
    BGColor := clNone;
    FGColor := clNone;
    if (TransientType = ttBefore) then
      with Editor do
      begin       
        FGColor := Match.TokenAttri.Foreground;
        if (ActiveLineColor <> clNone) and (CaretY = ALine) then
          BGColor := ActiveLineColor
        else
        begin
          BGColor := Match.TokenAttri.Background;
          if BGColor = clNone then
          begin
            iAttri := Highlighter.WhitespaceAttribute;
            if (iAttri <> nil) and (iAttri.Background <> clNone) then
              BGColor := iAttri.Background;
          end;
        end;
        if Assigned(Editor.OnSpecialLineColors) then
          Editor.OnSpecialLineColors(Editor, ALine, b, FGColor, BGColor);
      end
    else
    begin
      FGColor := Editor.Font.Color;
      if Abs(I) = 2 then
        BGColor := clAqua // matched color
      else
        BGColor := clYellow; // unmatched color
    end;

    if BGColor = clNone then
      BGColor := Editor.Color;
    if FGColor = clNone then
      FGColor := Editor.Font.Color;

    Canvas.Brush.Color := BGColor;
    Canvas.Font.Color :=  FGColor;
  end;

begin                  
  Editor := TSynEdit(Sender);
  if Editor.SelAvail then
    Exit;
  I := SynEditGetMatchingTokenEx(Editor, Editor.CaretXY, PasTokens, Match);
  if I = 0 then
    Exit;                              
  Canvas.Brush.Style := bsSolid;
  Canvas.Font.Style := Match.TokenAttri.Style;
  if I <> -1 then
  begin
    Pix := CharToPixels(Match.OpenTokenPos);
    UpdateColors(Match.OpenTokenPos.Line);
    Canvas.TextOut(Pix.X, Pix.Y, Match.OpenToken);
  end;
  if I <> 1 then
  begin
    Pix := CharToPixels(Match.CloseTokenPos);   
    UpdateColors(Match.CloseTokenPos.Line);
    Canvas.TextOut(Pix.X, Pix.Y, Match.CloseToken);
  end;
end;

end.
