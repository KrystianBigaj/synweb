unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynEditHighlighter, SynHighlighterWeb, SynEdit, SynHighlighterMulti,
  SynHighlighterCS;

type
  TfrmMain = class(TForm)
    SynEditTest: TSynEdit;
    SynWebEngine: TSynWebEngine;
    SynWebHtmlSyn: TSynWebHtmlSyn;
    SynCSSyn: TSynCSSyn;
    SynMultiSyn: TSynMultiSyn;
    procedure SynMultiSynCustomRange(Sender: TSynMultiSyn;
      Operation: TRangeOperation; var Range: Pointer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.SynMultiSynCustomRange(Sender: TSynMultiSyn;
  Operation: TRangeOperation; var Range: Pointer);
const
  cSYNWEB_MULTISYN_RANGECLEAN = $F801FFFF;
  cSYNWEB_MULTISYN_RANGE_INDEX = 26; // 26.27: CurrScheme (01, 02, 03, 04) 
  cSYNWEB_MULTISYN_RANGE_INDEX_MASK = $03;
  cSYNWEB_MULTISYN_RANGE_SCHEME = 17; // 17..26: 8bits of Scheme range
  cSYNWEB_MULTISYN_RANGE_SCHEME_MASK = $FF;
var
  lRange: Cardinal;
begin
  // Support only for SynWeb highlighters as DefaultHighlighter
  if not (Sender.DefaultHighlighter is TSynWebBase) then
    Exit;

  // Extract range and set it to highlighter
  if Operation = roSet then
  begin
    lRange := Cardinal(Range);

    // Is this a custom range? - 2 bits starting from 26bit
    Sender.CurrScheme := ((lRange shr cSYNWEB_MULTISYN_RANGE_INDEX) and cSYNWEB_MULTISYN_RANGE_INDEX_MASK) - 1;

    if Sender.CurrScheme >= Sender.Schemes.Count then
      Sender.CurrScheme := -1;

    // clean bits from 17 to 26 and set to SynWeb-HL
    Sender.DefaultHighlighter.SetRange(Pointer(lRange and cSYNWEB_MULTISYN_RANGECLEAN)); 

    if Sender.CurrScheme in [0, 1, 2] then
    begin
      // Get and clean scheme range
      lRange := (lRange shr cSYNWEB_MULTISYN_RANGE_SCHEME) and cSYNWEB_MULTISYN_RANGE_SCHEME_MASK;

      // Set range to highlighter scheme
      Sender.Schemes[Sender.CurrScheme].Highlighter.SetRange(Pointer(lRange));
    end;

  end else
  begin
    // Get SynWeb range and clean range
    lRange := Cardinal(Sender.DefaultHighlighter.GetRange) and cSYNWEB_MULTISYN_RANGECLEAN;

    // Save CurrScheme to range
    if Sender.CurrScheme in [0, 1, 2] then
    begin
      // Save CurrScheme index
      lRange := lRange or ((Sender.CurrScheme + 1) shl cSYNWEB_MULTISYN_RANGE_INDEX);

      // Save range
      lRange := lRange or ((Cardinal(Sender.Schemes[Sender.CurrScheme].Highlighter.GetRange) and 
	cSYNWEB_MULTISYN_RANGE_SCHEME_MASK) shl cSYNWEB_MULTISYN_RANGE_SCHEME);
    end;

    Range := Pointer(lRange);
  end;
end;

end.

