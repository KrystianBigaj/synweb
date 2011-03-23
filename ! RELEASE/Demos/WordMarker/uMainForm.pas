{-------------------------------------------------------------------------------
SynWeb
Copyright (C) 2005-2011  Krystian Bigaj

*** MPL
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is Krystian Bigaj.

Alternatively, the contents of this file may be used under the terms
of the GNU Lesser General Public license (the  "LGPL License"),
in which case the provisions of LGPL License are applicable instead of those
above. If you wish to allow use of your version of this file only
under the terms of the LGPL License and not to allow others to use
your version of this file under the MPL, indicate your decision by
deleting the provisions above and replace them with the notice and
other provisions required by the LGPL License. If you do not delete
the provisions above, a recipient may use your version of this file
under either the MPL or the LGPL License.

*** LGPL
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
***

You may retrieve the latest version of this file at the SynWeb home page,
located at http://sourceforge.net/projects/synweb

Contact: krystian.bigaj@gmail.com
Homepage: http://flatdev.ovh.org
-------------------------------------------------------------------------------}

unit uMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynEdit, StdCtrls, SynHighlighterWebMisc, ExtCtrls;

type

{ TfrmMain }

  TfrmMain = class(TForm)
    syn: TSynEdit;
    chkWordWrap: TCheckBox;
    Label1: TLabel;
    chkGutter: TCheckBox;
    gbConfig: TGroupBox;
    edtCustomText: TEdit;
    lblCustomText: TLabel;
    cbBGColor: TColorBox;
    chkWordMarker: TCheckBox;
    cbMode: TComboBox;
    Label3: TLabel;
    cbFGColor: TColorBox;
    chkCaseSensitive: TCheckBox;
    cbPaintMode: TComboBox;
    procedure chkWordMarkerClick(Sender: TObject);
    procedure chkWordWrapClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure synStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure FormDestroy(Sender: TObject);
    procedure cbBGColorChange(Sender: TObject);
    procedure chkGutterClick(Sender: TObject);
    procedure cbModeChange(Sender: TObject);
    procedure edtCustomTextChange(Sender: TObject);
    procedure cbFGColorChange(Sender: TObject);
    procedure chkCaseSensitiveClick(Sender: TObject);
    procedure cbPaintModeChange(Sender: TObject);
  private
    FConfigMode: TSynWebWordMarkerMode;
    FWordMarkers: array[TSynWebWordMarkerMode] of TSynWebWordMarker;

    procedure DoUILoadConfigMode;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses SynEditMiscClasses;

{$R *.dfm}

procedure TfrmMain.cbBGColorChange(Sender: TObject);
begin
  FWordMarkers[FConfigMode].BGColor := cbBGColor.Selected;
end;

procedure TfrmMain.cbFGColorChange(Sender: TObject);
begin
  FWordMarkers[FConfigMode].FGColor := cbFGColor.Selected;
end;

procedure TfrmMain.cbModeChange(Sender: TObject);
begin
  case cbMode.ItemIndex of
  0:
    FConfigMode := swwmSelectedText;

  1:         
    FConfigMode := swwmSelectedWord;

  2:                      
    FConfigMode := swwmCustomText;

  3:                            
    FConfigMode := swwmCustomWord;
  end;

  DoUILoadConfigMode;
end;

procedure TfrmMain.cbPaintModeChange(Sender: TObject);
begin
  case cbPaintMode.ItemIndex of
  0:
    FWordMarkers[FConfigMode].PaintMode := swwpFillRect;

  1:
    FWordMarkers[FConfigMode].PaintMode := swwpFrameRect;

  2:
    FWordMarkers[FConfigMode].PaintMode := swwpUnderline;
  end;
end;

procedure TfrmMain.chkCaseSensitiveClick(Sender: TObject);
begin
  FWordMarkers[FConfigMode].CaseSensitive := chkCaseSensitive.Checked;
end;

procedure TfrmMain.chkGutterClick(Sender: TObject);
begin
  syn.Gutter.Visible := chkGutter.Checked;
end;

procedure TfrmMain.chkWordMarkerClick(Sender: TObject);
begin
  FWordMarkers[FConfigMode].Enabled := chkWordMarker.Checked;
end;

procedure TfrmMain.chkWordWrapClick(Sender: TObject);
begin
  syn.WordWrap := chkWordWrap.Checked;
end;

procedure TfrmMain.DoUILoadConfigMode;
begin
  chkWordMarker.Checked := FWordMarkers[FConfigMode].Enabled;
  cbBGColor.Selected := FWordMarkers[FConfigMode].BGColor;
  cbFGColor.Selected := FWordMarkers[FConfigMode].FGColor;
  edtCustomText.Text := FWordMarkers[FConfigMode].CustomText;
  chkCaseSensitive.Checked := FWordMarkers[FConfigMode].CaseSensitive;
  
  case FWordMarkers[FConfigMode].PaintMode of
  swwpFillRect:
    cbPaintMode.ItemIndex := 0;

  swwpFrameRect:
    cbPaintMode.ItemIndex := 1;
    
  swwpUnderline:
    cbPaintMode.ItemIndex := 2;
  end;

  edtCustomText.Visible := FConfigMode in [swwmCustomWord, swwmCustomText];
  lblCustomText.Visible := FConfigMode in [swwmCustomWord, swwmCustomText];
end;

procedure TfrmMain.edtCustomTextChange(Sender: TObject);
begin
  FWordMarkers[FConfigMode].CustomText := edtCustomText.Text;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  lMode: TSynWebWordMarkerMode;
begin
  for lMode := Low(TSynWebWordMarkerMode) to High(TSynWebWordMarkerMode) do
  begin
    FWordMarkers[lMode] := TSynWebWordMarker.Create(syn);
    FWordMarkers[lMode].Mode := lMode;
  end;

  FWordMarkers[swwmSelectedWord].BGColor := syn.SelectedColor.Background;
  FWordMarkers[swwmSelectedWord].FGColor := syn.SelectedColor.Foreground;

  FWordMarkers[swwmSelectedText].BGColor := clGreen;
  FWordMarkers[swwmSelectedText].PaintMode := swwpFrameRect;

  FWordMarkers[swwmCustomWord].BGColor := clRed;
  FWordMarkers[swwmCustomWord].CustomText := 'const';
  FWordMarkers[swwmCustomWord].PaintMode := swwpFrameRect;
  
  FWordMarkers[swwmCustomText].BGColor := clBlue;
  FWordMarkers[swwmCustomText].CustomText := 'T';
  FWordMarkers[swwmCustomText].PaintMode := swwpUnderline;

  cbModeChange(Sender);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
var
  lMode: TSynWebWordMarkerMode;
begin
  for lMode := Low(TSynWebWordMarkerMode) to High(TSynWebWordMarkerMode) do
    FreeAndNil(FWordMarkers[lMode]);
end;

procedure TfrmMain.synStatusChange(Sender: TObject; Changes: TSynStatusChanges);
var
  lMode: TSynWebWordMarkerMode;
begin                                                               
  // Notify TSynWebWordMarker's about selection changes
  if scSelection in Changes then
    for lMode := Low(TSynWebWordMarkerMode) to High(TSynWebWordMarkerMode) do
      FWordMarkers[lMode].NotifySelChanged;
end;

end.
