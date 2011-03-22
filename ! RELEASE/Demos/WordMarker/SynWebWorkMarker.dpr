program SynWebWorkMarker;

uses
  Forms,
  uMainForm in 'uMainForm.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
