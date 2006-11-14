unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, RegExpr;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    Memo2: TMemo;
    Button2: TButton;
    Button3: TButton;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Memo1: TMemo;
    Button1: TButton;
    Label2: TLabel;
    Memo3: TMemo;
    GroupBox3: TGroupBox;
    Memo4: TMemo;
    CheckBox1: TCheckBox;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure MergeFile(AFileName:String);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  s:String;
begin
  s:=ChangeFileExt(Application.ExeName,'_Merge.txt');
  if FileExists(s) then
    Memo3.Lines.LoadFromFile(s);
  s:=ChangeFileExt(Application.ExeName,'_Exclude.txt');
  if FileExists(s) then
    Memo1.Lines.LoadFromFile(s);
  s:=ChangeFileExt(Application.ExeName,'_Export.txt');
  if FileExists(s) then
    Memo2.Lines.LoadFromFile(s);
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  s:String;
begin
  s:=ChangeFileExt(Application.ExeName,'_Merge.txt');
  Memo3.Lines.SaveToFile(s);
  s:=ChangeFileExt(Application.ExeName,'_Exclude.txt');
  Memo1.Lines.SaveToFile(s);
  s:=ChangeFileExt(Application.ExeName,'_Export.txt');
  Memo2.Lines.SaveToFile(s);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Button2Click(nil);
  Button1Click(nil);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i:integer;
begin      
  Enabled:=False;
  Label2.Caption:=Format('Executing [0/%d]',[Memo2.Lines.Count]);
  for i:=0 to Memo2.Lines.Count-1 do
  begin                         
    Application.ProcessMessages;
    Label2.Caption:=Format('Executing [%d/%d]: "%s"',[i+1, Memo2.Lines.Count, Memo2.Lines[i]]);
    Application.ProcessMessages;
    WinExec(PChar(Memo2.Lines[i]),SW_SHOW);
    Application.ProcessMessages;
    Memo4.Lines.Add(Format('Command "%s" executed.',[Memo2.Lines[i]]));
    Application.ProcessMessages;
  end;
  Label2.Caption:=' Ready.';
  Memo4.Lines.Add('--- Export done.');
  Enabled:=True;
end;

procedure TForm1.MergeFile(AFileName: String);
var
  r:TRegExpr;
  sl,sl2:TStringList;
  i,j,x:Integer;
  s,d,t:String;
  f:TFileStream;
begin
  Enabled:=False;
  Label2.Caption:=Format('Merging "%s".',[AFileName]);
  s:=ExtractFilePath(Application.ExeName)+AFileName;
  d:=ExtractFilePath(Application.ExeName)+'..\';
  r:=TRegExpr.Create;
  sl:=TStringList.Create;
  sl2:=TStringList.Create;
  sl.LoadFromFile(s);
  r.Expression:=' *\{\$I\ (.*?)\} *[\n\r]{0,2}';
  r.ModifierI:=True;
  x:=0;
  if r.Exec(sl.Text) then
    repeat         
      Label2.Caption:=Label2.Caption+'.';
      Application.ProcessMessages;
      if Memo1.Lines.IndexOf(r.Match[1])=-1 then
      begin
        if not FileExists(d+r.Match[1]) then
        begin
          Memo4.Lines.Add(Format('Error: File "%s" not found!.',[d+r.Match[1]]));
          Label2.Caption:='Ready.';
          Enabled:=True;
          sl2.Free;
          sl.Free;
          r.Free;
          Exit;
        end;
        sl2.LoadFromFile(d+r.Match[1]);
        if CheckBox1.Checked then
        begin
          sl2.Insert(0, Format('{ %s: Generated block - begin }',[r.Match[1]]));
          sl2.Add(Format('{ %s: Generated block - end }',[r.Match[1]]));
        end;
        sl.Text:=StringReplace(sl.Text,r.Match[0],sl2.Text,[]);
      end;
    until not r.ExecNext;
  s:=d+'! RELEASE\Source\'+ExtractFileName(AFileName);
  sl.SaveToFile(s);
  sl2.Free;
  r.Free;
  f:=TFileStream.Create(s,fmOpenRead);
  Memo4.Lines.Add(Format('File saved to "%s" (Size: %d kB, Lines: %d).',[s, f.Size div 1024, sl.Count]));
  Label2.Caption:='Ready.';
  sl.Free;
  f.Free;
  Enabled:=True;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i:integer;
begin
  for i:=0 to Memo3.Lines.Count-1 do
    MergeFile(Memo3.Lines[i]);       
  Memo4.Lines.Add('--- Merge done.');
end;

end.
