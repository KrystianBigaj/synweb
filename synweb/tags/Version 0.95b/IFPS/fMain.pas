unit fMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, uPSComponent, uPSCompiler, Menus, uPSRuntime, SynEdit,
  ComCtrls, SynEditHighlighter, SynHighlighterPas;

type
  TC=set of Char;

  TForm1 = class(TForm)
    Memo2: TMemo;
    Splitter1: TSplitter;
    PSScript: TPSScript;
    PS3DllPlugin: TPSDllPlugin;
    MainMenu1: TMainMenu;
    Compile1: TMenuItem;
    Save1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SynEdit1: TSynEdit;
    Config1: TMenuItem;
    Timer1: TTimer;
    StatusBar1: TStatusBar;
    SynPasSyn1: TSynPasSyn;
    procedure IFPS3ClassesPlugin1CompImport(Sender: TObject;
      x: TPSPascalCompiler);
    procedure IFPS3ClassesPlugin1ExecImport(Sender: TObject; Exec: TPSExec;
      x: TPSRuntimeClassImporter);
    procedure PSScriptCompile(Sender: TPSScript);
    procedure Compile1Click(Sender: TObject);
    procedure PSScriptExecute(Sender: TPSScript);
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cvb1Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure update_configmenu;
  end;

var
  Form1: TForm1;

implementation
uses
  uPSR_std,
  uPSC_std,
  uPSR_stdctrls,
  uPSC_stdctrls,
  uPSR_forms,
  uPSC_forms,
  uPSC_graphics,
  uPSC_controls,
  uPSC_classes,
  uPSR_graphics,
  uPSR_controls,
  uPSR_classes;

{$R *.DFM}

procedure TForm1.IFPS3ClassesPlugin1CompImport(Sender: TObject;
  x: TIFPSPascalcompiler);
begin
  SIRegister_Std(x);
  SIRegister_Classes(x, true);
  SIRegister_Graphics(x, true);
  SIRegister_Controls(x);
  SIRegister_stdctrls(x);
  SIRegister_Forms(x);
end;

procedure TForm1.IFPS3ClassesPlugin1ExecImport(Sender: TObject; Exec: TIFPSExec;
  x: TIFPSRuntimeClassImporter);
begin
  RIRegister_Std(x);
  RIRegister_Classes(x, True);
  RIRegister_Graphics(x, True);
  RIRegister_Controls(x);
  RIRegister_stdctrls(x);
  RIRegister_Forms(x);
end;

function ImportTest(S1: string; s2: Longint; s3: Byte; s4: word; var s5: string): string;
begin
  Result := s1 + ' ' + IntToStr(s2) + ' ' + IntToStr(s3) + ' ' + IntToStr(s4) + ' - OK!';
  S5 := s5 + ' '+ result + ' -   OK2!';
end;

procedure MyWriteln(const s: string);
begin
  Form1.Memo2.Lines.Add(s);
end;

function MyReadln(const question: string): string;
begin
  Result := InputBox(question, '', '');
end;

function MyIntToHex(I, L:Integer):String;
begin
  Result:=IntToHex(I,L);
end;

function MyLoCase(Ch:Char):Char;
begin
  if (Ch >= 'A') and (Ch <= 'Z') then
    Result:=Chr(Ord(Ch)+32)
  else
    Result:=Ch;
end;

function MyUpCase(Ch:Char):Char;
begin
  Result:=UpCase(Ch);
end;

function MyCheckCharIn(C:Char; S:String):Boolean;
var
  tk:Integer;
  t:tc;
  p,oldp:PChar;
  pc:Char;
  token:String;

  function NextToken:Boolean;
  var
    ts:PChar;
  begin
    tk:=0;
    Result:=False;
    while p^=' ' do
      Inc(p);
    ts:=p;
    case p^ of
    '-':
      begin
        Inc(p);
        tk:=1;
      end;
    '0'..'9':
      begin
        repeat
          Inc(p);
        until not (p^ in ['0'..'9']);
        tk:=2;
      end;
    '$':
      begin
        Inc(p);
        if not (p^ in ['0'..'9', 'a'..'f', 'A'..'F']) then
          Exit;
        repeat
          Inc(p);
        until not (p^ in ['0'..'9', 'a'..'f', 'A'..'F']);
        tk:=2;
      end;
    '[':
      begin
        Inc(p);
        if p^=#0 then
          Exit;
        Inc(p);
        if p^<>']' then
          Exit;
        Inc(p);
        tk:=3;
      end;
    else
      tk:=0;
    end;
    SetString(token,ts,p-ts);
    Result:=tk<>0;
  end;

  function GetChar:Char;
  begin
    case tk of
    2:
      Result:=Char(StrToInt(token));
    3:
      Result:=token[2];
    else
      raise Exception.Create('Invalid token');
    end;
  end;

begin
  p:=@s[1];
  NextToken;
  t:=[];
  while tk<>0 do
  begin
    pc:=GetChar;
    oldp:=p;
    NextToken;
    if tk=1 then
    begin
      if not NextToken then
        break;
      t:=t+[pc..GetChar];
    end else
    begin
      p:=oldp;
      t:=t+[pc];
    end;
    if not NextToken then
      break;      
  end;
  Result:=C in t;
end;

function MyTrimRight(S:String):String;
begin
  Result:=TrimRight(S);
end;

function MyOutDir:String;
begin
  Result:=ExtractFilePath(Application.ExeName)+'..\';
end;

procedure TForm1.PSScriptCompile(Sender: TPSScript);
begin

  Sender.AddFunction(@MyCheckCharIn, 'function Check(C:Char; S:String):Boolean;');
  Sender.AddFunction(@MyIntToHex, 'function IntToHex(I, L:Integer):String;');
  Sender.AddFunction(@MyTrimRight, 'function TrimRight(S:String):String;');
  Sender.AddFunction(@MyLoCase, 'function LoCase(Ch:Char):Char;');
  Sender.AddFunction(@MyUpCase, 'function UpCase(Ch:Char):Char;');
  Sender.AddFunction(@MyWriteln, 'procedure Writeln(s: string);');
  Sender.AddFunction(@MyOutDir, 'function OutDir:string;');
  Sender.AddFunction(@MyReadln, 'function Readln(question: string): string;');
  Sender.AddFunction(@ImportTest, 'function ImportTest(S1: string; s2: Longint; s3: Byte; s4: word; var s5: string): string;');
  Sender.AddRegisteredVariable('Application', 'TApplication');
  Sender.AddRegisteredVariable('Self', 'TForm');
  Sender.AddRegisteredVariable('Memo1', 'TMemo');
  Sender.AddRegisteredVariable('Memo2', 'TMemo');
end;

procedure TForm1.Compile1Click(Sender: TObject);
  procedure OutputMessages;
  var
    l: Longint;
    b: Boolean;
  begin
    b := False;

    for l := 0 to PSScript.CompilerMessageCount - 1 do
    begin
      Memo2.Lines.Add('Compiler: '+ PSScript.CompilerErrorToStr(l));
      if (not b) and (PSScript.CompilerMessages[l] is TIFPSPascalCompilerError) then
      begin
        b := True;
        SynEdit1.SelStart := PSScript.CompilerMessages[l].Pos;
      end;
    end;
  end;
begin
  Memo2.Lines.Clear;
  PSScript.Script.Assign(SynEdit1.Lines);
  Memo2.Lines.Add('Compiling');
  if PSScript.Compile then
  begin
    OutputMessages;
    Memo2.Lines.Add('Compiled succesfully');
    if not PSScript.Execute then
    begin
      SynEdit1.SelStart := PSScript.ExecErrorPosition;
      Memo2.Lines.Add(PSScript.ExecErrorToString +' at '+Inttostr(PSScript.ExecErrorProcNo)+'.'+Inttostr(PSScript.ExecErrorByteCodePosition));
    end else Memo2.Lines.Add('Succesfully executed');
  end else
  begin
    OutputMessages;
    Memo2.Lines.Add('Compiling failed');
  end;    
  if StatusBar1.SimpleText<>'' then
    SynEdit1.Lines.SaveToFile(StatusBar1.SimpleText);
  update_configmenu;
end;

procedure TForm1.PSScriptExecute(Sender: TPSScript);
begin
  PSScript.SetVarToInstance('APPLICATION', Application);
  PSScript.SetVarToInstance('SELF', Self);
  PSScript.SetVarToInstance('MEMO2', Memo2);
end;

procedure TForm1.Open1Click(Sender: TObject);
begin
  OpenDialog1.FileName:=StatusBar1.SimpleText;
  if OpenDialog1.Execute then
  begin
    StatusBar1.SimpleText:=OpenDialog1.FileName;
    SynEdit1.Lines.LoadFromFile(StatusBar1.SimpleText);
  end;
  update_configmenu;
end;

procedure TForm1.Save1Click(Sender: TObject);
begin
  SaveDialog1.FileName:=StatusBar1.SimpleText;
  if SaveDialog1.Execute then
  begin
    StatusBar1.SimpleText:=ChangeFileExt(SaveDialog1.FileName,'.ifps');
    SynEdit1.Lines.SaveToFile(StatusBar1.SimpleText);
  end;
  update_configmenu;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  update_configmenu;
end;

procedure TForm1.update_configmenu;
var
  f:TSearchRec;
  m:TMenuItem;

  function cf(s:string):string;
  begin        
    result:=UpperCase(ExpandFileName(s));
  end;
  
begin
  Config1.Clear;
  if FindFirst(ExtractFilePath(Application.ExeName)+'..\Config\*.ifps',faAnyFile,f)=0 then
  begin
    repeat
      if (f.Name<>'.') and (f.Name<>'..') then
        if f.Attr and faDirectory<>faDirectory then
        begin
          m:=TMenuItem.Create(Config1);
          m.Caption:=f.Name;
          m.Hint:=ExtractFilePath(Application.ExeName)+'..\Config\'+f.Name;
          m.OnClick:=cvb1Click;
          if cf(m.Hint)=cf(StatusBar1.SimpleText) then
            m.Default:=True;
          Config1.Add(m);
        end;
    until FindNext(f)<>0;
  end;
  if Config1.Count=0 then
  begin
    m:=TMenuItem.Create(Config1);
    m.Caption:='(None)';
    m.Enabled:=False;
    Config1.Add(m);
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if StatusBar1.SimpleText<>'' then
    SynEdit1.Lines.SaveToFile(StatusBar1.SimpleText);
  update_configmenu;
end;

procedure TForm1.cvb1Click(Sender: TObject);
begin
  StatusBar1.SimpleText:=ExpandFileName(TMenuItem(Sender).Hint);
  SynEdit1.Lines.LoadFromFile(StatusBar1.SimpleText);
  update_configmenu;
end;

end.

