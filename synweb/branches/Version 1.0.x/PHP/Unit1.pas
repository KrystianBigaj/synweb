unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Menus, IniFiles32, clipbrd, ExtCtrls, RegExpr,
  TestParser;

var
  mKeyHashTable: array[#0..#255] of Integer;

type
  TLexKeys = Class
  public
    KeyIndex: Integer;
    Key: Integer;
    Data: Pointer;

  end;
  
type
  TForm1 = class(TForm)
    TreeView1: TTreeView;
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button4: TButton;
    Label1: TLabel;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    PopupMenu1: TPopupMenu;
    N1: TMenuItem;
    None1: TMenuItem;
    Button18: TButton;
    Memo1: TMemo;
    Button3: TButton;
    Button5: TButton;
    php4box: TCheckBox;
    php5box: TCheckBox;
    keyword1: TMenuItem;
    const1: TMenuItem;
    function1: TMenuItem;
    N2: TMenuItem;
    php4: TMenuItem;
    php5: TMenuItem;
    var1: TMenuItem;
    Button7: TButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Button8: TButton;
    ComboBox1: TComboBox;
    pb: TProgressBar;
    Button9: TButton;
    Label5: TLabel;
    Button6: TButton;
    Label4: TLabel;
    Memo2: TMemo;
    pecl: TMenuItem;
    alias: TMenuItem;
    N3: TMenuItem;
    Button10: TButton;
    peclbox: TCheckBox;
    Memo3: TMemo;
    procedure TreeView1Compare(Sender: TObject; Node1, Node2: TTreeNode;
      Data: Integer; var Compare: Integer);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Html401Strict1Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure php4Click(Sender: TObject);
    procedure keyword1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure ComboBox1DropDown(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button10Click(Sender: TObject);
  private
    pn:TTreeNode;
    KeyList: TList;
    appdir:String;
    procedure MakeHashTable;
    procedure ClearLists;
  public
    function KeyHash(ToHash: String): Integer;
    procedure GenerateInc(AKeys:TStringList; AFileFunc, AFileFuncList, AFileFuncTable: String);
    function nGetBit(n:TTreeNode; b:Integer):Boolean;
    procedure nSetBit(n:TTreeNode; b:Integer);
    procedure nClearBit(n:TTreeNode; b:Integer);
    procedure nSwitchBit(n:TTreeNode; b:Integer);
    procedure AddAtrib(n:TTreeNode; s:String);
    function AddTag(s:String; b:Integer):TTreeNode;
    procedure nSetType(n:TTreeNode; b:Integer);
    procedure get_parsefiles(ADir:String; AStringList:TStringList);
  end;

var
  Form1: TForm1;

implementation

uses StrUtils, Math;

{$R *.dfm}

function CompareKeys(Item1, Item2: Pointer): Integer;
begin
  if TLexKeys(Item1).Key < TLexKeys(Item2).Key then
    Result := -1
  else
    if TLexKeys(Item1).Key > TLexKeys(Item2).Key then
      Result := 1
    else
      if (Longword(TLexKeys(Item1).Data)and(1 shl 0)<>0) and (Longword(TLexKeys(Item2).Data)and(1 shl 0)=0) then
        Result := -1
      else
        if (Longword(TLexKeys(Item1).Data)and(1 shl 0)=0) and (Longword(TLexKeys(Item2).Data)and(1 shl 0)<>0) then
          Result := 1
        else
          if TLexKeys(Item1).KeyIndex < TLexKeys(Item2).KeyIndex then
            Result := -1
          else
            if TLexKeys(Item1).KeyIndex > TLexKeys(Item2).KeyIndex then
              Result := 1
            else
              Result := 0;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    TreeView1.LoadFromFile(OpenDialog1.FileName);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    TreeView1.SaveToFile(SaveDialog1.FileName);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if TreeView1.Selected<>nil then
    TreeView1.Selected.Delete;
  TreeView1.SetFocus;
end;

procedure TForm1.Button11Click(Sender: TObject);
var
  t,a:Integer;
  attr:TStringList;
  n1:TTreeNode;
{
  procedure lcheck;
  var
    n3:TTreeNode;
  begin
    n3:=n2.getNextSibling;
    while n3<>nil do
    begin
      if n3.Level=1 then
        if n2.Text=n3.Text then
          if(nGetBit(n2,0)<>nGetBit(n3,0))or
            (nGetBit(n2,1)<>nGetBit(n3,1))or
            (nGetBit(n2,2)<>nGetBit(n3,2))or
            (nGetBit(n2,3)<>nGetBit(n3,3))or
            (nGetBit(n2,4)<>nGetBit(n3,4))or
            (nGetBit(n2,5)<>nGetBit(n3,5)) then
          begin
           Form3.Memo1.Lines.Add(format('Attr: %s (tag: %s)<>%s (tag: %s)',[n2.Text,n2.Parent.Text,n3.Text,n3.Parent.Text]));
           break;
          end;
      n3:=n3.GetNext;
    end;
  end;
}
begin
  attr:=TStringList.Create;
  t:=0;
  a:=0;
  n1:=TreeView1.Items.GetFirstNode;
  while n1<>nil do
  begin
    Inc(t);
    {n2:=n1.getFirstChild;
    while n2<>nil do
    begin
      if attr.IndexOf(n2.Text)=-1 then
      begin
        attr.Add(n2.Text);
        inc(a);
      end;
      n2:=n2.GetNextChild(n2);
    end; }
    n1:=n1.GetNextChild(n1);
  end;
  Label1.Caption:=format('%d %d',[t,a]);
  attr.Free;
end;

procedure TForm1.Button12Click(Sender: TObject);
begin
  TreeView1.Items.AlphaSort();
end;

procedure TForm1.Button14Click(Sender: TObject);
var
  ini:TIniFile32;
  t,a:Integer;
  n1,n2:TTreeNode;
begin
  if not OpenDialog1.Execute then
    Exit;
  TreeView1.Items.BeginUpdate;
  TreeView1.Items.Clear;
  ini:=TIniFile32.Create(OpenDialog1.FileName);
  ini.BeginUpdate;
  if ini.ReadInteger('Config','IsPhp',0)<>1 then
  begin
    ShowMessage('This is not Php-Config FILE!');
    ini.Free;
    Exit;
  end;
  for t:=0 to ini.ReadInteger('Config','Tags',0)-1 do
  begin
    n1:=TreeView1.Items.AddChild(nil,ini.ReadString(format('TAG_%d',[t]),'Name','!!! ERROR !!!'));
    n1.Data:=Pointer(StrToInt(ini.ReadString(format('TAG_%d',[t]),'Data','!!! ERROR !!!')));
    for a:=0 to ini.ReadInteger(format('TAG_%d',[t]),'Attr_Count',0)-1 do
    begin
      n2:=TreeView1.Items.AddChild(n1,ini.ReadString(format('TAG_%d',[t]),Format('Attr_%d_Name',[a]),'!!! ERROR !!!'));
      n2.Data:=Pointer(StrToInt(ini.ReadString(format('TAG_%d',[t]),Format('Attr_%d_Data',[a]),'!!! ERROR !!!')));
    end;
  end;
  Edit1.Text:=ini.ReadString('Config','vPhp4','Php 4');
  Edit2.Text:=ini.ReadString('Config','vPhp5','Php 5');
  ini.EndUpdate;
  ini.Free;
  TreeView1.Items.EndUpdate;
end;

procedure TForm1.Button13Click(Sender: TObject);
var
  f:TStringList;
  t:Integer;
  n1:TTreeNode;

  procedure SaveNode(node:TTreeNode);
  var
    x:Integer;
    fn:TTreeNode;
  begin
    fn:=node.getFirstChild;
    x:=node.Count;
    f.Add(StringReplace(StringReplace(node.Text,#13,'',[rfReplaceAll]),#10,'',[rfReplaceAll]));
    f.Add('$'+IntToHex(Longword(node.Data),8));
    f.Add(inttostr(x));
    while fn<>nil do
    begin
      SaveNode(fn);
      fn:=fn.GetNextChild(fn);
    end;
  end;

begin
  if not SaveDialog1.Execute then
    Exit;
  f:=TStringList.Create; // (SaveDialog1.FileName, fmCreate)
  f.Add('IsPhp=1');
  f.Add(Edit1.Text);
  f.Add(Edit2.Text);
  t:=0;
  n1:=TreeView1.Items.GetFirstNode;
  while n1<>nil do
  begin
    SaveNode(n1);
    n1:=n1.GetNextChild(n1);
    Inc(t);
  end;
  f.Insert(3,inttostr(t));
  f.SaveToFile(SaveDialog1.FileName);
  f.Free;
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);
begin
  pn:=TreeView1.Selected;
  keyword1.Enabled:=pn<>nil;
  const1.Enabled:=pn<>nil;
  function1.Enabled:=pn<>nil;
  var1.Enabled:=pn<>nil;
  
  php4.Enabled:=pn<>nil;
  php5.Enabled:=pn<>nil;
  
  keyword1.Checked:=nGetBit(pn, keyword1.Tag);
  const1.Checked:=nGetBit(pn, const1.Tag);
  function1.Checked:=nGetBit(pn, function1.Tag);
  var1.Checked:=nGetBit(pn, var1.Tag);

  php4.Checked:=nGetBit(pn, php4.Tag);
  php5.Checked:=nGetBit(pn, php5.Tag);

  pecl.Checked:=nGetBit(pn, pecl.Tag);
  alias.Checked:=nGetBit(pn, alias.Tag);
end;

procedure TForm1.Html401Strict1Click(Sender: TObject);
begin
  nSwitchBit(pn,TMenuItem(Sender).Tag);
  TreeView1.Selected:=pn;
end;

procedure TForm1.nClearBit(n: TTreeNode; b: Integer);
begin
  if n<>nil then
    n.Data:=Pointer(Longword(n.Data) and (not (1 shl b)));
end;

function TForm1.nGetBit(n: TTreeNode; b: Integer): Boolean;
begin             
  if n<>nil then
    Result:=Longword(n.Data) and (1 shl b)>0
  else
    Result:=False;
end;

procedure TForm1.nSetBit(n: TTreeNode; b: Integer);
begin
  if n<>nil then
    n.Data:=Pointer(Integer(n.Data) or (1 shl b));
end;

procedure TForm1.nSwitchBit(n: TTreeNode; b: Integer);
begin
  if n<>nil then
    n.Data:=Pointer(Integer(n.Data) xor (1 shl b));
end;

procedure TForm1.AddAtrib(n:TTreeNode; s:String);
var
  nn:TTreeNode;
begin
  if n=nil then
    Exit;
  while n.Level<>0 do
    n:=n.Parent;
  TreeView1.Selected:=n;
  s:=LowerCase(Trim(s));
  nn:=n.getFirstChild;
  while nn<>nil do
    if s=nn.Text then
      break
    else
      nn:=n.GetNextChild(nn);
  if nn=nil then
    nn:=TreeView1.Items.AddChild(n,s);
//  nSetBit(nn,ComboBox1.ItemIndex);
  nn.MakeVisible;
  TreeView1.SetFocus;
end;

function TForm1.AddTag(s: String; b:Integer):TTreeNode;
var
  n:TTreeNode;
begin
  Result:=nil;
  s:=(Trim(s));
  if s='' then
    exit;
  n:=TreeView1.Items.GetFirstNode;
  while n<>nil do
    if s=n.Text then
      break
    else
      n:=n.GetNextChild(n);
  if n=nil then
    n:=TreeView1.Items.AddChild(nil,s);
  if b<>3 then
    n.Selected:=True;
  Result:=n;

  if php4box.Checked then
    nSetBit(n,php4.Tag);
  if php5box.Checked then
    nSetBit(n,php5.Tag);
  if peclbox.Checked then
    nSetBit(n,pecl.Tag);

  nSetType(n,b);
end;

procedure TForm1.Button18Click(Sender: TObject);
var
  n:TTreeNode;
  i, x, t:Integer;
  s, s1,s2:TStringList;
  ss1,ss2:String;
begin
  Button12Click(nil);
  s:=TStringList.Create;
  s1:=TStringList.Create;
  s2:=TStringList.Create;
  s.Add(Format('    ''%s'',',[Edit1.Text]));
  s.Add(Format('    ''%s''',[Edit2.Text]));
  s.SaveToFile(appdir+'..\SynHighlighterWeb_PhpVersion.inc');
  s.Clear;
  n:=form1.TreeView1.Items.GetFirstNode;
  t:=0;
  ss1:='    ';
  ss2:='    ';
  while n<>nil do
  begin
    if nGetBit(n, keyword1.Tag) or nGetBit(n, function1.Tag) then
    begin
      s.AddObject(n.Text,TObject(n.Data));
  //    s1.Add(Format('    //%4.d',[t]));
  //    s1.Add(Forma('    ''%s'',',[n.Text]));
      if Length(ss1+n.Text+',')>80 then
      begin
        s1.Add(ss1);
        ss1:='    ';
      end;
      ss1:=Format('%s''%s'', ',[ss1,n.Text]);

      //s2.Add(Format('    //%4.d: %s',[t,n.Text]));
      //s2.Add(Format('    $%s,',[IntToHex(Longword(n.Data),8)]));
      if Length(ss2+IntToHex(Longword(n.Data),8)+',')>80 then
      begin
        s2.Add(ss2);
        ss2:='    ';
      end;
      ss2:=Format('%s$%s, ',[ss2,IntToHex(Longword(n.Data),8)]);
      inc(t);
    end;
    n:=n.GetNextChild(n);
  end;
  if ss1<>'' then
    s1.Add(ss1);
  if ss2<>'' then
    s2.Add(ss2);
  s1[s1.Count-1]:=Copy(s1[s1.Count-1],1,Length(s1[s1.Count-1])-2);
  s1.Insert(0,format('  TSynWeb_PhpKeywords:array[0..%d] of String=(',[t-1]));
  s1.Add('    );');

  x:=KeyHash(s[0]);
  for i:=1 to s.Count-1 do
    if KeyHash(s[i])>x then
      x:=KeyHash(s[i]);
  s1.Insert(0,'');

  s1.Insert(0,Format('  PhpKeywordsMaxKeyHash = %d;',[x]));

  s2[s2.Count-1]:=Copy(s2[s2.Count-1],1,Length(s2[s2.Count-1])-2);
  s2.Insert(0,format('  TSynWeb_PhpKeywordsData:array[0..%d] of Longword=(',[t-1]));

  s2.Add('    );');

  s1.Add('');
  s1.AddStrings(s2);
  s1.SaveToFile('..\SynHighlighterWeb_PhpKeywords.inc');

  GenerateInc(s,
      appdir+'..\SynHighlighterWeb_PhpKeywordsFunc.inc',
      appdir+'..\SynHighlighterWeb_PhpKeywordsFuncList.inc',
      appdir+'..\SynHighlighterWeb_PhpKeywordsFuncTable.inc');

  s2.Free;
  s1.Free;
  s.Free;
  if ParamCount<>1 then
    ShowMessage('Done.');
end;

procedure TForm1.Button3Click(Sender: TObject);

begin
  while Memo1.Lines.Count>0 do
  begin
    AddTag(Memo1.Lines[0],TButton(Sender).Tag);
    Memo1.Lines.Delete(0);
  end;
    TreeView1.SetFocus;
end;

procedure TForm1.php4Click(Sender: TObject);
begin
  nSetBit(TreeView1.Selected,TMenuItem(Sender).Tag);
end;

procedure TForm1.nSetType(n: TTreeNode; b: Integer);
begin
  if n=nil then
    exit;
  case b of
  0:
    begin
      nSetBit(n,keyword1.Tag);
      nClearBit(n,const1.Tag);
      nClearBit(n,var1.Tag);
      nClearBit(n,function1.Tag);
    end;
  1:
    begin
      nClearBit(n,keyword1.Tag);
      nSetBit(n,const1.Tag);
      nClearBit(n,var1.Tag);
      nClearBit(n,function1.Tag);
    end;
  2:
    begin
      nClearBit(n,keyword1.Tag);
      nClearBit(n,const1.Tag);
      nSetBit(n,var1.Tag);
      nClearBit(n,function1.Tag);
    end;
  3:
    begin
      nClearBit(n,keyword1.Tag);
      nClearBit(n,const1.Tag);
      nClearBit(n,var1.Tag);
      nSetBit(n,function1.Tag);
    end;
  end;
end;

procedure TForm1.keyword1Click(Sender: TObject);
begin
  nSetType(pn,TMenuItem(Sender).Tag);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    FormStyle:=fsStayOnTop
  else
    FormStyle:=fsNormal;
end;

procedure TForm1.ComboBox1DropDown(Sender: TObject);
var
  f:TSearchRec;
  s:String;
begin
  s:=ExtractFilePath(Application.ExeName)+'..\!DOC\';
  ComboBox1.Clear;
  if FindFirst(s+'*',faAnyFile,f)=0 then
  begin
    if (f.Attr and faDirectory=faDirectory) and (f.Name<>'.') and (f.Name<>'..') then
      ComboBox1.Items.Add(s+f.Name);
    while FindNext(f)=0 do         
      if (f.Attr and faDirectory=faDirectory) and (f.Name<>'.') and (f.Name<>'..') then
        ComboBox1.Items.Add(s+f.Name);
  end;
end;

procedure TForm1.get_parsefiles(ADir: String; AStringList:TStringList);
var
  f:TSearchRec;
  r:TRegExpr;
begin
  if RightStr(ADir,1)<>'\' then
    ADir:=ADir+'\';
  if FindFirst(ADir+'*',faAnyFile,f)<>0 then
    Exit;
  r:=TRegExpr.Create;
  r.ModifierStr:='i';
  r.Expression:='\.(c|cpp|h|ec)$';
  repeat
    if (f.Name<>'.') and (f.Name<>'..') then
      if f.Attr and faDirectory=faDirectory then
        get_parsefiles(ADir+f.Name,AStringList)
      else
        if r.Exec(ADir+f.Name) then
          AStringList.Add(ADir+f.Name);
  until FindNext(f)<>0;
  r.Free;
end;

procedure TForm1.Button8Click(Sender: TObject);
const
  lx_:array[0..7] of Char=('|', '/', '-', '\', '|', '/', '-', '\');
var
  sl:TStringList;    
  r,rf,rx:TRegExpr;
  f:TFileStream;
  s,lf:String;
  i,xx,tf:Integer;
  n:TTreeNode;
  lt,lx:Longword;
  ds,dn:TDateTime;

  procedure updatestat;
  begin
    dn:=Time-ds;
    if (GetTickCount-lt>1000) then
    begin
      if i<(sl.Count/10) then
        Label4.Caption:=TimeToStr(dn)+' / -'
      else
        Label4.Caption:=TimeToStr(dn)+' / '+TimeToStr(dn*(sl.Count/(i+1)));
      //Label4.Repaint;
      Label5.Caption:=Format(lf,[lx_[lx mod 8], i, tf]);
      //Label5.Repaint;
      pb.Position:=i;
      inc(lx);
      lt:=GetTickCount;
      Application.ProcessMessages;
    end;
  end;

begin
  if not php4box.Checked and not php5box.Checked and not peclbox.Checked then
    raise Exception.Create('Php version?');
  ds:=Time;
  pb.Position:=0;
  sl:=TStringList.Create;
  get_parsefiles(ComboBox1.Text,sl);
  if sl.Count=0 then
    exit;
  sl.Sort;
  pb.Max:=sl.Count-1;
  r:=TRegExpr.Create;
  rf:=TRegExpr.Create;
  rx:=TRegExpr.Create;
  r.ModifierStr:='is-gm';
  rf.ModifierStr:='isg-m';
  rx.ModifierStr:='s-igm';
  //r.Expression:='/[*]\s*[{]{3}\s*proto\s+(([*][^/]|[^*])*)[*]/(\s|[#]([\\]([\n]|[\n][\r]|[\r][\n])|[^\n\r])*([\n]|[\n][\r]|[\r][\n])|/[*](([*][^/]|[^*])*)[*]/)*\s*(Php|ZEND)_FUNCTION';
  r.Expression:='/[*]\s*[{]{3}\s*proto\s+(([*][^/]|[^*])*)[*]/(.*)/[*]\s*[}]{3}\s*[*]/';
  rf.Expression:='^(\S*?)?\s*(\S+)\s*[(]([^)]*)[)]\s*(.*)$';

  rx.Expression:='^(\s*$|(\s+|/[*](([*][^/]|[^*])*)[*]/|[#]([\\]([\n]|[\r][\n]?)[^\n\r]|[^\n\r])*(([\n]|[\r][\n]?)|$)|(/[^*]|[^/#]))*(Php|ZEND)_(NAMED_)?FUNCTION)';
  //r.Expression:='^\s/\*\s+\{{3}\s+proto\s+(public\s+)?(\S+)\s+?([^(]+[(])?([^)]+)(.+?)\*/';
  TreeView1.Items.BeginUpdate;
  xx:=0;
  lf:=Format('[%%s] %%4.d/%4.d (%%4.d)',[sl.Count]);
  lt:=0;
  lx:=0;
  tf:=0;
  TreeView1.SortType:=stNone;
  Memo2.Clear;
  for i:=0 to sl.Count-1 do
  begin
    updatestat;
    f:=TFileStream.Create(sl[i],fmOpenRead);
    SetLength(s,f.Size);
    f.Read(s[1],length(s));
    f.Free;
    //if pos('proto mixed msg_receive',s)>0 then
    //  s:=StringReplace(s,'errorcode]]]','errorcode]]])',[rfReplaceAll]);
    if r.Exec(s) then
      repeat

    if tf>=299 then
      updatestat;
        if rf.Exec(r.Match[1]) then
        begin
          if not rx.Exec(r.Match[3]) then
            memo3.Lines.Add('#define ERROR_RegCheck_Match_Error_in_FILE "'+sl[i]+'"'#13#10+r.Match[0])
          else
          begin
            if pos('::',rf.Match[2])>0 then
            begin
              memo2.Lines.Add('#define ERROR_Static_Method_Found_In_Proto_Def_In_FILE "'+sl[i]+'"'#13#10+r.Match[0]);
              Continue;
            end;
            if pos('->',rf.Match[2])>0 then
            begin
              memo2.Lines.Add('#define ERROR_Dynamic_Method_Found_In_Proto_Def_In_FILE "'+sl[i]+'"'#13#10+r.Match[0]);
              Continue;
            end;
            n:=AddTag(trim(rf.Match[2]),3);
            Inc(tf);

            if php4box.Checked then
            begin
              n:=TreeView1.Items.AddChild(n,Edit1.Text);
              nSetBit(n,php4.Tag);
            end else
              if php5box.Checked then
              begin
                n:=TreeView1.Items.AddChild(n,Edit2.Text);
                nSetBit(n,php5.Tag);
              end else
                if peclbox.Checked then
                begin
                  n:=TreeView1.Items.AddChild(n,'PECL');
                  nSetBit(n,pecl.Tag);
                end;
            if rf.Match[1]<>'' then
              TreeView1.Items.AddChild(n,'Return: '+trim(rf.Match[1]))
            else
              TreeView1.Items.AddChild(n,'Return ?');
            TreeView1.Items.AddChild(n,'Parameters: '+StringReplace(trim(rf.Match[3]),#10,' ',[rfReplaceAll]));
            TreeView1.Items.AddChild(n,'Description: '+StringReplace(trim(rf.Match[4]),#10,' ',[rfReplaceAll]));
            TreeView1.Items.AddChild(n,'File: '+ExtractRelativePath(ComboBox1.Text,sl[i]));
          {  if rx.Exec(rf.Match[1]) then
              TreeView1.Items.AddChild(n,'Return: '+trim(rx.Match[2]))
            else
              TreeView1.Items.AddChild(n,'Return ?');
            TreeView1.Items.AddChild(n,'Parameters: '+StringReplace(trim(rf.Match[3]),#10,' ',[rfReplaceAll]));
            TreeView1.Items.AddChild(n,'Description: '+StringReplace(trim(rf.Match[4]),#10,' ',[rfReplaceAll])); }
          end;
        end else
        begin
          memo3.Lines.Add('#define ERROR_RegFunction_Match_Error_in_FILE "'+sl[i]+'"'#13#10+r.Match[0]);
          Inc(xx);
        end;
        updatestat;
        //  TreeView1.Items.EndUpdate; exit
      until not r.ExecNext;
  end;
  lt:=0;
  updatestat;     
  Caption:=inttostr(xx);
  Label5.Caption:='Sorting...';
  Label5.Repaint;
  TreeView1.SortType:=stBoth;
  TreeView1.Items.EndUpdate;
  r.Free;
  rf.Free;
  rx.Free;
  sl.Free;
  Label5.Caption:='Done';
  pb.Position:=0;
end;

procedure TForm1.Button9Click(Sender: TObject);
var
  n,nd:TTreeNode;
begin
  TreeView1.Items.BeginUpdate;
  n:=TreeView1.Items.GetFirstNode;
  while n<>nil do
  begin
    nd:=n;
    n:=n.GetNextChild(n);
    if nGetBit(nd,function1.Tag) then
      nd.Delete;
  end;
  TreeView1.Items.EndUpdate;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  f:TStringList;
  t,ln,i:Integer;

  function LoadNode(cn:TTreeNode=nil):TTreeNode;
  var
    x:Integer;
  begin
    Result:=TreeView1.Items.AddChildObject(cn,f[ln],Pointer(StrToInt(f[ln+1])));
    Inc(ln,3);
    for x:=0 to StrToIntDef(f[ln-1],0)-1 do
      LoadNode(Result);
  end;

begin
  if ParamCount=1 then
    OpenDialog1.FileName:=ParamStr(1)
  else
    if not OpenDialog1.Execute then
      Exit;
  TreeView1.Items.BeginUpdate;
  TreeView1.Items.Clear;
  f:=TStringList.Create; // (SaveDialog1.FileName, fmCreate)
  f.LoadFromFile(OpenDialog1.FileName);
  if (f.Count<4) or (f[0]<>'IsPhp=1') then
  begin                                         
    f.Free;
    TreeView1.Items.EndUpdate;
    ShowMessage('This is not Php-Config FILE!');
    Exit;
  end;
  Edit1.Text:=f[1];
  Edit2.Text:=f[2];
  t:=StrToIntDef(f[3],0);
  ln:=4;
  for i:=0 to t-1 do
    LoadNode;
  f.Free;
  TreeView1.Items.EndUpdate;
end;

procedure TForm1.ClearLists;
begin
  while KeyList.Count>0 do
  begin
    Dispose(KeyList[0]);
    KeyList.Delete(0);
  end;
end;

procedure TForm1.GenerateInc(AKeys: TStringList; AFileFunc, AFileFuncList, AFileFuncTable: String);
var
  i,mx,mm:Integer;
  l:TLexKeys;
  nf:TStringList;
  al:Boolean;
  tab:String;

  procedure AddData(islastgp:Boolean=false; islast:Boolean=false);
  const nc:array[False..True] of String=('or', 'then');
  const nc2:array[False..True] of String=('if', '  ');
  const nc3:array[False..True] of String=('if', '  ');
  begin
    case Longword(TLexKeys(KeyList[I]).Data) and $0F of
    $01:
      begin
        nf.add(tab+Format('%s  PhpKeywordComp(%d) %s',[nc2[al],TLexKeys(KeyList[I]).KeyIndex,nc[islastgp]]));
        al:=true;
        if islast then
          nf.Add(tab+'  Result := stkPhpKeyword')
        else
          if islastgp then
          begin
            nf.Add(tab+'  Result := stkPhpKeyword');
            nf.Add(tab+'else');
            tab:=tab+'  ';
            al:=false;
          end;
      end;
  {  $02:
      begin
        nf.add(tab+Format('if PhpConstComp(%d) then',[TLexKeys(KeyList[I]).KeyIndex]));
        nf.Add(tab+'  Result := stkPhpConst');
      end;
    $04:
      begin
        nf.add(tab+Format('if PhpVariableComp(%d) then',[TLexKeys(KeyList[I]).KeyIndex]));
        nf.Add(tab+'  Result := stkPhpVariable');
      end;    }
    $08:
      begin
        nf.add(tab+Format('%s  PhpFunctionComp(%d) %s',[nc2[al],TLexKeys(KeyList[I]).KeyIndex,nc[islastgp]]));
        if islast then
          nf.Add(tab+'  Result := stkPhpFunction');
        al:=true;
      end;
    else
      raise Exception.Create('Invalid type!');
    end;
    //nf.Add(tab+'else');
//        nf.add(Format('      // %s',[AKeys[TLexKeys(KeyList[I]).KeyIndex]]));
//        nf.add(Format('      %s(%d) or',[ACompFunc, TLexKeys(KeyList[I]).KeyIndex]));
    //
  end;

begin
  ClearLists;
  for i:=0 to AKeys.Count-1 do
  begin
    l:=TLexKeys.Create;
    l.KeyIndex:=i;
    l.Key:=KeyHash(AKeys[i]);
    l.Data:=AKeys.Objects[i];
    KeyList.Add(l);
  end;
  KeyList.Sort(CompareKeys);

  nf:=TStringList.Create;
  nf.add('    function PhpKeywordIdent: TSynWebTokenKind;');
  nf.add(Format('    function PhpKeywordFunc%d: TSynWebTokenKind;',[TLexKeys(KeyList[0]).Key]));
  for i:=1 to KeyList.Count-1 do
    if (TLexKeys(KeyList[i-1]).Key <> TLexKeys(KeyList[i]).Key) then
        nf.add(Format('    function PhpKeywordFunc%d: TSynWebTokenKind;',[TLexKeys(KeyList[i]).Key]));
  nf.SaveToFile(AFileFuncList);
  nf.Free;

  I := 0;
  nf:=TStringList.Create;
  while I < KeyList.Count do
  begin
    if I < KeyList.Count - 1 then
      while TLexKeys(KeyList[I]).Key = TLexKeys(KeyList[I + 1]).Key do
      begin
        inc(I);
        if I >= KeyList.Count - 1 then break;
      end;
    nf.add(Format('  FPhpIdentFuncTable[%d]:=PhpKeywordFunc%d;',[TLexKeys(KeyList[I]).Key, TLexKeys(KeyList[I]).Key]));
    inc(I);
  end;
  nf.SaveToFile(AFileFuncTable);
  nf.free;

  I := 0;
  nf:=TStringList.Create;
  nf.add('function TSynWebEngine.PhpKeywordIdent: TSynWebTokenKind;');
  nf.add('begin');
  nf.add('  Result := stkPhpIdentifier;');
  nf.add('end;');
  nf.add('');     
    mx:=0;
  while I < KeyList.Count do
  begin
    nf.add(Format('function TSynWebEngine.PhpKeywordFunc%d: TSynWebTokenKind;',[TLexKeys(KeyList[I]).Key]));
    nf.add('begin');
    tab:='  ';
    mm:=0;
    al:=false;
    if I < KeyList.Count - 1 then
      while TLexKeys(KeyList[I]).Key = TLexKeys(KeyList[I + 1]).Key do
      begin
        AddData(Longword(TLexKeys(KeyList[I]).Data) and $0F<>Longword(TLexKeys(KeyList[I+1]).Data) and $0F);
        inc(I);
        inc(mm);
        if I >= KeyList.Count - 1 then break;
      end;      
        inc(mm);
    AddData(True,True);
    if mm>mx then
      mx:=mm;
{    nf.add('      // ' + AKeys[TLexKeys(KeyList[I]).KeyIndex]);
    nf.add(Format('      %s(%d) then',[ACompFunc, TLexKeys(KeyList[I]).KeyIndex]));
    nf.add(Format('    Result := %s',[AResTrue]));}
    nf.add(tab+'else');
    nf.add(tab+'  Result := stkPhpIdentifier;');
    nf.add('end;');
    inc(I);
    if I < KeyList.Count then
      nf.add('');
  end;
  nf.SaveToFile(AFileFunc);
  nf.Free;
  if ParamCount<>1 then
    ShowMessage(IntToStr(mx));
end;

function TForm1.KeyHash(ToHash: String): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(ToHash) do
    inc(Result, mKeyHashTable[ToHash[I]]);
end;

procedure TForm1.MakeHashTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do
  begin
    J := UpperCase(I)[1];
    Case I in ['A'..'Z', 'a'..'z', ':', '-'] of
      True: mKeyHashTable[I] := Ord(J) - 64;
    else
      mKeyHashTable[I] := 0;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if ParamCount=1 then
    Left:=Screen.Width;            
  appdir:=ExtractFilePath(Application.ExeName);
  MakeHashTable;
  KeyList:=TList.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ClearLists;
  KeyList.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if ParamCount=1 then
  begin
    Button12Click(nil);
    Button6Click(nil);
    Button18Click(nil);
    Close;
  end;
end;

procedure TForm1.TreeView1Compare(Sender: TObject; Node1, Node2: TTreeNode;
  Data: Integer; var Compare: Integer);
begin
  if (Longword(Node1.Data)and 1<>0) and (Longword(Node2.Data)and 1=0) then
    Compare := -1
  else
    if (Longword(Node1.Data)and 1=0) and (Longword(Node2.Data)and 1<>0) then
      Compare := 1
    else
      Compare:=AnsiCompareStr(Node1.Text,Node2.Text);
end;

procedure TForm1.Button10Click(Sender: TObject);
const
  lx_:array[0..7] of Char=('|', '/', '-', '\', '|', '/', '-', '\');
var
  sl:TStringList;
  f:TFileStream;
  s,lf:String;
  i,xx,tf:Integer;
  lt,lx:Longword;
  ds,dn:TDateTime;

  p: TTestParser;

  procedure updatestat;
  begin
    dn:=Time-ds;
    if (GetTickCount-lt>1000) then
    begin
      if i<(sl.Count/10) then
        Label4.Caption:=TimeToStr(dn)+' / -'
      else
        Label4.Caption:=TimeToStr(dn)+' / '+TimeToStr(dn*(sl.Count/(i+1)));
      //Label4.Repaint;
      Label5.Caption:=Format(lf,[lx_[lx mod 8], i, tf]);
      //Label5.Repaint;
      pb.Position:=i;
      inc(lx);
      lt:=GetTickCount;
      Application.ProcessMessages;
    end;
  end;

begin
  ds:=Time;
  pb.Position:=0;
  sl:=TStringList.Create;
  get_parsefiles(ComboBox1.Text,sl);
  if sl.Count=0 then
    exit;
  sl.Sort;
  pb.Max:=sl.Count-1;

  TreeView1.Items.BeginUpdate;
  xx:=0;
  lf:=Format('[%%s] %%4.d/%4.d (%%4.d)',[sl.Count]);
  lt:=0;
  lx:=0;
  tf:=0;
  TreeView1.SortType:=stNone;
  Memo2.Clear;
  p:=TTestParser.Create;
  p.FAddTag:=AddTag;
  p.Tree:=TreeView1;
  if php4box.Checked then
    p.SubPrefix:='Php4-'
  else
    p.SubPrefix:='Php5-';
  for i:=0 to sl.Count-1 do
  begin
    updatestat;
    f:=TFileStream.Create(sl[i],fmOpenRead);
    SetLength(s,f.Size);
    f.Read(s[1],length(s));
    f.Free;
    p.SourceString:=s;
    p.ErrorStr:='';
    p.counter:=0;
    p.Parse;
    if p.ErrorStr<>'' then
      Exception.Create(sl[i]+#13#10+p.ErrorStr);
    Inc(tf,p.counter);
  {  if true then
    begin
          n:=AddTag(trim(rf.Match[2]),3);
          Inc(tf);
          if php4box.Checked then
          begin
            n:=TreeView1.Items.AddChild(n,Edit1.Text);
            nSetBit(n,php4.Tag);
          end else
            if php5box.Checked then
            begin
              n:=TreeView1.Items.AddChild(n,Edit2.Text);
              nSetBit(n,php5.Tag);
            end;

    end;     }
  end;
  p.Free;
  lt:=0;
  updatestat;
  Label5.Caption:='Sorting...';
  Label5.Repaint;
  TreeView1.SortType:=stBoth;
  Caption:=inttostr(xx);
  TreeView1.Items.EndUpdate;
  Label5.Caption:='Done';
  pb.Position:=0;
end;

end.


