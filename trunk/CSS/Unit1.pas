unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Menus, IniFiles32, clipbrd, ExtCtrls;

type
  TForm1 = class(TForm)
    TreeView1: TTreeView;
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Memo5: TMemo;
    Edit1: TEdit;
    Label1: TLabel;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    PopupMenu1: TPopupMenu;
    Html401Strict1: TMenuItem;
    Html401Transitional1: TMenuItem;
    Html401Frameset1: TMenuItem;
    N1: TMenuItem;
    None1: TMenuItem;
    ComboBox1: TComboBox;
    CheckBox1: TCheckBox;
    Button18: TButton;
    Memo1: TMemo;
    Button6: TButton;
    Timer1: TTimer;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    N2: TMenuItem;
    SPECIAL1: TMenuItem;
    IDENT1: TMenuItem;
    Button10: TButton;
    Edit2: TEdit;
    frequency2: TMenuItem;
    angle6: TMenuItem;
    time2: TMenuItem;
    length2: TMenuItem;
    identifier2: TMenuItem;
    string2: TMenuItem;
    color2: TMenuItem;
    percentage3: TMenuItem;
    number2: TMenuItem;
    number3: TMenuItem;
    integer2: TMenuItem;
    integer3: TMenuItem;
    percentage2: TMenuItem;
    N1009002: TMenuItem;
    length3: TMenuItem;
    CLOSEPOPUP1: TMenuItem;
    CheckBox2: TCheckBox;
    N3: TMenuItem;
    iss: TCheckBox;
    N4: TMenuItem;
    Button15: TButton;
    CheckBox3: TCheckBox;
    media1: TMenuItem;
    pseudo1: TMenuItem;
    atkeyword1: TMenuItem;
    page1: TMenuItem;
    Values1: TMenuItem;
    css21pseudo1: TMenuItem;
    Button16: TButton;
    page2: TMenuItem;
    dpi1: TMenuItem;
    Button17: TButton;
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Html401Strict1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
  private
    pn:TTreeNode;
  public
    cl:String;
    function nGetBit(n:TTreeNode; b:Integer):Boolean;
    procedure nSetBit(n:TTreeNode; b:Integer);
    procedure nClearBit(n:TTreeNode; b:Integer);
    procedure nSwitchBit(n:TTreeNode; b:Integer);
    procedure AddAtrib(n:TTreeNode; s:String);
    procedure AddTag(s:String);
  end;

var
  Form1: TForm1;

implementation

uses Unit2, StrUtils, Math;

{$R *.dfm}

procedure ParseLine(ALine: String; ADst: TStrings);
const
  cAlphaNum = ['a'..'z', 'A'..'Z', '0'..'9', '-'];
var
  lPos: Integer;
  lParam: String;
begin
  ALine := Trim(ALine);

  while ALine <> '' do
  begin
    if not (ALine[1] in cAlphaNum) then
    begin
      lPos := 1;

      while (lPos <= Length(ALine)) and not (ALine[lPos] in cAlphaNum) do
        Inc(lPos);

      Delete(ALine, 1, lPos - 1);
      Continue;
    end;

    lPos := 1;
    while (lPos <= Length(ALine)) and (ALine[lPos] in cAlphaNum) do
      Inc(lPos);

    lParam := Copy(ALine, 1, lPos - 1);
    Delete(ALine, 1, lPos - 1);
    if lParam <> '' then
      ADst.Add(lParam);
  end;
end;

function ParseFirstIdent(S: String): String;
var
  lList: TStringList;
begin
  lList := TStringList.Create;
  try
    ParseLine(S, lList);
    if lList.Count = 0 then
      Result := ''
    else
      Result := lList[0];
  finally
    lList.Free;
  end;
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

procedure TForm1.Button3Click(Sender: TObject);
begin
  if Edit1.Text='' then
    exit;
  AddTag(Edit1.Text);
  Edit1.Text:='';
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if TreeView1.Selected<>nil then
    TreeView1.Selected.Delete;
  TreeView1.SetFocus;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  i:Integer;
  lTagIdx: Integer;
  lTags, lList: TStringList;

  procedure ParseParams(ASrc, ADst: TStrings);
  var
    lIdx: Integer;
  begin
    for lIdx := 0 to ASrc.Count - 1 do
      ParseLine(ASrc[lIdx], ADst);
  end;

begin
  lTags := nil;
  lList := nil;
  try
    lList := TStringList.Create;
    lTags := TStringList.Create;

    ParseLine(Edit1.Text, lTags);
    ParseParams(Memo5.Lines, lList);

    if lTags.Count = 0 then
    begin
      for i:=0 to lList.Count-1 do
        AddAtrib(TreeView1.Selected, lList[i]);
    end else
    begin
      for lTagIdx := 0 to lTags.Count - 1 do
      begin
        Edit1.Text := lTags[lTagIdx];
        Button3.Click;

        for i:=0 to lList.Count-1 do
          AddAtrib(TreeView1.Selected, lList[i]);
      end;
    
    end;
  finally
    lList.Free;
    lTags.Free;
  end;
  edit1.Text:='';
  Memo5.Clear;
end;

procedure TForm1.Button11Click(Sender: TObject);
var
  t,a:Integer;
  attr:TStringList;
  n1,n2:TTreeNode;
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
    n2:=n1.getFirstChild;
    while n2<>nil do
    begin
      if attr.IndexOf(n2.Text)=-1 then
      begin
        attr.Add(n2.Text);
        inc(a);
      end;
      n2:=n2.GetNextChild(n2);
    end;
    n1:=n1.GetNextChild(n1);
  end;
  Label1.Caption:=format('%d %d',[t,a]);
  attr.Free;
end;

procedure TForm1.Button12Click(Sender: TObject);
begin
  TreeView1.Items.AlphaSort(CheckBox1.Checked);
end;

procedure TForm1.Button14Click(Sender: TObject);
var
  ini:TIniFile32;
  t,a:Integer;
  n1,n2:TTreeNode;
begin
  if ParamCount=1 then
    OpenDialog1.FileName:=ParamStr(1)
  else
    if not OpenDialog1.Execute then
      Exit;
  TreeView1.Items.BeginUpdate;
  TreeView1.Items.Clear;
  ini:=TIniFile32.Create(OpenDialog1.FileName);
  ini.BeginUpdate;
  if ini.ReadInteger('Config','IsCss',0)<>1 then
  begin
    ShowMessage('This is not Css-Config FILE!');
    ini.Free;
    Exit;
  end;
  ComboBox1.ItemIndex:=ini.ReadInteger('Config','Html',0);
  iss.Checked:=ini.ReadBool('Config','IsSpecial',False);
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
  ini.EndUpdate;
  ini.Free;
  TreeView1.Items.EndUpdate;
end;

procedure TForm1.Button13Click(Sender: TObject);
var
  ini:TIniFile32;
  t,a:Integer;
  n1,n2:TTreeNode;
begin
  if not SaveDialog1.Execute then
    Exit;
  ini:=TIniFile32.Create(SaveDialog1.FileName);
  ini.BeginUpdate;
  t:=0;
  n1:=TreeView1.Items.GetFirstNode;
  while n1<>nil do
  begin
    a:=0;
    ini.WriteString(format('TAG_%d',[t]),'Name',n1.Text); 
    ini.WriteString(format('TAG_%d',[t]),'Data','$'+IntToHex(Longword(n1.Data),8));
    n2:=n1.getFirstChild;
    while n2<>nil do
    begin
      ini.WriteString(format('TAG_%d',[t]),Format('Attr_%d_Name',[a]),n2.Text);
      ini.WriteString(format('TAG_%d',[t]),Format('Attr_%d_Data',[a]),'$'+IntToHex(Longword(n2.Data),8));
      n2:=n2.GetNextChild(n2);  
      inc(a);
    end;        
    ini.WriteInteger(format('TAG_%d',[t]),'Attr_Count',a);
    n1:=n1.GetNextChild(n1);  
    Inc(t);
  end;       
  ini.WriteInteger('Config','Tags',t);
  ini.WriteInteger('Config','IsCss',1);
  ini.WriteInteger('Config','Html',ComboBox1.ItemIndex);
  ini.WriteBool('Config','IsSpecial',iss.Checked);
  ini.EndUpdate;
  ini.Free;
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);
var
  i:Integer;
begin
  pn:=TreeView1.Selected;
  Html401Strict1.Enabled:=pn<>nil;
  Html401Transitional1.Enabled:=pn<>nil;
  Html401Frameset1.Enabled:=pn<>nil;

  IDENT1.Visible:=pn.Level=1;
  IDENT1.Enabled:=pn<>nil;

  SPECIAL1.Visible:=pn.Level=0;
  for i:=PopupMenu1.Items.IndexOf(angle6) to PopupMenu1.Items.Count-1 do
  begin
    if PopupMenu1.Items[i].Tag=-1 then
      Continue;
    if pn<>nil then
      PopupMenu1.Items[i].Visible:=pn.Level=0;
    PopupMenu1.Items[i].Enabled:=pn<>nil;
  end;

  if pn=nil then
  begin
    None1.Caption:='>>> NONE <<<';
    Exit;
  end;
  TreeView1.Selected:=pn;
  None1.Caption:=pn.Text;

  Html401Strict1.Checked:=nGetBit(pn,0);
  Html401Transitional1.Checked:=nGetBit(pn,1);
  Html401Frameset1.Checked:=nGetBit(pn,2);
  IDENT1.Checked:=nGetBit(pn,31);

  for i:=PopupMenu1.Items.IndexOf(angle6) to PopupMenu1.Items.Count-1 do  
    if PopupMenu1.Items[i].Tag=-1 then
      Continue
    else
      PopupMenu1.Items[i].Checked:=nGetBit(pn,PopupMenu1.Items[i].Tag);
end;

procedure TForm1.Html401Strict1Click(Sender: TObject);
begin
  nSwitchBit(pn,TMenuItem(Sender).Tag);
  TreeView1.Selected:=pn;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
i:Integer;
begin
  if ParamCount=1 then
    Left:=Screen.Width;
  clipboard.AsText:='';
  ComboBox1.ItemIndex:=1;
  cl:=clipboard.AsText;
  for i:=PopupMenu1.Items.IndexOf(angle6) to PopupMenu1.Items.Count-1 do
  begin
    if PopupMenu1.Items[i].Tag<>-1 then
      PopupMenu1.Items[i].Caption:=intTostr(PopupMenu1.Items[i].tag)+'     '+PopupMenu1.Items[i].Caption;
  end;
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
  nSetBit(nn,ComboBox1.ItemIndex);
  nn.MakeVisible;
  TreeView1.SetFocus;
end;

procedure TForm1.AddTag(s: String);
var
  n:TTreeNode;
begin
  s:=LowerCase(Trim(ParseFirstIdent(s)));
  if s = '' then
    Exit;
    
  n:=TreeView1.Items.GetFirstNode;
  while n<>nil do
    if s=n.Text then
      break
    else
      n:=n.GetNextChild(n);
  if n=nil then
    n:=TreeView1.Items.AddChild(nil,s);
  n.Selected:=True;
  nSetBit(n,ComboBox1.ItemIndex);
  TreeView1.SetFocus;
end;

procedure TForm1.Button18Click(Sender: TObject);
begin     
  Button12Click(nil);
  form2.Button4Click(nil);
end;

procedure TForm1.Button6Click(Sender: TObject);
const
  ch=[' ', '[', ']', '|', #13, #10, '.', '*', '+', ',', '?', '/', '{'];
var
  i:Integer;
  s,s2:String;
  sl:TStringList;

  procedure space;
  begin
    if s='' then
      exit;
    while s[1] in ch do
    begin
      if s[1]='{' then
      begin
        while not (s[1]='}') do  
          Delete(s,1,1);   
        Delete(s,1,1);
      end else
        Delete(s,1,1);
      if s='' then
        Exit;
    end;
  end;

  function readname:Boolean;
  begin
    Result:=false;
    space;
    if s[1]=#9 then
      exit;
    Delete(s,1,1);
    i:=1;
    while s[i]<>#39 do
      Inc(i);
    sl.add(copy(s,1,i-1));   
    Delete(s,1,i);
    space;
    result:=true;
  end;

  function chk(x:String):String;
  var
    ci:Integer;
    b:Boolean;
  begin
    x:=StringReplace(x,'<','[',[rfReplaceAll]);
    x:=StringReplace(x,'>',']',[rfReplaceAll]);
    b:=false;
    for ci:=1 to length(x) do
      if x[ci]=#39 then
      begin
        if b then
          x[ci]:='>'
        else
          x[ci]:='<';
        b:=not b;
      end;
    Result:=x;
  end;

begin
  Memo5.Clear;
  s:=memo1.Text;
  if ComboBox1.ItemIndex=0 then
  begin
    while not (s[1] in [#39]) do
      Delete(s,1,1);
    Delete(s,1,1);
    i:=1;
    while s[i]<>#39 do
      Inc(i);
    Edit1.Text:=copy(s,1,i-1);
    while not (s[1] in [':']) do
      Delete(s,1,1);
    Delete(s,1,1);
    while s<>'' do
    begin
      space;  
      i:=1;
      if s='' then
        break;
      while not (s[i] in ch) do
        Inc(i);
      memo5.lines.add(copy(s,1,i-1));
      delete(s,1,i-1);
    end;         
    Button3Click(nil);
    Button5Click(nil);
    Memo1.Clear;
  end else
  begin
    sl:=TStringList.Create;
    while readname do
      ;
    Delete(s,1,1);
    s2:='';
    while s<>'' do
    begin
      space;
      i:=1;
      if s='' then
        break;
      while not (s[i] in ch) do
        Inc(i);
      s2:=s2+copy(s,1,i-1)+#13#10;
      delete(s,1,i-1);
    end;
    s2:=chk(s2);
    while sl.Count>0 do
    begin
      Edit1.Text:=sl[0];
      memo5.Text:=s2;
      Button3Click(nil);
      Button5Click(nil);
      Memo1.Clear;
      sl.Delete(0);
    end;
    sl.free;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if not Clipboard.HasFormat(CF_TEXT) then
    Exit;
  if cl=clipboard.AsText then
    exit;
  cl:=clipboard.AsText;
  if cl='' then
    exit;
  if ComboBox1.ItemIndex=0 then
  begin
    if (cl[1]<>'5') or (cl[2]<>'.') then
      exit;
  end else
    if cl[1]<>#39 then
      Exit;
  Memo1.Text:=cl;
  Button6.Click;
  MessageBeep(0);
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  i,k:Integer;
  t,x:TTreeNode;
  sl:TStringList;

  function GetAttrsCss1(tg:TTreeNode):String;
  var
    j:Integer;  
    n:TTreeNode;

    function gv:String;
    begin
      Result:=IfThen(nGetBit(tg,0),'1','0')+IfThen(nGetBit(tg,1),'2','0');
    end;

  begin
    result:='';
    if (LeftStr(tg.Text,1)='<') and (RightStr(tg.Text,1)='>') then
    begin
      if tg.Text='<color>' then
        result:='[color]'+gv+#13#10
      else
      begin
        for j:=0 to TreeView1.Items.Count-1 do
        begin
          n:=TreeView1.Items[j];
          if (n.Level<>0) or (n.Text<>Copy(tg.Text,2,Length(tg.Text)-2)) then
            continue
          else
            begin
              n:=n.getFirstChild;
              while n<>nil do
              begin
                result:=result+GetAttrsCss1(n);
                n:=n.GetNextChild(n);
              end;
              break;
            end;
        end;
      end;
    end;
    if result='' then
      result:=tg.Text+gv+#13#10;
    result:=StringReplace(result,'<','[',[rfReplaceAll]);
    result:=StringReplace(result,'>',']',[rfReplaceAll]);
  end;

begin
  sl:=TStringList.Create;
  TreeView1.Items.BeginUpdate;
  for i:=TreeView1.Items.Count-1 downto 0 do
  begin
    t:=TreeView1.Items[i];
    if t.Level<>1 then
      continue
    else
     // if (LeftStr(t.Text,1)='<') and (RightStr(t.Text,1)='>') then
      begin
        sl.Clear;
        sl.Text:=GetAttrsCss1(t);
        x:=t.Parent;
        t.Delete;
        for k:=0 to sl.Count-1 do
        begin
          if RightStr(sl[k],2)[1]='1' then
          begin
            ComboBox1.ItemIndex:=0;
            AddAtrib(x,copy(sl[k],1,length(sl[k])-2));
          end;
          
          if RightStr(sl[k],1)[1]='2' then
          begin
            ComboBox1.ItemIndex:=1;
            AddAtrib(x,copy(sl[k],1,length(sl[k])-2));
          end;
        end;
      end;
  end;   
  TreeView1.Items.EndUpdate;
end;

procedure TForm1.Button8Click(Sender: TObject);
var
  i:Integer;
  t:TTreeNode;
begin
  for i:=TreeView1.Items.Count-1 downto 0 do
  begin
    t:=TreeView1.Items[i];
    if t.Level<>1 then
      continue
    else
      if t.Text='color' then
      begin
        t.Text:='[color]';
//        AddAtrib(t.Parent,'[color]');
//        t.Delete;
      end;
  end;
end;

procedure TForm1.Button9Click(Sender: TObject);
var
  i:Integer;
  t:TTreeNode;
begin
  TreeView1.Items.BeginUpdate;
  for i:=TreeView1.Items.Count-1 downto 0 do
  begin                
    ComboBox1.ItemIndex:=1;
    t:=TreeView1.Items[i];
    if t.Level<>1 then
      continue
    else
      if t.Text='[absolute-size]' then
      begin
        if nGetBit(t.Parent,0) then
        begin
          ComboBox1.ItemIndex:=0;
          AddAtrib(t.Parent,'xx-small');
          AddAtrib(t.Parent,'x-small');
          AddAtrib(t.Parent,'small');
          AddAtrib(t.Parent,'medium');
          AddAtrib(t.Parent,'large');
          AddAtrib(t.Parent,'x-large');
          AddAtrib(t.Parent,'xx-large');
        end;

        ComboBox1.ItemIndex:=1;
        AddAtrib(t.Parent,'xx-small');
        AddAtrib(t.Parent,'x-small');
        AddAtrib(t.Parent,'small');
        AddAtrib(t.Parent,'medium');
        AddAtrib(t.Parent,'large');
        AddAtrib(t.Parent,'x-large');
        AddAtrib(t.Parent,'xx-large');
        t.Delete;
      end else
      if t.Text='[angle]' then
      begin
        // SPECIAL !!! css2.1
        nSetBit(t.Parent,31);
        t.Delete;
      end else
      if t.Text='[border-style]' then
      begin
        if nGetBit(t.Parent,0) then
        begin
          ComboBox1.ItemIndex:=0;
          AddAtrib(t.Parent,'none');
          AddAtrib(t.Parent,'dotted');
          AddAtrib(t.Parent,'dashed');
          AddAtrib(t.Parent,'solid');
          AddAtrib(t.Parent,'double');
          AddAtrib(t.Parent,'groove');
          AddAtrib(t.Parent,'ridge');
          AddAtrib(t.Parent,'inset');
          AddAtrib(t.Parent,'outset');
        end;

        ComboBox1.ItemIndex:=1;
        AddAtrib(t.Parent,'none');
        AddAtrib(t.Parent,'hidden');
        AddAtrib(t.Parent,'dotted');
        AddAtrib(t.Parent,'dashed');
        AddAtrib(t.Parent,'solid');
        AddAtrib(t.Parent,'double');
        AddAtrib(t.Parent,'groove');
        AddAtrib(t.Parent,'ridge');
        AddAtrib(t.Parent,'inset');
        AddAtrib(t.Parent,'outset');
        t.Delete;
      end else
      if t.Text='[border-width]' then
      begin
        if nGetBit(t.Parent,0) then
        begin
          // SPECIAL !!! css1
          nSetBit(t.Parent,30);
        end;

        AddAtrib(t.Parent,'thin');
        AddAtrib(t.Parent,'medium');
        AddAtrib(t.Parent,'thick');
        // SPECIAL !!! css2.1
        nSetBit(t.Parent,30);
        t.Delete;
      end else
      if t.Text='[color]' then
      begin
        if nGetBit(t.Parent,0) then
        begin
          ComboBox1.ItemIndex:=0;
          AddAtrib(t.Parent,'rgb');
          AddAtrib(t.Parent,'maroon');
          AddAtrib(t.Parent,'red');
          // AddAtrib(t.Parent,'orange');
          AddAtrib(t.Parent,'yellow');
          AddAtrib(t.Parent,'olive');
          AddAtrib(t.Parent,'purple');
          AddAtrib(t.Parent,'fuchsia');
          AddAtrib(t.Parent,'white');
          AddAtrib(t.Parent,'lime');
          AddAtrib(t.Parent,'green');
          AddAtrib(t.Parent,'navy');
          AddAtrib(t.Parent,'blue');
          AddAtrib(t.Parent,'aqua');
          AddAtrib(t.Parent,'teal');
          AddAtrib(t.Parent,'black');
          AddAtrib(t.Parent,'silver');
          AddAtrib(t.Parent,'gray');  
        end;

        ComboBox1.ItemIndex:=1;
        AddAtrib(t.Parent,'rgb');
        AddAtrib(t.Parent,'maroon');
        AddAtrib(t.Parent,'red');
        AddAtrib(t.Parent,'orange');
        AddAtrib(t.Parent,'yellow');
        AddAtrib(t.Parent,'olive');
        AddAtrib(t.Parent,'purple');
        AddAtrib(t.Parent,'fuchsia');
        AddAtrib(t.Parent,'white');
        AddAtrib(t.Parent,'lime');
        AddAtrib(t.Parent,'green');
        AddAtrib(t.Parent,'navy');
        AddAtrib(t.Parent,'blue');
        AddAtrib(t.Parent,'aqua');
        AddAtrib(t.Parent,'teal');
        AddAtrib(t.Parent,'black');
        AddAtrib(t.Parent,'silver');
        AddAtrib(t.Parent,'gray');
        // SPECIAL !!! css1, css2.1    
        nSetBit(t.Parent,29);
        t.Delete;
      end else
      if t.Text='[counter]' then
      begin
        AddAtrib(t.Parent,'counter');
        // SPECIAL !!! css2.1      
        nSetBit(t.Parent,28);
        t.Delete;
      end else
      if t.Text='[family-name]' then
      begin
        // SPECIAL !!! css1 & css2.1 
        nSetBit(t.Parent,27);
        t.Delete;
      end else
      if t.Text='[frequency]' then
      begin
        // SPECIAL !!! css2.1  
        nSetBit(t.Parent,26);
        t.Delete;
      end else
      if t.Text='[generic-family]' then
      begin
        // SPECIAL !!! css1 & css2.1 
        nSetBit(t.Parent,25);
        t.Delete;
      end else
      if t.Text='[generic-voice]' then
      begin
        AddAtrib(t.Parent,'male');
        AddAtrib(t.Parent,'female');
        AddAtrib(t.Parent,'child');
        t.Delete;
      end else
      if t.Text='[identifier]' then
      begin
        // SPECIAL !!! css2.1 
        nSetBit(t.Parent,24);
        t.Delete;
      end else
      if t.Text='[Integer]' then
      begin
        // SPECIAL !!! css2.1 
        nSetBit(t.Parent,23);
        t.Delete;
      end else
      if t.Text='[length]' then
      begin
        // SPECIAL !!! css1 & css2.1
        nSetBit(t.Parent,22);
        t.Delete;
      end else
      if t.Text='[margin-width]' then
      begin
        AddAtrib(t.Parent,'auto');
        // SPECIAL !!! css2.1   
        nSetBit(t.Parent,21);
        t.Delete;
      end else
      if t.Text='[number]' then
      begin
        // SPECIAL !!! css1 & css2.1 
        nSetBit(t.Parent,20);
        t.Delete;
      end else
      if t.Text='[padding-width]' then
      begin
        // SPECIAL !!! css2.1  
        nSetBit(t.Parent,19);
        t.Delete;
      end else
      if t.Text='[percentage]' then
      begin
        // SPECIAL !!! css1 & css2.1 
        nSetBit(t.Parent,18);
        t.Delete;
      end else
      if t.Text='[relative-size]' then
      begin
        ComboBox1.ItemIndex:=0;
        AddAtrib(t.Parent,'larger');
        AddAtrib(t.Parent,'smaller');

        ComboBox1.ItemIndex:=1;
        AddAtrib(t.Parent,'larger');
        AddAtrib(t.Parent,'smaller');
        t.Delete;
      end else
      if t.Text='[shape]' then
      begin
        AddAtrib(t.Parent,'rect');
        // SPECIAL !!! css2.1  
        nSetBit(t.Parent,17);
        t.Delete;
      end else
      if t.Text='[specific-voice]' then
      begin
        // SPECIAL !!! css2.1 
        nSetBit(t.Parent,16);
        t.Delete;
      end else
      if t.Text='[String]' then
      begin
        // SPECIAL !!! css2.1 
        nSetBit(t.Parent,15);
        t.Delete;
      end else
      if t.Text='[time]' then
      begin
        // SPECIAL !!! css2.1 
        nSetBit(t.Parent,14);
        t.Delete;
      end else
      if t.Text='[uri]' then
      begin
        AddAtrib(t.Parent,'url');
        // SPECIAL !!! css2.1   
        nSetBit(t.Parent,13);
        t.Delete;
      end else
      if t.Text='[url]' then
      begin
        ComboBox1.ItemIndex:=0;
        AddAtrib(t.Parent,'url');
        // SPECIAL !!! css1
        nSetBit(t.Parent,12);
        t.Delete;
      end;
  end;
  TreeView1.Items.EndUpdate;
end;

procedure TForm1.Button10Click(Sender: TObject);
var
  i:Integer;
  n:TTreeNode;
begin
  if TreeView1.Selected<>nil then
    n:=TreeView1.Selected
  else
    n:=TreeView1.Items.GetFirstNode;
  for i:=n.AbsoluteIndex+1 to TreeView1.Items.Count-1 do
    if (TreeView1.Items[i].Level=Button10.Tag) and
      nGetBit(TreeView1.Items[i],strtoint(Edit2.Text)) then
    begin
      TreeView1.Items[i].Selected:=True;
      TreeView1.Items[i].MakeVisible;
      TreeView1.SetFocus;
      Exit;
    end;

end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
timer1.Enabled:=CheckBox2.Checked;
end;

procedure TForm1.Button15Click(Sender: TObject);
var
  i:Integer;
begin
  for i:=0 to TreeView1.Items.Count-1 do
    if TreeView1.Items[i].Level=1 then
      if (TreeView1.Items[i].Text='url') or (TreeView1.Items[i].Text='rgb') then
        nSetBit(TreeView1.Items[i],31);
end;

procedure TForm1.Button16Click(Sender: TObject);
var
  i:Integer;
  n: TTreeNode;
begin
  for i:=0 to TreeView1.Items.Count-1 do
  begin
    n := TreeView1.Items[i];
    if nGetBit(n, 1) then // css2
      nSetBit(n, 2);   // css3
  end;
end;

procedure TForm1.Button17Click(Sender: TObject);
var
  i:Integer;
  n: TTreeNode;
  lPseudo: Boolean;
  lList: TStringList;
begin
  Edit1.Text := '';
  lList := TStringList.Create;
  try
    lList.LoadFromFile('css3colors.txt');

    for i:=0 to TreeView1.Items.Count-1 do
    begin
      n := TreeView1.Items[i];

      if n.Level<>0 then
        Continue;

      if nGetBit(n, 18) then // [color]
      begin
        TreeView1.Selected := n;
        Memo5.Lines.Assign(lList);
        Button5.Click;
      end;
    end;
  finally
    lList.Free;
  end;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  if CheckBox3.Checked then
    Button10.Caption:='find bit (level=1)'
  else
    Button10.Caption:='find bit (level=0)';
  Button10.Tag:=IfThen(CheckBox3.Checked,1,0);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if ParamCount=1 then
  begin  
    Button14Click(nil);
    Button18Click(nil);
    Close;
  end;
end;

end.


