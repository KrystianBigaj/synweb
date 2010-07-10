unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Menus, IniFiles32, RegExpr;

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
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    Button10: TButton;
    Memo4: TMemo;
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
    XHtml10Strict1: TMenuItem;
    XHtml10Transitional1: TMenuItem;
    XHtml10Frameset1: TMenuItem;
    N1: TMenuItem;
    None1: TMenuItem;
    ComboBox1: TComboBox;
    N2: TMenuItem;
    N3: TMenuItem;
    Button15: TButton;
    Button16: TButton;
    Memo6: TMemo;
    XHtml10TransitionalDEPRECATED1: TMenuItem;
    Button17: TButton;
    XHtml10FramesetlDEPRECATED1: TMenuItem;
    N4: TMenuItem;
    Otherstatus1: TMenuItem;
    CheckBox1: TCheckBox;
    Button18: TButton;
    Memo7: TMemo;
    Button19: TButton;
    Memo8: TMemo;
    Button20: TButton;
    Memo9: TMemo;
    Button21: TButton;
    N5: TMenuItem;
    EMPTY1: TMenuItem;
    Button22: TButton;
    NoCLASS1: TMenuItem;
    CheckBox2: TCheckBox;
    Button24: TButton;
    CheckBox3: TCheckBox;
    isEXT: TMenuItem;
    Wml111: TMenuItem;
    Wml121: TMenuItem;
    Wml131: TMenuItem;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    Button23: TButton;
    ComboBox2: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    N6: TMenuItem;
    XSLT101: TMenuItem;
    XSLT201: TMenuItem;
    Html5pop: TMenuItem;
    magicHtml5button: TButton;
    Button25: TButton;
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
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
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button19Click(Sender: TObject);
    procedure Button20Click(Sender: TObject);
    procedure Button21Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button22Click(Sender: TObject);
    procedure Button24Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button23Click(Sender: TObject);
    procedure magicHtml5buttonClick(Sender: TObject);
    procedure Button25Click(Sender: TObject);
  private
    pn:TTreeNode;
  public
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

uses Unit2;

{$R *.dfm}

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
begin
  for i:=0 to Memo5.Lines.Count-1 do
    AddAtrib(TreeView1.Selected,Memo5.Lines[i]);
  Memo5.Clear;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  i:Integer;
begin
  for i:=0 to Memo1.Lines.Count-1 do
    AddAtrib(TreeView1.Selected,Memo1.Lines[i]);
  TreeView1.SetFocus;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  i:Integer;
begin
  for i:=0 to Memo2.Lines.Count-1 do
    AddAtrib(TreeView1.Selected,Memo2.Lines[i]);
  TreeView1.SetFocus;
end;

procedure TForm1.Button8Click(Sender: TObject);
var
  i:Integer;
begin
  for i:=0 to Memo3.Lines.Count-1 do
    AddAtrib(TreeView1.Selected,Memo3.Lines[i]);
  TreeView1.SetFocus;
end;

procedure TForm1.Button10Click(Sender: TObject);
var
  i:Integer;
begin
  for i:=0 to Memo4.Lines.Count-1 do
    AddAtrib(TreeView1.Selected,Memo4.Lines[i]);
  TreeView1.SetFocus;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  if Edit1.Text<>'' then
    Button3.Click;
  Button6.Click;
  Button7.Click;
  Button8.Click;
  if Memo5.Text<>'' then
    Button5.Click;
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
  if ini.ReadInteger('Config','IsHtml',00)<>1 then
  begin
    ShowMessage('This is not Html-Config FILE!');
    ini.Free;
    Exit;
  end;
  ComboBox1.ItemIndex:=ini.ReadInteger('Config','Html',0);
  CheckBox3.Checked:=ini.ReadBool('Config','Html-Special',False);
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
  ini.WriteInteger('Config','Html',ComboBox1.ItemIndex);
  ini.WriteInteger('Config','IsHtml',1);
  ini.WriteBool('Config','Html-Special',CheckBox3.Checked);
  ini.EndUpdate;
  ini.Free;
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);
begin
  pn:=TreeView1.Selected;
  Html401Strict1.Enabled:=pn<>nil;
  Html401Transitional1.Enabled:=pn<>nil;
  Html401Frameset1.Enabled:=pn<>nil;
  Html5pop.Enabled:=pn<>nil;
  XHtml10Strict1.Enabled:=pn<>nil;
  XHtml10Transitional1.Enabled:=pn<>nil;
  XHtml10Frameset1.Enabled:=pn<>nil;
  XHtml10TransitionalDEPRECATED1.Enabled:=pn<>nil;
  XHtml10FramesetlDEPRECATED1.Enabled:=pn<>nil;
  XSLT101.Enabled:=pn<>nil;
  XSLT201.Enabled:=pn<>nil;

  Wml111.Enabled := pn <> nil;
  Wml121.Enabled := pn <> nil;
  Wml131.Enabled := pn <> nil;

  isEXT.Enabled:=pn<>nil;
  isEXT.Visible:=pn.Level=0;
  NoCLASS1.Enabled:=pn<>nil;
  NoCLASS1.Visible:=pn.Level=0;
  EMPTY1.Enabled:=pn<>nil;
  EMPTY1.Visible:=pn.Level=0;

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
  Html5pop.Checked:=nGetBit(pn,3);
  XHtml10Strict1.Checked:=nGetBit(pn,4);
  XHtml10Transitional1.Checked:=nGetBit(pn,5);
  XHtml10Frameset1.Checked:=nGetBit(pn,6);
  Wml111.Checked:=nGetBit(pn,7);
  Wml121.Checked:=nGetBit(pn,8);
  Wml131.Checked:=nGetBit(pn,9);
  XSLT101.Checked:=nGetBit(pn,10);
  XSLT201.Checked:=nGetBit(pn,11);
  XHtml10TransitionalDEPRECATED1.Checked:=nGetBit(pn,16);
  XHtml10FramesetlDEPRECATED1.Checked:=nGetBit(pn,17);
  isEXT.Checked:=nGetBit(pn, 29);
  NoCLASS1.Checked:=nGetBit(pn,30);
  EMPTY1.Checked:=nGetBit(pn,31);
end;

procedure TForm1.Html401Strict1Click(Sender: TObject);
begin
  nSwitchBit(pn,TMenuItem(Sender).Tag);
  TreeView1.Selected:=pn;
end;

procedure TForm1.magicHtml5buttonClick(Sender: TObject);
const
  // http://dev.w3.org/html5/spec/elements.html#global-attributes as of 08-07-2010
  cHtml5Globals: array[0..66] of String = (
    'accesskey',
    'class',
    'contenteditable',
    'contextmenu',
    'dir',
    'draggable',
    'hidden',
    'id',
    'lang',
    'spellcheck',
    'style',
    'tabindex',
    'title',
    'onabort',
    'onblur',
    'oncanplay',
    'oncanplaythrough',
    'onchange',
    'onclick',
    'oncontextmenu',
    'ondblclick',
    'ondrag',
    'ondragend',
    'ondragenter',
    'ondragleave',
    'ondragover',
    'ondragstart',
    'ondrop',
    'ondurationchange',
    'onemptied',
    'onended',
    'onerror',
    'onfocus',
    'onformchange',
    'onforminput',
    'oninput',
    'oninvalid',
    'onkeydown',
    'onkeypress',
    'onkeyup',
    'onload',
    'onloadeddata',
    'onloadedmetadata',
    'onloadstart',
    'onmousedown',
    'onmousemove',
    'onmouseout',
    'onmouseover',
    'onmouseup',
    'onmousewheel',
    'onpause',
    'onplay',
    'onplaying',
    'onprogress',
    'onratechange',
    'onreadystatechange',
    'onscroll',
    'onseeked',
    'onseeking',
    'onselect',
    'onshow',
    'onstalled',
    'onsubmit',
    'onsuspend',
    'ontimeupdate',
    'onvolumechange',
    'onwaiting');

var
  i:Integer;
  n:TTreeNode;
  lIni: TIniFile32;
  tags:TStringList;
  attrs: TStringList;
  lTag: String;
  lAttr: String;

  procedure moveBitNext(x:Integer);
  begin
    if nGetBit(n,x) then
      nSetBit(n,x+1)
    else
      nClearBit(n,x+1);

    nClearBit(n,x);
  end;

begin
  magicHtml5button.Enabled:=false;

  if MessageDlg('insert html5 after html4?', mtConfirmation, mbYesNo, 0) = mrYes then
    for i:=0 to TreeView1.Items.Count-1 do
    begin
      n :=TreeView1.Items[i];
      moveBitNext(10);
      moveBitNext(9);
      moveBitNext(8);
      moveBitNext(7);
      moveBitNext(6);
      moveBitNext(5);
      moveBitNext(4);
      moveBitNext(3);
    end;

  ComboBox1.ItemIndex := 3{html5};
  if OpenDialog1.Execute then
  begin
    lIni := nil;
    tags := nil;
    attrs := nil;
    try
      lIni := TIniFile32.Create(OpenDialog1.FileName);
      tags := TStringList.Create;
      attrs := TStringList.Create;
      lIni.ReadSection('html5', tags);
      for lTag in tags do
      begin
        AddTag(lTag);
        attrs.DelimitedText := lIni.ReadString('html5', lTag, '');
        for lAttr in attrs do
          if not SameText(lAttr, 'globals') then
            AddAtrib(TreeView1.Selected, lAttr)
          else
          begin
            i := 0;
            while i < Length(cHtml5Globals) do
            begin
              AddAtrib(TreeView1.Selected, cHtml5Globals[i]);
              Inc(i);
            end;
          end;

      end;
    finally
      attrs.Free;
      tags.Free;
      lIni.Free;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin            
  if ParamCount=1 then
    Left:=Screen.Width;  
  ComboBox1.ItemIndex:=0;
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
    n.Data:=Pointer(Longword(n.Data) or (1 shl b));
end;

procedure TForm1.nSwitchBit(n: TTreeNode; b: Integer);
begin
  if n<>nil then
    n.Data:=Pointer(Longword(n.Data) xor (1 shl b));
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
  if CheckBox3.Checked then
    s:=Trim(s)
  else
    s:=LowerCase(Trim(s));
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

procedure TForm1.Button15Click(Sender: TObject);
var
  i:Integer;
begin
  for i:=0 to TreeView1.Items.Count-1 do
    if nGetBit(TreeView1.Items[i],1) then
      nSetBit(TreeView1.Items[i],2);
end;

procedure TForm1.Button16Click(Sender: TObject);
var
  i:Integer;
begin
  for i:=0 to Memo6.Lines.Count-1 do
    AddAtrib(TreeView1.Selected,Memo6.Lines[i]);
  TreeView1.SetFocus;
end;

procedure TForm1.Button17Click(Sender: TObject);
var
  i:Integer;
begin
  for i:=0 to TreeView1.Items.Count-1 do
    if nGetBit(TreeView1.Items[i],4) then
      nSetBit(TreeView1.Items[i],5);
end;

procedure TForm1.Button18Click(Sender: TObject);
begin       
  Button12Click(nil);
  form2.showmodal;
end;

procedure TForm1.Button19Click(Sender: TObject);
begin
  Memo5.Lines.AddStrings(Memo7.Lines);
end;

procedure TForm1.Button20Click(Sender: TObject);
begin
  Memo5.Lines.AddStrings(Memo8.Lines);
end;

procedure TForm1.Button21Click(Sender: TObject);
begin
  Memo5.Lines.AddStrings(Memo9.Lines);
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=vk_f1 then
  begin
    Button3.Click;
    memo5.SetFocus;
  end;
  if key=vk_f2 then
  begin
    Button9.Click;
    Edit1.SetFocus;
  end;
  if key=vk_f3 then
  begin
    Button5.Click;
    Edit1.SetFocus;
  end;
end;

procedure TForm1.Button22Click(Sender: TObject);
var
  t,a:TTreeNode;
  b:Boolean;
begin          
  Memo8.Clear;
  Memo9.Clear;
  t:=TreeView1.Items.GetFirstNode;
  while t<>nil do
  begin
    a:=t.getFirstChild;
    b:=false;
    while a<>nil do
    begin
      if a.Text='class' then
        b:=true;
      a:=a.GetNextChild(a);
    end;
    if b then
      Memo8.Lines.Add(t.Text)
    else
      Memo9.Lines.Add(t.Text);
    if CheckBox2.Checked then
      if not b then
        nSetBit(t,30)
      else
        nClearBit(t,30);
    t:=t.GetNextChild(t);
  end;
end;

procedure TForm1.Button24Click(Sender: TObject);
var
  r:TRegExpr;
begin
  r:=TRegExpr.Create;
  r.Expression:='<!ENTITY ([a-zA-Z0-9]+)';
  while Memo7.Lines.Count>0 do
  begin
    if r.Exec(Memo7.Lines[0]) then
    begin
      if ComboBox1.ItemIndex<3 then
      begin
        ComboBox1.ItemIndex:=0;
        Edit1.Text:=r.Match[1];
        Button3.Click;

        ComboBox1.ItemIndex:=1;
        Edit1.Text:=r.Match[1];
        Button3.Click;

        ComboBox1.ItemIndex:=2;
        Edit1.Text:=r.Match[1];
        Button3.Click;
      end else
      begin
        ComboBox1.ItemIndex:=3;
        Edit1.Text:=r.Match[1];
        Button3.Click;

        ComboBox1.ItemIndex:=4;
        Edit1.Text:=r.Match[1];
        Button3.Click;

        ComboBox1.ItemIndex:=5;
        Edit1.Text:=r.Match[1];
        Button3.Click;
      end;
    end;
    Memo7.Lines.Delete(0);
  end;
  r.Free;
end;

procedure TForm1.Button25Click(Sender: TObject);
var
  lSL: TStringList;
  lTagSpecial: String;
begin
  ComboBox1.ItemIndex := 3;
  if not OpenDialog1.Execute then
    Exit;

  lSL := TStringList.Create;
  try
    lSL.LoadFromFile(OpenDialog1.FileName);

    TreeView1.Items.BeginUpdate;
    try
      for lTagSpecial in lSL do
        AddTag(lTagSpecial);
    finally
      TreeView1.Items.EndUpdate;
    end;
  finally
    lSL.Free;
  end;
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

procedure TForm1.Button23Click(Sender: TObject);
var
  i,b1,b2:Integer;
begin
  b1 := StrToInt(Edit2.Text);
  b2 := StrToInt(Edit3.Text);
  case ComboBox2.ItemIndex of
  0:
    begin
      for i:=0 to TreeView1.Items.Count-1 do
        if nGetBit(TreeView1.Items[i],b1) then
          nClearBit(TreeView1.Items[i],b2);
    end;
  1:
    begin
      for i:=0 to TreeView1.Items.Count-1 do
        if nGetBit(TreeView1.Items[i],b1) then
          nSetBit(TreeView1.Items[i],b2);
    end;
  end;
end;

end.
