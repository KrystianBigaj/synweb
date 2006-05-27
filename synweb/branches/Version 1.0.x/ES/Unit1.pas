unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Menus, IniFiles32, clipbrd, ExtCtrls, RegExpr;

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
    keyword1: TMenuItem;
    const1: TMenuItem;
    function1: TMenuItem;
    var1: TMenuItem;
    Button7: TButton;
    CheckBox1: TCheckBox;
    Button6: TButton;
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
    procedure keyword1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    pn:TTreeNode;
    KeyList: TList;
    appdir:string;
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
  if ini.ReadInteger('Config','IsECMAScript',0)<>1 then
  begin
    ShowMessage('This is not ECMAScript-Config FILE!');
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
    f.Add(node.Text);
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
  f.Add('IsECMAScript=1');
  t:=0;
  n1:=TreeView1.Items.GetFirstNode;
  while n1<>nil do
  begin
    SaveNode(n1);
    n1:=n1.GetNextChild(n1);
    Inc(t);
  end;
  f.Insert(1,inttostr(t));
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

  keyword1.Checked:=nGetBit(pn, keyword1.Tag);
  const1.Checked:=nGetBit(pn, const1.Tag);
  function1.Checked:=nGetBit(pn, function1.Tag);
  var1.Checked:=nGetBit(pn, var1.Tag);

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

  nSetType(n,b);
end;

procedure TForm1.Button18Click(Sender: TObject);
var
  n:TTreeNode;
  i, x, t:Integer;
  s, s1,s2:TStringList;
  ss1,ss2:string;
begin
  Button12Click(nil);
  s:=TStringList.Create;
  s1:=TStringList.Create;
  s2:=TStringList.Create;
  s.Clear;
  n:=form1.TreeView1.Items.GetFirstNode;
  t:=0;
  ss1:='    ';
  ss2:='    ';
  while n<>nil do
  begin
    //if nGetBit(n, keyword1.Tag) or nGetBit(n, function1.Tag) then
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
  s1.Insert(0,format('  TSynWeb_EsKeywords:array[0..%d] of String=(',[t-1]));
  s1.Add('    );');

  x:=KeyHash(s[0]);
  for i:=1 to s.Count-1 do
    if KeyHash(s[i])>x then
      x:=KeyHash(s[i]);
  s1.Insert(0,'');

  s1.Insert(0,Format('  Es_KeywordsMaxKeyHash = %d;',[x]));

  s2[s2.Count-1]:=Copy(s2[s2.Count-1],1,Length(s2[s2.Count-1])-2);
  s2.Insert(0,format('  TSynWeb_EsKeywordsData:array[0..%d] of Longword=(',[t-1]));

  s2.Add('    );');

  s1.Add('');
  s1.AddStrings(s2);
  s1.SaveToFile('..\SynHighlighterWeb_EsKeywords.inc');

  GenerateInc(s,
      appdir+'..\SynHighlighterWeb_EsKeywordsFunc.inc',
      appdir+'..\SynHighlighterWeb_EsKeywordsFuncList.inc',
      appdir+'..\SynHighlighterWeb_EsKeywordsFuncTable.inc');

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
  if (f.Count<2) or (f[0]<>'IsECMAScript=1') then
  begin                                         
    f.Free;
    TreeView1.Items.EndUpdate;
    ShowMessage('This is not ECMAScript-Config FILE!');
    Exit;
  end;
  t:=StrToIntDef(f[1],0);
  ln:=2;
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

  procedure AddData(islastgp:boolean=false; islast:boolean=false);
  const nc:array[False..True] of String=('or', 'then');
  const nc2:array[False..True] of String=('if', '  ');
  const nc3:array[False..True] of String=('if', '  ');
  begin
    case Longword(TLexKeys(KeyList[I]).Data) and $0F of
    $01, $02:
      begin
        nf.add(tab+Format('%s  Es_KeywordComp(%d) %s',[nc2[al],TLexKeys(KeyList[I]).KeyIndex,nc[islastgp]]));
        al:=true;
        if islast then
          nf.Add(tab+'  Result := stkEsKeyword')
        else
          if islastgp then
          begin
            nf.Add(tab+'  Result := stkEsKeyword');
            nf.Add(tab+'else');
            tab:=tab+'  ';
            al:=false;
          end;
      end;
  {  $02:
      begin
        nf.add(tab+Format('if Php_ConstComp(%d) then',[TLexKeys(KeyList[I]).KeyIndex]));
        nf.Add(tab+'  Result := stkPhpConst');
      end;
    $04:
      begin
        nf.add(tab+Format('if Php_VariableComp(%d) then',[TLexKeys(KeyList[I]).KeyIndex]));
        nf.Add(tab+'  Result := stkPhpVariable');
      end;    }
   { $08:
      begin
        nf.add(tab+Format('%s  Es_FunctionComp(%d) %s',[nc2[al],TLexKeys(KeyList[I]).KeyIndex,nc[islastgp]]));
        if islast then
          nf.Add(tab+'  Result := stkEsFunction');
        al:=true;
      end; }
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
  nf.add('    function Es_KeywordIdent: TSynWebTokenKind;');
  nf.add(Format('    function Es_KeywordFunc%d: TSynWebTokenKind;',[TLexKeys(KeyList[0]).Key]));
  for i:=1 to KeyList.Count-1 do
    if (TLexKeys(KeyList[i-1]).Key <> TLexKeys(KeyList[i]).Key) then
        nf.add(Format('    function Es_KeywordFunc%d: TSynWebTokenKind;',[TLexKeys(KeyList[i]).Key]));
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
    nf.add(Format('  fEs_IdentFuncTable[%d]:=Es_KeywordFunc%d;',[TLexKeys(KeyList[I]).Key, TLexKeys(KeyList[I]).Key]));
    inc(I);
  end;
  nf.SaveToFile(AFileFuncTable);
  nf.free;

  I := 0;
  nf:=TStringList.Create;
  nf.add('function TSynWebEngine.Es_KeywordIdent: TSynWebTokenKind;');
  nf.add('begin');
  nf.add('  Result := stkEsIdentifier;');
  nf.add('end;');
  nf.add('');     
    mx:=0;
  while I < KeyList.Count do
  begin
    nf.add(Format('function TSynWebEngine.Es_KeywordFunc%d: TSynWebTokenKind;',[TLexKeys(KeyList[I]).Key]));
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
    nf.add(tab+'  Result := stkEsIdentifier;');
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

end.


