unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TLexKeys = Class
  public
    KeyIndex: Integer;
    Key: Integer;
  end;

  TForm2 = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    ht2: TCheckBox;
    ha2: TCheckBox;
    Label4: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ht3: TCheckBox;
    ha3: TCheckBox;
    ht1: TCheckBox;
    ha1: TCheckBox;
    xt1: TCheckBox;
    xa1: TCheckBox;
    xt2: TCheckBox;
    xa2: TCheckBox;
    xt3: TCheckBox;
    xa3: TCheckBox;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    SaveDialog1: TSaveDialog;
    Bevel3: TBevel;
    Button4: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    KeyList: TList;
    appdir:String;
    procedure ClearLists;
  public
    function KeyHash(ToHash: AnsiString): Integer;
    procedure GenerateInc(AKeys:TStringList; AResType, AResTrue, AResFalse, AFunc, AFuncUndef, AFuncTable,
      ACompFunc, AFileFunc, AFileFuncList, AFileFuncTable: String);
  end;

var
  Form2: TForm2;

implementation

uses Unit1;

{$R *.dfm}

function CompareKeys(Item1, Item2: Pointer): Integer;
begin
  if TLexKeys(Item1).Key < TLexKeys(Item2).Key then
    Result := -1
  else
    if TLexKeys(Item1).Key > TLexKeys(Item2).Key then
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

procedure TForm2.Button1Click(Sender: TObject);
var
  attr:TStringList;
  n:TTreeNode;
begin
  if not SaveDialog1.Execute then
    exit;
  attr:=TStringList.Create;
  n:=form1.TreeView1.Items.GetFirstNode;
  while n<>nil do
  begin
    if (ht1.Checked and Form1.nGetBit(n,0))or
       (ht2.Checked and Form1.nGetBit(n,1))or
       (ht3.Checked and Form1.nGetBit(n,2))or
       (xt1.Checked and Form1.nGetBit(n,3))or
       (xt2.Checked and Form1.nGetBit(n,4))or
       (xt3.Checked and Form1.nGetBit(n,5)) then
      attr.Add(n.Text);
    n:=n.GetNextChild(n);
  end;
  attr.Sort;
  attr.SaveToFile(SaveDialog1.FileName);
  attr.Free;
end;

procedure TForm2.Button3Click(Sender: TObject);
var
  attr:TStringList;
  n1,n2:TTreeNode;
begin
  if not SaveDialog1.Execute then
    exit;
  attr:=TStringList.Create;
  n1:=form1.TreeView1.Items.GetFirstNode;
  while n1<>nil do
  begin
    n2:=n1.getFirstChild;
    while n2<>nil do
    begin
      if ((ha1.Checked and Form1.nGetBit(n2,0))or
         (ha2.Checked and Form1.nGetBit(n2,1))or
         (ha3.Checked and Form1.nGetBit(n2,2))or
         (xa1.Checked and Form1.nGetBit(n2,3))or
         (xa2.Checked and Form1.nGetBit(n2,4))or
         (xa3.Checked and Form1.nGetBit(n2,5)))and
         (attr.IndexOf(n2.Text)=-1) then
        attr.Add(n2.Text);
      n2:=n2.GetNextChild(n2);
    end;
    n1:=n1.GetNextChild(n1);
  end;
  attr.Sort;
  attr.SaveToFile(SaveDialog1.FileName);
  attr.Free;
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  attr:TStringList;
  n1,n2:TTreeNode;
begin         
  if not SaveDialog1.Execute then
    exit;
  attr:=TStringList.Create;
  n1:=form1.TreeView1.Items.GetFirstNode;
  while n1<>nil do
  begin
    if (ht1.Checked and Form1.nGetBit(n1,0))or
       (ht2.Checked and Form1.nGetBit(n1,1))or
       (ht3.Checked and Form1.nGetBit(n1,2))or
       (xt1.Checked and Form1.nGetBit(n1,3))or
       (xt2.Checked and Form1.nGetBit(n1,4))or
       (xt3.Checked and Form1.nGetBit(n1,5)) then
    begin
      attr.Add(n1.Text);
      n2:=n1.getFirstChild;
      while n2<>nil do
      begin       
        if (ha1.Checked and Form1.nGetBit(n2,0))or
           (ha2.Checked and Form1.nGetBit(n2,1))or
           (ha3.Checked and Form1.nGetBit(n2,2))or
           (xa1.Checked and Form1.nGetBit(n2,3))or
           (xa2.Checked and Form1.nGetBit(n2,4))or
           (xa3.Checked and Form1.nGetBit(n2,5)) then
          attr.Add(#9+n2.Text);
        n2:=n2.GetNextChild(n2);
      end;
    end;
    n1:=n1.GetNextChild(n1);
  end;
  attr.SaveToFile(SaveDialog1.FileName);
  attr.Free;
end;

procedure TForm2.Button4Click(Sender: TObject);
var
  n:TTreeNode;
  i, t,x:Integer;
  s, s1,s2:TStringList; 
  ss1,ss2:String;
begin
  s:=TStringList.Create;
  s1:=TStringList.Create;
  s2:=TStringList.Create;
  n:=form1.TreeView1.Items.GetFirstNode;
  t:=0;    
  ss1:='    ';
  ss2:='    ';
  while n<>nil do
  begin
    s.Add(n.Text);
    //s1.Add(Format('    //%4.d',[t]));
    //s1.Add(Format('    ''%s'',',[n.Text]));
    if Length(ss1)+Length(n.Text)>80 then
    begin
      s1.Add(ss1);
      ss1:='    ';
    end;
    ss1:=Format('%s''%s'', ',[ss1,n.Text]);

    //s2.Add(Format('    //%4.d: %s',[t,n.Text]));
    //s2.Add(Format('    $%s,',[IntToHex(Longword(n.Data),8)]));
    if Length(ss2)+Length(IntToHex(Longword(n.Data),8))>80 then
    begin
      s2.Add(ss2);
      ss2:='    ';
    end;
    ss2:=Format('%s$%s, ',[ss2,IntToHex(Longword(n.Data),8)]);
    
    inc(t);
    n:=n.GetNextChild(n);
  end;
  if ss1<>'' then
    s1.Add(ss1);
  if ss2<>'' then
    s2.Add(ss2);
  s1[s1.Count-1]:=Copy(s1[s1.Count-1],1,Length(s1[s1.Count-1])-2);
  if Form1.CheckBox3.Checked then
    s1.Insert(0,format('  TSynWeb_Special:array[0..%d] of AnsiString=(',[t-1]))
  else
    s1.Insert(0,format('  TSynWeb_Tags:array[0..%d] of AnsiString=(',[t-1]));
  s1.Add('    );');

  x:=KeyHash(s[0]);
  for i:=1 to s.Count-1 do
    if KeyHash(s[i])>x then
      x:=KeyHash(s[i]);
  s1.Insert(0,'');
  if Form1.CheckBox3.Checked then
    s1.Insert(0,Format('  MLSpecialMaxKeyHash = %d;',[x]))
  else
  begin                   
    s1.Insert(0,Format('  MLTagMaxKeyHash = %d;',[x]));
    s1.Insert(0,Format('  MLTagID_Style = %d;',[s.IndexOf('style')]));
    s1.Insert(0,Format('  MLTagID_Script = %d;',[s.IndexOf('script')]));
  end;

  s2[s2.Count-1]:=Copy(s2[s2.Count-1],1,Length(s2[s2.Count-1])-2);
  if Form1.CheckBox3.Checked then
    s2.Insert(0,format('  TSynWeb_SpecialData:array[0..%d] of Longword=(',[t-1]))
  else
    s2.Insert(0,format('  TSynWeb_TagsData:array[0..%d] of Longword=(',[t-1]));
    
  s2.Add('    );');

  s1.Add('');
  s1.AddStrings(s2);
  if Form1.CheckBox3.Checked then
    s1.SaveToFile(appdir+'..\SynHighlighterWeb_Special.inc')
  else
    s1.SaveToFile(appdir+'..\SynHighlighterWeb_Tags.inc');
                  
  if Form1.CheckBox3.Checked then
    GenerateInc(s,'Boolean', 'True', 'False',
      'MLSpecialFunc', 'MLSpecialUndef',
      'FMLSpecialIdentFuncTable', 'MLSpecialKeyComp',
      appdir+'..\SynHighlighterWeb_SpecialFunc.inc',
      appdir+'..\SynHighlighterWeb_SpecialFuncList.inc',
      appdir+'..\SynHighlighterWeb_SpecialFuncTable.inc')
  else
    GenerateInc(s,'TSynWebTokenKind', 'stkMLTagName', 'stkMLTagNameUndef',
      'MLTagFunc', 'MLTagUndef',
      'FMLTagIdentFuncTable', 'MLTagKeyComp',
      appdir+'..\SynHighlighterWeb_TagsFunc.inc',
      appdir+'..\SynHighlighterWeb_TagsFuncList.inc',
      appdir+'..\SynHighlighterWeb_TagsFuncTable.inc');

  s2.Free;
  s1.Free;
  s.Free;
  Button5Click(nil);
  if ParamCount<>1 then
    ShowMessage('Done.')
  else
    Application.Terminate;
end;

procedure TForm2.Button5Click(Sender: TObject);
var
  sl:TStringList;
  n1,n2:TTreeNode;
  p:array of Longword;
  i,j,k,l:Longword;
  x:Integer;
  s:String;  
  s1,s2:TStringList;    
  ss1:String;

//  function si(C:Char):String;
//  begin
//    result:=Inttohex(Byte(c),2);
//  end;

begin     
  if Form1.CheckBox3.Checked then
    Exit;
  sl:=TStringList.Create;
  s1:=TStringList.Create;
  s2:=TStringList.Create;
  n1:=form1.TreeView1.Items.GetFirstNode;
  while n1<>nil do
  begin
    n2:=n1.getFirstChild;
    while n2<>nil do
    begin
      if sl.IndexOf(n2.Text)=-1 then
        sl.Add(n2.Text);
      n2:=n2.GetNextChild(n2);
    end;
    n1:=n1.GetNextChild(n1);
  end;
  sl.Sort;
  SetLength(p,sl.Count*3*9);

  n1:=form1.TreeView1.Items.GetFirstNode;
  while n1<>nil do
  begin
    n2:=n1.getFirstChild;
    while n2<>nil do
    begin
      x:=sl.IndexOf(n2.Text);
      j:=3*9*x+Longword(n1.Index div 32);
      l:=1 shl Longword(n1.Index mod 32);
      for i:=0 to 8 do
        if Form1.nGetBit(n1,i) and Form1.nGetBit(n2,i) then
          p[j+3*i]:=p[j+3*i] or l;
      n2:=n2.GetNextChild(n2);
    end;
    n1:=n1.GetNextChild(n1);
  end;

  ss1:='    ';
  for i:=0 to sl.Count-1 do
  begin
    //s1.Add(Format('    //%4.d',[i]));
    //s1.Add(Format('    ''%s'',',[sl[i]]));
    if Length(ss1)+Length(sl[i])>80 then
    begin
      s1.Add(ss1);
      ss1:='    ';
    end;
    ss1:=Format('%s''%s'', ',[ss1,sl[i]]);

    //s2.Add(Format('    //%4.d: %s',[i,sl[i]]));
    for j:=0 to 8 do
    begin
      if j=0 then
        s:='  (('
      else
        s:='   (';
      for k:=0 to 3-1 do
        if k=2 then
          s:=s+Format('$%s',[IntToHex(p[3*9*i+3*j+k],8)])
        else
          s:=s+Format('$%s, ',[IntToHex(p[3*9*i+3*j+k],8)]);
      if j=8 then
      begin
        if i=sl.Count-1 then
          s:=s+'))'
        else
          s:=s+')),';
      end else
        s:=s+'),';
      s2.Add(s);
    end;
  end;      
  if ss1<>'' then
    s1.Add(ss1);
  s1[s1.Count-1]:=Copy(s1[s1.Count-1],1,Length(s1[s1.Count-1])-2);
  s1.Insert(0,format('  TSynWeb_Attrs:array[0..%d] of AnsiString=(',[sl.Count-1]));
  s1.Add('    );');

  x:=KeyHash(sl[0]);
  for i:=1 to sl.Count-1 do
    if KeyHash(sl[i])>x then
      x:=KeyHash(sl[i]);
  s1.Insert(0,'');
  s1.Insert(0,Format('  MLAttrMaxKeyHash = %d;',[x]));
  s1.Insert(0,Format('  MLAttrID_Language = %d;',[sl.IndexOf('language')]));


  s2.Insert(0,format('  TSynWeb_AttrsData:array[0..%d] of array[0..%d] of array[0..%d] of Longword=(',[sl.Count-1,9-1,3-1]));
  s2.Add('    );');

  s1.Add('');
  s1.AddStrings(s2);
  s1.SaveToFile(appdir+'..\SynHighlighterWeb_Attrs.inc');

  SetLength(p,0);

  GenerateInc(sl,'TSynWebTokenKind', 'stkMLTagKey', 'stkMLTagKeyUndef',
    'MLAttrFunc', 'MLAttrUndef',
    'FMLAttrIdentFuncTable', 'MLAttrKeyComp',
    appdir+'..\SynHighlighterWeb_AttrsFunc.inc',
    appdir+'..\SynHighlighterWeb_AttrsFuncList.inc',
    appdir+'..\SynHighlighterWeb_AttrsFuncTable.inc');

  sl.Free;        
  s2.Free;
  s1.Free;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin                    
  if ParamCount=1 then
    Left:=Screen.Width;  
  appdir:=ExtractFilePath(Application.ExeName);
  KeyList:=TList.Create;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  KeyList.Free;
end;

type
  TSynWebHashTable = array[AnsiChar] of Longword;
const
{$I ../SynHighlighterWeb_Tables.inc}

function TForm2.KeyHash(ToHash: AnsiString): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(ToHash) do
    inc(Result, TSynWebIdentHashTable[ToHash[I]]);
end;

procedure TForm2.ClearLists;
begin
  while KeyList.Count>0 do
  begin
    Dispose(KeyList[0]);
    KeyList.Delete(0);
  end;
end;

procedure TForm2.GenerateInc(AKeys:TStringList; AResType, AResTrue, AResFalse, AFunc, AFuncUndef, AFuncTable,
  ACompFunc, AFileFunc, AFileFuncList, AFileFuncTable: String);
var
  i,ia:Integer;
  l:TLexKeys;   
  nf:TStringList;
  stemp:String;
begin
  AKeys.Sort;
  ClearLists;
  for i:=0 to AKeys.Count-1 do
  begin
    l:=TLexKeys.Create;
    l.KeyIndex:=i;
    l.Key:=KeyHash(AKeys[i]);
    KeyList.Add(l);
  end;
  KeyList.Sort(CompareKeys);

  nf:=TStringList.Create;
  nf.add(Format('    function %s: %s;',[AFuncUndef, AResType]));
  nf.add(Format('    function %s%d: %s;',[AFunc, TLexKeys(KeyList[0]).Key, AResType]));
  for i:=1 to KeyList.Count-1 do
    if (TLexKeys(KeyList[i-1]).Key <> TLexKeys(KeyList[i]).Key) then
        nf.add(Format('    function %s%d: %s;',[AFunc, TLexKeys(KeyList[i]).Key, AResType]));
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
    nf.add(Format('  %s[%d]:=%s%d;',[AFuncTable, TLexKeys(KeyList[I]).Key, AFunc, TLexKeys(KeyList[I]).Key]));
    inc(I);
  end;
  nf.SaveToFile(AFileFuncTable);
  nf.free;

  I := 0;
  nf:=TStringList.Create;
  nf.add(Format('function TSynWebEngine.%s: %s;',[AFuncUndef, AResType]));
  nf.add('begin');
  nf.add(Format('  Result:=%s;',[AResFalse]));
  nf.add('end;');
  nf.add('');
  while I < KeyList.Count do
  begin
    nf.add(Format('function TSynWebEngine.%s%d: %s;',[AFunc,TLexKeys(KeyList[I]).Key,AResType]));
    nf.add('begin');
    ia:=nf.Count;
//    nf.add('  if');
    if I < KeyList.Count - 1 then
      while TLexKeys(KeyList[I]).Key = TLexKeys(KeyList[I + 1]).Key do
      begin
        //nf.add(Format('      // %s',[AKeys[TLexKeys(KeyList[I]).KeyIndex]]));
        nf.add(Format('      %s(%d) or',[ACompFunc, TLexKeys(KeyList[I]).KeyIndex]));
        inc(I);
        if I >= KeyList.Count - 1 then break;
      end;
    //nf.add('      // ' + AKeys[TLexKeys(KeyList[I]).KeyIndex]);
    nf.add(Format('      %s(%d) then',[ACompFunc, TLexKeys(KeyList[I]).KeyIndex]));
    stemp:=nf[ia];
    stemp[3]:='i';
    stemp[4]:='f';
    nf[ia]:=stemp;
    nf.add(Format('    Result := %s',[AResTrue]));
    nf.add('  else');
    nf.add(Format('    Result := %s;',[AResFalse]));
    nf.add('end;');
    inc(I);
    if I < KeyList.Count then
      nf.add('');
  end;
  nf.SaveToFile(AFileFunc);
  nf.Free;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  if ParamCount=1 then
    Button4Click(nil);
end;

end.
