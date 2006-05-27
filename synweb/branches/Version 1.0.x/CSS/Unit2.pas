unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

var
  mKeyHashTable: array[#0..#255] of Integer;

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
    ht1: TCheckBox;
    ha1: TCheckBox;
    Bevel2: TBevel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    SaveDialog1: TSaveDialog;
    Bevel3: TBevel;
    CheckBox3: TCheckBox;
    Button6: TButton;
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
    appdir:string;
    procedure MakeHashTable;
    procedure ClearLists;
  public
    function KeyHash(ToHash: String): Integer;
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
    if CheckBox3.Checked or (ht1.Checked and Form1.nGetBit(n,0))or
       (ht2.Checked and Form1.nGetBit(n,1)) then
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
      if (CheckBox3.Checked or (ha1.Checked and Form1.nGetBit(n2,0))or
         (ha2.Checked and Form1.nGetBit(n2,1)))and
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
       (ht2.Checked and Form1.nGetBit(n1,1))then
    begin
      attr.Add(n1.Text);
      n2:=n1.getFirstChild;
      while n2<>nil do
      begin       
        if CheckBox3.Checked or (ha1.Checked and Form1.nGetBit(n2,0))or
           (ha2.Checked and Form1.nGetBit(n2,1)) then
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
  i, x, t:Integer;
  s, s1,s2:TStringList; 
  ss1,ss2:string;
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
  if form1.iss.Checked then
    s1.Insert(0,format('  TSynWeb_CssSpecial:array[0..%d] of String=(',[t-1]))
  else
    s1.Insert(0,format('  TSynWeb_CssProps:array[0..%d] of String=(',[t-1]));
  s1.Add('    );');

  x:=KeyHash(s[0]);
  for i:=1 to s.Count-1 do
    if KeyHash(s[i])>x then
      x:=KeyHash(s[i]);
  s1.Insert(0,'');

  if form1.iss.Checked then
  begin
    s1.Insert(0,Format('  Css_SpecialID_Auto = %d;',[s.IndexOf('auto')]));
    s1.Insert(0,Format('  Css_SpecialID_Url = %d;',[s.IndexOf('url')]));
    s1.Insert(0,Format('  Css_SpecialID_Lang = %d;',[s.IndexOf('lang')]));
    s1.Insert(0,Format('  Css_SpecialID_Important = %d;',[s.IndexOf('important')]));
    s1.Insert(0,Format('  Css_SpecialID_Charset = %d;',[s.IndexOf('charset')]));
    s1.Insert(0,Format('  Css_SpecialID_Page = %d;',[s.IndexOf('page')]));
    s1.Insert(0,Format('  Css_SpecialID_Media = %d;',[s.IndexOf('media')]));
    s1.Insert(0,Format('  Css_SpecialID_Import = %d;',[s.IndexOf('import')]));
    s1.Insert(0,Format('  Css_SpecialMaxKeyHash = %d;',[x]));
  end else
    s1.Insert(0,Format('  Css_PropMaxKeyHash = %d;',[x]));

  s2[s2.Count-1]:=Copy(s2[s2.Count-1],1,Length(s2[s2.Count-1])-2);
  if form1.iss.Checked then
    s2.Insert(0,format('  TSynWeb_CssSpecialData:array[0..%d] of Longword=(',[t-1]))
  else
    s2.Insert(0,format('  TSynWeb_CssPropsData:array[0..%d] of Longword=(',[t-1]));

  s2.Add('    );');

  s1.Add('');
  s1.AddStrings(s2);
  if form1.iss.Checked then
    s1.SaveToFile(appdir+'..\SynHighlighterWeb_CssSpecial.inc')
  else
    s1.SaveToFile(appdir+'..\SynHighlighterWeb_CssProps.inc');

  if form1.iss.Checked then   
    GenerateInc(s,'Boolean', 'True', 'False',
      'Css_SpecialFunc', 'Css_SpecialUndef',
      'fCss_SpecialIdentFuncTable', 'Css_SpecialKeyComp',
      appdir+'..\SynHighlighterWeb_CssSpecialFunc.inc',
      appdir+'..\SynHighlighterWeb_CssSpecialFuncList.inc',
      appdir+'..\SynHighlighterWeb_CssSpecialFuncTable.inc')
  else                                                     
    GenerateInc(s,'TSynWebTokenKind', 'stkCssProp', 'stkCssPropUndef',
      'Css_PropFunc', 'Css_PropUndef',
      'fCss_PropIdentFuncTable', 'Css_PropKeyComp',
      appdir+'..\SynHighlighterWeb_CssPropsFunc.inc',
      appdir+'..\SynHighlighterWeb_CssPropsFuncList.inc',
      appdir+'..\SynHighlighterWeb_CssPropsFuncTable.inc');

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
  p:array[0..300] of array[0..1] of array[0..3] of Longword;
  i,j,k,l:LongWord;
  x:Integer;
  s:String;    
  s1,s2:TStringList;  
  ss1,ss2:string;

  function si(C:Char):String;
  begin
    result:=Inttohex(Byte(c),2);
  end;

begin      
  if form1.iss.Checked then
    Exit;
  sl:=TStringList.Create;
  s1:=TStringList.Create;
  s2:=TStringList.Create;
  n1:=form1.TreeView1.Items.GetFirstNode;   
  ss1:='    ';
  ss2:='    ';
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

  n1:=form1.TreeView1.Items.GetFirstNode;
  while n1<>nil do
  begin
    n2:=n1.getFirstChild;
    while n2<>nil do
    begin
      x:=sl.IndexOf(n2.Text);
      j:=(n1.Index div 32);
      l:=1 shl (n1.Index mod 32);
      for i:=0 to 1 do
      begin
        if Form1.nGetBit(n2, 31) then
          p[x][i][3]:=p[x][i][3] or (1 shl 31); // isFUNCTION
        if Form1.nGetBit(n1,i) and Form1.nGetBit(n2,i) then
          p[x][i][j]:=p[x][i][j] or l;
      end;
      n2:=n2.GetNextChild(n2);
    end;
    n1:=n1.GetNextChild(n1);
  end;  

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
    for j:=0 to 1 do
    begin
      if j=0 then
        s:='  (('
      else
        s:='   (';
      for k:=0 to 4-1 do
      begin        
        s:=s+Format('$%s',[IntToHex(p[i][j][k],8)]);
        if k<>3 then
          s:=s+', ';
      end;
      if j=1 then
      begin
        if Integer(i)=sl.Count-1 then
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
  s1.Insert(0,format('  TSynWeb_CssVals:array[0..%d] of String=(',[sl.Count-1]));
  s1.Add('    );');

  x:=KeyHash(sl[0]);
  for i:=1 to sl.Count-1 do
    if KeyHash(sl[i])>x then
      x:=KeyHash(sl[i]);
  s1.Insert(0,'');
  s1.Insert(0,Format('  Css_ValID_Rect = %d;',[sl.IndexOf('rect')]));
  s1.Insert(0,Format('  Css_ValID_Url = %d;',[sl.IndexOf('url')]));
  s1.Insert(0,Format('  Css_ValID_Rgb = %d;',[sl.IndexOf('rgb')]));
  s1.Insert(0,Format('  Css_ValMaxKeyHash = %d;',[x]));

//  Form3.Memo2.Lines[Form3.Memo2.Lines.Count-1]:=Copy(Form3.Memo2.Lines[Form3.Memo2.Lines.Count-1],1,Length(Form3.Memo2.Lines[Form3.Memo2.Lines.Count-1])-1);
  s2.Insert(0,format('  TSynWeb_CssValsData:array[0..%d] of array[0..%d] of array[0..%d] of Longword=(',[sl.Count-1,2-1,4-1]));
  s2.Add('    );');

  s1.Add('');
  s1.AddStrings(s2);
  s1.SaveToFile(appdir+'..\SynHighlighterWeb_CssVals.inc');
    
  GenerateInc(sl,'TSynWebTokenKind', 'stkCssVal', 'stkCssValUndef',
    'Css_ValFunc', 'Css_ValUndef',
    'fCss_ValIdentFuncTable', 'Css_ValKeyComp',
    appdir+'..\SynHighlighterWeb_CssValsFunc.inc',
    appdir+'..\SynHighlighterWeb_CssValsFuncList.inc',
    appdir+'..\SynHighlighterWeb_CssValsFuncTable.inc');

  sl.Free;
  s2.Free;
  s1.Free;
 { sf.Free;}
end;

procedure TForm2.MakeHashTable;
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

function TForm2.KeyHash(ToHash: String): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(ToHash) do
    inc(Result, mKeyHashTable[ToHash[I]]);
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
  stemp:string;
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
    //nf.add('  if');
    ia:=nf.Count;
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

procedure TForm2.FormCreate(Sender: TObject);
begin
  if ParamCount=1 then
    Left:=Screen.Width;  
  appdir:=ExtractFilePath(Application.ExeName);
  MakeHashTable;
  KeyList:=TList.Create;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  KeyList.Free;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  if ParamCount=1 then
    Button4Click(nil);
end;

end.
