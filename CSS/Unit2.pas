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
    Button4: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

procedure TForm2.Button4Click(Sender: TObject);
var
  n:TTreeNode;
  i, x, t:Integer;
  s, s1,s2:TStringList; 
  ss1,ss2:AnsiString;
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
    s1.Insert(0,format('  TSynWeb_CssSpecial:array[0..%d] of AnsiString=(',[t-1]))
  else
    s1.Insert(0,format('  TSynWeb_CssProps:array[0..%d] of AnsiString=(',[t-1]));
  s1.Add('    );');

  x:=KeyHash(s[0]);
  for i:=1 to s.Count-1 do
    if KeyHash(s[i])>x then
      x:=KeyHash(s[i]);
  s1.Insert(0,'');

  if form1.iss.Checked then
  begin
    s1.Insert(0,Format('  CssSpecialID_Auto = %d;',[s.IndexOf('auto')]));
    s1.Insert(0,Format('  CssSpecialID_Url = %d;',[s.IndexOf('url')]));
//    s1.Insert(0,Format('  CssSpecialID_Lang = %d;',[s.IndexOf('lang')]));
    s1.Insert(0,Format('  CssSpecialID_Important = %d;',[s.IndexOf('important')]));
    s1.Insert(0,Format('  CssSpecialID_Charset = %d;',[s.IndexOf('charset')]));
    s1.Insert(0,Format('  CssSpecialID_Page = %d;',[s.IndexOf('page')]));
    s1.Insert(0,Format('  CssSpecialID_Media = %d;',[s.IndexOf('media')]));
    s1.Insert(0,Format('  CssSpecialID_Import = %d;',[s.IndexOf('import')]));
    s1.Insert(0,Format('  CssSpecialMaxKeyHash = %d;',[x]));
  end else
  begin
    s1.Insert(0,Format('  CssPropID_Font = %d;',[s.IndexOf('font')]));
    s1.Insert(0,Format('  CssPropMaxKeyHash = %d;',[x]));
  end;

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
      'CssSpecialFunc', 'CssSpecialUndef',
      'FCssSpecialIdentFuncTable', 'CssSpecialKeyComp',
      appdir+'..\SynHighlighterWeb_CssSpecialFunc.inc',
      appdir+'..\SynHighlighterWeb_CssSpecialFuncList.inc',
      appdir+'..\SynHighlighterWeb_CssSpecialFuncTable.inc')
  else                                                     
    GenerateInc(s,'TSynWebTokenKind', 'stkCssProp', 'stkCssPropUndef',
      'CssPropFunc', 'CssPropUndef',
      'FCssPropIdentFuncTable', 'CssPropKeyComp',
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
const
  cCssVersions = 3;
  cCssProperty32bitsCount = 10; // 10*32 = 320properties, in case of more, increse this to fit more
var
  sl:TStringList;
  n1,n2:TTreeNode;
  p:array[0..2000] of array[0..cCssVersions-1] of array[0..cCssProperty32bitsCount-1] of DWORD;
  i,j,k,l:Longword;
  x:Integer;
  s:String;    
  s1,s2:TStringList;  
  ss1,ss2:String;

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
      if n1.Index >= cCssProperty32bitsCount*32 then
        raise Exception.Create('Increse cCssProperty32bitsCount!!!');

      x:=sl.IndexOf(n2.Text);
      j:=(n1.Index div 32);
      l:=1 shl (n1.Index mod 32);
      for i:=0 to cCssVersions-1 do
      begin
        if Form1.nGetBit(n2, 31) then
          p[x][i][cCssProperty32bitsCount-1]:=
            p[x][i][cCssProperty32bitsCount-1] or DWORD(1 shl 31); // isFUNCTION;

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
    for j:=0 to cCssVersions-1 do
    begin
      if j=0 then
        s:='  (('
      else
        s:='   (';
      for k:=0 to cCssProperty32bitsCount-1 do
      begin        
        s:=s+Format('$%s',[IntToHex(p[i][j][k],8)]);
        if k<>cCssProperty32bitsCount-1 then
          s:=s+', ';
      end;
      if j=cCssVersions-1 then
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
  s1.Insert(0,format('  TSynWeb_CssVals:array[0..%d] of AnsiString=(',[sl.Count-1]));
  s1.Add('    );');

  x:=KeyHash(sl[0]);
  for i:=1 to sl.Count-1 do
    if KeyHash(sl[i])>x then
      x:=KeyHash(sl[i]);
  s1.Insert(0,'');
  s1.Insert(0,Format('  CssValID_Rect = %d;',[sl.IndexOf('rect')]));
  s1.Insert(0,Format('  CssValID_Url = %d;',[sl.IndexOf('url')]));
  s1.Insert(0,Format('  CssValID_Rgb = %d;',[sl.IndexOf('rgb')]));
  s1.Insert(0,Format('  CssValID_XX_Small = %d;',[sl.IndexOf('xx-small')]));
  s1.Insert(0,Format('  CssValID_X_Small = %d;',[sl.IndexOf('x-small')]));
  s1.Insert(0,Format('  CssValID_Small = %d;',[sl.IndexOf('small')]));
  s1.Insert(0,Format('  CssValID_Medium = %d;',[sl.IndexOf('medium')]));
  s1.Insert(0,Format('  CssValID_Large = %d;',[sl.IndexOf('large')]));
  s1.Insert(0,Format('  CssValID_X_Large = %d;',[sl.IndexOf('x-large')]));
  s1.Insert(0,Format('  CssValID_XX_Large = %d;',[sl.IndexOf('xx-large')]));
  s1.Insert(0,Format('  CssValID_Larger = %d;',[sl.IndexOf('larger')]));
  s1.Insert(0,Format('  CssValID_Smaller = %d;',[sl.IndexOf('smaller')]));
  s1.Insert(0,Format('  CssValMaxKeyHash = %d;',[x]));

//  Form3.Memo2.Lines[Form3.Memo2.Lines.Count-1]:=Copy(Form3.Memo2.Lines[Form3.Memo2.Lines.Count-1],1,Length(Form3.Memo2.Lines[Form3.Memo2.Lines.Count-1])-1);
  s2.Insert(0,format('  TSynWeb_CssValsData:array[0..%d] of array[0..%d] of array[0..%d] of Longword=(',
    [sl.Count-1,cCssVersions-1,cCssProperty32bitsCount-1]));
  s2.Add('    );');

  s1.Add('');
  s1.AddStrings(s2);
  s1.SaveToFile(appdir+'..\SynHighlighterWeb_CssVals.inc');
    
  GenerateInc(sl,'TSynWebTokenKind', 'stkCssVal', 'stkCssValUndef',
    'CssValFunc', 'CssValUndef',
    'FCssValIdentFuncTable', 'CssValKeyComp',
    appdir+'..\SynHighlighterWeb_CssValsFunc.inc',
    appdir+'..\SynHighlighterWeb_CssValsFuncList.inc',
    appdir+'..\SynHighlighterWeb_CssValsFuncTable.inc');

  sl.Free;
  s2.Free;
  s1.Free;
 { sf.Free;}
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
