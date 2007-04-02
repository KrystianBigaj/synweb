
{*******************************************************}
{                                                       }
{       Delphi VCL Extension Library                    }
{                                                       }
{       Copyright (c) 1998 Stephan Schneider            }
{                                                       }
{*******************************************************}

unit IniFiles32;

{$B-}

interface

uses Classes;

type

{ TIniFile32 class }

  TIniFile32 = class(TObject)
  private
    FUpdate: Integer;
    FFileName: String;
    FFileBuffer: TStringList;
    FLastSectionIndex: Integer;
    function GetName(const Line: String): String;
    function GetValue(const Line, Name: String): String;
    function IsSection(const Line: String): Boolean;
    function GetSectionIndex(const Section: String): Integer;
  protected
  public                 
    procedure LoadFromFile;
    procedure SaveToFile;
    procedure BeginUpdate;
    procedure EndUpdate;
    constructor Create(const FileName: String);
    destructor Destroy; override;
    procedure DeleteKey(const Section, Ident: String);
    procedure EraseSection(const Section: String);
    function ReadBool(const Section, Ident: String; Default: Boolean): Boolean;
    function ReadInteger(const Section, Ident: String; Default: Longint): Longint;
    procedure ReadSection(const Section: String; Strings: TStrings);
    procedure ReadSections(Strings: TStrings);
    procedure ReadSectionValues(const Section: String; Strings: TStrings);
    function ReadString(const Section, Ident: String; Default: String): String;
    procedure WriteBool(const Section, Ident: String; Value: Boolean);
    procedure WriteInteger(const Section, Ident: String; Value: Longint);
    procedure WriteString(const Section, Ident: String; Value: String);
    property FileName: String read FFileName;
  end;

implementation

uses SysUtils;

resourcestring

{ Private Assertions }

  SStringsUnassignedError = 'Param Strings must be assigned';

const
  Brackets: array[0..1] of Char = ('[', ']');
  Separator: Char = '=';

function AnsiPos(const Substr, S: String): Integer;
begin
  Result := System.Pos(Substr, S);
end;

{ TIniFile32 }

constructor TIniFile32.Create(const FileName: String);
begin
  FFileName := FileName;
  FFileBuffer := TStringList.Create;
  if FileExists(FileName) then LoadFromFile;
end;

destructor TIniFile32.Destroy;
begin
  FFileBuffer.Free;
  Finalize(FFileName);
end;

function TIniFile32.GetName(const Line: String): String;
var
  I: Integer;
begin
  I := AnsiPos(Separator, Line);
  if I <> 0 then Result := Trim(System.Copy(Line, 1, I-1))
  else Result := EmptyStr;
end;

function TIniFile32.GetValue(const Line, Name: String): String;
var
  I, J: Integer;
begin
  Result := EmptyStr;
  if (Line <> EmptyStr) and (Name <> EmptyStr) then
  begin
    I := AnsiPos(Name, Line);
    J := AnsiPos(Separator, Line);
    if (I <> 0) and (J <> 0) and (J > I) then
      Result := Trim(System.Copy(Line, J+1, Maxint));
  end;
end;

function TIniFile32.IsSection(const Line: String): Boolean;
begin
  Result := False;
  if Line <> EmptyStr then
  begin
    //S := Trim(Line);
    if (Line[1] = Brackets[0]) and (Line[System.Length(Line)] = Brackets[1]) then
      Result := True;
  end;
end;

function TIniFile32.GetSectionIndex(const Section: String): Integer;
var
  s:String;
begin
  s:=Brackets[0] + Section + Brackets[1];
  if (FLastSectionIndex>=0)and(FLastSectionIndex<FFileBuffer.Count)and(FFileBuffer[FLastSectionIndex]=s) then
    Result:=FLastSectionIndex
  else
  begin
    Result := FFileBuffer.IndexOf(s);
    FLastSectionIndex:=Result;
  end;
end;

procedure TIniFile32.LoadFromFile;
begin
  FFileBuffer.LoadFromFile(FFileName);
end;

procedure TIniFile32.SaveToFile;
begin
  if FUpdate=0 then
    FFileBuffer.SaveToFile(FFileName);
end;

{ Read all Names of one Section }

procedure TIniFile32.ReadSection(const Section: String; Strings: TStrings);
var
  I: Integer;
  N: String;
begin
  Assert(Assigned(Strings), SStringsUnassignedError);
  Strings.BeginUpdate;
  try
    Strings.Clear;
    if FFileBuffer.Count > 0 then
    begin
      I := GetSectionIndex(Section);
      if I <> -1 then
      begin
        Inc(I);
        while (I < FFileBuffer.Count) and not IsSection(FFileBuffer[I]) do
        begin
          N := GetName(FFileBuffer[I]);
          if N <> EmptyStr then Strings.Add(N);
          Inc(I);
        end;
      end;
    end;
  finally
    Strings.EndUpdate;
  end;
end;

{ Read all Sections of the Ini-File }

procedure TIniFile32.ReadSections(Strings: TStrings);
var
  I: Integer;
  Section: String;
begin
  Assert(Assigned(Strings), SStringsUnassignedError);
  Strings.BeginUpdate;
  try
    Strings.Clear;
    if FFileBuffer.Count > 0 then
    begin
      I := 0;
      while (I < FFileBuffer.Count) do
      begin
        if IsSection(FFileBuffer[I]) then
        begin
          Section := Trim(FFileBuffer[I]);
          System.Delete(Section, 1, 1);
          System.Delete(Section, System.Length(Section), 1);
          Strings.Add(Trim(Section));
        end;
        Inc(I);
      end;
    end;
  finally
    Strings.EndUpdate;
  end;
end;

{ Reads a String-Value of Ident in one Section.
  The result is Default if
  o Section doesn't exists
  o Ident doesn't exists
  o Ident doesn't have any assigned value }

function TIniFile32.ReadString(const Section, Ident: String; Default: String): String;
var
  I,J: Integer;
  V,S: String;
begin
  Result := Default;
//  if FFileBuffer.Count > 0 then
  begin
    I := GetSectionIndex(Section);
    if I <> -1 then
    begin
      Inc(I);
      if I = FFileBuffer.Count then
        Exit;
      S:=FFileBuffer[I];
      while not IsSection(S) do
      begin
        J:=Length(Ident)+2;
        if Copy(s,1,J-1)=Ident+Separator then
        //if GetName(FFileBuffer[I]) = Ident then
        begin
          V := Copy(s,J,MaxInt); //GetValue(s, Ident);
          if V <> EmptyStr then Result := V;
          Exit;
        end;
        Inc(I);
        if I = FFileBuffer.Count then
          Exit;
        S:=FFileBuffer[I];
      end;
    end;
  end;
end;

{ Reads an Integer-Value of Ident in one Section }

function TIniFile32.ReadInteger(const Section, Ident: String; Default: Longint): Longint;
var
  IntStr: string;
begin
  IntStr := ReadString(Section, Ident, EmptyStr);
  // convert a Hex-Value
  if (Length(IntStr) > 2) and (IntStr[1] = '0') and ((IntStr[2] = 'X') or (IntStr[2] = 'x')) then
    IntStr := '$' + System.Copy(IntStr, 3, Maxint);
  Result := StrToIntDef(IntStr, Default);
end;

{ Reads a Bool-Value of Ident in one Section }

function TIniFile32.ReadBool(const Section, Ident: String; Default: Boolean): Boolean;
begin
  Result := ReadInteger(Section, Ident, Ord(Default)) <> 0;
end;

{ Reads all Names + Values of one Section }

procedure TIniFile32.ReadSectionValues(const Section: String; Strings: TStrings);
var
  N, V: String;
  I: Integer;
begin
  Assert(Assigned(Strings), SStringsUnassignedError);
  Strings.BeginUpdate;
  try
    Strings.Clear;
    if FFileBuffer.Count > 0 then
    begin
      I := GetSectionIndex(Section);
      if I <> -1 then
      begin
        Inc(I);
        while (I < FFileBuffer.Count) and not IsSection(FFileBuffer[I]) do
        begin
          N := GetName(FFileBuffer[I]);
          if N <> EmptyStr then
          begin
            V := GetValue(FFileBuffer[I], N);
            Strings.Add(N + Separator + V);
          end;  
          Inc(I);
        end;
      end;
    end;
  finally
    Strings.EndUpdate;
  end;
end;

{ Writes a String-Value for Ident in one Section.
  Note: If Section and/or Ident don't exist, they will be placed in the Ini-File }

procedure TIniFile32.WriteString(const Section, Ident: String; Value: String);
var
  I: Integer;
begin
  I := GetSectionIndex(Section);
  // Section exists
  if I <> -1 then
  begin
    Inc(I);
    while (I < FFileBuffer.Count) and not IsSection(FFileBuffer[I]) and
      (GetName(FFileBuffer[I]) <> Ident) do Inc(I);
    // End of File or Ident doesn't exists in the Section
    if (I >= FFileBuffer.Count) or IsSection(FFileBuffer[I]) then
    begin
      if Ident <> EmptyStr then FFileBuffer.Insert(I, Ident + Separator + Value);
    end
    // Ident does exists in the section
    else if Ident <> EmptyStr then FFileBuffer[I] := Ident + Separator + Value;
  end
  // Section doesn't exists, so add new [Section] with Ident=Value
  else
  begin
    FFileBuffer.Add(EmptyStr);
    FFileBuffer.Add(Brackets[0] + Section + Brackets[1]);
    if Ident <> EmptyStr then FFileBuffer.Add(Ident + Separator + Value);
  end;
  SaveToFile;
end;

{ Writes an Integer-Value for Ident in one Section }

procedure TIniFile32.WriteInteger(const Section, Ident: String; Value: Longint);
begin
  WriteString(Section, Ident, IntToStr(Value));
end;

{ Writes a Bool-Value for Ident in one Section }

procedure TIniFile32.WriteBool(const Section, Ident: String; Value: Boolean);
const
  Values: array[Boolean] of string = ('0', '1');
begin
  WriteString(Section, Ident, Values[Value]);
end;

{ Deletes the Value of Ident in one Section.
  Note: Only if Section and Ident exist, the Value of Ident will be set to NULL }

procedure TIniFile32.DeleteKey(const Section, Ident: String);
var
  I: Integer;
begin
  I := GetSectionIndex(Section);
  if I <> -1 then
  begin
    Inc(I);
    while (I < FFileBuffer.Count) and not IsSection(FFileBuffer[I]) and
      (GetName(FFileBuffer[I]) <> Ident) do Inc(I);
    // Ident does exists  
    if not (I >= FFileBuffer.Count) and not IsSection(FFileBuffer[I]) then
    begin
      FFileBuffer.Delete(I);
      SaveToFile;
    end;  
  end;
end;

{ Erases the whole Section from an Ini-File }

procedure TIniFile32.EraseSection(const Section: String);
var
  I: Integer;
begin
  I := GetSectionIndex(Section);
  if I <> -1 then
  begin
    // Delete Section-Header
    FFileBuffer.Delete(I);
    // Delete Section-Items
    while (I < FFileBuffer.Count) and not IsSection(FFileBuffer[I]) do
      FFileBuffer.Delete(I);
    if I > 0 then FFileBuffer.Insert(I, EmptyStr);  
    SaveToFile;
  end;
end;

procedure TIniFile32.BeginUpdate;
begin
  Inc(FUpdate);
end;

procedure TIniFile32.EndUpdate;
begin
  Dec(FUpdate);
  SaveToFile;
end;

end.
