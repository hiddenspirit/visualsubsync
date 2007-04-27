unit SSAParserUnit;

interface

uses Classes;

type
  TAbstractLineParser = class
    procedure ParseLine(line: string); virtual; abstract;
  end;

  TMemorizerParser = class(TAbstractLineParser)
  private
    memory : string;
  public
    procedure ParseLine(line: string); override;
  end;

  TFormatedParser = class(TAbstractLineParser)
  private
    keywordLst : TStringList;
    parsedLines : TList;
  public
    constructor Create;
    destructor Destroy; override;
    function ParseFormatLine(line: string; start : string) : Integer;
    function ParseFormatedLine(line: string; start : string) : Integer;
    function GetCount : Integer;
    function GetValueAsInteger(lineIndex : integer; valueKey : string) : Integer;
    function GetValueAsString(lineIndex : integer; valueKey : string) : string;
    function GetValueAsBoolean(lineIndex : integer; valueKey : string) : Boolean;
    function KeyExists(key : string) : Boolean;
  end;

  TStyleParser = class(TFormatedParser)
  public
    function IsSection(line : string) : Boolean;
    procedure ParseLine(line: string); override;
  end;

  TDialogueParser = class(TFormatedParser)
  public
    function IsSection(line : string) : Boolean;
    procedure ParseLine(line: string); override;
  end;

  TSSAParser = class
  private
    StyleParser : TStyleParser;
    DialogueParser : TDialogueParser;
    HeaderParser : TMemorizerParser;
    SubtitleFileHeader : string;
    IsUTF8 : Boolean;

    function GetParserForSection(line : string) : TAbstractLineParser;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Parse(Filename: string);
    function GetStylesCount : Integer;
    function GetDialoguesCount : Integer;
    function GetStyleValueAsString(lineIndex : integer; valueKey : string) : string;
    function GetStyleValueAsInteger(lineIndex : integer; valueKey : string) : Integer;
    function GetStyleValueAsBoolean(lineIndex : integer; valueKey : string) : Boolean;
    function GetDialogueValueAsString(lineIndex : integer; valueKey : string) : string;
    function GetDialogueValueAsInteger(lineIndex : integer; valueKey : string) : Integer;
    function GetHeader : string;
    function GetIsUTF8 : Boolean;
    function StyleKeyExists(key : string) : Boolean;
    function DialogueKeyExists(key : string) : Boolean;
  end;

implementation

uses SysUtils, StrUtils, MiscToolsUnit, Windows;

// =============================================================================

procedure TMemorizerParser.ParseLine(line: string);
begin
  if Length(memory) > 0 then
    memory := memory + #13#10 + line
  else
    memory := line;
end;

// =============================================================================

constructor TFormatedParser.Create;
begin
  keywordLst := TStringList.Create;
  keywordLst.CaseSensitive := False;
  parsedLines := TList.Create;
end;

destructor TFormatedParser.Destroy;
begin
  keywordLst.Free;
  while (parsedLines.Count > 0) do
  begin
    TStringList(parsedLines.Items[0]).Free;
    parsedLines.Delete(0);
  end;
end;

function TFormatedParser.GetCount : Integer;
begin
  Result := parsedLines.Count;
end;

function TFormatedParser.ParseFormatLine(line: string; start : string) : Integer;
var p : integer;
begin
  Delete(line, 1, Length(start));
  while True do
  begin
    p := Pos(',', line);
    if p = 0 then
    begin
      keywordLst.Add(Trim(line));
      Break;
    end;
    keywordLst.Add(Trim(Copy(line, 1, p - 1)));
    Delete(line,1,p);
  end;
  Result := keywordLst.Count;
end;

function TFormatedParser.ParseFormatedLine(line: string; start : string) : Integer;
var parsedLine : TStringList;
    p : integer;
begin
  parsedLine := TStringList.Create;
  Delete(line, 1, Length(start));
  while True do
  begin
    if (parsedLine.Count = keywordLst.Count - 1) then
    begin
      // Last element
      parsedLine.Add(Trim(line));
      Break;
    end;

    p := Pos(',', line);
    if p = 0 then
    begin
      parsedLine.Add(Trim(line));
      Break;
    end;

    parsedLine.Add(Trim(Copy(line, 1, p - 1)));
    Delete(line,1,p);
  end;
  parsedLines.Add(parsedLine);
  if (parsedLine.Count < keywordLst.Count) then
    Result := -1
  else
    Result := parsedLine.Count;
end;

function TFormatedParser.GetValueAsBoolean(lineIndex : integer; valueKey : string) : Boolean;
var intResult : Integer;
begin
  intResult := GetValueAsInteger(lineIndex, valueKey);
  if intResult = 0 then
    Result := False
  else
    Result := True;
end;

function TFormatedParser.GetValueAsInteger(lineIndex : integer; valueKey : string) : Integer;
var valueAsString : string;
begin
  valueAsString := GetValueAsString(lineIndex, valueKey);
  Result := StrToIntDef(valueAsString,0);
end;

function TFormatedParser.GetValueAsString(lineIndex : integer; valueKey : string) : string;
var keyIndex : Integer;
    lineValues : TStringList;
begin
  Result := '';
  keyIndex := keywordLst.IndexOf(valueKey);
  if (keyIndex <> -1) then
  begin
    if (lineIndex < parsedLines.Count) then
    begin
      lineValues := TStringList(parsedLines[lineIndex]);
      if (keyIndex < lineValues.Count) then
      begin
        Result := lineValues[keyIndex];
      end;
    end;
  end;
end;

function TFormatedParser.KeyExists(key : string) : Boolean;
begin
  Result := keywordLst.IndexOf(key) <> -1;
end;

// =============================================================================

function TStyleParser.IsSection(line : string) : Boolean;
begin
  Result := AnsiStartsText('[V4', Trim(line)) and AnsiEndsText('Styles]', Trim(line));
end;

procedure TStyleParser.ParseLine(line: string);
begin
  if AnsiStartsText('Format:', line) then
  begin
    ParseFormatLine(line, 'Format:');
  end
  else if AnsiStartsText('Style:', line) then
  begin
    ParseFormatedLine(line, 'Style:');
  end;
end;

// =============================================================================

function TDialogueParser.IsSection(line : string) : Boolean;
begin
  Result := AnsiStartsText('[Events]', Trim(line));
end;

procedure TDialogueParser.ParseLine(line: string);
begin
  if AnsiStartsText('Format:', line) then
  begin
    ParseFormatLine(line, 'Format:');
  end
  else if AnsiStartsText('Dialogue:', line) then
  begin
    ParseFormatedLine(line, 'Dialogue:');
  end;
end;

// =============================================================================

constructor TSSAParser.Create;
begin
  StyleParser := TStyleParser.Create;
  DialogueParser := TDialogueParser.Create;
  HeaderParser := TMemorizerParser.Create;
end;

destructor TSSAParser.Destroy;
begin
  StyleParser.Free;
  DialogueParser.Free;
  HeaderParser.Free;
  inherited Destroy;
end;

function TSSAParser.GetParserForSection(line : string) : TAbstractLineParser;
begin
  if StyleParser.IsSection(line) then
    Result := StyleParser
  else if DialogueParser.IsSection(line) then
    Result := DialogueParser
  else
    Result := HeaderParser;
end;

procedure TSSAParser.Parse(Filename: string);
var FS : TFileStream;
    BOM : array[0..3] of BYTE;
    line : string;
    sectionParser : TAbstractLineParser;
begin
  FS := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);

  // Check for UTF BOM
  ZeroMemory(@BOM[0], Length(BOM));
  FS.Read(BOM, 4);
  if (BOM[0] = $EF) and (BOM[1] = $BB) and (BOM[2] = $BF) then
  begin
    // UTF8
    IsUTF8 := True;
    FS.Seek(3, soFromBeginning);
  end
  else
  begin
    IsUTF8 := False;
    FS.Seek(0, soFromBeginning);
  end;

  // Read file
  sectionParser := HeaderParser;
  while not ReadLineStream(FS, line) do
  begin
    if AnsiStartsText('[', line) and AnsiEndsText(']', Trim(line))  then
    begin
      // New section
      sectionParser := GetParserForSection(line);
    end;
    sectionParser.ParseLine(line);
  end;
  FS.Free;
end;

function TSSAParser.GetStylesCount : Integer;
begin
  Result := StyleParser.GetCount;
end;

function TSSAParser.GetDialoguesCount : Integer;
begin
  Result := DialogueParser.GetCount;
end;

function TSSAParser.GetStyleValueAsString(lineIndex : integer; valueKey : string) : string;
begin
  Result := StyleParser.GetValueAsString(lineIndex, valueKey);
end;

function TSSAParser.GetStyleValueAsInteger(lineIndex : integer; valueKey : string) : Integer;
begin
  Result := StyleParser.GetValueAsInteger(lineIndex, valueKey);
end;

function TSSAParser.GetStyleValueAsBoolean(lineIndex : integer; valueKey : string) : Boolean;
begin
  Result := StyleParser.GetValueAsBoolean(lineIndex, valueKey);
end;

function TSSAParser.GetDialogueValueAsString(lineIndex : integer; valueKey : string) : string;
begin
  Result := DialogueParser.GetValueAsString(lineIndex, valueKey);
end;

function TSSAParser.GetDialogueValueAsInteger(lineIndex : integer; valueKey : string) : Integer;
begin
  Result := DialogueParser.GetValueAsInteger(lineIndex, valueKey);
end;

function TSSAParser.GetHeader : string;
begin
  Result := HeaderParser.memory;
end;

function TSSAParser.GetIsUTF8 : Boolean;
begin
  Result := IsUTF8;
end;

function TSSAParser.StyleKeyExists(key : string) : Boolean;
begin
  Result := StyleParser.KeyExists(key);
end;

function TSSAParser.DialogueKeyExists(key : string) : Boolean;
begin
  Result := DialogueParser.KeyExists(key);
end;

end.
// =============================================================================
