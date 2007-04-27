unit StyleFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, TntExtCtrls, SSAParserUnit;

type
  TSSAAlignment = (
    alBottomLeft = 1, alBottomCenter = 2, alBottomRight = 3,
    alMiddleLeft = 4, alMiddleCenter = 5, alMiddleRight = 6,
    alTopLeft = 7, alTopCenter = 8, alTopRight = 9);

  TSSAStyle = class
    name : string;
    fontname : string;
    fontsize : integer;
    primaryColor : cardinal;
    secondaryColor : cardinal;
    outlineColor : cardinal;
    backColor : cardinal;
    bold : cardinal;
    italic : cardinal;
    underline : cardinal;
    strikeout : cardinal;
    scaleX : double;
    scaleY : double;
    spacing : double;
    angle : double;
    borderStyle : cardinal;
    outline : cardinal;
    shadow : cardinal;
    alignment : cardinal;
    marginL : cardinal;
    marginR : cardinal;
    marginV : cardinal;
    alphaLevel : cardinal;
    encoding : cardinal;

    constructor Create;
    procedure Assign(style : TSSAStyle);
    function getAsSSA : string;
    function getAsASS : string;
    function Equals(style : TSSAStyle) : Boolean;
  end;

  TStyleForm = class(TForm)
    lstStyles: TTntListBox;
    stFontPreview: TTntStaticText;
    bttFont: TTntButton;
    rgVAlignment: TTntRadioGroup;
    TntGroupBox2: TTntGroupBox;
    edHMargin: TTntEdit;
    rgHAlignment: TTntRadioGroup;
    TntGroupBox1: TTntGroupBox;
    edVMargin: TTntEdit;
    TntLabel1: TTntLabel;
    TntLabel2: TTntLabel;
    TntLabel3: TTntLabel;
    TntLabel4: TTntLabel;
    TntLabel5: TTntLabel;
    edOutline: TTntEdit;
    bttNew: TTntButton;
    bttDelete: TTntButton;
    bttClose: TTntButton;
    FontDialog1: TFontDialog;
    ColorDialog1: TColorDialog;
    pnlPrimaryColor: TTntStaticText;
    TntLabel6: TTntLabel;
    pnlSecondaryColor: TTntStaticText;
    TntLabel7: TTntLabel;
    pnlOutlineColor: TTntStaticText;
    pnlBackColor: TTntStaticText;
    TntLabel8: TTntLabel;
    TntLabel9: TTntLabel;
    bttCopy: TTntButton;
    TntLabel10: TTntLabel;
    edStyleName: TTntEdit;
    TntLabel11: TTntLabel;
    TntLabel12: TTntLabel;
    edShadow: TTntEdit;
    bttApply: TTntButton;
    bttReset: TTntButton;
    procedure bttFontClick(Sender: TObject);
    procedure bttPrimaryColorClick(Sender: TObject);
    procedure pnlColorClick(Sender: TObject);
    procedure bttCloseClick(Sender: TObject);
    procedure bttNewClick(Sender: TObject);
    procedure bttCopyClick(Sender: TObject);
    procedure bttDeleteClick(Sender: TObject);
    procedure lstStylesClick(Sender: TObject);
    procedure bttApplyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure checkChangedSender(Sender: TObject);
    procedure bttResetClick(Sender: TObject);
  private
    { Private declarations }
    FStylesChanged : Boolean;
    
    procedure ShowSelection;
    procedure Clear;
    procedure ClearList;
    procedure FormToData(var style : TSSAStyle);
    procedure DataToForm(style : TSSAStyle);
    procedure UpdateSelectionData;
    procedure EnableControls(Enable : Boolean);
    procedure CheckChange;
    function GetSelectedStyle : TSSAStyle;
  public
    { Public declarations }
    procedure LoadStylesFromParser(ssaParser : TSSAParser);
    function GetCount : Cardinal;
    function GetStyleAt(Index : Cardinal) : TSSAStyle;
    function HaveStylesChanged : Boolean;
    procedure PreSelect(StyleName : WideString);    
  end;

var
  StyleForm: TStyleForm;

implementation

{$R *.dfm}

uses MiscToolsUnit, Math, main;

// =============================================================================

constructor TSSAStyle.Create;
begin
  name := 'New Style';
  fontname := 'Arial';
  fontsize := 16;
  primaryColor := clYellow;
  secondaryColor := clBlack;
  outlineColor := clBlack;
  backColor := clBlack;
  bold := 0;
  italic := 0;
  underline := 0;
  strikeout := 0;
  scaleX := 100;
  scaleY := 100;
  spacing := 0;
  angle := 0;
  borderStyle :=0;
  outline := 1;
  shadow := 0;
  alignment := 2;
  marginL := 15;
  marginR := 15;
  marginV := 15;
  encoding := 0;
end;

// -----------------------------------------------------------------------------

procedure TSSAStyle.Assign(style : TSSAStyle);
begin
  name := style.name;
  fontname := style.fontname;
  fontsize := style.fontsize;
  primaryColor := style.primaryColor;
  secondaryColor := style.secondaryColor;
  outlineColor := style.outlineColor;
  backColor := style.backColor;
  bold := style.bold;
  italic := style.italic;
  underline := style.underline;
  strikeout := style.strikeout;
  scaleX := style.scaleX;
  scaleY := style.scaleY;
  spacing := style.spacing;
  angle := style.angle;
  borderStyle := style.borderStyle;
  outline := style.outline;
  shadow := style.shadow;
  alignment := style.alignment;
  marginL := style.marginL;
  marginR := style.marginR;
  marginV := style.marginV;
  alphaLevel := style.alphaLevel;
  encoding := style.encoding;
end;

function TSSAStyle.getAsSSA : string;
begin
  // Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, TertiaryColour, BackColour, Bold, Italic, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, AlphaLevel, Encoding
  // Style: Default,Arial,28,65535,255,16744448,-2147483640,-1,0,1,3,0,2,30,30,30,0,128
  Result := Format('Style: %s,%s,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d',
    [name, fontname, fontsize, primaryColor, secondaryColor, outlineColor, backColor,
      bold, italic, borderStyle, outline, shadow, alignment, marginL, marginR, marginV,
      alphaLevel, encoding]);
end;

function TSSAStyle.getAsASS : string;
begin
  // Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic,  Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding
  // Style: *Default,Arial,28,&H00FFFFFF,&H00400040,&H00C0C0C0,&H82C0C0C0,0,0,0,0,100,100,0,0,0,0,0,5,15,15,15,0
  Result := Format('Style: %s,%s,%d,%s,%s,%s,%s,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,',
    [name, fontname, fontsize, TColor2AssColorString(primaryColor),
    TColor2AssColorString(secondaryColor), TColor2AssColorString(outlineColor),
    TColor2AssColorString(backColor), bold, italic, underline, strikeout, scaleX, scaleY,
    spacing, angle, borderStyle, outline, shadow, alignment, marginL, marginR, marginV, encoding]);
end;

function TSSAStyle.Equals(style : TSSAStyle) : Boolean;
begin
  Result := (style.name = name)
    and (style.fontname = fontname)
    and (style.fontsize = fontsize)
    and (style.primaryColor = primaryColor)
    and (style.secondaryColor = secondaryColor)
    and (style.outlineColor = outlineColor)
    and (style.backColor = backColor)
    and (style.bold = bold)
    and (style.italic = italic)
    and (style.underline = underline)
    and (style.strikeout = strikeout)
    and (style.scaleX = scaleX)
    and (style.scaleY = scaleY)
    and (style.spacing = spacing)
    and (style.angle = angle)
    and (style.borderStyle = borderStyle)
    and (style.outline = outline)
    and (style.shadow = shadow)
    and (style.alignment = alignment)
    and (style.marginL = marginL)
    and (style.marginR = marginR)
    and (style.marginV = marginV)
    and (style.alphaLevel = alphaLevel)
    and (style.encoding = encoding);

end;

// =============================================================================

procedure TStyleForm.bttFontClick(Sender: TObject);
begin
  FontDialog1.Font.Assign(stFontPreview.Font);
  if FontDialog1.Execute then
  begin
    stFontPreview.Font.Assign(FontDialog1.Font);
    stFontPreview.Caption := Font2String(stFontPreview.Font);
    CheckChange;
  end;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.bttPrimaryColorClick(Sender: TObject);
begin
  ColorDialog1.Color := pnlPrimaryColor.Color;
  if ColorDialog1.Execute then
  begin
    pnlPrimaryColor.Color := ColorDialog1.Color;
  end;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.pnlColorClick(Sender: TObject);
var panel : TTntStaticText;
begin
  panel := Sender as TTntStaticText;
  ColorDialog1.Color := panel.Color;
  if ColorDialog1.Execute then
  begin
    panel.Color := ColorDialog1.Color;
    CheckChange;
  end;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.bttCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.bttNewClick(Sender: TObject);
var newStyle : TSSAStyle;
begin
  // Create a style with default settings
  newStyle := TSSAStyle.Create;
  lstStyles.AddItem(newStyle.name, newStyle);
  lstStyles.ItemIndex := lstStyles.Count-1;
  ShowSelection;
  EnableControls(True);
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.bttCopyClick(Sender: TObject);
var style, newStyle : TSSAStyle;
begin
  if (lstStyles.ItemIndex <> -1) then
  begin
    // Do a copy of the selcted style
    style := GetSelectedStyle;
    newStyle := TSSAStyle.Create;
    newStyle.Assign(style);
    newStyle.name := style.name + ' Copy';
    lstStyles.AddItem(newStyle.name, newStyle);
    lstStyles.ItemIndex := lstStyles.Count-1;
    ShowSelection;
  end;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.bttDeleteClick(Sender: TObject);
var style : TSSAStyle;
    index : integer;
begin
  if (lstStyles.ItemIndex <> -1) then
  begin
    index := lstStyles.ItemIndex;
    style := TSSAStyle(lstStyles.Items.Objects[index]);
    style.Free;
    lstStyles.DeleteSelected;
    // calculate new selection index
    if (index >= lstStyles.Count) then
      index := lstStyles.Count-1;
    if (index >= 0) then
    begin
      lstStyles.ItemIndex := index;
      ShowSelection;
    end
    else
    begin
      Clear;
      EnableControls(False);
    end;
  end
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.lstStylesClick(Sender: TObject);
begin
  ShowSelection;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.ShowSelection;
var style : TSSAStyle;
begin
  if (lstStyles.ItemIndex <> -1) then
  begin
    style := GetSelectedStyle;
    DataToForm(style);
    CheckChange;
  end;
end;

// -----------------------------------------------------------------------------

function TStyleForm.GetSelectedStyle : TSSAStyle;
begin
  Result := TSSAStyle(lstStyles.Items.Objects[lstStyles.ItemIndex]);
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.DataToForm(style : TSSAStyle);
var newFontStyles : TFontStyles;
begin
  edStyleName.Text := style.name;
  stFontPreview.Font.Name := style.fontname;
  stFontPreview.Font.Size := style.fontsize;
  newFontStyles := [];
  if (style.bold = 1) then
    newFontStyles := newFontStyles + [fsBold];
  if (style.italic = 1) then
    newFontStyles := newFontStyles + [fsItalic];
  if (style.underline = 1) then
    newFontStyles := newFontStyles + [fsUnderline];
  if (style.strikeout = 1) then
    newFontStyles := newFontStyles + [fsStrikeOut];
  stFontPreview.Font.Style := newFontStyles;
  stFontPreview.Font.Color := clBlack;
  stFontPreview.Caption := Font2String(stFontPreview.Font);

  // 7 8 9
  // 4 5 6
  // 1 2 3
  case style.alignment of
    7,4,1: rgHAlignment.ItemIndex := 0;
    8,5,2: rgHAlignment.ItemIndex := 1;
    9,6,3: rgHAlignment.ItemIndex := 2
  end;
  case style.alignment of
    7,8,9: rgVAlignment.ItemIndex := 0;
    4,5,6: rgVAlignment.ItemIndex := 1;
    1,2,3: rgVAlignment.ItemIndex := 2;
  end;
  edHMargin.Text := IntToStr(Max(style.marginR,style.marginL));
  edVMargin.Text := IntToStr(style.marginV);
  pnlPrimaryColor.Color := style.primaryColor;
  pnlSecondaryColor.Color := style.secondaryColor;
  pnlOutlineColor.Color := style.outlineColor;
  pnlBackColor.Color := style.backColor;
  edOutline.Text := IntToStr(style.outline);
  edShadow.Text := IntToStr(style.shadow);
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.Clear;
begin
  edStyleName.Text := '';
  stFontPreview.Caption := '';
  rgHAlignment.ItemIndex := -1;
  rgVAlignment.ItemIndex := -1;
  edHMargin.Text := '';
  edVMargin.Text := '';
  pnlPrimaryColor.Color := clBlack;
  pnlSecondaryColor.Color := clBlack;
  pnlOutlineColor.Color := clBlack;
  pnlBackColor.Color := clBlack;
  edOutline.Text := '';
  edShadow.Text := '';
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.ClearList;
var i : integer;
    style : TSSAStyle;
begin
  for i := 0 to lstStyles.Count-1 do
  begin
    style := TSSAStyle(lstStyles.Items.Objects[i]);
    style.Free;
  end;
  lstStyles.Clear;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.FormToData(var style : TSSAStyle);
begin
  style.name := edStyleName.Text;
  style.fontname := stFontPreview.Font.Name;
  style.fontsize := stFontPreview.Font.Size;
  if fsBold in stFontPreview.Font.Style then
    style.bold := 1
  else
    style.bold := 0;
  if fsItalic in stFontPreview.Font.Style then
    style.italic := 1
  else
    style.italic := 0;
  if fsUnderline in stFontPreview.Font.Style then
    style.underline := 1
  else
    style.underline := 0;
  if fsStrikeOut in stFontPreview.Font.Style then
    style.strikeout := 1
  else
    style.strikeout := 0;
      
  case rgHAlignment.ItemIndex of
    0: case rgVAlignment.ItemIndex of
          0: style.alignment := Ord(alTopLeft);
          1: style.alignment := Ord(alMiddleLeft);
          2: style.alignment := Ord(alBottomLeft);
       end;
    1: case rgVAlignment.ItemIndex of
          0: style.alignment := Ord(alTopCenter);
          1: style.alignment := Ord(alMiddleCenter);
          2: style.alignment := Ord(alBottomCenter);
       end;
    2: case rgVAlignment.ItemIndex of
          0: style.alignment := Ord(alTopRight);
          1: style.alignment := Ord(alMiddleRight);
          2: style.alignment := Ord(alBottomRight);
       end;
  end;
  style.marginR := StrToIntDef(edHMargin.Text,0);
  style.marginL := StrToIntDef(edHMargin.Text,0);
  style.marginV := StrToIntDef(edVMargin.Text,0);

  style.primaryColor := pnlPrimaryColor.Color;
  style.secondaryColor := pnlSecondaryColor.Color;
  style.outlineColor := pnlOutlineColor.Color;
  style.backColor := pnlBackColor.Color;
  style.outline := StrToIntDef(edOutline.Text,0);
  style.shadow := StrToIntDef(edShadow.Text,0);
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.UpdateSelectionData;
var style : TSSAStyle;
begin
  if (lstStyles.ItemIndex <> -1) then
  begin
    style := GetSelectedStyle;
    // If the style name has changed, check if doesn't exist yet
    if (style.name <> edStyleName.Text) then
    begin
      if (lstStyles.Items.IndexOf(edStyleName.Text) <> -1) then
      begin
        MessageBoxW(Handle, PWideChar(WideString('This style name already exists')),
          PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
        edStyleName.SetFocus;
        Exit;
      end
      else
      begin
        // Rename style in subtitles
        MainForm.RenameStyle(style.name, edStyleName.Text);
      end;
    end;
    FormToData(style);
    lstStyles.Items.Strings[lstStyles.ItemIndex] := style.name;
    FStylesChanged := True;
    CheckChange;
  end;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.bttApplyClick(Sender: TObject);
begin
  UpdateSelectionData;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.EnableControls(Enable : Boolean);
begin
  edStyleName.Enabled := Enable;
  bttFont.Enabled := Enable;
  rgHAlignment.Enabled := Enable;
  rgVAlignment.Enabled := Enable;
  edHMargin.Enabled := Enable;
  edVMargin.Enabled := Enable;
  pnlPrimaryColor.Enabled := Enable;
  pnlSecondaryColor.Enabled := Enable;
  pnlOutlineColor.Enabled := Enable;
  pnlBackColor.Enabled := Enable;
  edOutline.Enabled := Enable;
  edShadow.Enabled := Enable;
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.FormCreate(Sender: TObject);
begin
  EnableControls(False);
end;

// -----------------------------------------------------------------------------

procedure TStyleForm.LoadStylesFromParser(ssaParser : TSSAParser);
var i : integer;
    newStyle : TSSAStyle;
begin
  Clear;
  ClearList;
  for i := 0 to ssaParser.GetStylesCount-1 do
  begin
    newStyle := TSSAStyle.Create;
    newStyle.name := ssaParser.GetStyleValueAsString(i, 'Name');
    newStyle.fontname := ssaParser.GetStyleValueAsString(i, 'Fontname');
    newStyle.fontsize := ssaParser.GetStyleValueAsInteger(i, 'Fontsize');
    newStyle.primaryColor := AssColorString2TColor(ssaParser.GetStyleValueAsString(i, 'PrimaryColour'));
    newStyle.secondaryColor := AssColorString2TColor(ssaParser.GetStyleValueAsString(i, 'SecondaryColour'));
    if ssaParser.StyleKeyExists('TertiaryColour') then
      newStyle.outlineColor := AssColorString2TColor(ssaParser.GetStyleValueAsString(i, 'TertiaryColour'))
    else
      newStyle.outlineColor := AssColorString2TColor(ssaParser.GetStyleValueAsString(i, 'OutlineColour'));
    newStyle.backColor := AssColorString2TColor(ssaParser.GetStyleValueAsString(i, 'BackColour'));
    newStyle.bold := ssaParser.GetStyleValueAsInteger(i, 'Bold');
    newStyle.italic := ssaParser.GetStyleValueAsInteger(i, 'Italic');
    newStyle.underline := ssaParser.GetStyleValueAsInteger(i, 'Underline');
    newStyle.strikeout := ssaParser.GetStyleValueAsInteger(i, 'Strikeout');
    newStyle.scaleX := ssaParser.GetStyleValueAsInteger(i, 'ScaleX');
    newStyle.scaleY := ssaParser.GetStyleValueAsInteger(i, 'ScaleY');
    newStyle.spacing := ssaParser.GetStyleValueAsInteger(i, 'Spacing');
    newStyle.angle := ssaParser.GetStyleValueAsInteger(i, 'Angle');
    newStyle.borderStyle := ssaParser.GetStyleValueAsInteger(i, 'BorderStyle');
    newStyle.outline := ssaParser.GetStyleValueAsInteger(i, 'Outline');
    newStyle.shadow := ssaParser.GetStyleValueAsInteger(i, 'Shadow');
    newStyle.alignment := ssaParser.GetStyleValueAsInteger(i, 'Alignment');
    newStyle.marginL := ssaParser.GetStyleValueAsInteger(i, 'MarginL');
    newStyle.marginR := ssaParser.GetStyleValueAsInteger(i, 'MarginR');
    newStyle.marginV := ssaParser.GetStyleValueAsInteger(i, 'MarginV');
    newStyle.alphaLevel := ssaParser.GetStyleValueAsInteger(i, 'AlphaLevel');
    newStyle.encoding := ssaParser.GetStyleValueAsInteger(i, 'Encoding');
    lstStyles.AddItem(newStyle.name, newStyle);
  end;
end;

function TStyleForm.GetCount : Cardinal;
begin
  Result := lstStyles.Count;
end;

function TStyleForm.GetStyleAt(Index : Cardinal) : TSSAStyle;
begin
  Result := TSSAStyle(lstStyles.Items.Objects[Index]);
end;

procedure TStyleForm.FormShow(Sender: TObject);
begin
  if (lstStyles.Count > 0) then
  begin
    EnableControls(True);
    ShowSelection;
  end;
  FStylesChanged := False;
  ShowSelection;
end;

procedure TStyleForm.CheckChange;
var MaybeChangedStyle : TSSAStyle;
begin
  MaybeChangedStyle := TSSAStyle.Create;
  MaybeChangedStyle.Assign(GetSelectedStyle);
  FormToData(maybeChangedStyle);
  bttApply.Enabled := not MaybeChangedStyle.Equals(GetSelectedStyle);
  bttReset.Enabled := bttApply.Enabled;
end;

procedure TStyleForm.checkChangedSender(Sender: TObject);
begin
  CheckChange;
end;

procedure TStyleForm.bttResetClick(Sender: TObject);
begin
  ShowSelection;
end;

function TStyleForm.HaveStylesChanged : Boolean;
begin
  Result := FStylesChanged;
end;

procedure TStyleForm.PreSelect(StyleName : WideString);
var StyleIndex : Integer;
begin
  StyleIndex := lstStyles.Items.IndexOf(StyleName);
  if (StyleIndex <> -1) then
  begin
    lstStyles.ItemIndex := StyleIndex;
  end
  else if (lstStyles.ItemIndex = -1) and (lstStyles.Count > 0) then
  begin
    lstStyles.ItemIndex := 0;
  end;
end;

end.
// =============================================================================
