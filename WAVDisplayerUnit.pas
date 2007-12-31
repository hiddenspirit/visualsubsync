// -----------------------------------------------------------------------------
//  VisualSubSync
// -----------------------------------------------------------------------------
//  Copyright (C) 2003 Christophe Paris
// -----------------------------------------------------------------------------
//  This Program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2, or (at your option)
//  any later version.
//
//  This Program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with GNU Make; see the file COPYING.  If not, write to
//  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
//  http://www.gnu.org/copyleft/gpl.html
// -----------------------------------------------------------------------------

{
  Updates:

  13 aug 2004 (committed by Kaiousama)

  + Two SelectionModes : Cooledit-like (default) and SSA-like
  + MouseWheel performs Time scrolling
  + Shift_Wheel performs Vertical zoom the waveform
  + Ctrl_Wheel performs Horizontal zoom the waveform
  * PlayRange2() overloaded with PlayRange
  * ZoomRange2() overloaded with ZoomRange
}

unit WAVDisplayerUnit;

interface

uses Windows, Messages, Classes, Controls, Graphics, WAVFileUnit,
  Math, ExtCtrls, Renderer, MiniScrollBarUnit, MMSystem, Types;

type
  TRange = class
  public
    StartTime : Integer;
    StopTime : Integer;
    SubTime : TIntegerDynArray;

    procedure Assign(const Range : TRange); virtual;
    procedure AddSubTime(const NewTime : Integer);
    procedure ClearSubTimes;
    procedure DelSubTimeAt(const Idx : Integer);
    // !!! Indexes used below are index of the interval between markers !!!
    function  GetSubTimeRange(const PosMS : Integer; Range : TRange) : Integer;
    procedure GetSubTimeRangeAt(const Idx : Integer; Range : TRange);
    function UpdateSubTimeFromText(const Text : WideString) : Boolean;
    function SetTimes(Start,Stop : Integer) : Boolean; // Return true if changed
    function ToString : string;
  end;

  TRangeFactory = class
    function CreateRange : TRange; virtual;
    function CreateRangeSS(Start, Stop : Integer) : TRange; virtual;
  end;

  TRangeList = class
  private
    FList : TList;
    FSearchStartAt : Integer;
    FSearchIdx : Integer;
    FSearchExpandBy : Integer;

    function GetItem(const Index: Integer) : TRange;
    function GetCount : Integer;

  public
    constructor Create;
    destructor Destroy; override;
    function Add(const Range : TRange) : Integer;
    function AddAndReturnSibling(const Range : TRange) : TRange;
    procedure AddAtEnd(const Range : TRange);
    procedure FullSort;
    function FindInsertPos(const Range : TRange) : Integer; overload;
    function FindInsertPos(const Start, Stop : Integer) : Integer; overload;
    function GetRangeIdxAt(const PosMs : Integer) : Integer;
    procedure Delete(const Index : Integer);
    function IndexOf(const Range : TRange) : Integer;
    procedure Move(const CurIndex, NewIndex: Integer);
    procedure Clear;
    function FindFirstRangeAt(const PosMs : Integer; const ExpandBy : Integer = 0) : TRange;
    function FindNextRange : TRange;
    property Count : Integer read GetCount;
    property Ranges[const Index: Integer]: TRange read GetItem; default;
  end;

  // ----------

  TPeak = record
    Max : SmallInt;
    Min : Smallint;
  end;
  PPeak = ^TPeak;

  // ----------

  TSelectionMode = (smCoolEdit, smSSA);
  TMouseWheelModifier = (mwmShift = Ord(ssShift), mwmAlt = Ord(ssAlt), mwmCtrl = Ord(ssCtrl), mwmNone);

  TUpdateViewFlag = (uvfCursor, uvfSelection, uvfRange, uvfPosition, uvfPageSize, uvfPlayCursor);
  TUpdateViewFlags = set of TUpdateViewFlag;

  TPeakFileCreationEventType = (pfcevtStart, pfcevtProgress, pfcevtStop);
  TPeakFileCreationEvent = procedure (Sender: TObject;
    EventType : TPeakFileCreationEventType; Param : Integer) of object;

  TDynamicEditMode = (demNone, demStart, demStop, demKaraoke);

  TKaraokeTimeChangedEvent = procedure (Sender: TObject; Range : TRange; SubTimeIndex, OldTime : Integer) of object;
  TSelectedKaraokeRangeEvent = procedure (Sender: TObject; Range : TRange) of object;
  TSelectedRangeEvent = procedure (Sender: TObject; Range : TRange; IsDynamic : Boolean) of object;
  TSubtitleChangedEvent = procedure (Sender: TObject; OldStart, OldStop : Integer; NeedSort : Boolean) of object;
  TCustomDrawRange = procedure (Sender: TObject; ACanvas: TCanvas; Range : TRange; Rect : TRect) of object;

  TMinBlankInfoPart = (mbipStart, mbipStop, mbipInvalid);
  TMinBlankInfo = class(TObject)
  private
    Range : TRange;
    Part : TMinBlankInfoPart;
  public
    constructor Create;
    function Exists : Boolean;
    function SetInfo(NewRange : TRange; NewPart : TMinBlankInfoPart) : Boolean;
    function GetStart(MinBlankTime : Integer) : Integer;
    function GetStop(MinBlankTime : Integer) : Integer;
    function GetSnappingPoint(MinBlankTime : Integer) : Integer;
  end;

  TWAVDisplayer = class(TCustomControl)
  private
    { Private declarations }
    FPeakTab : array of TPeak;
    FPeakTabSize : Cardinal;
    FSamplesPerPeak : Cardinal;

    FOffscreen : TBitmap;
    FOffscreenWAV : TBitmap;

    FRangeList : TRangeList;
    FCursorMs : Integer;
    FPlayCursorMs : Integer;
    FOldPlayCursorMs : Integer;
    FAutoScrolling : Boolean;
    FPositionMs : Integer;
    FPageSizeMs : Integer;
    FLengthMs : Integer;
    FVerticalScaling : Integer; // 1..800%

    FOldPositionMs : Integer; // Used to optimize drawing
    FOldPageSizeMs : Integer;    

    FSelection : TRange;
    FSelectionOrigin : Integer;
    FSelMode : TSelectionMode;
    FScrollOrigin : Integer;
    FSelectedRange : TRange;
    FNeedToSortSelectedSub : Boolean;

    FScrollBar : TMiniScrollBar;

    FOnCursorChange : TNotifyEvent;
    FOnPlayCursorChange : TNotifyEvent;
    FOnSelectionChange : TNotifyEvent;
    FOnViewChange : TNotifyEvent;
    FOnSelectedRange : TSelectedRangeEvent;
    FOnSelectedRangeChange : TNotifyEvent;
    FOnSelectedRangeChanged : TSubtitleChangedEvent;
    FOnPeakFileCreation : TPeakFileCreationEvent;
    FOnAutoScrollChange : TNotifyEvent;
    FOnStartPlaying : TNotifyEvent;
    FOnStopPlaying : TNotifyEvent;
    FOnKaraokeChange : TNotifyEvent;
    FOnKaraokeChanged : TKaraokeTimeChangedEvent;
    FOnSelectedKaraokeRange : TSelectedKaraokeRangeEvent;
    FOnCustomDrawRange: TCustomDrawRange;

    FRenderer : TRenderer;
    FUpdateCursorTimer : TTimer;
    FIsPlaying : Boolean;
    FPeakDataLoaded : Boolean;
    FWavFormat : TWaveFormatEx;

    FWheelTimeScroll, FWheelVZoom, FWheelHZoom : TMouseWheelModifier;
    FMouseIsDown : Boolean; // this fix WAVDisplay refresh bug when double clicking title bar

    FDisplayRuler : Boolean;
    FDisplayRulerHeight : Integer;

    FDynamicEditMode : TDynamicEditMode;
    FDynamicSelRange : TRange;
    FDynamicSelRangeOld : TRange;
    FDynamicEditTime : Integer;

    FSelectedKaraokeIndex : Integer; // Index of the range, 0 -> [StartTime,SubTime[0]]
    FSelectedKaraokeRange : TRange;

    FEnableMouseAntiOverlapping : Boolean;
    FMinSelTime : Integer;
    FMaxSelTime : Integer;

    FRangeOldStart, FRangeOldStop : Integer; // Range time before dynamic modificaion
    FOldKaraokeSubTime : Integer;

    FSceneChangeList : TIntegerDynArray;
    FSceneChangeStartOffset, FSceneChangeStopOffset, FSceneChangeFilterOffset : Integer;
    FSceneChangeEnabled : Boolean;
    FMinimumBlank : Integer; // Minimum blank between subtitle in ms
    FMinBlankInfo1, FMinBlankInfo2 : TMinBlankInfo;

    procedure DrawAlphaRect(ACanvas : TCanvas; t1, t2 : Integer);
    procedure PaintWavOnCanvas(ACanvas : TCanvas; TryOptimize : Boolean);
    procedure PaintOnCanvas(ACanvas : TCanvas);
    procedure PaintRulerOnCanvas(ACanvas : TCanvas);

    procedure CreatePeakTab(WAVFile : TWAVFile);
    function NormalizePeakTab(NormFactor : Double) : Boolean;
    function PixelToTime(const Pixel : Integer) : Integer;
    function TimeToPixel(const Time : Integer) : Integer;
    procedure SetScrollBar(Value : TMiniScrollBar);
    procedure OnScrollBarChange(Sender: TObject);
    function GetPositionMs : Integer;
    procedure InternalOnStopPlaying(Sender : TObject);
    procedure OnUpdateCursor(Sender : TObject);

    procedure SetSelectedRange(Value : TRange);
    procedure SetSelectedRangeEx(Value : TRange; UpdateDisplay : Boolean = True);
    procedure SetPlayCursorPos(NewPos : Integer);
    procedure SetAutoScroll(const Value : Boolean);
    procedure SetVerticalScaling(Value : Integer);
    function CheckSubtitleForDynamicSelection(Range : TRange;
      CursorPosMs, RangeSelWindow : Integer; X,Y : Integer) : Boolean;

    procedure MouseDownSSA(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; var UpdateFlags : TUpdateViewFlags);
    procedure MouseDownCoolEdit(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; var UpdateFlags : TUpdateViewFlags);

    function GetWavCanvasHeight : Integer;
    function IsFilteredSceneChange(SceneChange : Integer) : Boolean;

    function SetMinBlankOnIdx(Idx : Integer) : Boolean;
    function SetMinBlankAt(TimeMs : Integer) : Boolean;

    function FindSnappingPoint(PosMs : Integer) : Integer;
        
  protected
    procedure DblClick; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure WM_EraseBKGND(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure Paint; override;
    procedure Resize; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadWAV(filename : WideString) : Boolean;
    procedure Close;
    function GetCursorPos : Integer;
    procedure SetCursorPos(NewPos : Integer);
    function GetPlayCursorPos : Integer;
    function AddRange(NewRange : TRange; UpdateDisplay : Boolean = True) : Integer;
    procedure ZoomRange(const Range : TRange); overload;
    procedure ZoomRange(const Start, Stop : Integer); overload;
    procedure ZoomCenteredOn(const Center, PageSize : Integer);
    procedure ZoomAll;
    procedure ZoomIn;
    procedure ZoomOut;
    procedure ZoomAndSelectRange(const Range : TRange);
    procedure SetPageSizeMs(NewPageSize : Integer);
    procedure SetPositionMs(NewPosition : Integer);
    procedure DeleteRangeAt(const Pos : Integer; const UpdateDisplay : Boolean = True);
    procedure DeleteRangeAtIdx(const Idx : Integer; const UpdateDisplay : Boolean = True);

    procedure PlayRange(const Range : TRange; const Loop : Boolean = False); overload;
    procedure PlayRange(const Start, Stop : Integer; const Loop : Boolean = False); overload;
    procedure Pause;
    procedure UpdatePlayRange(const Start, Stop : Integer);
    procedure Stop;
    function SelectionIsEmpty : Boolean;
    procedure UpdateView(UpdateViewFlags : TUpdateViewFlags);
    function RedirectedMousewheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint) : Boolean;
    procedure SetRenderer(Renderer : TRenderer);
    procedure ClearSelection;
    function GetWAVAverageBytePerSecond : Integer;

    procedure SelectNextKaraoke;
    procedure SelectPreviousKaraoke;
    procedure SelecteKaraoke(Range : TRange; Index : Integer);

    procedure Scroll(ViewPercent : Integer);
    procedure SetSceneChangeList(SceneChangeList : TIntegerDynArray);

    property RangeList : TRangeList read FRangeList;
    property SelectedRange : TRange read FSelectedRange write SetSelectedRange;
    property Selection : TRange read FSelection;
    property KaraokeSelectedIndex : Integer read FSelectedKaraokeIndex;
    property KaraokeSelectedRange : TRange read FSelectedKaraokeRange;

  published
    { Published declarations }
    property OnCursorChange : TNotifyEvent read FOnCursorChange write FOnCursorChange;
    property OnPlayCursorChange : TNotifyEvent read FOnPlayCursorChange write FOnPlayCursorChange;
    property OnSelectionChange : TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnViewChange : TNotifyEvent read FOnViewChange write FOnViewChange;
    property OnSelectedRange : TSelectedRangeEvent read FOnSelectedRange write FOnSelectedRange;
    property OnSelectedRangeChange : TNotifyEvent read FOnSelectedRangeChange write FOnSelectedRangeChange;
    property OnSelectedRangeChanged : TSubtitleChangedEvent read FOnSelectedRangeChanged write FOnSelectedRangeChanged;
    property OnPeakFileCreation : TPeakFileCreationEvent read FOnPeakFileCreation write FOnPeakFileCreation;
    property OnAutoScrollChange : TNotifyEvent read FOnAutoScrollChange write FOnAutoScrollChange;
    property OnStartPlaying : TNotifyEvent read FOnStartPlaying write FOnStartPlaying;
    property OnStopPlaying : TNotifyEvent read FOnStopPlaying write FOnStopPlaying;
    property OnKaraokeChange : TNotifyEvent read FOnKaraokeChange write FOnKaraokeChange;
    property OnKaraokeChanged : TKaraokeTimeChangedEvent read FOnKaraokeChanged write FOnKaraokeChanged;
    property OnSelectedKaraokeRange : TSelectedKaraokeRangeEvent read FOnSelectedKaraokeRange write FOnSelectedKaraokeRange;
    property OnCustomDrawRange : TCustomDrawRange read FOnCustomDrawRange write FOnCustomDrawRange;

    property Align;
    property Anchors;
    property AutoScrolling : Boolean read FAutoScrolling write SetAutoScroll;
    property IsPlaying : Boolean read FIsPlaying;
    property Enabled;
    property Length : Integer read FLengthMs;
    property EnableMouseAntiOverlapping : Boolean read FEnableMouseAntiOverlapping write FEnableMouseAntiOverlapping;
    property MinimumBlank : Integer read FMinimumBlank write FMinimumBlank;
    property PageSize : Integer read FPageSizeMs;
    property PopupMenu;
    property Position : Integer read FPositionMs;
    property SelMode : TSelectionMode read FSelMode write FSelMode default smCoolEdit;
    property VerticalScaling : Integer read FVerticalScaling write SetVerticalScaling;
    property Visible;
    property WheelTimeScroll : TMouseWheelModifier read FWheelTimeScroll write FWheelTimeScroll default mwmNone;
    property WheelVZoom : TMouseWheelModifier read FWheelVZoom write FWheelVZoom default mwmShift;
    property WheelHZoom : TMouseWheelModifier read FWheelHZoom write FWheelHZoom default mwmCtrl;
    property SceneChangeEnabled : Boolean read FSceneChangeEnabled write FSceneChangeEnabled;
    property SceneChangeStartOffset : Integer read FSceneChangeStartOffset write FSceneChangeStartOffset;
    property SceneChangeStopOffset : Integer read FSceneChangeStopOffset write FSceneChangeStopOffset;
    property SceneChangeFilterOffset : Integer read FSceneChangeFilterOffset write FSceneChangeFilterOffset;
  end;

function CompareRanges(R1, R2: TRange): Integer;
procedure KaraSplit(const Text : WideString; var WordArray : TWideStringDynArray;
  var TimeArray : TIntegerDynArray);
procedure KaraSplit2(const Text : WideString; var WordArray : TWideStringDynArray;
  var TimeArray : TIntegerDynArray);

implementation

uses SysUtils, MiscToolsUnit, TntClasses, TntSysUtils, VirtualTrees;

const
  WAV_COLOR : TColor = $00A7F24A;
  WAV_BACK_COLOR : TColor = clBlack;
  ZERO_LINE_COLOR : TColor = $00518B0A;
  RANGE_COLOR_1 : TColor = $003333FF;
  RANGE_COLOR_2 : TColor = $00FF8000;
  RULER_BACK_COLOR : TColor = $00514741;
  RULER_TOP_BOTTOM_LINE_COLOR : TColor = $00BEB5AE;
  RULER_TEXT_COLOR : TColor = $00E0E0E0;
  RULER_TEXT_SHADOW_COLOR : TColor = clBlack;
  CURSOR_COLOR : TColor = clYellow;
  DISABLED_BACK_COLOR : TColor = clGray;

// =============================================================================

procedure KaraSplit(const Text : WideString; var WordArray : TWideStringDynArray;
  var TimeArray : TIntegerDynArray);
var
  i, i1, i2 : integer;
  s : WideString;
begin
  SetLength(WordArray, 0);
  SetLength(TimeArray, 0);
  s := Text;
  while (Length(s) > 0) do
  begin
    i1 := Pos('{\k', s);
    i2 := Pos('{\K', s);
    if (i1 = 0) or (i2 = 0) then
      i := i1 + i2
    else
      i := Min(i1, i2);
    if (i > 0) then
    begin
      SetLength(WordArray, Length(WordArray)+1);
      WordArray[Length(WordArray)-1] := Copy(s, 1, i-1);
      Delete(s, 1, i-1);
      i := Pos('}', s);
      if(i > 0) then
      begin
        SetLength(TimeArray, Length(TimeArray)+1);
        TimeArray[Length(TimeArray)-1] := StrToIntDef(Copy(s, 4, i-4),0);
        Delete(s, 1, i);
      end
      else
      begin
        WordArray[Length(WordArray)-1] := WordArray[Length(WordArray)-1] + s;
        s := '';
      end;
    end
    else
    begin
      SetLength(WordArray, Length(WordArray)+1);
      WordArray[Length(WordArray)-1] := s;
      s := '';
    end;
  end;
end;

// ------

// include tag in text
procedure KaraSplit2(const Text : WideString; var WordArray : TWideStringDynArray;
  var TimeArray : TIntegerDynArray);
var
  i, i1, i2 : integer;
  s : WideString;
begin
  SetLength(WordArray, 0);
  SetLength(TimeArray, 0);
  s := Text;
  while (Length(s) > 0) do
  begin
    i1 := Pos('{\k', s);
    i2 := Pos('{\K', s);
    if (i1 = 0) or (i2 = 0) then
      i := i1 + i2
    else
      i := Min(i1, i2);
    if (i > 0) then
    begin
      SetLength(WordArray, Length(WordArray)+1);
      WordArray[Length(WordArray)-1] := Copy(s, 1, i + Length('{\k') - 1);
      Delete(s, 1, i + Length('{\k') - 1);
      i := Pos('}', s);
      if(i > 0) then
      begin
        SetLength(TimeArray, Length(TimeArray)+1);
        TimeArray[Length(TimeArray)-1] := StrToIntDef(Copy(s, 1, i-1),0);
        Delete(s, 1, i-1);
      end
      else
      begin
        WordArray[Length(WordArray)-1] := WordArray[Length(WordArray)-1] + s;
        s := '';
      end;
    end
    else
    begin
      SetLength(WordArray, Length(WordArray)+1);
      WordArray[Length(WordArray)-1] := s;
      s := '';
    end;
  end;
end;

// =============================================================================

constructor TMinBlankInfo.Create;
begin
  inherited;
  Range := nil;
end;

function TMinBlankInfo.Exists : Boolean;
begin
  Result := Assigned(Range);
end;

function TMinBlankInfo.SetInfo(NewRange : TRange; NewPart : TMinBlankInfoPart) : Boolean;
begin
  Result := False;
  if (Range <> NewRange) then
  begin
    Range := NewRange;
    Result := True; // Changed
  end;
  if (Part <> NewPart) then
  begin
    Part := NewPart;
    Result := True; // Changed
  end;
end;

function TMinBlankInfo.GetStart(MinBlankTime : Integer): Integer;
begin
  if (Part = mbipStart) then
    Result := Range.StartTime - MinBlankTime
  else
    Result := Range.StopTime;
end;

function TMinBlankInfo.GetStop(MinBlankTime : Integer) : Integer;
begin
  if (Part = mbipStart) then
    Result := Range.StartTime
  else
    Result := Range.StopTime + MinBlankTime;
end;

function TMinBlankInfo.GetSnappingPoint(MinBlankTime : Integer) : Integer;
begin
  if (Part = mbipStart) then
    Result := GetStart(MinBlankTime)
  else
    Result := GetStop(MinBlankTime);
end;

// =============================================================================

procedure TRange.Assign(const Range : TRange);
var i : integer;
begin
  Self.StartTime := Range.StartTime;
  Self.StopTime := Range.StopTime;
  SetLength(Self.SubTime, Length(Range.SubTime));
  for i := Low(Range.SubTime) to High(Range.SubTime) do
  begin
    Self.SubTime[i] := Range.SubTime[i];
  end;
end;

// -----------------------------------------------------------------------------

procedure TRange.AddSubTime(const NewTime : Integer);
var i, InsertIdx : integer;
begin
  // Search the insertion place
  InsertIdx := 0;
  while (InsertIdx < Length(SubTime)) do
  begin
    if(SubTime[InsertIdx] > NewTime) then
      Break;
    Inc(InsertIdx);
  end;
  // Make place for our new element
  SetLength(SubTime, Length(SubTime)+1);
  // Move elements
  for i:=Length(SubTime)-2 downto InsertIdx do
  begin
    SubTime[i+1] := SubTime[i];
  end;
  // Insert new one
  SubTime[InsertIdx] := NewTime;
end;

// -----------------------------------------------------------------------------

procedure TRange.ClearSubTimes;
begin
  SetLength(SubTime,0);
end;

// -----------------------------------------------------------------------------

procedure TRange.DelSubTimeAt(const Idx : Integer);
var i : Integer;
begin
  // Move elements
  for i:=Idx to Length(SubTime)-2 do
  begin
    SubTime[i] := SubTime[i+1];
  end;
  // Trunc array
  SetLength(SubTime, Length(SubTime)-1);
end;

// -----------------------------------------------------------------------------

function TRange.GetSubTimeRange(const PosMS : Integer; Range : TRange) : Integer;
var i, KStart, KStop : Integer;
begin
  Result := -1;
  if(Length(SubTime) <= 0) or (PosMS < StartTime) or (PosMS > StopTime) then
    Exit;

  KStart := -1;
  KStop := -1;
  for i:=0 to Length(SubTime)-1 do
  begin
    if(PosMS < SubTime[i]) then
    begin
      Result := i;
      if (i > 0) then
        KStart := SubTime[i-1]
      else
        KStart := StartTime;
      KStop := SubTime[i];
      Break;
    end;
  end;
  if (KStart = -1) then
  begin
    Result := Length(SubTime);
    KStart := SubTime[Length(SubTime)-1];
    KStop := StopTime;
  end;
  if Assigned(Range) then
  begin
    Range.StartTime := KStart;
    Range.StopTime := KStop;
  end;
end;

// -----------------------------------------------------------------------------

procedure TRange.GetSubTimeRangeAt(const Idx : Integer; Range : TRange);
begin
  // Return Index of the range,
  // For example :
  // - GetSubTimeRangeAt(0,...) -> [StartTime, SubTime[0]]
  // - GetSubTimeRangeAt(1,...) -> [SubTime[0], SubTime[1]]
  // - GetSubTimeRangeAt(n,...) -> [SubTime[n-1], StopTime] (n = Length(SubTime))

  if (Length(SubTime) <= 0) or (Idx < 0) or (Idx > Length(SubTime)) then
    Exit;

  if (Idx = 0) then
  begin
    Range.StartTime := StartTime;
    Range.StopTime := SubTime[0];
  end
  else if (Idx = Length(SubTime)) then
  begin
    Range.StartTime := SubTime[Idx-1];
    Range.StopTime := StopTime;
  end
  else
  begin
    Range.StartTime := SubTime[Idx-1];
    Range.StopTime := SubTime[Idx];
  end;
end;

// -----------------------------------------------------------------------------

// TODO : move this in another class

function TRange.UpdateSubTimeFromText(const Text : WideString) : Boolean;
var
  WordArray : TWideStringDynArray;
  KTimeArray : TIntegerDynArray;
  AbsTimeArray : TIntegerDynArray;
  AccuTime : Integer;
  i : Integer;
  AreTimesDifferent : Boolean;
begin
  // Parse text
  KaraSplit(Text, WordArray, KTimeArray);

  if Length(KTimeArray) = 0 then
  begin
    if Length(SubTime) > 0 then
    begin
      ClearSubTimes;
      Result := True;
    end
    else
      Result := True;
    Exit;
  end;

  // Convert \k time (we forget last time)
  SetLength(AbsTimeArray, Length(KTimeArray)-1);
  AccuTime := StartTime;
  for i:=0 to Length(KTimeArray)-2 do
  begin
    AccuTime := AccuTime + (KTimeArray[i] * 10);
    AbsTimeArray[i] := AccuTime;
  end;

  // Compare times array
  AreTimesDifferent := False;
  if (Length(AbsTimeArray) <> Length(SubTime)) then
  begin
    // Length mismatch
    AreTimesDifferent := True;
  end
  else
  begin
    i := 0;
    while (i < Length(AbsTimeArray)) do
    begin
      if (AbsTimeArray[i] <> SubTime[i]) then
      begin
        AreTimesDifferent := True;
        Break;
      end;
      Inc(i);
    end;
  end;

  if (AreTimesDifferent = True) then
  begin
    // Update array
    SetLength(SubTime, Length(AbsTimeArray));
    for i:=0 to Length(AbsTimeArray)-1 do
      SubTime[i] := AbsTimeArray[i];
  end;

  Result := AreTimesDifferent;
end;

function TRange.SetTimes(Start, Stop : Integer) : Boolean; // Return true if changed
begin
  Result := False;
  if (Self.StartTime <> Start) then
  begin
    Self.StartTime := Start;
    Result := True;
  end;
  if (Self.StopTime <> Stop) then
  begin
    Self.StopTime := Stop;
    Result := True;
  end;
end;

function TRange.ToString : string;
begin
  Result := Format('[%d,%d]', [StartTime, StopTime]);
end;

// =============================================================================

function TRangeFactory.CreateRange : TRange;
begin
  Result := TRange.Create;
end;

//------------------------------------------------------------------------------

function TRangeFactory.CreateRangeSS(Start, Stop : Integer) : TRange;
begin
  Result := CreateRange;
  if (Start <= Stop) then
  begin
    Result.StartTime := Start;
    Result.StopTime := Stop;
  end
  else
  begin
    Result.StartTime := Stop;
    Result.StopTime := Start;
  end;
end;

// =============================================================================

constructor TRangeList.Create;
begin
  FList := TList.Create;
end;

//------------------------------------------------------------------------------

destructor TRangeList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TRangeList.Add(const Range : TRange) : Integer;
begin
  Result := FindInsertPos(Range);
  FList.Insert(Result, Range);
end;

//------------------------------------------------------------------------------

function TRangeList.AddAndReturnSibling(const Range : TRange) : TRange;
var InsertPos : Integer;
begin
  InsertPos := FindInsertPos(Range);
  if(InsertPos >= 0) and (InsertPos < FList.Count) then
    Result := FList[InsertPos]
  else
    Result := nil;
  FList.Insert(InsertPos,Range);
end;

//------------------------------------------------------------------------------

procedure TRangeList.AddAtEnd(const Range : TRange);
begin
  FList.Add(Range);
end;

//------------------------------------------------------------------------------

function TRangeList.GetCount : Integer;
begin
  Result := FList.Count;
end;

//------------------------------------------------------------------------------

function CompareRanges(R1, R2: TRange): Integer;
begin
  if R1.StartTime < R2.StartTime then
    Result := 1
  else if R1.StartTime > R2.StartTime then
    Result := -1
  else
  begin
    if R1.StopTime < R2.StopTime then
      Result := 1
    else if R1.StopTime > R2.StopTime then
      Result := -1
    else
      Result := 0
  end;
end;

//------------------------------------------------------------------------------

function CompareRangesFullsort(R1, R2: TRange): Integer;
begin
  Result := -CompareRanges(R1,R2);
end;

//------------------------------------------------------------------------------

function TRangeList.FindInsertPos(const Range : TRange) : Integer;
var Min,Mid,Max : integer;
    RangeCursor : TRange;
    CompareResult : Integer;
begin
  Min := 0;
  Max := FList.Count-1;
  Mid := (Max+Min) div 2;
  
  while (Min <= Max) do
  begin
    RangeCursor := TRange(FList[Mid]);
    CompareResult := CompareRanges(RangeCursor,Range);
    if CompareResult = 1 then // RangeCursor < Range
      Min := Mid+1
    else if CompareResult = -1 then // RangeCursor > Range
      Max := Mid-1
    else // r = Range
      Break;
    Mid := (Max+Min) div 2;
  end;
  
  Result := Min;
end;

//------------------------------------------------------------------------------

function TRangeList.FindInsertPos(const Start,Stop : Integer) : Integer;
var Range : TRange;
begin
  Range := TRange.Create;
  Range.StartTime := Start;
  if Stop = -1 then
    Range.StopTime := Start + 1
  else
    Range.StopTime := Stop;
  Result := FindInsertPos(Range);
  Range.Free;
end;

//------------------------------------------------------------------------------

function TRangeList.GetRangeIdxAt(const PosMs : Integer) : Integer;
var r : TRange;
    i : Integer;
begin
  Result := -1;
  if FList.Count = 0 then
    Exit;
  i := FindInsertPos(PosMs, -1);
  Constrain(i, 0, FList.Count-1);
  while (i >= 0) do
  begin
    r := FList[i];
    if (r.StartTime <= PosMs) and (r.StopTime >= PosMs) then
    begin
      Result := i;
      Exit;
    end;
    Dec(i);
  end;
end;

//------------------------------------------------------------------------------

function TRangeList.GetItem(const Index: Integer): TRange;
begin
  Result := TRange(FList.Items[Index]);
end;

//------------------------------------------------------------------------------

procedure TRangeList.FullSort;
begin
  FList.Sort(@CompareRangesFullsort);
end;

//------------------------------------------------------------------------------

procedure TRangeList.Delete(const Index : Integer);
var Range : TRange; 
begin
  if (Index >= 0) and (Index < FList.Count) then
  begin
    Range := FList[Index];
    FList.Delete(Index);
    Range.Free;
  end;
end;

//------------------------------------------------------------------------------

function TRangeList.IndexOf(const Range : TRange) : Integer;
begin
  Result := FList.IndexOf(Range);
end;

//------------------------------------------------------------------------------

procedure TRangeList.Move(const CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

//------------------------------------------------------------------------------

procedure TRangeList.Clear;
var i : Integer;
begin
  for i:=0 to FList.Count-1 do
  begin
    TRange(FList[i]).Free;
  end;
  FList.Clear;
end;

//------------------------------------------------------------------------------

function TRangeList.FindFirstRangeAt(const PosMs : Integer; const ExpandBy : Integer) : TRange;
begin
  if FList.Count = 0 then
  begin
    Result := nil;
    Exit;
  end;
  FSearchStartAt := PosMs;
  FSearchExpandBy := ExpandBy;
  FSearchIdx := FindInsertPos(FSearchStartAt - FSearchExpandBy, -1);
  Constrain(FSearchIdx, 0, FList.Count-1);
  while (FSearchIdx > 0) and
        (TRange(FList[FSearchIdx]).StopTime > (FSearchStartAt + FSearchExpandBy)) do
  begin
    Dec(FSearchIdx);
  end;
  Result := FindNextRange;
end;

//------------------------------------------------------------------------------

function TRangeList.FindNextRange : TRange;
var Range : TRange;
begin
  Result := nil;
  while (FSearchIdx >= 0) and (FSearchIdx < FList.Count) do
  begin
    Range := FList[FSearchIdx];
    Inc(FSearchIdx);
    if((Range.StartTime - FSearchExpandBy) > FSearchStartAt) then
      Exit
    else if ((Range.StartTime - FSearchExpandBy) <= FSearchStartAt) and
            ((Range.StopTime + FSearchExpandBy) >= FSearchStartAt) then
    begin
      Result := Range;
      Exit;
    end;
  end;
end;

// =============================================================================

constructor TWAVDisplayer.Create(AOwner: TComponent);
var MiniSB : TMiniScrollBar;
begin
  inherited Create(AOwner);
  BevelInner := bvNone;
  BevelOuter := bvNone;

  TabStop := True;
  FSelectionOrigin := -1;
  FScrollOrigin := -1;  
  FDynamicEditMode := demNone;
  FDynamicSelRange := nil;
  FDynamicSelRangeOld := nil;
  FSelectedKaraokeIndex := -1;
  FSelectedKaraokeRange := nil;
  FMouseIsDown := False;
  FMinSelTime := -1;
  FMaxSelTime := -1;
  
  FOffscreen := TBitmap.Create;
  FOffscreen.PixelFormat := pf32bit; // for faster drawing
  FOffscreenWAV := TBitmap.Create;
  FOffscreenWAV.PixelFormat := pf32bit; // for faster drawing

  FRangeList := TRangeList.Create;
  FSelection := TRange.Create;
  FSelection.StartTime := 0;
  FSelection.StopTime := 0;
  FNeedToSortSelectedSub := False;

  FSceneChangeStartOffset := 130;
  FSceneChangeStopOffset := 130;
  FSceneChangeFilterOffset := 250;
  FSceneChangeEnabled := True;
  FMinimumBlank := 0;
  FMinBlankInfo1 := TMinBlankInfo.Create;
  FMinBlankInfo2 := TMinBlankInfo.Create;
  
  FUpdateCursorTimer := TTimer.Create(nil);
  FUpdateCursorTimer.Enabled := False;
  FUpdateCursorTimer.Interval := 40;
  FUpdateCursorTimer.OnTimer := OnUpdateCursor;
  FIsPlaying := False;
  FPeakDataLoaded := False;
  FVerticalScaling := 100;
  FDisplayRuler := True;
  FDisplayRulerHeight := 20;

  FWheelTimeScroll := mwmNone;
  FWheelVZoom := mwmShift;
  FWheelHZoom := mwmCtrl;

  MiniSB := TMiniScrollBar.Create(Self);
  MiniSB.Parent := Self;
  MiniSB.Height := 12;
  MiniSB.Align := alBottom;
  SetScrollBar(MiniSB);
end;

//------------------------------------------------------------------------------

destructor TWAVDisplayer.Destroy;
begin
  FPeakTab := nil;
  FUpdateCursorTimer.Enabled := False;
  FUpdateCursorTimer.Free;
  FSelection.Free;
  FRangeList.Free;
  FOffscreenWAV.Free;
  FOffscreen.Free;
  FMinBlankInfo1.Free;
  FMinBlankInfo2.Free;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.WM_EraseBKGND(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.PaintWavOnCanvas(ACanvas : TCanvas; TryOptimize : Boolean);
var x, y1, y2 : Integer;
    x1_update, x2_update, x_optim : Integer;
    x_scaled : Cardinal;
    PeaksPerPixelScaled : Double;
    StartPositionInPeaks : Double;
    Middle : Integer;
    i : Integer;
    PeakMax, PeakMin : Integer;
    Rect : TRect;
    RectHeight : Integer;
begin
  PeaksPerPixelScaled := (((FPageSizeMs / 1000.0) * FWavFormat.nSamplesPerSec) / FSamplesPerPeak) / Width;
  StartPositionInPeaks := ((FPositionMs / 1000.0) * FWavFormat.nSamplesPerSec) / FSamplesPerPeak;

  x1_update := 0;
  x2_update := Width;

  if TryOptimize and (FOldPageSizeMs = FPageSizeMs) then
  begin
    // Calculate intersection between old and new view, and update only the new part
    if (FPositionMs > FOldPositionMs) then
    begin
      x_optim := TimeToPixel(FPositionMs - FOldPositionMs);
      x2_update := Width;
      x1_update := x2_update - x_optim;
      Constrain(x1_update, 0, Width);
      if (x1_update <> 0) then
        BitBlt(ACanvas.Handle, 0, 0, x1_update, Height,
          ACanvas.Handle, x_optim, 0, SRCCOPY);
    end
    else
    begin
      x_optim := TimeToPixel(FOldPositionMs - FPositionMs);
      x1_update := 0;
      x2_update := x_optim;
      Constrain(x2_update, 0, Width);
      if (x2_update <> Width) then
        BitBlt(ACanvas.Handle, x_optim, 0, Width-x_optim, Height,
          ACanvas.Handle, 0, 0, SRCCOPY);
    end;
  end;

  Rect := ClientRect;
  Rect.Left := x1_update;
  Rect.Right := x2_update;
  Rect.Bottom := Rect.Bottom - FScrollBar.Height;
  if FDisplayRuler then
    Rect.Bottom := Rect.Bottom - FDisplayRulerHeight;

  RectHeight := Rect.Bottom - Rect.Top;
  Middle := Rect.Top + (RectHeight div 2);

  // Back
  ACanvas.Brush.Color := WAV_BACK_COLOR;
  ACanvas.FillRect(Rect);

  // Wave
  ACanvas.Pen.Color := WAV_COLOR;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Mode := pmCopy;

  // TODO : when using huge zoom, use wav data directly if available
  for x:=x1_update to x2_update do
  begin
    x_scaled := Round((x * PeaksPerPixelScaled) + StartPositionInPeaks);

    //assert(x_scaled < FPeakTabSize);
    if (x_scaled >= FPeakTabSize) then
      x_scaled := FPeakTabSize - 1;

    // calculate peak from x_scaled to next x_scaled
    PeakMax := FPeakTab[x_scaled].Max;
    PeakMin := FPeakTab[x_scaled].Min;
    for i:= x_scaled+1 to Min(Round(((x+1)*PeaksPerPixelScaled)+StartPositionInPeaks),FPeakTabSize)-1 do
    begin
      if FPeakTab[i].Max > PeakMax then
        PeakMax := FPeakTab[i].Max;
      if FPeakTab[i].Min < PeakMin then
        PeakMin := FPeakTab[i].Min;
    end;

    y1 := Round((((PeakMax * FVerticalScaling) / 100) * RectHeight) / 65536);
    y2 := Round((((PeakMin * FVerticalScaling) / 100) * RectHeight) / 65536);

    ACanvas.MoveTo(x, Middle-y1);
    ACanvas.LineTo(x, Middle-y2);
  end;

  // 0 line
  ACanvas.Pen.Color := ZERO_LINE_COLOR;
  ACanvas.MoveTo(0, Middle);
  ACanvas.LineTo(Width, Middle);
end;

//------------------------------------------------------------------------------

function TWAVDisplayer.GetWavCanvasHeight : Integer;
begin
  Result := Height - FScrollBar.Height;
  if FDisplayRuler then
    Result := Result - FDisplayRulerHeight;
end;

//------------------------------------------------------------------------------

function TimeMsToShortString(TimeMS: Cardinal; Precision : Cardinal) : string;
var
  min, sec, ms: Cardinal;
begin
  ms := TimeMs div 1000;
  min := ms div 60;
  sec := ms mod 60;
  ms := (TimeMS - (min * 60 * 1000) - (sec * 1000)) div Precision;
  if (min > 0) then
  begin
    if (ms > 0) then
      Result := Format('%d:%.2d.%d', [min, sec, ms])
    else
      Result := Format('%d:%.2d', [min, sec]);
  end
  else
    Result := Format('%d.%d', [sec, ms])
end;

//------------------------------------------------------------------------------

function TWAVDisplayer.IsFilteredSceneChange(SceneChange : Integer) : Boolean;
var pos : Integer;
    range : TRange;
begin
  Result := False;
  pos := FRangeList.GetRangeIdxAt(SceneChange);
  if (pos >= 0) and (pos < FRangeList.GetCount) and (FSceneChangeFilterOffset > 0) then
  begin
    range := FRangeList[pos];
    Result := (SceneChange > range.StartTime + FSceneChangeFilterOffset) and
      (SceneChange < range.StopTime - FSceneChangeFilterOffset);
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.DrawAlphaRect(ACanvas : TCanvas; t1, t2 : Integer);
var x1, x2 : Integer;
    r : TRect;
begin
  x1 := TimeToPixel(t1 - FPositionMs);
  x2 := TimeToPixel(t2 - FPositionMs);

  Constrain(x1, 0, Width);
  Constrain(x2, 0, Width);
  r := ClientRect;
  r.Left := x1;
  r.Right := x2;
  r.Bottom := r.Bottom - FScrollBar.Height;
  if FDisplayRuler then
    r.Bottom := r.Bottom - FDisplayRulerHeight;

  VirtualTrees.AlphaBlend(ACanvas.Handle, ACanvas.Handle, r,
    Point(0,0), bmConstantAlphaAndColor, 80, ACanvas.Pen.Color);
end;

procedure TWAVDisplayer.PaintOnCanvas(ACanvas : TCanvas);
var x, SceneChange : Integer;
    x1, x2, y1, y2 : Integer;
    i, j, t1, t2 : Integer;
    r : TRange;
    SelRect, CustomDrawRect : TRect;
    CanvasHeight : Integer;
    ShowStart, ShowStop, FullHLines : Boolean;
begin
  CanvasHeight := GetWavCanvasHeight;
  y1 := CanvasHeight div 10;
  y2 := (CanvasHeight * 9) div 10;

  // Scene change
  if FSceneChangeEnabled then
  begin
    ACanvas.Pen.Color := $36C9FF;
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Mode := pmCopy;
    for i := Low(FSceneChangeList) to High(FSceneChangeList) do
    begin
      SceneChange := FSceneChangeList[i];
      if (SceneChange >= FPositionMs - FSceneChangeStopOffset) and
         (SceneChange <= FPositionMs + FPageSizeMs + FSceneChangeStartOffset) then
      begin
        if (FSceneChangeStopOffset + FSceneChangeStartOffset > 0) and
           (not IsFilteredSceneChange(SceneChange))  then
        begin
          DrawAlphaRect(ACanvas, SceneChange - FSceneChangeStartOffset,
            SceneChange + FSceneChangeStopOffset);
        end;

        x := TimeToPixel(SceneChange - FPositionMs);
        ACanvas.MoveTo(x, 0);
        ACanvas.LineTo(x, CanvasHeight);
      end;
    end;
  end;

  //OutputDebugString(PChar(Format('FDynamicEditMode = %d, FSelectionOrigin = %d, FCursorMs = %d, FDynamicSelRange = %p, SelStartTime = %d',
  //  [Ord(FDynamicEditMode), FSelectionOrigin, FCursorMs, Pointer(FDynamicSelRange), FSelection.StartTime])));

  if (FMinBlankInfo1.Exists) then
  begin
    ACanvas.Pen.Color := clWhite;
    DrawAlphaRect(ACanvas, FMinBlankInfo1.GetStart(FMinimumBlank),
      FMinBlankInfo1.GetStop(FMinimumBlank));
  end;

  if (FMinBlankInfo2.Exists) then
  begin
    ACanvas.Pen.Color := clWhite;
    DrawAlphaRect(ACanvas, FMinBlankInfo2.GetStart(FMinimumBlank),
      FMinBlankInfo2.GetStop(FMinimumBlank));
  end;

  // TODO improvement : this is very slow when lot's of range are on screen
  // We should do this in 2 pass to group ranges, and use another color

  // Range
  ACanvas.Pen.Mode := pmCopy;
  ACanvas.Brush.Style := bsClear;
  for i:=0 to FRangeList.Count-1 do
  begin
    r := FRangeList[i];
    x1 := -1; x2 := -1;
    if (r.StartTime >= FPositionMs) and (r.StartTime <= FPositionMs + FPageSizeMs) then
    begin
      x1 := TimeToPixel(r.StartTime - FPositionMs);
    end;
    if (r.StopTime >= FPositionMs) and (r.StopTime <= FPositionMs + FPageSizeMs) then
    begin
      x2 := TimeToPixel(r.StopTime - FPositionMs);
    end;

    ShowStart := (x1 <> -1);
    ShowStop := (x2 <> -1) and (x2 <> x1);
    FullHLines := (r.StartTime < FPositionMs) and (r.StopTime > FPositionMs + FPageSizeMs);
    
    if (ShowStart or ShowStop or FullHLines) then
    begin
      if (i mod 2) = 0 then
        ACanvas.Pen.Color := RANGE_COLOR_1
      else
        ACanvas.Pen.Color := RANGE_COLOR_2;
    end;

    // Paint start time
    if ShowStart then
    begin
      if (FDynamicEditMode = demStart) and (FDynamicSelRange = r) then
        ACanvas.Pen.Style := psSolid
      else
        ACanvas.Pen.Style := psDot;
      ACanvas.MoveTo(x1, 0);
      ACanvas.LineTo(x1, CanvasHeight);
    end;

    // Paint stop time
    if ShowStop then
    begin
      if (FDynamicEditMode = demStop) and (FDynamicSelRange = r) then
        ACanvas.Pen.Style := psSolid
      else
        ACanvas.Pen.Style := psDot;
      ACanvas.MoveTo(x2, 0);
      ACanvas.LineTo(x2, CanvasHeight);
    end;

    // Draw the top and bottom horizontal lines
    if FullHLines then
    begin
      x1 := 0;
      x2 := Width-1;
    end;
    if ((x1 <> -1) or (x2 <> -1)) then
    begin
      if (x1 = -1) then
        x1 := 0
      else if (x2 = -1) then
        x2 := Width-1;
      ACanvas.Pen.Style := psSolid;
      ACanvas.Pen.Width := 2;
      ACanvas.MoveTo(x1, y1);
      ACanvas.LineTo(x2, y1);
      ACanvas.MoveTo(x1, y2);
      ACanvas.LineTo(x2, y2);
      ACanvas.Pen.Width := 1;

      // Custom draw
      if Assigned(FOnCustomDrawRange) and ((x2 - x1) > 10) then
      begin
        CustomDrawRect.Top := y1;
        CustomDrawRect.Left := x1;
        CustomDrawRect.Right := x2;
        CustomDrawRect.Bottom := y2;
        FOnCustomDrawRange(Self, ACanvas, r, CustomDrawRect);
      end;
      
      // Karaoke
      if (System.Length(r.SubTime) > 0) then
      begin
        for j:=0 to System.Length(r.SubTime)-1 do
        begin
          if (r.SubTime[j] >= FPositionMs) and
             (r.SubTime[j] <= FPositionMs + FPageSizeMs) and
             (r.SubTime[j] >= r.StartTime) and
             (r.SubTime[j] <= r.StopTime) then
          begin
            x1 := TimeToPixel(r.SubTime[j] - FPositionMs);
            if (FDynamicEditMode = demKaraoke) and
               (FDynamicEditTime = j) and
               (FDynamicSelRange = r) then
            begin
              ACanvas.Pen.Style := psSolid
            end
            else
            begin
              ACanvas.Pen.Style := psDot;
            end;
            ACanvas.MoveTo(x1, y1);
            ACanvas.LineTo(x1, y2);
          end;
        end;
      end;

    end;
  end;

  // Selection
  if (FSelection.StopTime > 0) then
  begin
    ACanvas.Pen.Color := clWhite;
    ACanvas.Pen.Mode := pmXor;
    ACanvas.Pen.Style := psSolid;
    
    x1 := TimeToPixel(FSelection.StartTime - FPositionMs);
    x2 := TimeToPixel(FSelection.StopTime - FPositionMs);

    if (x1 = x2) then
    begin
      if (FSelection.StartTime >= FPositionMs) and
       (FSelection.StartTime <= FPositionMs + FPageSizeMs) then
      begin
        // points are on each other and in the display range
        ACanvas.MoveTo(x1, 0);
        ACanvas.LineTo(x1, CanvasHeight);
      end;
    end
    else
    begin
      Constrain(x1, 0, Width);
      Constrain(x2, 0, Width);
      if (x1 <> x2) then
      begin
        SelRect := ClientRect;
        SelRect.Left := x1;
        SelRect.Right := x2+1;
        SelRect.Bottom := SelRect.Bottom - FScrollBar.Height;
        if FDisplayRuler then
          SelRect.Bottom := SelRect.Bottom - FDisplayRulerHeight;
        InvertRect(ACanvas.Handle, SelRect);
      end;
    end;
  end;

  // Cursor
  if (FCursorMs >= FPositionMs) and (FCursorMs <= FPositionMs + FPageSizeMs) then
  begin
    ACanvas.Pen.Color := CURSOR_COLOR;
    ACanvas.Pen.Style := psDot;
    ACanvas.Pen.Mode := pmXor;
    x := TimeToPixel(FCursorMs - FPositionMs);
    ACanvas.MoveTo(x, 0);
    ACanvas.LineTo(x, CanvasHeight);
  end;

  // Play Cursor
  if FIsPlaying then
  begin
    if (FPlayCursorMs >= FPositionMs) and (FPlayCursorMs <= FPositionMs + FPageSizeMs) then
    begin
      ACanvas.Pen.Color := clWhite;
      ACanvas.Pen.Style := psSolid;
      ACanvas.Pen.Mode := pmXor;
      x := TimeToPixel(FPlayCursorMs - FPositionMs);
      ACanvas.MoveTo(x, 0);
      ACanvas.LineTo(x, CanvasHeight);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.PaintRulerOnCanvas(ACanvas : TCanvas);
var PosRect : TRect;
    PosString : string;
    p, x, x1, x2, MaxPosStep, StepMs, StepLog : Integer;
begin
  if FDisplayRuler then
  begin
    PosRect := ClientRect;
    PosRect.Bottom := PosRect.Bottom - FScrollBar.Height;
    PosRect.Top := PosRect.Bottom - FDisplayRulerHeight;

    // Draw background
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := RULER_BACK_COLOR;
    ACanvas.FillRect(PosRect);

    // Draw horizontal line at top and bottom
    ACanvas.Pen.Mode := pmCopy;  
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Color := RULER_TOP_BOTTOM_LINE_COLOR;
    ACanvas.MoveTo(0, PosRect.Top);
    ACanvas.LineTo(Width, PosRect.Top);
    ACanvas.MoveTo(0, PosRect.Bottom-1);
    ACanvas.LineTo(Width, PosRect.Bottom-1);

    // Set the text font
    ACanvas.Pen.Color := RULER_TEXT_COLOR;
    ACanvas.Pen.Style := psSolid;
    ACanvas.Font.Name := 'Time New Roman';
    ACanvas.Font.Color := RULER_TEXT_COLOR;
    ACanvas.Font.Size := 8;
    ACanvas.Brush.Style := bsClear;

    // Do some little calculation to try to show "round" time
    MaxPosStep := Round(Width / (ACanvas.TextWidth('000:00.0') * 2));
    StepMs := Round(FPageSizeMs / MaxPosStep);
    if StepMs = 0 then
      StepMs := 1;
    StepLog := Trunc(Power(10, Trunc(Log10(StepMs))));
    StepMs := StepMs div StepLog * StepLog;

    p := (FPositionMs div StepMs * StepMs);
    while (p < FPositionMs + FPageSizeMs) do
    begin
      // Draw main division
      x := TimeToPixel(p - FPositionMs);
      ACanvas.MoveTo(x, PosRect.Top + 1);
      ACanvas.LineTo(x, PosRect.Top + 5);
      PosString := TimeMsToShortString(p, StepLog);
      // Calculate text coordinate
      x1 := x - (ACanvas.TextWidth(PosString) div 2);
      // Draw text shadow
      ACanvas.Font.Color := RULER_TEXT_SHADOW_COLOR;
      ACanvas.TextOut(x1 + 2, PosRect.Top + 4 + 2, PosString);
      // Draw text
      ACanvas.Font.Color := RULER_TEXT_COLOR;
      ACanvas.TextOut(x1, PosRect.Top + 4, PosString);
      // Draw subdivision
      x2 := x + TimeToPixel(StepMs div 2);
      ACanvas.MoveTo(x2, PosRect.Top + 1);
      ACanvas.LineTo(x2, PosRect.Top + 3);
      p := p + StepMs;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.UpdateView(UpdateViewFlags : TUpdateViewFlags);
begin
  if (UpdateViewFlags = []) then
    Exit;

  FOffscreenWAV.Width := Width;
  FOffscreenWAV.Height := Height - FScrollBar.Height;
  FOffscreen.Width := FOffscreenWAV.Width;
  FOffscreen.Height := FOffscreenWAV.Height;

  if (not FPeakDataLoaded) then
  begin
    FOffscreen.Canvas.Brush.Color := DISABLED_BACK_COLOR;
    FOffscreen.Canvas.FillRect(FOffscreen.Canvas.ClipRect);
    Invalidate;
    Exit;
  end;

  FOffscreenWAV.Canvas.Lock;

  if (uvfPageSize in UpdateViewFlags) then
  begin
    // We need to recalculate all
    PaintWavOnCanvas(FOffscreenWAV.Canvas, False);
    FOldPositionMs := FPositionMs;
    FOldPageSizeMs := FPageSizeMs;
    PaintRulerOnCanvas(FOffscreenWAV.Canvas);
  end
  else if (uvfPosition in UpdateViewFlags) then
  begin
    // Maybe we can draw only a part of the WAV
    PaintWavOnCanvas(FOffscreenWAV.Canvas, True);
    FOldPositionMs := FPositionMs;
    FOldPageSizeMs := FPageSizeMs;
    PaintRulerOnCanvas(FOffscreenWAV.Canvas);
  end;

  // Copy Wav
  FOffscreen.Canvas.Lock;

  FOffscreen.Canvas.Draw(0, 0, FOffscreenWAV);
  FOffscreenWAV.Canvas.Unlock;

  // Add selection, range, cursor
  PaintOnCanvas(FOffscreen.Canvas);
  FOffscreen.Canvas.Unlock;
  // TODO : separate cursors painting, and use xor tricks to paint cursors

  Repaint;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.Paint;
var NewWidth, NewHeight : Integer;
begin
  NewWidth := Width;
  NewHeight := Height - FScrollBar.Height;
  if (FOffscreen.Width <> NewWidth) or (FOffscreen.Height <> NewHeight) then
  begin
    UpdateView([uvfPageSize]);
  end;
  Canvas.CopyRect(Canvas.ClipRect, FOffscreen.Canvas, Canvas.ClipRect);
end;

//------------------------------------------------------------------------------

function TWAVDisplayer.FindSnappingPoint(PosMs : Integer) : Integer;
var Candidate : Integer;
    SnappingDistanceTime : Integer;
    Idx, IdxCursor : Integer;
const SNAPPING_DISTANCE_PIXEL : Integer = 8;
begin
  Result := -1;

  SnappingDistanceTime := PixelToTime(SNAPPING_DISTANCE_PIXEL);

  if (FMinimumBlank > 0) then
  begin
    if (FMinBlankInfo1.Exists) then
    begin
      Candidate := FMinBlankInfo1.GetSnappingPoint(FMinimumBlank);
      if Abs(Candidate - PosMs) <= SnappingDistanceTime then
      begin
        Result := Candidate;
        Exit;
      end;
    end;

    if (FMinBlankInfo2.Exists) then
    begin
      Candidate := FMinBlankInfo2.GetSnappingPoint(FMinimumBlank);
      if Abs(Candidate - PosMs) <= SnappingDistanceTime then
      begin
        Result := Candidate;
        Exit;
      end;
    end;
  end;

  if FSceneChangeEnabled and (System.Length(FSceneChangeList) > 0) then
  begin
    Idx := BinarySearch(FSceneChangeList, PosMs);
    // Search forward for a not filtered scene change
    IdxCursor := Idx;
    while (IdxCursor < System.Length(FSceneChangeList))
           and IsFilteredSceneChange(FSceneChangeList[IdxCursor]) do
    begin
      Inc(IdxCursor);
    end;

    if (IdxCursor < System.Length(FSceneChangeList)) and (FSelectionOrigin < FSceneChangeList[IdxCursor]) then
    begin
      Candidate := FSceneChangeList[IdxCursor] - FSceneChangeStartOffset;
      if Abs(Candidate - PosMs) <= SnappingDistanceTime then
      begin
        Result := Candidate;
        Exit;
      end;
    end;

    // Search backward for a not filtered scene change
    IdxCursor := Idx - 1;
    while (IdxCursor >= 0) and IsFilteredSceneChange(FSceneChangeList[IdxCursor]) do
    begin
      Dec(IdxCursor);
    end;
    if (IdxCursor >= 0) and (FSelectionOrigin > FSceneChangeList[IdxCursor]) then
    begin
      Candidate := FSceneChangeList[IdxCursor] + FSceneChangeStopOffset;
      if Abs(Candidate - PosMs) <= SnappingDistanceTime then
      begin
        Result := Candidate;
        Exit;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.MouseDownCoolEdit(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var UpdateFlags : TUpdateViewFlags);
var NewCursorPos : Integer;
    ClipKaraokeRect, ClipSubRect : TRect;
    x1, x2, i, SnappingPos : Integer;
begin
  if (ssLeft in Shift) then
  begin

    if (FDynamicEditMode = demKaraoke) and Assigned(FDynamicSelRange) then
    begin
      if (FDynamicEditTime >= 0) and (FDynamicEditTime < System.Length(FDynamicSelRange.SubTime)) then
      begin
        FOldKaraokeSubTime := FDynamicSelRange.SubTime[FDynamicEditTime];
        // Re-adjust mouse cursor position
        X := TimeToPixel(FDynamicSelRange.SubTime[FDynamicEditTime] - FPositionMs);
        Windows.SetCursorPos(X + ClientOrigin.X, Y + ClientOrigin.Y);
        // Limit mouse cursor position
        if (FDynamicEditTime = 0) then
          x1 := TimeToPixel(FDynamicSelRange.StartTime - FPositionMs)
        else
          x1 := TimeToPixel(FDynamicSelRange.SubTime[FDynamicEditTime-1] - FPositionMs);

        if (FDynamicEditTime < System.Length(FDynamicSelRange.SubTime)-1) then
          x2 := TimeToPixel(FDynamicSelRange.SubTime[FDynamicEditTime+1] - FPositionMs)
        else
          x2 := TimeToPixel(FDynamicSelRange.StopTime - FPositionMs);

        ClipKaraokeRect.Left := (x1 + 3);
        ClipKaraokeRect.Right := (x2 - 2);
        ClipKaraokeRect.Top := 0;
        ClipKaraokeRect.Bottom := Height;
        // Convert in screen coordinate
        OffsetRect(ClipKaraokeRect, ClientOrigin.X, ClientOrigin.Y);
        ClipCursor(@ClipKaraokeRect);
      end;
      Exit;
    end;

    if(FSelectedKaraokeIndex <> -1) then
    begin
      FSelectedKaraokeIndex := -1;
      FSelectedKaraokeRange := nil;
      if Assigned(FOnSelectedKaraokeRange) then
        FOnSelectedKaraokeRange(Self, nil);
    end;

    // Re-adjust mouse cursor position
    if (FDynamicEditMode = demStart) or (FDynamicEditMode = demStop) then
    begin
      X := TimeToPixel(FDynamicEditTime - FPositionMs);
      Windows.SetCursorPos(X + ClientOrigin.X, Y + ClientOrigin.Y);
    end;
    NewCursorPos := PixelToTime(X) + FPositionMs;

    if (not (ssCtrl in Shift)) and (FDynamicEditMode <> demKaraoke) then
    begin
      SnappingPos := FindSnappingPoint(NewCursorPos);
      if (SnappingPos <> -1) then
      begin
        NewCursorPos := SnappingPos;
      end
    end;

    if (ssShift in Shift) or (FDynamicEditMode = demStart) or (FDynamicEditMode = demStop) then
    begin
      if Assigned(FDynamicSelRange) and (FDynamicSelRange <> FSelectedRange) then
      begin
        SetSelectedRangeEx(FDynamicSelRange, False);
        Include(UpdateFlags, uvfSelection);
        if Assigned(FOnSelectedRange) then
          FOnSelectedRange(Self, FSelectedRange, True);
      end;

      // Selection modification using shift key
      if (NewCursorPos > FSelection.StartTime + ((FSelection.StopTime - FSelection.StartTime) div 2)) then
      begin
        // We are close to the end of the selection
        if SelectionIsEmpty then
        begin
          if (NewCursorPos > FCursorMs) then
          begin
            FSelection.StopTime := NewCursorPos;
            FSelection.StartTime := FCursorMs;
          end else begin
            FSelection.StopTime := FCursorMs;
            FSelection.StartTime := NewCursorPos;
          end;
        end
        else
          FSelection.StopTime := NewCursorPos;
        FSelectionOrigin := FSelection.StartTime;
      end else begin
        // We are close to the start of the selection
        FSelection.StartTime := NewCursorPos;
        FSelectionOrigin := FSelection.StopTime;
      end;
      if Assigned(FSelectedRange) then
      begin
        FNeedToSortSelectedSub := True;
        FRangeOldStart := FSelectedRange.StartTime;
        FRangeOldStop := FSelectedRange.StopTime;
        FSelectedRange.StartTime := FSelection.StartTime;
        FSelectedRange.StopTime := FSelection.StopTime;
        Include(UpdateFlags, uvfRange);
        if Assigned(FOnSelectedRangeChange) and (FDynamicEditMode = demNone) then
          FOnSelectedRangeChange(Self);
      end;
      if Assigned(FOnSelectionChange) then
        FOnSelectionChange(Self);
      Include(UpdateFlags, uvfSelection);
    end else begin
      if (FSelection.StartTime <> FSelection.StopTime) then
        Include(UpdateFlags, uvfSelection); // clear selection
      FSelectionOrigin := NewCursorPos;
      SetSelectedRangeEx(nil, False);
    end;

    if (FEnableMouseAntiOverlapping = True) then
    begin
      // Clip mouse left/right position to avoid overlapp on previous/next subtitle
      x1 := 0;
      x2 := Width;
      FMinSelTime := -1;
      FMaxSelTime := -1;
      i := RangeList.FindInsertPos(NewCursorPos,-1);
      if (i >= 0) then
      begin
        if Assigned(FSelectedRange) then
        begin
          if(NewCursorPos = FSelectedRange.StartTime) then
          begin
            if (i > 0) then
            begin
              FMinSelTime := RangeList[i-1].StopTime + 1;
              x1 := TimeToPixel(FMinSelTime - FPositionMs);
            end;
            FMaxSelTime := FSelectedRange.StopTime - 1;
            x2 := TimeToPixel(FMaxSelTime - FPositionMs);
          end
          else
          begin
            // TODO : better change stop when ovelapping on next sub ???
            FMinSelTime := FSelectedRange.StartTime + 1;
            x1 := TimeToPixel(FMinSelTime - FPositionMs );
            if(i < RangeList.Count) then
            begin
              FMaxSelTime := RangeList[i].StartTime - 1;
              x2 := TimeToPixel(FMaxSelTime - FPositionMs);
            end;
          end;
        end
        else
        begin
          if (i > 0) and
             (NewCursorPos >= RangeList[i-1].StartTime) and
             (NewCursorPos <= RangeList[i-1].StopTime) then
          begin
            // Selection only INSIDE subtitle range
          end
          else
          begin
            // Selection only OUTSIDE subtitle range
            if (i > 0) then
            begin
              FMinSelTime := RangeList[i-1].StopTime  + 1;
              x1 := TimeToPixel(FMinSelTime - FPositionMs);
            end;
            if(i < RangeList.Count) then
            begin
              FMaxSelTime := RangeList[i].StartTime - 1;
              x2 := TimeToPixel(FMaxSelTime - FPositionMs);
            end;
          end;
        end;
      end;
      Constrain(x1, 0, Width);
      Constrain(x2, 0, Width);
      // allow the cursor to go a bit further than allowed, we will do the real
      // clipping based on FMinSelTime and FMaxSelTime.
      ClipSubRect.Left := x1-2;
      ClipSubRect.Right := x2+3;
      ClipSubRect.Top := 0;
      ClipSubRect.Bottom := Height;
      // Convert in screen coordinate
      OffsetRect(ClipSubRect, ClientOrigin.X, ClientOrigin.Y);
      ClipCursor(@ClipSubRect);
    end;

    if (FCursorMs <> NewCursorPos) and (FDynamicEditMode = demNone) then
    begin
      FCursorMs := NewCursorPos;
      if Assigned(FOnCursorChange) then
        FOnCursorChange(Self);
      Include(UpdateFlags, uvfCursor);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.MouseDownSSA(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var UpdateFlags : TUpdateViewFlags);
var NewCursorPos : Integer;
begin
  NewCursorPos := PixelToTime(X) + FPositionMs;
  if(FSelectedKaraokeIndex <> -1) then
  begin
    FSelectedKaraokeIndex := -1;
    FSelectedKaraokeRange := nil;
    if Assigned(FOnSelectedKaraokeRange) then
      FOnSelectedKaraokeRange(Self, nil);
  end;

  if (ssLeft in Shift) then
  begin
    if SelectionIsEmpty then
    begin
      if (NewCursorPos < FCursorMs) then
      begin
        FSelection.StartTime := NewCursorPos;
        FSelection.StopTime := FCursorMs;
        FSelectionOrigin := FCursorMs;
      end
      else
        FSelectionOrigin := NewCursorPos;
    end
    else
    begin
      if (NewCursorPos < FSelection.StopTime) then
      begin
        FSelection.StartTime := NewCursorPos;
        FSelectionOrigin := FSelection.StopTime;
      end
      else
      begin
        FSelectionOrigin := NewCursorPos;
        SetSelectedRangeEx(nil, False);
      end;
    end;
  end;

  if (ssRight in Shift) and (not(ssShift in Shift)) then
  begin
    if SelectionIsEmpty then
    begin
      if (NewCursorPos > FCursorMs) then
      begin
        FSelection.StopTime := NewCursorPos;
        FSelection.StartTime := FCursorMs;
        FSelectionOrigin := FCursorMs;
      end
      else
        FSelectionOrigin := NewCursorPos;
    end
    else
    begin
      if (NewCursorPos > FSelection.StartTime) then
      begin
        FSelection.StopTime := NewCursorPos;
        FSelectionOrigin := FSelection.StartTime;
      end
      else
      begin
        FSelectionOrigin := NewCursorPos;
        SetSelectedRangeEx(nil, False);
      end;
    end;
  end;

  if (ssRight in Shift) or (ssLeft in Shift) then
  begin
    if Assigned(FSelectedRange) then
    begin
      FNeedToSortSelectedSub := True;
      FRangeOldStart := FSelectedRange.StartTime;
      FRangeOldStop := FSelectedRange.StopTime;
      FSelectedRange.StartTime := FSelection.StartTime;
      FSelectedRange.StopTime := FSelection.StopTime;
      Include(UpdateFlags, uvfRange);
      if Assigned(FOnSelectedRangeChange) then
        FOnSelectedRangeChange(Self);
    end;
    if Assigned(FOnSelectionChange) then
      FOnSelectionChange(Self);
    Include(UpdateFlags, uvfSelection);
  end;

  if (FCursorMs <> NewCursorPos) and (not (ssMiddle in Shift)) then
  begin
    FCursorMs := NewCursorPos;
    if Assigned(FOnCursorChange) then
      FOnCursorChange(Self);
    Include(UpdateFlags, uvfCursor);
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var UpdateFlags : TUpdateViewFlags;
begin
  inherited;
  UpdateFlags := [];

  if (ssDouble in Shift) or (not FPeakDataLoaded) then
    Exit;

  if (not InRange(X, 0, Width)) then
    Exit;

  FMouseIsDown := True;

  Case FSelMode of
    smCoolEdit : MouseDownCoolEdit(Button, Shift, X, Y, UpdateFlags);
    smSSA : MouseDownSSA(Button, Shift, X, Y, UpdateFlags);
  end; // case

  if (ssMiddle in Shift) then // Middle button = precision time scrolling activated
  begin
    Cursor := crHandPoint;
    MouseCapture := True;
    FScrollOrigin := PixelToTime(X);
  end;

  UpdateView(UpdateFlags);
end;

//------------------------------------------------------------------------------

function TWAVDisplayer.CheckSubtitleForDynamicSelection(Range : TRange;
  CursorPosMs, RangeSelWindow : Integer; X,Y : Integer) : Boolean;
var NewDynamicEditMode : TDynamicEditMode;
    i : Integer;
    CanvasHeight : Integer;
    y1, y2 : Integer;
begin
  Result := False;
  NewDynamicEditMode := demNone;

  if (((Range.StopTime - Range.StartTime) / RangeSelWindow) > 2) then
  begin
    CanvasHeight := GetWavCanvasHeight;
    if (Y > 0) and (Y < CanvasHeight) then
    begin
      if (Abs(CursorPosMs - Range.StartTime) < RangeSelWindow) then
      begin
        NewDynamicEditMode := demStart;
        FDynamicEditTime := Range.StartTime;
      end
      else if (Abs(Range.StopTime - CursorPosMs) < RangeSelWindow) then
      begin
        NewDynamicEditMode := demStop;
        FDynamicEditTime := Range.StopTime;
      end
      else
      begin
        // Karaoke check
        //CanvasHeight := GetWavCanvasHeight;
        y1 := CanvasHeight div 10;
        y2 := (CanvasHeight * 9) div 10;
        if (Y > y1) and (Y < y2) then
        begin
          // todo : search for the closer time
          for i:=0 to System.Length(Range.SubTime)-1 do
          begin
            if(Abs(CursorPosMs - Range.SubTime[i]) < RangeSelWindow) then
            begin
              NewDynamicEditMode := demKaraoke;
              FDynamicEditTime := i;
              Break;
            end;
          end;
        end;
      end;
    end;
  end;

  if (NewDynamicEditMode <> demNone) then
  begin
    Result := True;
    Cursor := crHSplit;
    FDynamicEditMode := NewDynamicEditMode;
    FDynamicSelRangeOld := FDynamicSelRange;
    if (Range <> FSelection) then
      FDynamicSelRange := Range
    else if Assigned(FSelectedRange) then
      FDynamicSelRange := FSelectedRange
    else
      FDynamicSelRange := nil;
  end;

  if (FDynamicSelRange <> FDynamicSelRangeOld) then
  begin
    if Assigned(FDynamicSelRange) and ((FDynamicEditMode = demStart) or (FDynamicEditMode = demStop)) then
    begin
      SetMinBlankAt(FDynamicSelRange.StartTime)
    end;
    UpdateView([uvfRange]);
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.MouseMove(Shift: TShiftState; X, Y: Integer);
var NewCursorPos, CursorPosMs, SnappingPos : Integer;
    ScrollDiff : Integer;
    DiffMuliplier : Integer;
    UpdateFlags : TUpdateViewFlags;
    RangeUnder : TRange;
    RangeSelWindow : Integer;
begin
  inherited;

  if (ssDouble in Shift) or (not FPeakDataLoaded) then
    Exit;

  UpdateFlags := [];

  if (FMouseIsDown) then
  begin
    case FSelMode of
    smCoolEdit :
      begin
        if (ssLeft in Shift) then
        begin
          Constrain(X, 0, Width);
          NewCursorPos := PixelToTime(X) + FPositionMs;

          // Make sur to clip selection
          if(FMinSelTime <> -1) then
          begin
            Constrain(NewCursorPos, FMinSelTime, MaxInt);
          end;
          if(FMaxSelTime <> -1) then
          begin
            Constrain(NewCursorPos, 0, FMaxSelTime);
          end;

          if (not (ssCtrl in Shift)) and (FDynamicEditMode <> demKaraoke) then
          begin
            SnappingPos := FindSnappingPoint(NewCursorPos);
            if (SnappingPos <> -1) then
            begin
              NewCursorPos := SnappingPos;
            end;
          end;

          if (FDynamicEditMode = demKaraoke) and Assigned(FDynamicSelRange) then
          begin
            if (FDynamicEditTime >= 0) and (FDynamicEditTime < System.Length(FDynamicSelRange.SubTime)) then
            begin
              FDynamicSelRange.SubTime[FDynamicEditTime] := NewCursorPos;
              Include(UpdateFlags, uvfRange);
              if (FSelectedKaraokeIndex <> -1) then
              begin
                // Update selection according to selected karaoke range
                FDynamicSelRange.GetSubTimeRangeAt(FSelectedKaraokeIndex, FSelection);
                if Assigned(FOnSelectionChange) then
                  FOnSelectionChange(Self);
                if Assigned(FOnKaraokeChange) then
                  FOnKaraokeChange(Self);
                Include(UpdateFlags, uvfSelection);
              end;
            end;
          end;

          if (FSelectionOrigin <> -1) and (FSelectionOrigin <> NewCursorPos) then
          begin
            // Update selection
            if (NewCursorPos > FSelectionOrigin) then
            begin
                FSelection.StartTime := FSelectionOrigin;
                FSelection.StopTime := NewCursorPos;
            end else begin
                FSelection.StartTime := NewCursorPos;
                FSelection.StopTime := FSelectionOrigin;
            end;
            if Assigned(FSelectedRange) then
            begin
              FNeedToSortSelectedSub := True;
              if (FSelectedRange.StartTime <> FSelection.StartTime) or
                 (FSelectedRange.StopTime <> FSelection.StopTime) then
              begin
                FSelectedRange.StartTime := FSelection.StartTime;
                FSelectedRange.StopTime := FSelection.StopTime;
                Include(UpdateFlags, uvfRange);
                if Assigned(FOnSelectedRangeChange) then
                  FOnSelectedRangeChange(Self);
              end;
            end;
            if Assigned(FOnSelectionChange) then
              FOnSelectionChange(Self);
            Include(UpdateFlags, uvfSelection);
          end;

          if (FCursorMs <> NewCursorPos) and (FDynamicEditMode = demNone) then
          begin
            FCursorMs := NewCursorPos;
            if Assigned(FOnCursorChange) then
              FOnCursorChange(Self);
            Include(UpdateFlags,uvfCursor);
          end;
        end;
      end;

    smSSA :
      begin
        if (ssLeft in Shift) or (ssRight in Shift) then
        begin
          Constrain(X, 0, Width);
          NewCursorPos := PixelToTime(X) + FPositionMs;
          if (FCursorMs <> NewCursorPos) then
          begin
            FCursorMs := NewCursorPos;

            if (FSelectionOrigin <> -1) then
            begin
              // Update selection
              if (FCursorMs > FSelectionOrigin) then
              begin
                  FSelection.StartTime := FSelectionOrigin;
                  FSelection.StopTime := FCursorMs;
              end else begin
                  FSelection.StartTime := FCursorMs;
                  FSelection.StopTime := FSelectionOrigin;
              end;
              if Assigned(FSelectedRange) then
              begin
                FNeedToSortSelectedSub := True;
                FSelectedRange.StartTime := FSelection.StartTime;
                FSelectedRange.StopTime := FSelection.StopTime;
                Include(UpdateFlags,uvfRange);
                if Assigned(FOnSelectedRangeChange) then
                  FOnSelectedRangeChange(Self);
              end;
              if Assigned(FOnSelectionChange) then
                FOnSelectionChange(Self);
              Include(UpdateFlags,uvfSelection);
            end;

            if Assigned(FOnCursorChange) then
              FOnCursorChange(Self);
            Include(UpdateFlags,uvfCursor);
          end;
        end;
      end;
    end; // case

    if (ssMiddle in Shift) then
    begin
      // pseudo "pixel accurate" scrolling
      SetAutoScroll(False);
      if (ssShift in Shift) then DiffMuliplier := 4 else DiffMuliplier := 1;
      ScrollDiff := PixelToTime(X) - FScrollOrigin;
      SetPositionMs(FPositionMs - (ScrollDiff*DiffMuliplier));
      FScrollOrigin := PixelToTime(X);
    end;
  end
  else
  begin
    Constrain(X, 0, Width);
    CursorPosMs := PixelToTime(X) + FPositionMs;

    // "Dynamic selection"
    if (FSelMode = smCoolEdit) and (Shift = []) then
    begin
      // Find a subtitle under the mouse
      RangeSelWindow := PixelToTime(4);
      if (RangeSelWindow < 1) then RangeSelWindow := 1;

      // First pass : check only inside sub
      RangeUnder := FRangeList.FindFirstRangeAt(CursorPosMs, 0);
      while Assigned(RangeUnder) do
      begin
        if CheckSubtitleForDynamicSelection(RangeUnder, CursorPosMs, RangeSelWindow, X, Y) then
          Exit;
        RangeUnder := FRangeList.FindNextRange;
      end;

      // 2nd pass : Wider search
      RangeSelWindow := PixelToTime(2);
      if (RangeSelWindow < 1) then RangeSelWindow := 1;
      RangeUnder := FRangeList.FindFirstRangeAt(CursorPosMs, RangeSelWindow);
      while Assigned(RangeUnder) do
      begin
        if CheckSubtitleForDynamicSelection(RangeUnder, CursorPosMs, RangeSelWindow, X, Y) then
          Exit;
        RangeUnder := FRangeList.FindNextRange;
      end;

      // Check selection
      if (not SelectionIsEmpty) then
      begin
        RangeSelWindow := PixelToTime(4);
        if (RangeSelWindow < 1) then RangeSelWindow := 1;
        if CheckSubtitleForDynamicSelection(FSelection, CursorPosMs, RangeSelWindow, X, Y) then
          Exit;
      end;
    end;
    
    if SetMinBlankAt(CursorPosMs) then
    begin
      Include(UpdateFlags, uvfRange);
    end;

    Cursor := crIBeam;
    FDynamicEditMode := demNone;
    FDynamicSelRangeOld := FDynamicSelRange;
    FDynamicSelRange := nil;
    if (FDynamicSelRange <> FDynamicSelRangeOld) then
    begin
      Include(UpdateFlags, uvfRange);
    end;
  end;

  UpdateView(UpdateFlags);
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  //TODO : Add user setting for anti-empty-selection
  if ((FSelection.StopTime - FSelection.StartTime) < 80) and
    (not (Assigned(FSelectedRange) or Assigned(FDynamicSelRange) or
    Assigned(FSelectedKaraokeRange))) then
  begin
    ClearSelection;
  end;

  // The selected sub has changed, we need to keep range list sorted
  if FNeedToSortSelectedSub then
  begin
    if Assigned(FOnSelectedRangeChanged) and
       Assigned(FSelectedRange) then
    begin
      FOnSelectedRangeChanged(Self, FRangeOldStart, FRangeOldStop, FNeedToSortSelectedSub);
    end;
    // Make sure we keep the list sorted internally
    FRangeList.FullSort;
    FNeedToSortSelectedSub := False;
  end;

    // TODO : auto clear karoke timing that are out of subtitle bound ???

  if (FDynamicEditMode = demKaraoke) and
     Assigned(FDynamicSelRange) and
     Assigned(FOnKaraokeChanged) then
  begin
    FOnKaraokeChanged(Self, FDynamicSelRange, FDynamicEditTime, FOldKaraokeSubTime);
  end;

  {
  if ((FDynamicEditMode = demStart) or
      (FDynamicEditMode = demStop)) and
     Assigned(FDynamicSelRange) and
     Assigned(FOnSelectedRangeChanged) then
  begin
    FOnSelectedRangeChanged(Self);
  end;
  }

  FSelectionOrigin := -1;
  FScrollOrigin := -1;
  FRangeOldStart := -1;
  FRangeOldStop := -1;
  FOldKaraokeSubTime := -1;
  Cursor := crIBeam;
  MouseCapture := False;
  FMouseIsDown := False;
  FDynamicEditMode := demNone;
  FDynamicSelRange := nil;
  FMinSelTime := -1;
  FMaxSelTime := -1;
  ClipCursor(nil);
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.DblClick;
var idx : Integer;
    Range : TRange;
    MouseCursorPT : TPoint;
    CanvasHeight, y1, y2 : Integer;
begin
  inherited;

  // Disable subtitle selection for SSA mode
  if (not FPeakDataLoaded) or (SelMode = smSSA) then
    Exit;
  idx := FRangeList.GetRangeIdxAt(FCursorMs);
  if (idx = -1) then
    Exit;

  Range := FRangeList[idx];

  // Karaoke subtime selection
  if (System.Length(Range.SubTime) > 0) then
  begin
    Windows.GetCursorPos(MouseCursorPT);
    MouseCursorPT := ScreenToClient(MouseCursorPT);
    CanvasHeight := GetWavCanvasHeight;
    y1 := CanvasHeight div 10;
    y2 := (CanvasHeight * 9) div 10;
    if (MouseCursorPT.Y > y1) and (MouseCursorPT.Y < y2) then
    begin
      FSelectedKaraokeIndex := Range.GetSubTimeRange(FCursorMs, FSelection);
      UpdateView([uvfSelection,uvfRange]);
      if Assigned(FOnSelectionChange) then
        FOnSelectionChange(Self);
      if Assigned(FOnSelectedKaraokeRange) then
        FOnSelectedKaraokeRange(Self, Range);
      FSelectedRange := nil;
      FSelectedKaraokeRange := Range;
      Exit;
    end
  end;

  // Full subtitle selection
  SetSelectedRangeEx(Range, False);
  UpdateView([uvfSelection, uvfRange]);
  if Assigned(FOnSelectedRange) then
    FOnSelectedRange(Self, FSelectedRange, False);
end;

//------------------------------------------------------------------------------

function Single2SmallInt(Value : Single) : SmallInt;
var i : Integer;
begin
  i := Round(Value * 32767);
  if (i > 32767) then i := 32767;
  if (i < -32768) then i := -32768;
  Result := i;
end;

procedure TWAVDisplayer.CreatePeakTab(WAVFile : TWAVFile);
var
    Buffer8 : TByteDynArray;
    Buffer16 : TSmallIntDynArray;
    Buffer32 : TSingleDynArray;
    i, j : Integer;
    PeakMax, PeakMin : SmallInt;
    PeakMax8, PeakMin8 : Byte;
    PeakMax32, PeakMin32 : Single;
    PeakMaxMax, PeakMinMin : SmallInt;
    MaxAbsoluteValue : Integer;
    NormFactor : Double;
begin
  if Assigned(FOnPeakFileCreation) then
    FOnPeakFileCreation(Self,pfcevtStart,0);

  // Get 1 peak value every ~10ms
  FSamplesPerPeak := WAVFile.SamplesPerSecond div 100;
  FPeakTabSize := Ceil((WAVFile.SamplesCount / FSamplesPerPeak) / WAVFile.Channels);
  // Allocate the big peak tab
  FPeakTab := nil;
  SetLength(FPeakTab, FPeakTabSize);

// WAV Data format  Maximum value    Minimum value	    Midpoint value
//     8-bit PCM	  255 (0xFF)       0	                128 (0x80)
//     16-bit PCM	  32,767 (0x7FFF)  - 32,768 (0x8000)	0

  PeakMaxMax := -32768;
  PeakMinMin := 32767;

  if (WAVFile.BitsPerSample = 8) then
  begin
    // Allocate the small buffer
    SetLength(Buffer8, FSamplesPerPeak * WAVFile.Channels);
    for i:=0 to FPeakTabSize-1 do
    begin
      ZeroMemory(Buffer8, FSamplesPerPeak * SizeOf(Shortint) * WAVFile.Channels);
      WAVFile.Read(Buffer8, FSamplesPerPeak * SizeOf(Shortint) * WAVFile.Channels);
      PeakMax8 := 0;
      PeakMin8 := 255;
      for j:=0 to (FSamplesPerPeak * WAVFile.Channels)-1 do
      begin
        if Buffer8[j] > PeakMax8 then
          PeakMax8 := Buffer8[j];
        if Buffer8[j] < PeakMin8 then
          PeakMin8 := Buffer8[j];
      end;
      // Convert 8 bits to 16 bits
      PeakMax := ((PeakMax8 - 128) shl 8);
      PeakMin := ((PeakMin8 - 128) shl 8);

      FPeakTab[i].Max := PeakMax;
      FPeakTab[i].Min := PeakMin;

      if (PeakMax > PeakMaxMax) then
        PeakMaxMax := PeakMax;
      if (PeakMin < PeakMinMin) then
        PeakMinMin := PeakMin;

      if Assigned(FOnPeakFileCreation) then
        FOnPeakFileCreation(Self, pfcevtProgress, (i*100) div Integer(FPeakTabSize));
    end;
  end
  else if (WAVFile.BitsPerSample = 16) then
  begin
    // Allocate the small buffer
    SetLength(Buffer16, FSamplesPerPeak * WAVFile.Channels);
    for i:=0 to FPeakTabSize-1 do
    begin
      ZeroMemory(Buffer16, FSamplesPerPeak * SizeOf(SmallInt) * WAVFile.Channels);
      WAVFile.Read(Buffer16, FSamplesPerPeak * SizeOf(SmallInt) * WAVFile.Channels);
      PeakMax := -32768;
      PeakMin := 32767;
      for j:=0 to (FSamplesPerPeak * WAVFile.Channels)-1 do
      begin
        if Buffer16[j] > PeakMax then
          PeakMax := Buffer16[j];
        if Buffer16[j] < PeakMin then
          PeakMin := Buffer16[j];
      end;
      FPeakTab[i].Max := PeakMax;
      FPeakTab[i].Min := PeakMin;

      if PeakMax > PeakMaxMax then
        PeakMaxMax := PeakMax;
      if PeakMin < PeakMinMin then
        PeakMinMin := PeakMin;

      if Assigned(FOnPeakFileCreation) then
        FOnPeakFileCreation(Self, pfcevtProgress,(i*100) div Integer(FPeakTabSize));
    end;
  end
  else if (WAVFile.BitsPerSample = 32) then
  begin
    // Allocate the small buffer
    SetLength(Buffer32, FSamplesPerPeak * WAVFile.Channels);
    for i:=0 to FPeakTabSize-1 do
    begin
      ZeroMemory(Buffer32, FSamplesPerPeak * SizeOf(SmallInt) * WAVFile.Channels);
      WAVFile.Read(Buffer32, FSamplesPerPeak * SizeOf(Single) * WAVFile.Channels);
      PeakMax32 := -1.0;
      PeakMin32 := 1.0;
      for j:=0 to (FSamplesPerPeak * WAVFile.Channels)-1 do
      begin
        if Buffer32[j] > PeakMax32 then
          PeakMax32 := Buffer32[j];
        if Buffer32[j] < PeakMin32 then
          PeakMin32 := Buffer32[j];
      end;
      // Convert 32 bits float to 16 bits integer
      PeakMax := Single2SmallInt(PeakMax32);
      PeakMin := Single2SmallInt(PeakMin32);

      FPeakTab[i].Max := PeakMax;
      FPeakTab[i].Min := PeakMin;

      if PeakMax > PeakMaxMax then
        PeakMaxMax := PeakMax;
      if PeakMin < PeakMinMin then
        PeakMinMin := PeakMin;

      if Assigned(FOnPeakFileCreation) then
        FOnPeakFileCreation(Self, pfcevtProgress,(i*100) div Integer(FPeakTabSize));
    end;
  end;
  // Calc. normalize factor
  MaxAbsoluteValue := Max(Abs(PeakMaxMax), Abs(PeakMinMin));
  NormFactor := 32768.0 / MaxAbsoluteValue;
  // Normalize peak tab
  NormalizePeakTab(NormFactor);
  Buffer8 := nil;
  Buffer16 := nil;
  if Assigned(FOnPeakFileCreation) then
    FOnPeakFileCreation(Self,pfcevtStop,0);
end;


//------------------------------------------------------------------------------

function TWAVDisplayer.NormalizePeakTab(NormFactor : Double) : Boolean;
var i, MaxAbsoluteValue, Value : Integer;
begin
  Result := False;
  if (NormFactor = -1) then
  begin
    MaxAbsoluteValue := 0;
    // First pass, calculate normalization factor
    for i:=0 to FPeakTabSize-1 do
    begin
      Value := FPeakTab[i].Max;
      if (Value > MaxAbsoluteValue) then
        MaxAbsoluteValue := Value;
      Value := Abs(FPeakTab[i].Min);
      if (Value > MaxAbsoluteValue) then
        MaxAbsoluteValue := Value;
    end;
    NormFactor := 32768.0 / MaxAbsoluteValue;
  end;
  if (NormFactor > 1.1) then
  begin
    // Apply normalization factor
    for i:=0 to FPeakTabSize-1 do
    begin
      FPeakTab[i].Max := Round(FPeakTab[i].Max * NormFactor);
      FPeakTab[i].Min := Round(FPeakTab[i].Min * NormFactor);
    end;
    Result := True;
  end;
end;

//------------------------------------------------------------------------------

function TWAVDisplayer.LoadWAV(filename : WideString) : Boolean;
var PeakFilename : WideString;
    PeakFS : TTntFileStream;
    PeakFileIDRead : string;
    PeakFileVerRead : Cardinal;
    WAVFile : TWAVFile;
    HDRSize : Integer;
    CreatePeakFile : Boolean;
    Normalized : Boolean;
const
    PeakFileID : string = 'PeakFile';
    PeakFileVer : Cardinal = $0100;

    procedure SavePeakFile;
    begin
      PeakFS := TTntFileStream.Create(PeakFilename, fmCreate);
      PeakFS.WriteBuffer(PeakFileID[1], System.Length(PeakFileID));
      PeakFS.WriteBuffer(PeakFileVer, SizeOf(PeakFileVer));
      PeakFS.WriteBuffer(FLengthMs, SizeOf(FLengthMs));
      PeakFS.WriteBuffer(FWavFormat.nSamplesPerSec, SizeOf(FWavFormat.nSamplesPerSec));
      PeakFS.WriteBuffer(FWavFormat.nChannels, SizeOf(FWavFormat.nChannels));
      PeakFS.WriteBuffer(FWavFormat.wBitsPerSample, SizeOf(FWavFormat.wBitsPerSample));
      PeakFS.WriteBuffer(FSamplesPerPeak, SizeOf(FSamplesPerPeak));
      PeakFS.WriteBuffer(FPeakTabSize, SizeOf(FPeakTabSize));
      PeakFS.WriteBuffer(FPeakTab[0], FPeakTabSize*SizeOf(TPeak));
      PeakFS.Free;
    end;
begin
  FPeakDataLoaded := False;
  CreatePeakFile := True;

  // Search for a "peak" file with the same name
  PeakFilename := WideChangeFileExt(filename,'.peak');
  if WideFileExists(PeakFilename) then
  begin
    // TODO : check if the wav file match, if it exists

    // Load peak file
    PeakFS := TTntFileStream.Create(PeakFilename, fmOpenRead or fmShareDenyWrite);

    // Check filesize, we need at least
    HDRSize := System.Length(PeakFileID) + SizeOf(PeakFileVerRead) + SizeOf(FLengthMs) +
      SizeOf(FWavFormat.nSamplesPerSec) + SizeOf(FWavFormat.nChannels) +
      SizeOf(FWavFormat.wBitsPerSample) + SizeOf(FSamplesPerPeak) +
      SizeOf(FPeakTabSize);

    if (PeakFS.Size > HDRSize) then
    begin
      SetLength(PeakFileIDRead, System.Length(PeakFileID));
      PeakFS.ReadBuffer(PeakFileIDRead[1], System.Length(PeakFileID));
      PeakFS.ReadBuffer(PeakFileVerRead, SizeOf(PeakFileVerRead));
      PeakFS.ReadBuffer(FLengthMs, SizeOf(FLengthMs));
      PeakFS.ReadBuffer(FWavFormat.nSamplesPerSec, SizeOf(FWavFormat.nSamplesPerSec));
      PeakFS.ReadBuffer(FWavFormat.nChannels, SizeOf(FWavFormat.nChannels));
      PeakFS.ReadBuffer(FWavFormat.wBitsPerSample, SizeOf(FWavFormat.wBitsPerSample));
      PeakFS.ReadBuffer(FSamplesPerPeak, SizeOf(FSamplesPerPeak));
      PeakFS.ReadBuffer(FPeakTabSize, SizeOf(FPeakTabSize));
      FPeakTab := nil;
      SetLength(FPeakTab, FPeakTabSize);
      PeakFS.Read(FPeakTab[0], FPeakTabSize * SizeOf(TPeak));
      PeakFS.Free;

      Normalized := NormalizePeakTab(-1);
      if Normalized then
      begin
        // rewrite normalized data
        SavePeakFile;
      end;

      CreatePeakFile := False;
    end;
  end;

  if CreatePeakFile then
  begin
    // No peak file
    if not WideFileExists(filename) then
    begin
      Result := False;
      Exit;
    end;
    WAVFile := TWAVFile.Create;
    if (WAVFile.Open(filename) = False) then
    begin
      WAVFile.Free;
      Result := False;
      Exit;
    end;
      
    FLengthMs := WAVFile.Duration;
    FWavFormat := WAVFile.GetWaveFormatEx^;
    // Create the "peak" file
    CreatePeakTab(WAVFile);
    // Save it
    SavePeakFile;
    WAVFile.Close;
    WAVFile.Free;
  end;

  FPageSizeMs := FLengthMs;
  FPositionMs := 0;
  FSelection.StartTime := 0;
  FSelection.StopTime := 0;

  FScrollBar.Min := 0;
  FScrollBar.Max := FLengthMs;
  FScrollBar.Position := 0;
  FScrollBar.PageSize := FLengthMs;

  FPeakDataLoaded := True;

  UpdateView([uvfPageSize]);
  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
  if Assigned(FOnViewChange) then
    FOnViewChange(Self);

  Result := True;
  Cursor := crIBeam;
end;

//------------------------------------------------------------------------------

function TWAVDisplayer.PixelToTime(const Pixel : Integer) : Integer;
begin
  if FPageSizeMs = 0 then
    Result := 0
  else
    Result := Round( Pixel * (FPageSizeMs / Width) );
end;

//------------------------------------------------------------------------------

function TWAVDisplayer.TimeToPixel(const Time : Integer) : Integer;
begin
  if FPageSizeMs = 0 then
    Result := 0
  else
    Result := Round(Time / (FPageSizeMs / Width));
end;

//------------------------------------------------------------------------------

function TWAVDisplayer.GetCursorPos : Integer;
begin
  Result := FCursorMs;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.SetCursorPos(NewPos : Integer);
begin
  Constrain(NewPos, 0, FLengthMs);
  if FCursorMs <> NewPos then
  begin
    FCursorMs := NewPos;
    UpdateView([uvfCursor]);
    if Assigned(FOnCursorChange) then
      FOnCursorChange(Self);
  end;
end;

//------------------------------------------------------------------------------

function TWAVDisplayer.GetPlayCursorPos : Integer;
begin
  Result := FPlayCursorMs;
end;

//------------------------------------------------------------------------------

function TWAVDisplayer.AddRange(NewRange : TRange; UpdateDisplay : Boolean) : Integer;
begin
  Result := FRangeList.Add(NewRange);
  if UpdateDisplay then
    UpdateView([uvfRange]);
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.SetScrollBar(Value : TMiniScrollBar);
begin
  if Value <> FScrollBar then
  begin
    FScrollBar := Value;
    FScrollBar.Min := 0;
    FScrollBar.Max := FLengthMs;
    FScrollBar.Position := 0;
    FScrollBar.PageSize := FLengthMs;
    FScrollBar.OnChange := OnScrollBarChange;    
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.OnScrollBarChange(Sender: TObject);
begin
  SetAutoScroll(False);
  FPositionMs := GetPositionMs;
  UpdateView([uvfPosition]);
  if Assigned(FOnViewChange) then
    FOnViewChange(Self);
end;

//------------------------------------------------------------------------------

// Called only internally when the scroll bar change
function TWAVDisplayer.GetPositionMs : Integer;
begin
  if (FScrollBar.Position + FPageSizeMs - 1) > FLengthMs then
    Result := (FLengthMs - FPageSizeMs)
  else
    Result := FScrollBar.Position;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.SetPositionMs(NewPosition : Integer);
begin
  Constrain(NewPosition, 0, FLengthMs - FPageSizeMs);
  if NewPosition <> FPositionMs then
  begin
    FPositionMs := NewPosition;
    FScrollBar.Position := FPositionMs;
    UpdateView([uvfPosition]);
    if Assigned(FOnViewChange) then
      FOnViewChange(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.SetPageSizeMs(NewPageSize : Integer);
begin
  Constrain(NewPageSize, 0, FLengthMs);
  if FPageSizeMs <> NewPageSize then
  begin
    FPageSizeMs := NewPageSize;
    FPositionMs := GetPositionMs;
    FScrollBar.PageSize := FPageSizeMs;
    UpdateView([uvfPageSize]);
    if Assigned(FOnViewChange) then
      FOnViewChange(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.ZoomRange(const Range : TRange);
var NewPosition, NewPageSize : Integer;
    UpdateFlags : TUpdateViewFlags;
begin
  if (Range.StartTime >= Range.StopTime) then
    Exit;

  NewPageSize := Range.StopTime - Range.StartTime;
  if (NewPageSize < 100) then
  begin
    NewPageSize := 100; // Zoom to 100ms max
    NewPosition := Range.StartTime + ((Range.StopTime - Range.StartTime - NewPageSize) div 2);
  end
  else
  begin
    Constrain(NewPageSize, 0, FLengthMs);
    NewPosition := Range.StartTime;
  end;

  Constrain(NewPosition, 0, FLengthMs - NewPageSize);
  FScrollBar.SetPositionAndPageSize(NewPosition, NewPageSize);

  UpdateFlags := [];
  if (NewPosition <> FPositionMs) or (NewPageSize <> FPageSizeMs) then
  begin
    if(NewPageSize <> FPageSizeMs) then
      Include(UpdateFlags,uvfPageSize);
    FPageSizeMs := NewPageSize;
    if(NewPosition <> FPositionMs) then
      Include(UpdateFlags,uvfPosition);
    FPositionMs := NewPosition;
    UpdateView(UpdateFlags);
    if Assigned(FOnViewChange) then
      FOnViewChange(Self);    
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.ZoomRange(const Start, Stop : Integer);
var Range : TRange;
begin
  Range := TRange.Create;
  Range.StartTime := Start;
  Range.StopTime := Stop;
  ZoomRange(Range);
  Range.Free;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.ZoomCenteredOn(const Center, PageSize : Integer);
var Range : TRange;
begin
  Range := TRange.Create;
  Range.StartTime := Center - (PageSize div 2);
  Range.StopTime :=  Center + (PageSize div 2);
  ZoomRange(Range);
  Range.Free;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.ZoomAll;
var Range : TRange;
begin
  Range := TRange.Create;
  Range.StartTime := 0;
  Range.StopTime := FLengthMs;
  ZoomRange(Range);
  Range.Free;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.ZoomIn;
var Range : TRange;
begin
  // Zoom on (PageSize / 3) centered
  Range := TRange.Create;
  Range.StartTime := FPositionMs + Round(FPageSizeMs / 3);
  Range.StopTime := FPositionMs + Round((FPageSizeMs / 3) * 2);
  ZoomRange(Range);
  Range.Free;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.ZoomOut;
var Range : TRange;
begin
  // Zoom on (3 * PageSize) centered
  Range := TRange.Create;
  Range.StartTime := FPositionMs - FPageSizeMs;
  Range.StopTime := FPositionMs + (FPageSizeMs * 2);
  ZoomRange(Range);
  Range.Free;  
end;

//------------------------------------------------------------------------------

function NoKeyModifierIn(Shift: TShiftState) : Boolean;
begin
  Result := not ((ssShift in Shift) or (ssAlt in Shift) or (ssCtrl in Shift));
end;

function TWAVDisplayer.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
type
  ByteSet = Set of 0..7;
var
  RelativePos : TPoint;
  Range : TRange;
  NewPageSize, MouseCursorPosTime, NewMouseCursorPosTime : Integer;
  bShift: ByteSet absolute Shift;
  ScrollAmount : Integer;
begin
  Result := True;
  RelativePos := ScreenToClient(MousePos);
  if not InRange(RelativePos.X, 0, Width) then Exit;

  // default:  
  // Ctrl + Wheel  = Horizontal Zoom
  // Shift + Wheel = Vertical Zoom
  // Wheel only    = Time scrolling
  if ((FWheelHZoom = mwmNone) and NoKeyModifierIn(Shift)) or (Ord(FWheelHZoom) in bShift) then
  begin
    NewPageSize := Round(FPageSizeMs * 80 / 100);

    MouseCursorPosTime := PixelToTime(RelativePos.X) + FPositionMs;
    // convert pixel to time in new time representation
    NewMouseCursorPosTime := Round( RelativePos.X * (NewPageSize / Width) );

    Range := TRange.Create;
    Range.StartTime := MouseCursorPosTime - NewMouseCursorPosTime;
    Range.StopTime := MouseCursorPosTime + (NewPageSize - NewMouseCursorPosTime);
    ZoomRange(Range);
    FreeAndNil(Range);
  end
  else if ((FWheelVZoom = mwmNone) and NoKeyModifierIn(Shift)) or (Ord(FWheelVZoom) in bShift) then
  begin
    if InRange(FVerticalScaling, 0, 395) then
      Inc(FVerticalScaling, 5);
  end
  else if ((FWheelTimeScroll = mwmNone) and NoKeyModifierIn(Shift)) or (Ord(FWheelTimeScroll) in bShift) then
  begin
    ScrollAmount := Round(FPageSizeMs / 4); // scroll amount = 1/4 of visible interval
    if (ScrollAmount = 0) then ScrollAmount := 1;
    SetPositionMs(FPositionMs - ScrollAmount);
  end
  else
    Exit;

  UpdateView([uvfPageSize]);
end;

//------------------------------------------------------------------------------

function TWAVDisplayer.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
type
  ByteSet = Set of 0..7;
var
  RelativePos : TPoint;
  Range : TRange;
  NewPageSize, MouseCursorPosTime, NewMouseCursorPosTime : Integer;
  bShift: ByteSet absolute Shift;
  ScrollAmount : Integer;
begin
  Result := True;
  RelativePos := ScreenToClient(MousePos);
  if not InRange(RelativePos.X, 0, Width) then Exit;

  // default:
  // Ctrl + Wheel  = Horizontal Zoom
  // Shift + Wheel = Vertical Zoom
  // Wheel only    = Time scrolling

  if ((FWheelHZoom = mwmNone) and NoKeyModifierIn(Shift)) or (Ord(FWheelHZoom) in bShift) then
  begin
    NewPageSize := Round(FPageSizeMs * 125 / 100);

    MouseCursorPosTime := PixelToTime(RelativePos.X) + FPositionMs;
    // convert pixel to time in new time representation
    NewMouseCursorPosTime := Round( RelativePos.X * (NewPageSize / Width) );

    Range := TRange.Create;
    Range.StartTime := MouseCursorPosTime - NewMouseCursorPosTime;
    Range.StopTime := MouseCursorPosTime + (NewPageSize - NewMouseCursorPosTime);
    ZoomRange(Range);
    FreeAndNil(Range);
  end
  else if ((FWheelVZoom = mwmNone) and NoKeyModifierIn(Shift)) or (Ord(FWheelVZoom) in bShift) then
  begin
    if InRange(FVerticalScaling, 5, 400) then
      Dec(FVerticalScaling, 5);
  end
  else if ((FWheelTimeScroll = mwmNone) and NoKeyModifierIn(Shift)) or (Ord(FWheelTimeScroll) in bShift) then
  begin
    ScrollAmount := Round(FPageSizeMs / 4); // scroll amount = 1/4 of visible interval
    if (ScrollAmount = 0) then ScrollAmount := 1;
    SetPositionMs(FPositionMs + ScrollAmount);
  end
  else
    Exit;
    
  UpdateView([uvfPageSize]); // Refresh waveform
end;

//------------------------------------------------------------------------------


procedure TWAVDisplayer.DeleteRangeAt(const Pos : Integer; const UpdateDisplay : Boolean);
var Idx : Integer;
begin
  Idx := FRangeList.GetRangeIdxAt(Pos);
  if Idx <> -1 then
  begin
    DeleteRangeAtIdx(Idx);
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.DeleteRangeAtIdx(const Idx : Integer; const UpdateDisplay : Boolean);
begin
  if (Idx < 0) or (Idx >= FRangeList.Count) then
    Exit;
  if FRangeList[Idx] = FSelectedRange then
  begin
    SetSelectedRangeEx(nil, False);
  end;
  FRangeList.Delete(Idx);
  if (UpdateDisplay) then
    UpdateView([uvfRange]);
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.PlayRange(const Range : TRange; const Loop : Boolean);
begin
  PlayRange(Range.StartTime,Range.StopTime, Loop);
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.PlayRange(const Start, Stop : Integer; const Loop : Boolean);
begin
  if Assigned(FOnStartPlaying) then
    FOnStartPlaying(Self);
  FRenderer.OnStopPlaying := InternalOnStopPlaying;
  FRenderer.PlayRange(Start, Stop, Loop);
  FUpdateCursorTimer.Enabled := True;
  FIsPlaying := True;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.UpdatePlayRange(const Start, Stop : Integer);
begin
  if FIsPlaying then
    FRenderer.UpdatePlayRange(Start,Stop);
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.Stop;
begin
  FRenderer.Stop;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.InternalOnStopPlaying(Sender : TObject);
begin
  FUpdateCursorTimer.Enabled := False;
  FIsPlaying := False;
  UpdateView([uvfPlayCursor]); // hide the play cursor
  if Assigned(FOnStopPlaying) then
    FOnStopPlaying(Self);
end;

//------------------------------------------------------------------------------

function TWAVDisplayer.SelectionIsEmpty : Boolean;
begin
  Result := (FSelection.StopTime = 0);  
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.SetPlayCursorPos(NewPos : Integer);
begin
  Constrain(NewPos, 0, FLengthMs);
  if FPlayCursorMs <> NewPos then
  begin
    FOldPlayCursorMs := FPlayCursorMs;
    FPlayCursorMs := NewPos;
    if Assigned(FOnPlayCursorChange) then
      FOnPlayCursorChange(Self);
    // TODO : maybe we could do a nice smooth scrolling
    if (FAutoScrolling = True) and
       (FMouseIsDown = False) and
       ((NewPos < FPositionMs) or (FPlayCursorMs > (FPositionMs + FPageSizeMs))) then
    begin
      SetPositionMs(NewPos - (FPageSizeMs div 10))
    end
    else
      UpdateView([uvfPlayCursor])
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.OnUpdateCursor(Sender : TObject);
begin
  SetPlayCursorPos(FRenderer.GetPosition);
end;

//------------------------------------------------------------------------------

function TWAVDisplayer.RedirectedMousewheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint) : Boolean;
var ClientPos : TPoint;
begin
  ClientPos := Self.ScreenToClient(MousePos);
  if (WheelDelta <> 0) and PtInRect(Self.ClientRect,ClientPos) then
  begin
    if (WheelDelta > 0) then
      DoMouseWheelUp(Shift,MousePos)
    else
      DoMouseWheelDown(Shift,MousePos);
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.SetSelectedRange(Value : TRange);
begin
  SetSelectedRangeEx(Value);
end;

//------------------------------------------------------------------------------

function TWAVDisplayer.SetMinBlankOnIdx(Idx : Integer) : Boolean;
var ChangedInStep : Boolean;
begin
  Result := False;

  if (Idx > 0) then
  begin
    ChangedInStep := FMinBlankInfo1.SetInfo(FRangeList[Idx - 1], mbipStop);
  end
  else
  begin
    ChangedInStep := FMinBlankInfo1.SetInfo(nil, mbipInvalid);
  end;
  Result := Result or ChangedInStep;

  if (Idx < FRangeList.Count-1) then
  begin
    ChangedInStep := FMinBlankInfo2.SetInfo(FRangeList[Idx + 1], mbipStart);
  end
  else
  begin
    ChangedInStep := FMinBlankInfo2.SetInfo(nil, mbipInvalid);
  end;
  Result := Result or ChangedInStep;
end;

//------------------------------------------------------------------------------

function TWAVDisplayer.SetMinBlankAt(TimeMs : Integer) : Boolean;
var idx : Integer;
    ChangedInStep : Boolean;
begin
  Result := False;

  idx := FRangeList.GetRangeIdxAt(TimeMs);
  if (idx <> -1) then
  begin
    ChangedInStep := SetMinBlankOnIdx(idx);
    Result := Result or ChangedInStep;
  end
  else
  begin
    idx := FRangeList.FindInsertPos(TimeMs, -1);
    if (idx > 0) then
    begin
      ChangedInStep := FMinBlankInfo1.SetInfo(FRangeList[Idx - 1], mbipStop);
    end
    else
    begin
      ChangedInStep := FMinBlankInfo1.SetInfo(nil, mbipInvalid);
    end;
    Result := Result or ChangedInStep;

    if (idx < FRangeList.Count) then
    begin
      ChangedInStep := FMinBlankInfo2.SetInfo(FRangeList[Idx], mbipStart);
    end
    else
    begin
      ChangedInStep := FMinBlankInfo2.SetInfo(nil, mbipInvalid);
    end;
    Result := Result or ChangedInStep;
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.SetSelectedRangeEx(Value : TRange; UpdateDisplay : Boolean);
begin
  // Make sure to reset selection if Value is nil
  if (Value = nil) then
  begin
    FSelection.StartTime := 0;
    FSelection.StopTime := 0;
  end;
  
  if (FSelectedRange <> Value) then
  begin
    FSelectedRange := Value;
    if (FSelectedRange <> nil) then
    begin
      FSelection.StartTime := FSelectedRange.StartTime;
      FSelection.StopTime := FSelectedRange.StopTime;
      SetMinBlankAt(FSelectedRange.StartTime);
    end;
    
    if UpdateDisplay then
      UpdateView([uvfSelection, uvfRange]);
    if Assigned(FOnSelectionChange) then
      FOnSelectionChange(Self);
    //if Assigned(FOnSelectedRange) then
    //  FOnSelectedRange(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.ZoomAndSelectRange(const Range : TRange);
var TmpRange : TRange;
    RangeLen : Integer;
    DisplayLen : Integer;
    OldPos, OldPageSize : Integer;
const
    MinDisplayLen : Integer = 10000; // display minimum 10 seconds
begin
  TmpRange := TRange.Create;
  RangeLen := Range.StopTime - Range.StartTime;
  DisplayLen := Max(RangeLen, MinDisplayLen);
  DisplayLen := Round(DisplayLen * 1.2);

  TmpRange.StartTime := Range.StartTime + (RangeLen div 2) - (DisplayLen div 2);
  TmpRange.StopTime := Range.StartTime + (RangeLen div 2) + (DisplayLen div 2);

  OldPageSize := FPageSizeMs;
  OldPos := FPositionMs;
  SetSelectedRangeEx(Range, False);
  ZoomRange(TmpRange);
  if (OldPageSize = FPageSizeMs) and (OldPos = FPositionMs) then
    UpdateView([uvfSelection,uvfRange]);
  TmpRange.Free;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.SetRenderer(Renderer : TRenderer);
var WasPaused : Boolean;
begin
  WasPaused := False;
  if FIsPlaying then
  begin
    WasPaused := (not FUpdateCursorTimer.Enabled);
    FRenderer.Pause;
    Renderer.CopyState(FRenderer);
    FRenderer.Stop(False);
  end;
  if (FRenderer <> Renderer) then
  begin
    FRenderer := Renderer;
  end;
  if FIsPlaying and not WasPaused then
    FRenderer.Resume;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.ClearSelection;
begin
  SetSelectedRangeEx(nil, False);
  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
  UpdateView([uvfSelection,uvfRange]);
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.SetAutoScroll(const Value : Boolean);
begin
  if (Value <> FAutoScrolling) then
  begin
    FAutoScrolling := Value;
    if Assigned(FOnAutoScrollChange) then
      FOnAutoScrollChange(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.Close;
begin
  FPeakDataLoaded := False;
  FPeakTab := nil;
  FPeakTabSize := 0;
  FSamplesPerPeak := 0;

  FPageSizeMs := 0;
  FPositionMs := 0;
  FSelection.StartTime := 0;
  FSelection.StopTime := 0;
  SetSelectedRangeEx(nil, False);
  FCursorMs := 0;
  FPlayCursorMs := 0;

  ZeroMemory(@FWavFormat,SizeOf(FWavFormat));

  FScrollBar.Min := 0;
  FScrollBar.Max := 0;
  FScrollBar.Position := 0;
  FScrollBar.PageSize := 0;

  UpdateView([uvfPageSize]);
  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
  if Assigned(FOnViewChange) then
    FOnViewChange(Self);
  if Assigned(FOnCursorChange) then
    FOnCursorChange(Self);

  Cursor := crDefault;
end;

//------------------------------------------------------------------------------

function TWAVDisplayer.GetWAVAverageBytePerSecond : Integer;
begin
  Result := FWavFormat.nSamplesPerSec * FWavFormat.nChannels *
    (FWavFormat.wBitsPerSample div 8);
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.SetVerticalScaling(Value : Integer);
begin
  Constrain(Value, 1, 800);
  if (Value <> FVerticalScaling) then
  begin
    FVerticalScaling := Value;
    UpdateView([uvfPageSize]); // redraw all
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  inherited DoContextPopup(MousePos, Handled);
  // Because R_Click was used for other duties
  Handled := Handled or ((FSelMode = smSSA) and (GetKeyState(VK_SHIFT) >= 0));
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.SelectNextKaraoke;
var Idx : Integer;
    Range : TRange;
begin
  if Assigned(KaraokeSelectedRange) then
  begin
    if (KaraokeSelectedIndex < System.Length(KaraokeSelectedRange.SubTime)) then
    begin
      SelecteKaraoke(FSelectedKaraokeRange, FSelectedKaraokeIndex + 1);
    end
    else
    begin
      // Go to next sub and select karaokeindex 0
      Idx := FRangeList.IndexOf(FSelectedKaraokeRange) + 1;
      if (Idx < FRangeList.Count) then
      begin
        Range := FRangeList[Idx];
        if (System.Length(Range.SubTime) > 0) then
        begin
          SelecteKaraoke(Range, 0);
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.SelectPreviousKaraoke;
var Idx : Integer;
    Range : TRange;
begin
  if Assigned(FSelectedKaraokeRange) then
  begin
    if (KaraokeSelectedIndex > 0) then
    begin
      SelecteKaraoke(FSelectedKaraokeRange, FSelectedKaraokeIndex - 1);
    end
    else
    begin
      // Go to previous sub and select last karaokeindex
      Idx := FRangeList.IndexOf(FSelectedKaraokeRange) - 1;
      if (Idx >= 0) then
      begin
        Range := FRangeList[Idx];
        if (System.Length(Range.SubTime) > 0) then
        begin
          SelecteKaraoke(Range, System.Length(Range.SubTime));
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.SelecteKaraoke(Range : TRange; Index : Integer);
begin
  FSelectedKaraokeIndex := Index;
  FSelectedKaraokeRange := Range;
  if Assigned(Range) then
  begin
    FSelectedKaraokeRange.GetSubTimeRangeAt(FSelectedKaraokeIndex, FSelection);
  end;
  UpdateView([uvfSelection,uvfRange]);
  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
  if Assigned(FOnSelectedKaraokeRange) then
    FOnSelectedKaraokeRange(Self, FSelectedKaraokeRange);
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.Resize;
begin
  inherited;
  // Fix repaint bug
  Repaint;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.Pause;
begin
  if FUpdateCursorTimer.Enabled then
  begin
    FUpdateCursorTimer.Enabled := False;
    FRenderer.Pause;
  end
  else
  begin
    FUpdateCursorTimer.Enabled := True;
    FRenderer.Resume;
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.Scroll(ViewPercent : Integer);
var ScrollOffsetMs : Integer;
begin
  ScrollOffsetMs := Round(FPageSizeMs / 100.0 * ViewPercent);
  SetPositionMs(GetPositionMs + ScrollOffsetMs);
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.SetSceneChangeList(SceneChangeList : TIntegerDynArray);
begin
  SetLength(FSceneChangeList, System.Length(SceneChangeList));
  CopyMemory(@FSceneChangeList[0], @SceneChangeList[0],
    System.Length(SceneChangeList) * SizeOf(Integer));
end;

//------------------------------------------------------------------------------
end.
//------------------------------------------------------------------------------

