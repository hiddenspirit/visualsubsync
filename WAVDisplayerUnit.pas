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
  Math, ExtCtrls, Renderer, MiniScrollBarUnit, MMSystem;

type
  // ----------

  TRange = class
  public
    StartTime : Integer;
    StopTime : Integer;
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
    function GetItem(const Index: Integer) : TRange;
    function GetCount : Integer;

  public
    constructor Create;
    destructor Destroy; override;
    function Add(Range : TRange) : Integer;
    function AddAndReturnSibling(Range : TRange) : TRange;    
    procedure AddAtEnd(Range : TRange);
    procedure FullSort;
    function FindInsertPos(Range : TRange) : Integer;
    function FindInsertPosSS(Start,Stop : Integer) : Integer;  
    function GetRangeIdxAt(PosMs : Integer) : Integer;
    procedure Delete(Index : Integer);
    function IndexOf(Range : TRange) : Integer;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Clear;
    function FindFirstRangeAt(PosMs : Integer) : TRange;
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

  TDynamicEditMode = (demNone, demStart, demEnd);

  TWAVDisplayer = class(TCustomPanel)
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
    FVerticalScaling : Integer; // 1..400%

    FOldPositionMs : Integer; // Used to optimize drawing
    FOldPageSizeMs : Integer;    

    FSelection : TRange;
    FSelectionOrigin : Integer;
    FSelMode : TSelectionMode;
    FScrollOrigin : Integer;
    FSelectedRange : TRange;

    FScrollBar : TMiniScrollBar;

    FOnCursorChange : TNotifyEvent;
    FOnPlayCursorChange : TNotifyEvent;
    FOnSelectionChange : TNotifyEvent;
    FOnViewChange : TNotifyEvent;
    FOnSelectedRange : TNotifyEvent;
    FOnSelectedRangeChange : TNotifyEvent;
    FOnPeakFileCreation : TPeakFileCreationEvent;
    FOnAutoScrollChange : TNotifyEvent;

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

    procedure PaintWavOnCanvas(ACanvas : TCanvas; TryOptimize : Boolean);
    procedure PaintOnCanvas(ACanvas : TCanvas);
    procedure PaintRulerOnCanvas(ACanvas : TCanvas);

    procedure CreatePeakTab(WAVFile : TWAVFile);
    function PixelToTime(Pixel : Integer) : Integer;
    function TimeToPixel(Time : Integer) : Integer;
    procedure SetScrollBar(Value : TMiniScrollBar);
    procedure OnScrollBarChange(Sender: TObject);
    function GetPositionMs : Cardinal;
    procedure OnStopPlaying(Sender : TObject);
    procedure OnUpdateCursor(Sender : TObject);

    procedure SetSelectedRange(Value : TRange);    
    procedure SetSelectedRangeEx(Value : TRange; UpdateDisplay : Boolean = True);
    procedure SetPlayCursorPos(NewPos : Integer);
    procedure SetAutoScroll(Value : Boolean);
    procedure SetVerticalScaling(Value : Integer);

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
    
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadWAV(filename : string) : Boolean;
    procedure Close;
    function GetCursorPos : Integer;
    procedure SetCursorPos(NewPos : Integer);
    function GetPlayCursorPos : Integer;
    function AddRange(NewRange : TRange; UpdateDisplay : Boolean = True) : Integer;
    procedure ZoomRange(Range : TRange); overload;
    procedure ZoomRange(Start, Stop : Integer); overload;
    procedure ZoomCenteredOn(Center, PageSize : Integer);
    procedure ZoomAll;
    procedure ZoomIn;
    procedure ZoomOut;
    procedure ZoomAndSelectRange(Range : TRange);
    procedure SetPageSizeMs(NewPageSize : Integer);
    procedure SetPositionMs(NewPosition : Integer);
    procedure DeleteRangeAt(Pos : Integer; UpdateDisplay : Boolean = True);
    procedure DeleteRangeAtIdx(Idx : Integer; UpdateDisplay : Boolean = True);    

    procedure PlayRange(Range : TRange; Loop : Boolean = False); overload;
    procedure PlayRange(Start, Stop : Integer; Loop : Boolean = False); overload;
    procedure UpdatePlayRange(Start, Stop : Integer);
    procedure Stop;
    function SelectionIsEmpty : Boolean;
    procedure UpdateView(UpdateViewFlags : TUpdateViewFlags);
    function RedirectedMousewheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint) : Boolean;
    procedure SetRenderer(Renderer : TRenderer);
    procedure ClearSelection;
    function GetWAVAverageBytePerSecond : Integer;

    property RangeList : TRangeList read FRangeList;
    property SelectedRange : TRange read FSelectedRange write SetSelectedRange;
    property Selection : TRange read FSelection;

  published
    { Published declarations }
    property OnCursorChange : TNotifyEvent read FOnCursorChange write FOnCursorChange;
    property OnPlayCursorChange : TNotifyEvent read FOnPlayCursorChange write FOnPlayCursorChange;
    property OnSelectionChange : TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnViewChange : TNotifyEvent read FOnViewChange write FOnViewChange;
    property OnSelectedRange : TNotifyEvent read FOnSelectedRange write FOnSelectedRange;
    property OnSelectedRangeChange : TNotifyEvent read FOnSelectedRangeChange write FOnSelectedRangeChange;    
    property OnPeakFileCreation : TPeakFileCreationEvent read FOnPeakFileCreation write FOnPeakFileCreation;
    property OnAutoScrollChange : TNotifyEvent read FOnAutoScrollChange write FOnAutoScrollChange;

    property Align;
    property Anchors;
    property AutoScrolling : Boolean read FAutoScrolling write SetAutoScroll;
    property IsPlaying : Boolean read FIsPlaying;
    property Enabled;
    property Length : Integer read FLengthMs;
    property PageSize : Integer read FPageSizeMs;
    property PopupMenu;
    property Position : Integer read FPositionMs;
    property SelMode : TSelectionMode read FSelMode write FSelMode default smCoolEdit;
    property VerticalScaling : Integer read FVerticalScaling write SetVerticalScaling;
    property Visible;
    property WheelTimeScroll : TMouseWheelModifier read FWheelTimeScroll write FWheelTimeScroll default mwmNone;
    property WheelVZoom : TMouseWheelModifier read FWheelVZoom write FWheelVZoom default mwmShift;
    property WheelHZoom : TMouseWheelModifier read FWheelHZoom write FWheelHZoom default mwmCtrl;
  end;

function CompareRanges(R1, R2: TRange): Integer;

implementation

uses SysUtils, Types, MiscToolsUnit;

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

function TRangeList.Add(Range : TRange) : Integer;
begin
  Result := FindInsertPos(Range);
  FList.Insert(Result,Range);
end;

//------------------------------------------------------------------------------

function TRangeList.AddAndReturnSibling(Range : TRange) : TRange;
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

procedure TRangeList.AddAtEnd(Range : TRange);
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

function TRangeList.FindInsertPos(Range : TRange) : Integer;
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

function TRangeList.FindInsertPosSS(Start,Stop : Integer) : Integer;
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

function TRangeList.GetRangeIdxAt(PosMs : Integer) : Integer;
var r : TRange;
    i : Integer;
begin
  Result := -1;
  i := FindInsertPosSS(PosMs,-1);
  Constrain(i,0,FList.Count-1);
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

procedure TRangeList.Delete(Index : Integer);
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

function TRangeList.IndexOf(Range : TRange) : Integer;
begin
  Result := FList.IndexOf(Range);
end;

//------------------------------------------------------------------------------

procedure TRangeList.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex,NewIndex);
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

function TRangeList.FindFirstRangeAt(PosMs : Integer) : TRange;
begin
  FSearchStartAt := PosMs;
  FSearchIdx := FindInsertPosSS(FSearchStartAt, -1);
  Constrain(FSearchIdx, 0, FList.Count-1);
  while (FSearchIdx >= 0) and
        (TRange(FList[FSearchIdx]).StopTime > FSearchStartAt) do
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
  while (FSearchIdx < FList.Count) do
  begin
    Range := FList[FSearchIdx];
    Inc(FSearchIdx);
    if(Range.StartTime > FSearchStartAt) then
      Exit
    else if (Range.StartTime <= FSearchStartAt) and
            (Range.StopTime >= FSearchStartAt) then
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
  FDynamicEditMode := demNone;

  FOffscreen := TBitmap.Create;
  FOffscreenWAV := TBitmap.Create;
  FRangeList := TRangeList.Create;
  FSelection := TRange.Create;
  FSelection.StartTime := 0;
  FSelection.StopTime := 0;

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
    PeakMax, PeakMin : Smallint;
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
  ACanvas.Brush.Color := clBlack;
  ACanvas.FillRect(Rect);

  // Wave
  ACanvas.Pen.Color := $00A7F24A;
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
  ACanvas.Pen.Color := $00518B0A;
  ACanvas.MoveTo(0, Middle);
  ACanvas.LineTo(Width, Middle);
end;

//------------------------------------------------------------------------------

function TimeMsToShortString(TimeMS: Cardinal; Precision : Cardinal) : string;
var
  min, sec, ms: Cardinal;
begin
  ms := TimeMs div 1000;
  min := ms div 60;
  sec := ms mod 60;
  ms := (TimeMs - (min * 60 * 1000) - (sec * 1000)) div Precision;
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

procedure TWAVDisplayer.PaintOnCanvas(ACanvas : TCanvas);
var x : Integer;
    x1, x2, y1, y2 : Integer;
    i : Integer;
    r : TRange;
    SelRect : TRect;
    CanvasHeight : Integer;
begin
  CanvasHeight := Height - FScrollBar.Height;
  if FDisplayRuler then
    CanvasHeight := CanvasHeight - FDisplayRulerHeight;

  y1 := CanvasHeight div 10;
  y2 := (CanvasHeight * 9) div 10;

  ACanvas.Brush.Color := clBlack;

  // TODO : this is very slow when lot's of range are on screen
  // We should do this in 2 pass to group ranges, and use another color
  // Why dotted line are so slow to draw :(
  // use scanline to draw dotted line, or maybe we can draw line at 50% transparency

  // Range
  ACanvas.Pen.Color := clRed;
  ACanvas.Pen.Style := psDot;
  ACanvas.Pen.Mode := pmCopy;
  ACanvas.Brush.Style := bsClear;
  for i:=0 to FRangeList.Count-1 do
  begin
    r := FRangeList[i];
    x1 := -1; x2 := -1;
    if (i mod 2) = 0 then
      ACanvas.Pen.Color := $003333FF
    else
      ACanvas.Pen.Color := $00FF8000;
    if (r.StartTime >= FPositionMs) and (r.StartTime <= FPositionMs + FPageSizeMs) then
    begin
      x1 := TimeToPixel(r.StartTime - FPositionMs);
    end;
    if (r.StopTime >= FPositionMs) and (r.StopTime <= FPositionMs + FPageSizeMs) then
    begin
      x2 := TimeToPixel(r.StopTime - FPositionMs);
    end;

    if (x1 <> -1) then
    begin
      ACanvas.MoveTo(x1, 0);
      ACanvas.LineTo(x1, CanvasHeight);
    end;

    if (x2 <> -1) and (x2 <> x1) then
    begin
      ACanvas.MoveTo(x2, 0);
      ACanvas.LineTo(x2, CanvasHeight);
    end;

    if (r.StartTime < FPositionMs) and (r.StopTime > FPositionMs + FPageSizeMs) then
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
      ACanvas.Pen.Style := psDot;
    end;
  end;

  // Selection
  ACanvas.Pen.Color := clWhite;
  ACanvas.Pen.Mode := pmXor;
  ACanvas.Pen.Style := psSolid;
  if (FSelection.StopTime > 0) then
  begin
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
      if x1 <> x2 then
      begin
        SelRect := ClientRect;
        SelRect.Left := x1;
        SelRect.Right := x2+1;
        SelRect.Bottom := SelRect.Bottom - FScrollBar.Height;
        if FDisplayRuler then
          SelRect.Bottom := SelRect.Bottom - FDisplayRulerHeight;
        ACanvas.CopyMode := cmDstInvert;
        ACanvas.CopyRect(SelRect, ACanvas, SelRect);
        ACanvas.CopyMode := cmSrcCopy;
      end;
    end;
  end;

  // Cursor
  if (FCursorMs >= FPositionMs) and (FCursorMs <= FPositionMs + FPageSizeMs) then
  begin
    ACanvas.Pen.Color := clYellow;
    ACanvas.Pen.Style := psDot;
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
    ACanvas.Brush.Color := $514741;
    ACanvas.FillRect(PosRect);

    // Draw horizontal line at top and bottom
    ACanvas.Pen.Mode := pmCopy;  
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Color := $BEB5AE;
    ACanvas.MoveTo(0, PosRect.Top);
    ACanvas.LineTo(Width, PosRect.Top);
    ACanvas.MoveTo(0, PosRect.Bottom-1);
    ACanvas.LineTo(Width, PosRect.Bottom-1);

    // Set the text font
    ACanvas.Pen.Color := $E0E0E0; 
    ACanvas.Pen.Style := psSolid;
    ACanvas.Font.Name := 'Time New Roman';
    ACanvas.Font.Color := $E0E0E0;
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
      ACanvas.Font.Color := clBlack;
      ACanvas.TextOut(x1+2, PosRect.Top + 4 + 2, PosString);
      // Draw text
      ACanvas.Font.Color := $E0E0E0;
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
    FOffscreen.Canvas.Brush.Color := clGray;
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
  //Canvas.Lock;
  Canvas.Draw(0, 0, FOffscreen);
  //Canvas.Unlock;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var NewCursorPos : Integer;
    UpdateFlags : TUpdateViewFlags;
begin
  inherited;
  UpdateFlags := [];

  if (ssDouble in Shift) or (not FPeakDataLoaded) then
    Exit;

  FMouseIsDown := True;    

  Case FSelMode of
  smCoolEdit :
    begin
      if (ssLeft in Shift) then
      begin
        if not InRange(X, 0, Width) then Exit;
        NewCursorPos := PixelToTime(X) + FPositionMs;

        if (ssShift in Shift) or (FDynamicEditMode <> demNone) then
        begin
          if Assigned(FDynamicSelRange) then
          begin
            SetSelectedRangeEx(FDynamicSelRange,False);
            Include(UpdateFlags, uvfSelection);
          end;
          
          // Selection modification using shift key
          if NewCursorPos > FSelection.StartTime + ((FSelection.StopTime - FSelection.StartTime) div 2) then
          begin
            // We are close to the end of the selection
            if SelectionIsEmpty then
            begin
              if NewCursorPos > FCursorMs then
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
            // TODO : we need to keep range list sorted (or maybe we do it when we unselect)
            FSelectedRange.StartTime := FSelection.StartTime;
            FSelectedRange.StopTime := FSelection.StopTime;
            Include(UpdateFlags, uvfRange);
            if Assigned(FOnSelectedRangeChange) then
              FOnSelectedRangeChange(Self);
          end;
          if Assigned(FOnSelectionChange) then
            FOnSelectionChange(Self);
          Include(UpdateFlags, uvfSelection);
        end else begin
          if FSelection.StartTime <> FSelection.StopTime then
            Include(UpdateFlags, uvfSelection); // clear selection
          FSelectedRange := nil;
          FSelectionOrigin := NewCursorPos;
          FSelection.StartTime := 0;
          FSelection.StopTime := 0;
          if Assigned(FOnSelectionChange) then
            FOnSelectionChange(Self);
        end;
        if (FCursorMs <> NewCursorPos) then
        begin
          FCursorMs := NewCursorPos;
          if Assigned(FOnCursorChange) then
            FOnCursorChange(Self);
          Include(UpdateFlags, uvfCursor);
        end;
      end;
    end;
  smSSA :
    begin
      if not Inrange(X, 0, Width) then Exit;
      NewCursorPos := PixelToTime(X) + FPositionMs;

      if ssLeft in Shift then
      begin
        if SelectionIsEmpty then
        begin
          if NewCursorPos < FCursorMs then
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
          if NewCursorPos < FSelection.StopTime then
          begin
            FSelection.StartTime := NewCursorPos;
            FSelectionOrigin := FSelection.StopTime;
          end
          else
          begin
            FSelectedRange := nil;
            FSelectionOrigin := NewCursorPos;
            FSelection.StartTime := 0;
            FSelection.StopTime := 0;
            if Assigned(FOnSelectionChange) then
              FOnSelectionChange(Self);
          end;
        end;
      end;

      if (ssRight in Shift) and not(ssShift in Shift) then
      begin
        if SelectionIsEmpty then
        begin
          if NewCursorPos > FCursorMs then
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
          if NewCursorPos > FSelection.StartTime then
          begin
            FSelection.StopTime := NewCursorPos;
            FSelectionOrigin := FSelection.StartTime;
          end
          else
          begin
            FSelectedRange := nil;
            FSelectionOrigin := NewCursorPos;
            FSelection.StartTime := 0;
            FSelection.StopTime := 0;
            if Assigned(FOnSelectionChange) then
              FOnSelectionChange(Self);
          end;
        end;
      end;

      if (ssRight in Shift) or (ssLeft in Shift) then
      begin
        if Assigned(FSelectedRange) then
        begin
          // TODO : we need to keep range list sorted (or maybe we do it when we unselect)
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
  end; // case

  if ssMiddle in Shift then // Middle button = precision time scrolling activated
  begin
    Cursor := crHandPoint;
    MouseCapture := True;
    FScrollOrigin := PixelToTime(X);
  end;

  UpdateView(UpdateFlags);
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.MouseMove(Shift: TShiftState; X, Y: Integer);
var NewCursorPos, CursorPosMs, RangeSelWindow : Integer;
    ScrollDiff : Integer;
    DiffMuliplier : Integer;
    UpdateFlags : TUpdateViewFlags;
    RangeUnder : TRange;
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
          Constrain(X,0,Width);
          NewCursorPos := PixelToTime(X) + FPositionMs;
          if (FCursorMs <> NewCursorPos) then
          begin
            FCursorMs := NewCursorPos;

            if FSelectionOrigin <> -1 then
            begin
              // Update selection
              if FCursorMs > FSelectionOrigin then
              begin
                  FSelection.StartTime := FSelectionOrigin;
                  FSelection.StopTime := FCursorMs;
              end else begin
                  FSelection.StartTime := FCursorMs;
                  FSelection.StopTime := FSelectionOrigin;
              end;
              if Assigned(FSelectedRange) then
              begin
                // TODO : we need to keep range list sorted (or maybe we do it when we unselect)
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
    smSSA :
      begin
        if (ssLeft in Shift) or (ssRight in Shift) then
        begin
          Constrain(X,0,Width);
          NewCursorPos := PixelToTime(X) + FPositionMs;
          if (FCursorMs <> NewCursorPos) then
          begin
            FCursorMs := NewCursorPos;

            if FSelectionOrigin <> -1 then
            begin
              // Update selection
              if FCursorMs > FSelectionOrigin then
              begin
                  FSelection.StartTime := FSelectionOrigin;
                  FSelection.StopTime := FCursorMs;
              end else begin
                  FSelection.StartTime := FCursorMs;
                  FSelection.StopTime := FSelectionOrigin;
              end;
              if Assigned(FSelectedRange) then
              begin
                // TODO : we need to keep range list sorted (or maybe we do it when we unselect)
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

    if ssMiddle in Shift then
    begin
      // pseudo "pixel accurate" scrolling
      SetAutoScroll(False);
      if ssShift in Shift then DiffMuliplier := 4 else DiffMuliplier := 1;
      ScrollDiff := PixelToTime(X) - FScrollOrigin;
      SetPositionMs(FPositionMs - (ScrollDiff*DiffMuliplier));
      FScrollOrigin := PixelToTime(X);
    end;
  end
  else
  begin
    // "Dynamic selection" 
    if (FSelMode = smCoolEdit) and (Shift = []) then
    begin
      // Find a subtitle under the mouse
      Constrain(X, 0, Width);
      CursorPosMs := PixelToTime(X) + FPositionMs;
      RangeUnder := FRangeList.FindFirstRangeAt(CursorPosMs);
      while Assigned(RangeUnder) do
      begin
        RangeSelWindow := PixelToTime(6);
        if (((RangeUnder.StopTime - RangeUnder.StartTime) / RangeSelWindow) > 2) then
        begin
          if Abs(RangeUnder.StartTime - CursorPosMs) < RangeSelWindow then
          begin
            Cursor := crHSplit;
            FDynamicEditMode := demStart;
            FDynamicSelRange := RangeUnder;
            Exit;
          end
          else if Abs(RangeUnder.StopTime - CursorPosMs) < RangeSelWindow then
          begin
            Cursor := crHSplit;
            FDynamicEditMode := demEnd;
            FDynamicSelRange := RangeUnder;
            Exit;
          end;
        end;
        RangeUnder := FRangeList.FindNextRange;
      end;
    end;
    Cursor := crIBeam;
    FDynamicEditMode := demNone;
    FDynamicSelRange := nil;
  end;

  UpdateView(UpdateFlags);
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  FSelectionOrigin := -1;
  FScrollOrigin := -1;
  Cursor := crIBeam;
  MouseCapture := False;
  FMouseIsDown := False;
  FDynamicEditMode := demNone;
  FDynamicSelRange := nil;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.CreatePeakTab(WAVFile : TWAVFile);
var
    Buffer8 : array of ShortInt;
    Buffer16 : array of SmallInt;
    i,j : Integer;
    PeakMax, PeakMin : SmallInt;
    PeakMaxMax, PeakMinMin : SmallInt;
    MaxAbsoluteValue : Integer; 
    NormFactor : Single;
begin
  if Assigned(FOnPeakFileCreation) then
    FOnPeakFileCreation(Self,pfcevtStart,0);

  NormFactor := 1.0;

  // Get 1 peak value every ~10ms
  FSamplesPerPeak := WAVFile.SamplesPerSecond div 100;
  FPeakTabSize := Ceil((WAVFile.SamplesCount / FSamplesPerPeak) / WAVFile.Channels);
  // Allocate the big peak tab
  FPeakTab := nil;
  SetLength(FPeakTab, FPeakTabSize);

  if (WAVFile.BitsPerSample = 8) then
  begin
    // Allocate the small buffer
    SetLength(Buffer8, FSamplesPerPeak * WAVFile.Channels);
    PeakMaxMax := -128;
    PeakMinMin := 127;
    for i:=0 to FPeakTabSize-1 do
    begin
      ZeroMemory(Buffer8, FSamplesPerPeak * SizeOf(Shortint) * WAVFile.Channels);
      WAVFile.Read(Buffer8, FSamplesPerPeak * SizeOf(Shortint) * WAVFile.Channels);
      PeakMax := -128;
      PeakMin := 127;
      for j:=0 to (FSamplesPerPeak * WAVFile.Channels)-1 do
      begin
        if Buffer8[j] > PeakMax then
          PeakMax := Buffer8[j];
        if Buffer8[j] < PeakMin then
          PeakMin := Buffer8[j];
      end;
      FPeakTab[i].Max := PeakMax shl 16;
      FPeakTab[i].Min := PeakMin shl 16;

      if PeakMax > PeakMaxMax then
        PeakMaxMax := PeakMax;
      if PeakMin < PeakMinMin then
        PeakMinMin := PeakMin;      

      if Assigned(FOnPeakFileCreation) then
        FOnPeakFileCreation(Self, pfcevtProgress, (i*100) div Integer(FPeakTabSize));
    end;
    // Calc. normalize factor
    MaxAbsoluteValue := Max(Abs(PeakMaxMax), Abs(PeakMinMin));
    NormFactor := 127 / MaxAbsoluteValue;
  end
  else if (WAVFile.BitsPerSample = 16) then
  begin
    // Allocate the small buffer
    SetLength(Buffer16, FSamplesPerPeak * WAVFile.Channels);
    PeakMaxMax := -32768;
    PeakMinMin := 32767;
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
        FOnPeakFileCreation(Self,pfcevtProgress,(i*100) div Integer(FPeakTabSize));
    end;
    // Calc. normalize factor
    MaxAbsoluteValue := Max(Abs(PeakMaxMax), Abs(PeakMinMin));
    NormFactor := 32767 / MaxAbsoluteValue;
  end;
  // Normalize peak tab
  for i:=0 to FPeakTabSize-1 do
  begin
    FPeakTab[i].Max := Round(FPeakTab[i].Max * NormFactor);
    FPeakTab[i].Min := Round(FPeakTab[i].Min * NormFactor);
  end;
  Buffer8 := nil;
  Buffer16 := nil;
  if Assigned(FOnPeakFileCreation) then
    FOnPeakFileCreation(Self,pfcevtStop,0);
end;

//------------------------------------------------------------------------------

function TWAVDisplayer.LoadWAV(filename : string) : Boolean;
var PeakFilename : string;
    PeakFS : TFileStream;
    PeakFileIDRead : string;
    PeakFileVerRead : Cardinal;
    WAVFile : TWAVFile;
    HDRSize : Integer;
    CreatePeakFile : Boolean;    
const
    PeakFileID : string = 'PeakFile';
    PeakFileVer : Cardinal = $0100;
begin
  FPeakDataLoaded := False;
  CreatePeakFile := True;

  // Search for a "peak" file with the same name
  PeakFilename := ChangeFileExt(filename,'.peak');
  if FileExists(PeakFilename) then
  begin
    // TODO : check if the wav file match, if it exists

    // Load peak file
    PeakFS := TFileStream.Create(PeakFilename, fmOpenRead or fmShareDenyWrite);

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
      SetLength(FPeakTab,FPeakTabSize);
      PeakFS.Read(FPeakTab[0], FPeakTabSize*SizeOf(TPeak));
      PeakFS.Free;
      CreatePeakFile := False;
    end;
  end;

  if CreatePeakFile then
  begin
    // No peak file
    if not FileExists(filename) then
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
    PeakFS := TFileStream.Create(PeakFilename,fmCreate);
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

function TWAVDisplayer.PixelToTime(Pixel : Integer) : Integer;
begin
  if FPageSizeMs = 0 then
    Result := 0
  else
    Result := Round( Pixel * (FPageSizeMs / Width) );
end;

//------------------------------------------------------------------------------

function TWAVDisplayer.TimeToPixel(Time : Integer) : Integer;
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
  Constrain(NewPos,0,FLengthMs);
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
function TWAVDisplayer.GetPositionMs : Cardinal;
begin
  if (FScrollBar.Position + FPageSizeMs - 1) > FLengthMs then
    Result := (FLengthMs - FPageSizeMs)
  else
    Result := FScrollBar.Position;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.SetPositionMs(NewPosition : Integer);
begin
  Constrain(NewPosition,0,FLengthMs - FPageSizeMs);
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
  Constrain(NewPageSize,0,FLengthMs);
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

procedure TWAVDisplayer.ZoomRange(Range : TRange);
var NewPosition, NewPageSize : Integer;
    UpdateFlags : TUpdateViewFlags;
begin
  if Range.StartTime >= Range.StopTime then
    Exit;

  NewPageSize := Range.StopTime - Range.StartTime;
  Constrain(NewPageSize,0,FLengthMs);
  NewPosition := Range.StartTime;
  Constrain(NewPosition, 0, FLengthMs - NewPageSize);
  FScrollBar.SetPositionAndPageSize(NewPosition,NewPageSize);

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

procedure TWAVDisplayer.ZoomRange(Start, Stop : Integer);
var Range : TRange;
begin
  Range := TRange.Create;
  Range.StartTime := Start;
  Range.StopTime := Stop;
  ZoomRange(Range);
  Range.Free;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.ZoomCenteredOn(Center, PageSize : Integer);
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
  bShift: ByteSet absolute shift;  
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
    SetPositionMs(FPositionMs - (FPageSizeMs div 4)) // scroll amount = 1/4 of visible interval
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
  bShift: ByteSet absolute shift;
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
    SetPositionMs(FPositionMs + (FPageSizeMs div 4)) // scroll amount = 1/4 of visible interval
  else
    Exit;
    
  UpdateView([uvfPageSize]); // Refresh waveform
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.DblClick;
var idx : Integer;
begin
  inherited;
  // Disable subtitle selection for SSA mode
  if (not FPeakDataLoaded) or (SelMode = smSSA) then
    Exit;
  idx := FRangeList.GetRangeIdxAt(FCursorMs);
  if idx <> -1 then
  begin
    FSelectedRange := FRangeList[idx];
    FSelection.StartTime := FSelectedRange.StartTime;
    FSelection.StopTime := FSelectedRange.StopTime;
    UpdateView([uvfSelection,uvfRange]); 
    if Assigned(FOnSelectionChange) then
      FOnSelectionChange(Self);
    if Assigned(FOnSelectedRange) then
      FOnSelectedRange(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.DeleteRangeAt(Pos : Integer; UpdateDisplay : Boolean);
var Idx : Integer;
begin
  Idx := FRangeList.GetRangeIdxAt(Pos);
  if Idx <> -1 then
  begin
    DeleteRangeAtIdx(idx);
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.DeleteRangeAtIdx(Idx : Integer; UpdateDisplay : Boolean);
begin
  if (Idx < 0) or (Idx >= FRangeList.Count) then
    Exit;
  if FRangeList[Idx] = FSelectedRange then
    FSelectedRange := nil;
  FRangeList.Delete(Idx);
  if (UpdateDisplay) then
    UpdateView([uvfRange]);
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.PlayRange(Range : TRange; Loop : Boolean);
begin
  PlayRange(Range.StartTime,Range.StopTime, Loop);
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.PlayRange(Start, Stop : Integer; Loop : Boolean);
begin
  FRenderer.OnStopPlaying := OnStopPlaying;
  FRenderer.PlayRange(Start,Stop, Loop);
  FUpdateCursorTimer.Enabled := True;
  FIsPlaying := True;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.UpdatePlayRange(Start, Stop : Integer);
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

procedure TWAVDisplayer.OnStopPlaying(Sender : TObject);
begin
  FUpdateCursorTimer.Enabled := False;
  FIsPlaying := False;
  UpdateView([uvfPlayCursor]); // hide the play cursor
end;

//------------------------------------------------------------------------------

function TWAVDisplayer.SelectionIsEmpty : Boolean;
begin
  Result := (FSelection.StopTime = 0);  
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.SetPlayCursorPos(NewPos : Integer);
begin
  Constrain(NewPos,0,FLengthMs);
  if FPlayCursorMs <> NewPos then
  begin
    FOldPlayCursorMs := FPlayCursorMs;
    FPlayCursorMs := NewPos;
    if Assigned(FOnPlayCursorChange) then
      FOnPlayCursorChange(Self);
    // TODO : maybe we could do a nice smooth scrolling :)
    if FAutoScrolling and ((NewPos < FPositionMs) or (FPlayCursorMs > (FPositionMs + FPageSizeMs))) then
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

procedure TWAVDisplayer.SetSelectedRangeEx(Value : TRange; UpdateDisplay : Boolean);
begin
  if (FSelectedRange <> Value) then
  begin
    FSelectedRange := Value;
    if FSelectedRange <> nil then
    begin
      FSelection.StartTime := FSelectedRange.StartTime;
      FSelection.StopTime := FSelectedRange.StopTime;
    end;
    if UpdateDisplay then
      UpdateView([uvfSelection,uvfRange]);
    if Assigned(FOnSelectionChange) then
      FOnSelectionChange(Self);
    //if Assigned(FOnSelectedRange) then
    //  FOnSelectedRange(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.ZoomAndSelectRange(Range : TRange);
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
  SetSelectedRangeEx(Range,False);
  ZoomRange(TmpRange);
  if (OldPageSize = FPageSizeMs) and (OldPos = FPositionMs) then
    UpdateView([uvfSelection,uvfRange]);
  TmpRange.Free;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.SetRenderer(Renderer : TRenderer);
begin
  if FIsPlaying then
  begin
    Stop;
    while FIsPlaying do // yeah a bit ugly :p
    begin
      Sleep(40);
    end;
  end;
  if (FRenderer <> Renderer) then
  begin
    FRenderer := Renderer;
  end;
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.ClearSelection;
begin
  FSelection.StartTime := 0;
  FSelection.StopTime := 0;
  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
  UpdateView([uvfSelection,uvfRange]);
end;

//------------------------------------------------------------------------------

procedure TWAVDisplayer.SetAutoScroll(Value : Boolean);
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
  FSelectedRange := nil;
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
  Constrain(Value, 1, 400);
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
end.
//------------------------------------------------------------------------------

