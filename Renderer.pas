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

unit Renderer;

interface

uses Classes, Windows, DirectShow9, Messages;

type
  TWaitCompletionThread = class;

  TRenderer = class
  private
    FOnStopPlaying : TNotifyEvent;
  public
    function PlayRange(Start, Stop : Cardinal; Loop : Boolean = False) : Boolean; virtual; abstract;
    procedure UpdatePlayRange(Start, Stop : Cardinal); virtual;
    function GetPosition : Cardinal; virtual; abstract;
    procedure Stop; virtual; abstract;
    function IsOpen : Boolean; virtual; abstract;
    procedure SetRate(Rate : Integer); virtual;
  published
    property OnStopPlaying : TNotifyEvent read FOnStopPlaying write FOnStopPlaying;
  end;

  TDShowRenderer = class(TRenderer)
  private
    FGraphBuilder : IGraphBuilder;
    FMediaControl : IMediaControl;
    FMediaSeeking : IMediaSeeking;
    FMediaEventEx : IMediaEventEx;
    FVideoWindow : IVideoWindow;
    FDisplayWindowProc, FDisplayWindowOldProc : TFarProc;
    FLastResult : HRESULT;
    FWaitThread : TWaitCompletionThread;
    FLoop : Boolean;
    FStart, FStop : Int64;
    FVideoWidth, FVideoHeight : Integer;
    FDisplayWindow : THandle;
    FIsOpen : Boolean;
    FStartStopAccessCS : TRtlCriticalSection;

    function FGetStart : Int64;
    function FGetStop : Int64;

  protected
    procedure DisplayWindowProc(var Mesg : TMessage);

  public
    constructor Create;
    destructor Destroy; override;
    function GetLastErrorString : string;
    function Open(filename : string) : Boolean;
    function PlayRange(Start, Stop : Cardinal; Loop : Boolean = False) : Boolean; override;
    procedure UpdatePlayRange(Start, Stop : Cardinal); override;
    function GetPosition : Cardinal; override;
    procedure Stop; override;
    function IsPlaying : Boolean;
    procedure Replay;
    procedure SetDisplayWindow(WinHwnd : THandle);
    procedure UpdateDisplayWindow;
    procedure KillVideo;    
    procedure Close;
    function IsOpen : Boolean; override;
    procedure SetRate(Rate : Integer); override;        
  published
    property OnStopPlaying;
    property VideoWidth : Integer read FVideoWidth;
    property VideoHeight : Integer read FVideoHeight;
    property StartTime : Int64 read FGetStart;
    property StopTime : Int64 read FGetStop;
  end;

  TWaitCompletionThread = class(TThread)
  private
    FRenderer : TDShowRenderer;
    hTerminateEvent : THandle;
  public
    constructor Create(Renderer : TDShowRenderer);
    destructor Destroy; override;
    procedure Execute; override;
    procedure SignalTermination;
  end;

  TWAVExtractionType = (wetOnlyPeakFile, wetNoConversion, wetFastConversion);
  TDSWavExtractor = class
  private
    FGraphBuilder : IGraphBuilder;
    FMediaControl : IMediaControl;
    FMediaSeeking : IMediaSeeking;
    FMediaEventEx : IMediaEventEx;
    FLastResult : HRESULT;
    FSplitter : IBaseFilter;
    FAudioStreamCount : Integer;
    FDuration : Int64;
    FSourceFilename : WideString;
    FDestinationFilename : WideString;
    FHWavWriterInst : THandle;
    FWAVExtractionType : TWAVExtractionType;

    function GetOutputAudioPinCount(Filter : IBaseFilter) : Integer;
    function GetPCMOutputPin(Filter : IBaseFilter) : IPin;
    function InstallWriter(Pin : IPin) : Boolean;
        
  public
    constructor Create;
    destructor Destroy; override;
    function Open(Filename : WideString) : Boolean;
    function SelectAudioPin(Index : Integer) : Boolean;
    procedure Go;    
    function GetLastErrorString : string;
    function GetProgress : Integer;
    function IsFinished : Boolean;
    procedure Close;

  published
    property AudioStreamCount : Integer read FAudioStreamCount;
    property DestinationFilename : WideString read FDestinationFilename write FDestinationFilename;
    property WAVExtractionType : TWAVExtractionType read FWAVExtractionType write FWAVExtractionType;
  end;

  IWavWriterInterface = interface(IUnknown)
    function SetFastConversionMode(bFastConvertMode : DWORD) : HRESULT; stdcall;
    function GetFastConversionMode(out bFastConvertMode : DWORD) : HRESULT; stdcall;
    function SetSamplesPerPeakRatio(SamplePerPeakRatio : DWORD) : HRESULT; stdcall;
    function SetWritePeakFile(WritePeakFile : DWORD) : HRESULT; stdcall;
    function SetPeakFileName(pszFileName: PWideChar) : HRESULT; stdcall;
    function SetWriteWavFile(WriteWavFile : DWORD) : HRESULT; stdcall;
  end;

implementation

uses ActiveX, MMSystem, SysUtils, Types;

const
  CLSID_WavWriter : TGUID = '{F3AFF1C3-ABBB-41f9-9521-988881D9D640}';
  IID_IWavWriter : TGUID = '{3B7A03CE-3D3B-4fef-8CDE-3D6C2709AFF1}';

//==============================================================================

function CreateFilterFromFile(hLibInst : THandle; Guid : TGUID; out Filter) : HRESULT;
var DllGetClassObject : function(const clsid, iid : TGUID; var Obj) : HRESULT; stdcall;
    ClassFactory : IClassFactory;
begin
  Result := S_FALSE;
  DllGetClassObject := GetProcAddress(hLibInst, 'DllGetClassObject');
  if Assigned(DllGetClassObject) then
  begin
    Result := DllGetClassObject(Guid, IClassFactory, ClassFactory);
    if Result = S_OK then
    begin
      Result := ClassFactory.CreateInstance(nil,IBaseFilter,Filter);
    end;
  end;
end;

//------------------------------------------------------------------------------

function GetFilterPin(Filter : IBaseFilter; Direction : TPinDirection; out ResultPin : IPin) : HRESULT;
var EnumPins: IEnumPins;
    Pin: IPin;
    ul: ULONG;
    PinDir: TPinDirection;
begin
  Result := S_FALSE;
  if Filter = nil then
    Exit;

  if Filter.EnumPins(EnumPins) <> S_OK then
    Exit;

  while (EnumPins.Next(1,Pin, @ul) = S_OK) do
  begin
    Pin.QueryDirection(PinDir);
    if PinDir = Direction then
    begin
      ResultPin := Pin;
      Break;
    end;
    Pin := nil;
  end;
  Pin := nil;
  EnumPins := nil;
end;

//------------------------------------------------------------------------------

function NukeDownstream(GraphBuilder : IGraphBuilder; Pin : IPin) : Boolean;
var
  PinConnectedTo, PinOut : IPin;
  EnumPins : IEnumPins;
  ul: ULONG;
  Filter : IBaseFilter;
	PinInfo : TPinInfo;
  PinDir: TPinDirection;
begin
  Pin.ConnectedTo(PinConnectedTo);
  if PinConnectedTo = nil then
  begin
    Result := False;
    Exit;
  end;
  PinConnectedTo.QueryPinInfo(PinInfo);
  Filter := PinInfo.pFilter;
  Filter.EnumPins(EnumPins);

  while (EnumPins.Next(1,PinOut, @ul) = S_OK) do
  begin
    PinOut.QueryDirection(PinDir);
    if (PinDir = PINDIR_OUTPUT) then
    begin
      NukeDownstream(GraphBuilder,PinOut);
    end;
    PinOut := nil;
  end;
  EnumPins := nil;
  PinConnectedTo := nil;
  GraphBuilder.RemoveFilter(Filter);
  Filter := nil;
  Result := True;
end;

//------------------------------------------------------------------------------

procedure DeleteMediaType(pmt: PAMMediaType);
begin
  if (pmt <> nil) then
  begin
    if (pmt^.cbFormat <> 0) then
      CoTaskMemFree(pmt^.pbFormat);
    if (pmt^.pUnk <> nil) then
      pmt^.pUnk := nil;
    CoTaskMemFree(pmt);
  end;
end;

//------------------------------------------------------------------------------

function IsAudioPin(Pin : IPin) : Boolean;
var
  EnumMT : IEnumMediaTypes;
  ul : ULONG;
  pMT : PAMMediaType;
begin
  Result := False;
  Pin.EnumMediaTypes(EnumMT);
  while (EnumMT.Next(1,pMT,@ul) = S_OK) do
  begin
    if IsEqualGUID(pMT.majortype, MEDIATYPE_Audio) then
    begin
      Result := True;
      DeleteMediaType(pMT);
      Break;
    end
    else
      DeleteMediaType(pMT);
  end;
  EnumMT := nil;
end;

//==============================================================================

procedure TRenderer.UpdatePlayRange(Start, Stop : Cardinal);
begin
  // do nothing
end;

procedure TRenderer.SetRate(Rate : Integer);
begin
  // do nothing
end;

//==============================================================================

constructor TDShowRenderer.Create;
begin
  FGraphBuilder := nil;
  FMediaControl := nil;
  FMediaSeeking := nil;
  FMediaEventEx := nil;
  FOnStopPlaying := nil;
  FWaitThread := nil;
  FVideoWindow := nil;
  FLoop := False;
  FLastResult := S_OK;
  FIsOpen := False;
  FDisplayWindow := 0;
  FDisplayWindowProc := nil;
  FDisplayWindowOldProc := nil;
  InitializeCriticalSection(FStartStopAccessCS);
end;

//------------------------------------------------------------------------------

destructor TDShowRenderer.Destroy;
begin
  if Assigned(FWaitThread) then
  begin
    FWaitThread.SignalTermination;
    FWaitThread.WaitFor;
    FWaitThread.Free;
    FWaitThread := nil;
  end;
  Close;
  DeleteCriticalSection(FStartStopAccessCS);
  inherited;
end;

//------------------------------------------------------------------------------

function TDShowRenderer.GetLastErrorString : string;
begin
  SetLength(Result,MAX_ERROR_TEXT_LEN);
  AMGetErrorText(FLastResult,@Result[1],MAX_ERROR_TEXT_LEN);
end;

//------------------------------------------------------------------------------

function TDShowRenderer.Open(filename : string) : Boolean;
var
  filenameW : array[0..MAX_PATH-1] of wchar;
  BasicVideoI : IBasicVideo2;
begin
  if Assigned(FGraphBuilder) then
    Close;
  Result := False;
  FLastResult := CoCreateInstance(TGUID(CLSID_FilterGraph), nil, CLSCTX_INPROC,
    TGUID(IID_IGraphBuilder), FGraphBuilder);
  GetLastErrorString;
  if (FLastResult <> S_OK) then Exit;
  FLastResult := FGraphBuilder.QueryInterface(IID_IMediaControl, FMediaControl);
  if (FLastResult <> S_OK) then Exit;
  FLastResult := FGraphBuilder.QueryInterface(IID_IMediaSeeking, FMediaSeeking);
  if (FLastResult <> S_OK) then Exit;
  FLastResult := FGraphBuilder.QueryInterface(IID_IMediaEventEx, FMediaEventEx);
  if (FLastResult <> S_OK) then Exit;
  MultiByteToWideChar(CP_ACP, 0, pChar(filename), -1, @filenameW, MAX_PATH);
  FLastResult := FGraphBuilder.RenderFile(filenameW,nil);
  Result := (FLastResult = S_OK);
  FIsOpen := Result;

  FGraphBuilder.QueryInterface(IID_IBasicVideo2, BasicVideoI);
  BasicVideoI.get_VideoWidth(FVideoWidth);
  BasicVideoI.get_VideoHeight(FVideoHeight);
  BasicVideoI := nil;

  SetRate(100);
end;

//------------------------------------------------------------------------------

function TDShowRenderer.IsOpen : Boolean;
begin
  Result := FIsOpen;
end;

//------------------------------------------------------------------------------

function TDShowRenderer.PlayRange(Start, Stop : Cardinal; Loop : Boolean) : Boolean;
var StopDummy : Int64;
begin
  Result := False;
  FLoop := Loop;
  FLastResult := FMediaControl.Stop;
  if (FLastResult <> S_OK) then
    Exit;
  FStart := Int64(Start) * 10000;
  FStop := Int64(Stop) * 10000;
  StopDummy := 0;

  // We can't use the stop position
  // it cause problems with AVI (stop before the stop point :p),
  // and matroska splitter doesn't support stop

  //FLastResult := FMediaSeeking.SetPositions(FStart,
  //  AM_SEEKING_AbsolutePositioning, FStop, AM_SEEKING_AbsolutePositioning);
  FLastResult := FMediaSeeking.SetPositions(FStart,
    AM_SEEKING_AbsolutePositioning, StopDummy, AM_SEEKING_NoPositioning);
  if (FLastResult <> S_OK) then
    Exit;

  if Assigned(FWaitThread) then
  begin
    FWaitThread.SignalTermination;
    FWaitThread.WaitFor;
    FWaitThread.Free;
  end;
  FWaitThread := TWaitCompletionThread.Create(Self);
  FLastResult := FMediaControl.Run;
  Result := (FLastResult = S_OK);
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.UpdatePlayRange(Start, Stop : Cardinal);
begin
  EnterCriticalSection(FStartStopAccessCS);
  FStart := Int64(Start) * 10000;
  FStop := Int64(Stop) * 10000;
  LeaveCriticalSection(FStartStopAccessCS);  
end;

//------------------------------------------------------------------------------

function TDShowRenderer.FGetStart : Int64;
begin
  EnterCriticalSection(FStartStopAccessCS);
  Result := FStart;
  LeaveCriticalSection(FStartStopAccessCS);
end;

//------------------------------------------------------------------------------

function TDShowRenderer.FGetStop : Int64;
begin
  EnterCriticalSection(FStartStopAccessCS);
  Result := FStop;
  LeaveCriticalSection(FStartStopAccessCS);
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.Stop;
begin
  //FMediaControl.Stop;
  FMediaControl.Pause;
  if Assigned(FWaitThread) then
  begin
    FWaitThread.SignalTermination;
    FWaitThread.WaitFor;
    FWaitThread.Free;
    FWaitThread := nil;
  end;
  if Assigned(FOnStopPlaying) then
    FOnStopPlaying(Self);
end;

//------------------------------------------------------------------------------

function TDShowRenderer.GetPosition : Cardinal;
var CurrentTime : Int64;
begin
  FMediaSeeking.GetCurrentPosition(CurrentTime);
  Result := CurrentTime div 10000;
end;

//------------------------------------------------------------------------------

function TDShowRenderer.IsPlaying : Boolean;
var State : TFilterState;
begin
  Result := False;
  if Assigned(FMediaControl) then
  begin
    FMediaControl.GetState(0,State);
    Result := (State = State_Running);
  end;
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.Replay;
begin
  PlayRange(FStart, FStop, FLoop);
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.DisplayWindowProc(var Mesg : TMessage);
begin
  with Mesg do
  begin
    if Msg = WM_SIZE then
      UpdateDisplayWindow;
    Result := CallWindowProc(FDisplayWindowOldProc, FDisplayWindow, Msg,
      WParam, LParam);
  end;
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.SetDisplayWindow(WinHwnd : THandle);
var State : TFilterState;
begin
  if (FDisplayWindow = WinHwnd) then
    Exit;

  FMediaControl.GetState(0, State);
  if (State = State_Running) or (State = State_Paused) then
    FMediaControl.Stop;

  if Assigned(FVideoWindow) then
  begin
    FVideoWindow.put_Visible(False);
    FVideoWindow.put_MessageDrain(0);
    // This steal the focus, so we do it only that before releasing the graph
    if WinHwnd = 0 then
      FVideoWindow.put_Owner(0);
    FVideoWindow := nil;
  end;

  // Unsubclass old window
  if (FDisplayWindowOldProc <> nil) then
  begin
    SetWindowLong(FDisplayWindow, GWL_WNDPROC, LongInt(FDisplayWindowOldProc));
    FreeObjectInstance(FDisplayWindowProc);
    FDisplayWindowProc := nil;
    FDisplayWindowOldProc := nil;
  end;

  FDisplayWindow := WinHwnd;
  if (WinHwnd <> 0) then
  begin
    FLastResult := FGraphBuilder.QueryInterface(IID_IVideoWindow, FVideoWindow);
    if Succeeded(FLastResult) and Assigned(FVideoWindow) then
    begin
      // Subclass new window to handle resize
      FDisplayWindowProc := MakeObjectInstance(DisplayWindowProc);
      FDisplayWindowOldProc := Pointer(GetWindowLong(FDisplayWindow, GWL_WNDPROC));
      SetWindowLong(FDisplayWindow, GWL_WNDPROC, LongInt(FDisplayWindowProc));

      FVideoWindow.put_Owner(FDisplayWindow);
      //FVideoWindow.put_AutoShow(False);
      FVideoWindow.put_MessageDrain(FDisplayWindow);
      FVideoWindow.put_WindowStyle(WS_CHILD + WS_CLIPSIBLINGS + WS_CLIPCHILDREN);
      //FVideoWindow.put_Visible(True);

      UpdateDisplayWindow;
    end;

    if (State = State_Running) then
      FMediaControl.Run
    else if(State = State_Paused) then
      FMediaControl.Pause;
  end;     
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.UpdateDisplayWindow;
var Rect : TRect;
    WinWidth, WinHeight : Integer;
    NewWidth, NewHeight : Integer;
begin
  if (FDisplayWindow = 0) or (not Assigned(FVideoWindow)) or
     (FVideoWidth = 0) or (FVideoHeight = 0) then
    Exit;

  if(GetWindowRect(FDisplayWindow, Rect) = False) then
    Exit;
  WinWidth := Rect.Right - Rect.Left;
  WinHeight := Rect.Bottom - Rect.Top;

  if (FVideoWidth / FVideoHeight) > (WinWidth / WinHeight) then
  begin
    NewHeight := (WinWidth * FVideoHeight) div FVideoWidth;
    NewWidth := WinWidth;
  end
  else
  begin
    NewWidth := (FVideoWidth * WinHeight) div FVideoHeight;
    NewHeight := WinHeight;
  end;

  FVideoWindow.SetWindowPosition(
    (WinWidth - NewWidth) div 2,
    (WinHeight - NewHeight) div 2,
    NewWidth,
    NewHeight);
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.KillVideo;
var FiltersEnum : IEnumFilters;
    Filter, Splitter : IBaseFilter;
    ul : ULONG;
    PinsEnum : IEnumPins;
    Pin, VideoPin : IPin;
    PinDir: TPinDirection;
    HasVideo, HasAudio : Boolean;
    MT : TAMMediaType;
begin
  // We search for a filter that has a video output and an audio output (that
  // should be the splitter ;-)
  FGraphBuilder.EnumFilters(FiltersEnum);
  while FiltersEnum.Next(1,Filter,@ul) = S_OK do
  begin
    HasVideo := False;
    HasAudio := False;
    Filter.EnumPins(PinsEnum);
    while PinsEnum.Next(1,Pin,@ul) = S_OK do
    begin
      Pin.QueryDirection(PinDir);
      if (PinDir = PINDIR_OUTPUT) then
      begin
        if Pin.ConnectionMediaType(MT) = S_OK then
        begin
          if IsEqualGUID(MT.majortype, MEDIATYPE_Video) then
          begin
            HasVideo := True;
            VideoPin := nil;
            VideoPin := Pin;
          end
          else if IsEqualGUID(MT.majortype, MEDIATYPE_Audio) then
          begin
            HasAudio := True;
          end;
          if HasAudio and HasVideo then
            Break;
        end;
      end;
      Pin := nil;
    end;
    if HasAudio and HasVideo then
    begin
      Splitter := Filter;
      Break;
    end;
    Filter := nil;
  end;
  Pin := nil;  
  PinsEnum := nil;
  FiltersEnum := nil;
  Filter := nil;

  if Assigned(VideoPin) then
  begin
    NukeDownstream(FGraphBuilder,VideoPin);
  end;
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.SetRate(Rate : Integer);
begin
  if Assigned(FMediaSeeking) then
    FMediaSeeking.SetRate(Rate / 100);
end;

//------------------------------------------------------------------------------

procedure TDShowRenderer.Close;
begin
  FIsOpen := False;
  if Assigned(FMediaControl) then FMediaControl.Stop;
  SetDisplayWindow(0);
  if Assigned(FMediaControl) then FMediaControl := nil;
  if Assigned(FMediaEventEx) then FMediaEventEx := nil;
  if Assigned(FMediaSeeking) then FMediaSeeking := nil;
  if Assigned(FGraphBuilder) then FGraphBuilder := nil;
end;

//==============================================================================

constructor TWaitCompletionThread.Create(Renderer : TDShowRenderer);
begin
  FRenderer := Renderer;
  hTerminateEvent := CreateEvent(nil,False,False,nil);
  inherited Create(False);
end;

//------------------------------------------------------------------------------

destructor TWaitCompletionThread.Destroy;
begin
  CloseHandle(hTerminateEvent);
  inherited;
end;

//------------------------------------------------------------------------------

procedure TWaitCompletionThread.SignalTermination;
begin
  Terminate;
  SetEvent(hTerminateEvent);
end;

//------------------------------------------------------------------------------

procedure TWaitCompletionThread.Execute;
var
  hInQueueEvent : THandle;
  evCode, param1, param2 : Integer;
  bDone : Boolean;
  EventsArray : array[0..1] of THandle;
  WaitResult : Cardinal;
  CurrentPos, StartPos : Int64;
  StopDummy : Int64;
label TWaitCompletionThread_Restart;
begin
  if (FRenderer.FMediaEventEx.GetEventHandle(OAEVENT(hInQueueEvent)) <> S_OK) then
    // Insert failure-handling code here
    Exit;

  EventsArray[0] := hTerminateEvent;
  EventsArray[1] := hInQueueEvent;

TWaitCompletionThread_Restart:

  bDone := False;
  while (not bDone) and (not Self.Terminated) do
  begin
    WaitResult := WaitForMultipleObjects(2, @EventsArray, False, 40);
    if (WaitResult = WAIT_OBJECT_0+1) then
    begin
      while (FRenderer.FMediaEventEx.GetEvent(evCode, param1, param2, 0) = S_OK) do
      begin
        FRenderer.FMediaEventEx.FreeEventParams(evCode, param1, param2);
        bDone := (evCode = EC_COMPLETE);
      end;
    end;
    // Check position
    FRenderer.FMediaSeeking.GetCurrentPosition(CurrentPos);
    if ((CurrentPos >= FRenderer.StopTime) or
       ((Abs(CurrentPos - FRenderer.StopTime)) <= (20*10000))) or
       ((CurrentPos + 20*10000) < FRenderer.StartTime) then
    begin
      bDone := True;
    end;
  end;

  if bDone then
  begin
    if FRenderer.FLoop then
    begin
      //FRenderer.FMediaSeeking.SetPositions(FRenderer.FStart,
      //  AM_SEEKING_AbsolutePositioning, FRenderer.FStop, AM_SEEKING_AbsolutePositioning);
      StartPos := FRenderer.StartTime;
      FRenderer.FMediaSeeking.SetPositions(StartPos,
        AM_SEEKING_AbsolutePositioning, StopDummy, AM_SEEKING_NoPositioning);
      goto TWaitCompletionThread_Restart;
    end
    else
    begin
      FRenderer.FMediaControl.Pause;
      //FRenderer.FMediaControl.Stop;
      if Assigned(FRenderer.FOnStopPlaying) then
        FRenderer.FOnStopPlaying(FRenderer);
    end;
  end;
end;

//==============================================================================

constructor TDSWavExtractor.Create;
begin
  FLastResult := S_OK;
  FAudioStreamCount := 0;
  FHWavWriterInst := CoLoadLibrary('WavWriter.dll',True);
  if (FHWavWriterInst = 0) then
    MessageBox(0, 'Error while loading WavWriter.dll', 'Error', MB_OK);
end;

//------------------------------------------------------------------------------

destructor TDSWavExtractor.Destroy;
begin
  Close;
  if (FHWavWriterInst <> 0) then
  begin
    //CoFreeLibrary(FHWavWriterInst); // TODO : why it b0rks ?
    //FHWavWriterInst := 0;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.GetOutputAudioPinCount(Filter : IBaseFilter) : Integer;
var
  EnumPins : IEnumPins;
  ul: ULONG;
  Pin : IPin;
  PinDir: TPinDirection;
begin
  Result := 0;
  Filter.EnumPins(EnumPins);
  while (EnumPins.Next(1,Pin, @ul) = S_OK) do
  begin
    Pin.QueryDirection(PinDir);
    if (PinDir = PINDIR_OUTPUT) and IsAudioPin(Pin) then
      Inc(Result);
    Pin := nil;
  end;
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.Open(Filename : WideString) : Boolean;
var
  Filter : IBaseFilter;
  Pin1, Pin2 : IPin;
  PinInfo : TPinInfo;
  EnumPins : IEnumPins;
  ul: ULONG;
begin
  Result := False;
  if (FHWavWriterInst = 0) then
    Exit;

  if Assigned(FGraphBuilder) then
    Close;

  FLastResult := CoCreateInstance(TGUID(CLSID_FilterGraph), nil, CLSCTX_INPROC,
    TGUID(IID_IGraphBuilder), FGraphBuilder);
  GetLastErrorString;
  if (FLastResult <> S_OK) then Exit;
  FLastResult := FGraphBuilder.QueryInterface(IID_IMediaControl, FMediaControl);
  if (FLastResult <> S_OK) then Exit;
  FLastResult := FGraphBuilder.QueryInterface(IID_IMediaSeeking, FMediaSeeking);
  if (FLastResult <> S_OK) then Exit;
  FLastResult := FGraphBuilder.QueryInterface(IID_IMediaEventEx, FMediaEventEx);
  if (FLastResult <> S_OK) then Exit;

  FLastResult := FGraphBuilder.AddSourceFilter(@filename[1],nil,Filter);
  if (FLastResult <> S_OK) then Exit;

  // Try to render all pins on the filter
  Filter.EnumPins(EnumPins);
  while (FLastResult = S_OK) and (EnumPins.Next(1,Pin1, @ul) = S_OK) do
  begin
    FLastResult := FGraphBuilder.Render(Pin1);
    Pin1 := nil;
  end;
  EnumPins := nil;
  if (FLastResult <> S_OK) then Exit;

  // Get the duration
  FLastResult := FMediaSeeking.GetDuration(FDuration);

  // Check if this is a source/splitter filter
  FAudioStreamCount := GetOutputAudioPinCount(Filter);
  if FAudioStreamCount = 0 then
  begin
    // No audio stream available, this is probably the File source filter
    // Next filter is probably a splitter
    FLastResult := GetFilterPin(Filter,PINDIR_OUTPUT,Pin1);
    FLastResult := Pin1.ConnectedTo(Pin2);
    FLastResult := Pin2.QueryPinInfo(PinInfo);
    Filter := nil;
    Filter := PinInfo.pFilter;
    FAudioStreamCount := GetOutputAudioPinCount(Filter);
  end;
  FSplitter := Filter;
  Result := (FAudioStreamCount > 0);
  Filter := nil;
  Pin1 := nil;
  Pin2 := nil;
  FMediaSeeking := nil;
  FSourceFilename := Filename;
end;

//------------------------------------------------------------------------------

procedure TDSWavExtractor.Close;
begin
  FAudioStreamCount := 0;
  if Assigned(FSplitter) then FSplitter := nil;
  if Assigned(FMediaControl) then
  begin
    FMediaControl.Stop;
    FMediaControl := nil;
  end;
  if Assigned(FMediaEventEx) then FMediaEventEx := nil;
  if Assigned(FMediaSeeking) then FMediaSeeking := nil;
  if Assigned(FGraphBuilder) then FGraphBuilder := nil;
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.SelectAudioPin(Index : Integer) : Boolean;
var
  EnumPins : IEnumPins;
  ul: ULONG;
  Pin : IPin;
  PinDir: TPinDirection;
  AudioPinCurrentIndex : Integer;
begin
  assert(FSplitter <> nil);
  Result := False;
  AudioPinCurrentIndex := 0;
  FSplitter.EnumPins(EnumPins);
  while (EnumPins.Next(1,Pin, @ul) = S_OK) do
  begin
    Pin.QueryDirection(PinDir);
    if (PinDir = PINDIR_OUTPUT) then
    begin
      if IsAudioPin(Pin) then
      begin
        if (AudioPinCurrentIndex <> Index) then
          NukeDownstream(FGraphBuilder,Pin)
        else
        begin
          InstallWriter(Pin);
        end;
        Inc(AudioPinCurrentIndex);
      end
      else
        NukeDownstream(FGraphBuilder,Pin);
    end;
    Pin := nil;
  end;
end;

//------------------------------------------------------------------------------

function PinSupportPCMMediaType(Pin : IPin) : Boolean;
var
  EnumMT : IEnumMediaTypes;
  pMT : PAMMediaType;
  ul: ULONG;
begin
  Result := False;
  Pin.EnumMediaTypes(EnumMT);
  while (EnumMT.Next(1,pMT,@ul) = S_OK) do
  begin
    if IsEqualGUID(pMT.majortype, MEDIATYPE_Audio) and
      IsEqualGUID(pMT.subtype, MEDIASUBTYPE_PCM) and
      IsEqualGUID(pMT.formattype, FORMAT_WaveFormatEx) and
      (PWaveFormatEx(pMT.pbFormat).wFormatTag = WAVE_FORMAT_PCM) then
    begin
      Result := True;
      Break;
    end;
    DeleteMediaType(pMT);
  end;
  EnumMT := nil;
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.GetPCMOutputPin(Filter : IBaseFilter) : IPin;
var
  EnumPins : IEnumPins;
  ul: ULONG;
  Pin : IPin;
  PinDir: TPinDirection;
begin
  Result := nil;
  Filter.EnumPins(EnumPins);
  while (EnumPins.Next(1,Pin, @ul) = S_OK) do
  begin
    Pin.QueryDirection(PinDir);
    if (PinDir = PINDIR_OUTPUT) and PinSupportPCMMediaType(Pin) then
    begin
      Result := Pin;
      Break;
    end;
    Pin := nil;
  end;
  Pin := nil;
  EnumPins := nil;
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.InstallWriter(Pin : IPin) : Boolean;
var
  PCMPin, PinConnectedTo : IPin;
  Filter, WavWriterFilter : IBaseFilter;
  PinInfo : TPinInfo;
  FileSinkFilter : IFileSinkFilter;
  WavWriterInterface : IWavWriterInterface;
  DestinationFilenamePeak : WideString;
begin
  if PinSupportPCMMediaType(Pin) then
    PCMPin := Pin
  else
  begin
    // We need a pin with wformattag which is probably after a decoder :)
    Pin.ConnectedTo(PinConnectedTo);
    PinConnectedTo.QueryPinInfo(PinInfo);
    Filter := PinInfo.pFilter;
    PCMPin := GetPCMOutputPin(Filter);
  end;
  PinConnectedTo := nil;
  Filter := nil;
  assert(PCMPin <> nil);
  NukeDownstream(FGraphBuilder,PCMPin);

  // We are ready to connect, the WAV writer filter and the File Writer
  FLastResult := CreateFilterFromFile(FHWavWriterInst,CLSID_WavWriter, WavWriterFilter);
  FLastResult := WavWriterFilter.QueryInterface(IID_IFileSinkFilter, FileSinkFilter);
  FLastResult := FileSinkFilter.SetFileName(@DestinationFilename[1],nil);
  FileSinkFilter := nil;
  FLastResult := FGraphBuilder.AddFilter(WavWriterFilter,'WAV Writer');

  WavWriterFilter.QueryInterface(IID_IWavWriter,WavWriterInterface);
  if WavWriterInterface <> nil then
  begin
      if (FWAVExtractionType = wetOnlyPeakFile) then
        WavWriterInterface.SetWriteWavFile(0)
      else if (FWAVExtractionType = wetFastConversion) then
        WavWriterInterface.SetFastConversionMode(1);
      WavWriterInterface.SetWritePeakFile(1);
      DestinationFilenamePeak := ChangeFileExt(DestinationFilename,'.peak');
      WavWriterInterface.SetPeakFileName(@DestinationFilenamePeak[1]);
      WavWriterInterface.SetSamplesPerPeakRatio(100);
      WavWriterInterface := nil;
  end;

  FLastResult := FGraphBuilder.Render(PCMPin);

  FLastResult := WavWriterFilter.QueryInterface(IID_IMediaSeeking, FMediaSeeking);

  PCMPin := nil;  
  Filter := nil;
  WavWriterFilter := nil;
  Result := (FLastResult = S_OK);
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.GetLastErrorString : string;
begin
  SetLength(Result,MAX_ERROR_TEXT_LEN);
  AMGetErrorText(FLastResult,@Result[1],MAX_ERROR_TEXT_LEN);
end;

//------------------------------------------------------------------------------

procedure TDSWavExtractor.Go;
begin
  FMediaControl.Run;
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.GetProgress : Integer;
var Current, Duration : Int64;
begin
  if Assigned(FMediaSeeking) then
  begin
    FMediaSeeking.GetCurrentPosition(Current);
    FMediaSeeking.GetDuration(Duration);
    Result := Round(Current / Duration * 1000);
  end
  else
    Result := 0;
end;

//------------------------------------------------------------------------------

function TDSWavExtractor.IsFinished : Boolean;
var evCode, param1, param2 : Integer;
begin
  Result := False;
  while (not Result) and (FMediaEventEx.GetEvent(evCode, param1, param2,0) = S_OK) do
  begin
    FMediaEventEx.FreeEventParams(evCode, param1, param2);
    Result := (evCode = EC_COMPLETE);
  end;
end;

//------------------------------------------------------------------------------
end.
//------------------------------------------------------------------------------
