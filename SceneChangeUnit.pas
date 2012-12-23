unit SceneChangeUnit;

interface

uses Types;

procedure ExtractSceneChange(Filename : WideString; var SCArray : TIntegerDynArray);
procedure SaveSceneChange(Filename : WideString; var SCArray : TIntegerDynArray);
procedure LoadSceneChange(Filename : WideString; var SCArray : TIntegerDynArray);
procedure GenerateSceneChangeFile(Filename : WideString);
function FindSceneChange(Filename : WideString; Start, Stop : Integer) : Integer;

implementation

uses Windows, VFW, SysUtils, TntClasses, TntSysUtils, MiscToolsUnit,
  FileReaderUnit, MatroskaHelper, LogWriterIntf, MP4File;

// -----------------------------------------------------------------------------

procedure SaveSceneChange(Filename : WideString; var SCArray : TIntegerDynArray);
var SceneChangeSL : TTntStringList;
    I : Integer;
begin
  SceneChangeSL := TTntStringList.Create;
  SceneChangeSL.Add('SceneChangeFormatVersion=1');
  for I := Low(SCArray) to High(SCArray) do
  begin
    SceneChangeSL.Add(TimeMsToString(SCArray[I]));
  end;
  SceneChangeSL.SaveToFile(Filename);
  SceneChangeSL.Free;
end;

// -----------------------------------------------------------------------------

procedure LoadSceneChange(Filename : WideString; var SCArray : TIntegerDynArray);
var SceneChangeSL : TTntStringList;
    I, TimeMs : Integer;
begin
  if WideFileExists(Filename) then
  begin
    SceneChangeSL := TTntStringList.Create;
    SceneChangeSL.LoadFromFile(Filename);
    if (SceneChangeSL.Count > 0) and (SceneChangeSL.Names[0] = 'SceneChangeFormatVersion') then
    begin
      if (SceneChangeSL.ValueFromIndex[0] = '1') then
      begin
        for I := 1 to SceneChangeSL.Count-1 do
        begin
          TimeMs := TimeStringToMs(SceneChangeSL[I]);
          if (TimeMs <> -1) then
          begin
            SetLength(SCArray, Length(SCArray) + 1);
            SCArray[Length(SCArray) - 1] := TimeMs;
          end;
        end;
      end;
    end;
    SceneChangeSL.Free;
  end
end;

// -----------------------------------------------------------------------------

procedure ExtractKeyFramesAVI(Filename : WideString; var KFArray : TIntegerDynArray);
var hr : HRESULT;
    ppfile : IAVIFile;
    psi : TAVIStreamInfo;
    ppavi : IAVIStream;
    FramePos, PreviousFramePos, ConsecutiveKF : Integer;
    FrameRate : Double;
begin
  AVIFileInit;
  hr := AVIFileOpenW(ppfile, @Filename[1], OF_READ or OF_SHARE_DENY_NONE, nil);
  if Succeeded(hr) then
  begin
    hr := AVIFileGetStream(ppfile, ppavi, streamtypeVIDEO, 0);
    if Succeeded(hr) then
    begin
      AVIStreamInfo(ppavi, psi, SizeOf(TAVIStreamInfo));
      Framerate := (psi.dwRate / psi.dwScale);
      FramePos := 0;
      PreviousFramePos := 0;
      ConsecutiveKF := 0;
      while True do
      begin
        FramePos := AVIStreamFindSample(ppavi, framePos, FIND_KEY or FIND_NEXT);
        if (FramePos = -1) then
          Break;
        SetLength(KFArray, Length(KFArray) + 1);
        KFArray[Length(KFArray) - 1] := Trunc(FramePos / Framerate * 1000);

        // Check for keyframe only files
        if (FramePos - PreviousFramePos = 1) then
        begin
          Inc(ConsecutiveKF);
          if (ConsecutiveKF = 100) then
          begin
            SetLength(KFArray, 0);
            Break;
          end;
        end
        else
        begin
          ConsecutiveKF := 0;
        end;

        PreviousFramePos := FramePos;
        Inc(FramePos);
      end;
      //AVIStreamRelease(ppavi);
    end;
    //no need to release interfaces with delphi?
    //AVIFileRelease(ppfile);
  end;
  AVIFileExit;
end;

procedure ExtractKeyFramesMKV(Filename : WideString; var KFArray : TIntegerDynArray);
var MatroskaReader : TMatroskaReader;
    FileReader : TFileReader;
    LogWriterNull : TLogWriterNull;
    ParseResult : Boolean;
    I : Integer;
    CuePoint : PMatroskaCuePoint;
begin
  LogWriterNull := TLogWriterNull.Create;
  FileReader := TFileReader.Create(Filename);
  MatroskaReader := TMatroskaReader.Create(FileReader, LogWriterNull);
  ParseResult := False;
  try
    ParseResult := MatroskaReader.ReadHeader
  except
    on E: Exception do
      begin
        //Writer.AddTextLine(E.Message);
        //ParsingLogWriter.AddTextLine(E.Message);
      end;
  end;
  if ParseResult then
  begin
    for I := 0 to MatroskaReader.Cues.Count-1 do
    begin
      CuePoint := PMatroskaCuePoint(MatroskaReader.Cues[I]);
      SetLength(KFArray, Length(KFArray) + 1);
      KFArray[Length(KFArray) - 1] := Trunc((CuePoint.CueTime *
        MatroskaReader.SegmentInfo.TimecodeScale) / 1000000);
    end;
  end;
  MatroskaReader.Free;
  FileReader.Free;
  LogWriterNull.Free;
end;

procedure ExtractKeyFramesMP4(Filename : WideString; var KFArray : TIntegerDynArray);
var MP4File : TMP4File;
    FileReader : TFileReader;
    LogWriterNull : TLogWriterNull;
    ParseResult : Boolean;
    trackIdx, sampleIdx : Integer;
    track : TMP4Track;
    sample : TMP4Sample;
    ts : Extended;
begin
  LogWriterNull := TLogWriterNull.Create;
  FileReader := TFileReader.Create(Filename);
  MP4File := TMP4File.Create(FileReader, LogWriterNull);

  ParseResult := False;
  try
    ParseResult := MP4File.Parse;
  except
    on E: Exception do
      begin
        //Writer.AddTextLine(E.Message);
        //ParsingLogWriter.AddTextLine(E.Message);
      end;
  end;
  if ParseResult then
  begin
    for trackIdx := 0 to MP4File.TrackList.Count-1 do
    begin
      track := MP4File.TrackList.Items[trackIdx];
      if track.handlerType = 'vide' then
      begin
        for sampleIdx := 0 to Length(track.samples)-1 do
        begin
          sample := track.samples[sampleIdx];
          if sample.Sync then
          begin
            SetLength(KFArray, Length(KFArray) + 1);
            ts := Int64(sample.DecodingTime);
            ts := ts / Int64(track.timescale);
            ts := ts * 1000.0;
            KFArray[Length(KFArray) - 1] := Trunc(ts);
          end;
        end;
        Break;
      end;
    end;
  end;
  MP4File.Free;
  FileReader.Free;
  LogWriterNull.Free;
end;

procedure ExtractSceneChange(Filename : WideString; var SCArray : TIntegerDynArray);
var Ext : WideString;
begin
  SetLength(SCArray, 0);
  Ext := WideLowerCase(WideExtractFileExt(Filename));
  if (Ext = '.avi') then
  begin
    ExtractKeyFramesAVI(Filename, SCArray);
  end
  else if (Ext = '.mkv') then
  begin
    ExtractKeyFramesMKV(Filename, SCArray);
  end
  else if (Ext = '.mp4') then
  begin
    ExtractKeyFramesMP4(Filename, SCArray);
  end;
end;

procedure GenerateSceneChangeFile(Filename : WideString);
var Cmd : WideString;
    StartInfo  : TStartupInfo;
    ProcInfo   : TProcessInformation;
begin
  ZeroMemory(@StartInfo, SizeOf(StartInfo));
  StartInfo.cb := SizeOf(StartInfo);
  StartInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartInfo.wShowWindow := SW_SHOW;
  Cmd := '"' + ExtractFilePath(ParamStr(0)) +
         'vss-companion\generate_scenechange_file.exe" "' +
         Filename + '"';
  if CreateProcessW(nil, PWideChar(Cmd), nil, nil, false,
            NORMAL_PRIORITY_CLASS, nil, nil,
            StartInfo, ProcInfo) then
  begin
    WaitForSingleObject(ProcInfo.hProcess, INFINITE);
    CloseHandle(ProcInfo.hProcess);
    CloseHandle(ProcInfo.hThread);
  end
  else
  begin
    MessageBox(0, 'Couldn''t start generate_scenechange_file.exe.', 'Error', MB_ICONERROR);
  end;
end;

function FindSceneChange(Filename : WideString; Start, Stop : Integer) : Integer;
const
  ReadBuffer = 2400;
var Cmd : WideString;
    Security : TSecurityAttributes;
    ReadPipe, WritePipe : THandle;
    StartInfo : TStartupInfo;
    ProcInfo : TProcessInformation;
    Buffer : Pchar;
    BytesRead : DWord;
    AppRunning : DWord;
    ExitCode: DWord;
begin
  Result := -1;
  with Security do
  begin
    nlength := SizeOf(TSecurityAttributes);
    binherithandle := true;
    lpsecuritydescriptor := nil;
  end;
  if CreatePipe(ReadPipe, WritePipe, @Security, 0) then
  begin
    Buffer := AllocMem(ReadBuffer + 1) ;
    FillChar(StartInfo, SizeOf(StartInfo), #0);
    StartInfo.cb := SizeOf(StartInfo) ;
    StartInfo.hStdInput := ReadPipe;
    StartInfo.hStdOutput := WritePipe;
    StartInfo.dwFlags := STARTF_USESTDHANDLES + STARTF_USESHOWWINDOW;
    StartInfo.wShowWindow := SW_HIDE;
    Cmd := '"' + ExtractFilePath(ParamStr(0)) +
           'vss-companion\find_scenechange.exe" "' + Filename +
           '" --start-time ' + IntToStr(Start) +
           ' --end-time ' + IntToStr(Stop);
    if CreateProcessW(nil, PWideChar(Cmd), @Security, @Security, true,
              NORMAL_PRIORITY_CLASS, nil, nil, StartInfo, ProcInfo) then
    begin
      repeat
        Apprunning := WaitForSingleObject(ProcInfo.hProcess, 500);
      until (Apprunning <> WAIT_TIMEOUT);
      GetExitCodeProcess(ProcInfo.hProcess, ExitCode);
      if (ExitCode = 0) then
      begin
        repeat
          BytesRead := 0;
          ReadFile(ReadPipe, Buffer[0], ReadBuffer, BytesRead, nil);
          Buffer[BytesRead] := #0;
        until (BytesRead < ReadBuffer);
        Result := StrToInt(TrimRight(String(Buffer)));
      end
      else
      begin
        if (ExitCode = 4) then
          MessageBox(0, 'Range is too large.', 'Error', MB_ICONERROR)
        else
          MessageBox(0, pChar(IntToStr(ExitCode)), 'Error', MB_ICONERROR);
      end;
      CloseHandle(ProcInfo.hProcess);
      CloseHandle(ProcInfo.hThread);
      CloseHandle(ReadPipe);
      CloseHandle(WritePipe);
    end
    else
    begin
      MessageBox(0, 'Couldn''t start find_scenechange.exe.', 'Error', MB_ICONERROR);
    end;
    FreeMem(Buffer);
  end
  else
  begin
    MessageBox(0, 'Couldn''t create pipe for find_scenechange.exe.', 'Error', MB_ICONERROR);
  end;
  { MessageBox(0, pChar(IntToStr(Result)), 'Error', MB_ICONERROR); }
end;

// -----------------------------------------------------------------------------
end.
