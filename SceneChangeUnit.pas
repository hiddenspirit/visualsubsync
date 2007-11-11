unit SceneChangeUnit;

interface

uses Types;

procedure ExtractSceneChange(Filename : WideString; var SCArray : TIntegerDynArray);
procedure SaveSceneChange(Filename : WideString; var SCArray : TIntegerDynArray);
procedure LoadSceneChange(Filename : WideString; var SCArray : TIntegerDynArray);

implementation

uses Windows, VFW, SysUtils, TntClasses, TntSysUtils, MiscToolsUnit,
  MatroskaHelper, LogWriterIntf;

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
    FramePos : Integer;
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
      while True do
      begin
        FramePos := AVIStreamFindSample(ppavi, framePos, FIND_KEY or FIND_NEXT);
        if (FramePos = -1) then
          Break;
        SetLength(KFArray, Length(KFArray) + 1);
        KFArray[Length(KFArray) - 1] := Trunc(FramePos / Framerate * 1000);
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
  end;
end;

// -----------------------------------------------------------------------------
end.
