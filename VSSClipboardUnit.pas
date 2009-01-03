unit VSSClipboardUnit;

interface

uses VirtualTrees, Classes;

procedure CopyVTVToClipboard(vtv: TVirtualStringTree);
procedure PasteClipboard(SubList: TList);

var
  VSSClipBoardFORMAT : Cardinal;

const
  CopyPasteStreamVersion = 1;

implementation

uses Windows, SubStructUnit, SysUtils, MiscToolsUnit, TntClipBrd, TntSysUtils;

//==================================================================================================

procedure CopyVTVToClipboard(vtv: TVirtualStringTree);
var Node : PVirtualNode;
    NodeData : PTreeData;
    Msg : WideString;
    SubtitleRange : TSubtitleRange;
    MemStream : TMemoryStream;
    MemHandle : THandle;
    MemPointer : Pointer;
begin
  if (vtv.SelectedCount <= 0) then
    Exit;

  // TODO : http://delphi.about.com/od/windowsshellapi/a/clipboard_spy_3.htm
  // http://www.delphipages.com/news/detaildocs.cfm?ID=145
  // http://homepage2.nifty.com/Mr_XRAY/Halbow/Notes/N014.html
  // TACtion HandlesTarget

  MemStream := TMemoryStream.Create;
  SaveToStreamInt(MemStream, CopyPasteStreamVersion);
  SaveToStreamInt(MemStream, vtv.SelectedCount);
  Node := vtv.GetFirstSelected;
  while Assigned(Node) do
  begin
    NodeData := vtv.GetNodeData(Node);
    SubtitleRange := NodeData.Range;
    Msg := Msg + Sub2SrtString(SubtitleRange) + CRLF + CRLF;
    SubtitleRange.SaveToStream(MemStream);
    Node := vtv.GetNextSelected(Node);
  end;
  if (Length(Msg) > 0) then
  begin
    TntClipboard.Open;
    try
      TntClipboard.Clear;
      // Set text format data
      TntClipboard.AsWideText := Trim(Msg);
      // Set custom format data
      MemHandle := GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE, MemStream.Size);
      MemPointer := GlobalLock(MemHandle);
      MemStream.Position := 0;
      MemStream.ReadBuffer(MemPointer^, MemStream.Size);
      TntClipboard.SetAsHandle(VSSClipBoardFORMAT, MemHandle);
      GlobalUnLock(MemHandle);
    finally
      TntClipboard.Close;
    end;
  end;
  MemStream.Free;
end;

// -------------------------------------------------------------------------------------------------

procedure PasteClipboard(SubList: TList);
var MemHandle : THandle;
    MemPointer : Pointer;
    MemStream : TMemoryStream;

    StreamVersion, SubCount, i : Integer;
    SubRange : TSubtitleRange;
begin
  if TntClipboard.HasFormat(VSSClipBoardFORMAT) then
  begin
    MemStream := TMemoryStream.Create;
    TntClipboard.Open;
    MemHandle := TntClipboard.GetAsHandle(VSSClipBoardFORMAT);
    MemPointer := GlobalLock(MemHandle);
    MemStream.WriteBuffer(MemPointer^, GlobalSize(MemHandle));
    GlobalUnlock(MemHandle);
    TntClipboard.Close;

    MemStream.Position := 0;
    LoadFromStreamInt(MemStream, StreamVersion);
    if (StreamVersion = CopyPasteStreamVersion) then
    begin
      // Load each sub
      LoadFromStreamInt(MemStream, SubCount);
      for i := 0 to SubCount-1 do
      begin
        SubRange := TSubtitleRange.Create;
        SubRange.LoadFromStream(MemStream);
        SubList.Add(SubRange);
      end;
    end;
    MemStream.Free;
  end;
end;

//==================================================================================================

initialization
  VSSClipBoardFORMAT := RegisterClipboardFormat(PChar('CF_VisualSubSync'));

//==================================================================================================
end.
//==================================================================================================
