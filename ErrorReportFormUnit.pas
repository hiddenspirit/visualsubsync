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

unit ErrorReportFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, ComCtrls, TntComCtrls, Menus, TntMenus, StdCtrls,
  TntStdCtrls, SubStructUnit;

type
  TErrorType = (etOverlapping, etTooShort, etTooLong, etLineTooLong);

type
  TErrorTreeData = record
    Range : TSubtitleRange;
    Color : TColor;
    GlobalMsg : WideString;
    ContextMsg : WideString; // a context dependant msg (like the number of Char/s reached ...)
    Filename : WideString; // plugin filename
    PluginName : WideString;
  end;
  PErrorTreeData = ^TErrorTreeData;

  TErrorReportForm = class(TForm)
    vtvErrorList: TVirtualDrawTree;
    TntStatusBar1: TTntStatusBar;
    ErrorListPopupMenu: TTntPopupMenu;
    pmiClearAll: TTntMenuItem;
    TntMainMenu1: TTntMainMenu;
    miFile: TTntMenuItem;
    miRecheck: TTntMenuItem;
    miClose: TTntMenuItem;
    N1: TTntMenuItem;
    miPreferences: TTntMenuItem;
    miClearAll: TTntMenuItem;
    N2: TTntMenuItem;
    pmiFixError: TTntMenuItem;
    pmiFixAllXXX: TTntMenuItem;
    pmiFixAllYYY: TTntMenuItem;
    miClear: TTntMenuItem;
    N3: TTntMenuItem;
    procedure pmiClearAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure vtvErrorListDrawNode(Sender: TBaseVirtualTree;
      const PaintInfo: TVTPaintInfo);
    procedure FormDestroy(Sender: TObject);
    procedure vtvErrorListDblClick(Sender: TObject);
    procedure miCloseClick(Sender: TObject);
    procedure miClearAllClick(Sender: TObject);
    procedure pmiFixErrorClick(Sender: TObject);
    procedure ErrorListPopupMenuPopup(Sender: TObject);
    procedure pmiFixAllXXXClick(Sender: TObject);
    procedure pmiFixAllYYYClick(Sender: TObject);
    procedure vtvErrorListFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure miClearClick(Sender: TObject);
  private
    { Private declarations }
    BoldFont : HFONT;
    NormalFont : HFONT;
    procedure CreateFont;

    function CalculateNodeHeight : Integer;
  public
    { Public declarations }
    procedure AddError(Range : TSubtitleRange; Color : TColor;
      GlobalMsg, ContextMsg, Filename, PluginName : WideString);
    procedure Clear;
    procedure DeleteError(Range : TSubtitleRange);
    procedure GoToNextError;
    procedure GoToPreviousError;
  end;

var
  ErrorReportForm: TErrorReportForm;

const
  NodeLeftMargin : Integer = 10;
  NodeTopMargin : Integer = 2;
  NodeInterline : Double = 1.1;

implementation

uses Types, MiscToolsUnit, Main, PreferencesFormUnit, CursorManager;

{$R *.dfm}

//------------------------------------------------------------------------------

procedure TErrorReportForm.FormCreate(Sender: TObject);
begin
  with vtvErrorList do
  begin
    Canvas.Font.Name := 'Arial';
    Canvas.Font.Size := 8;
    NodeDataSize := SizeOf(TErrorTreeData);
    TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toReportMode];
    TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toFullRowSelect];
    TreeOptions.PaintOptions := TreeOptions.PaintOptions -
      [toShowTreeLines,toShowRoot] +
      [toHideFocusRect, toShowHorzGridLines, toShowVertGridLines];
  end;
  CreateFont;
  vtvErrorList.DefaultNodeHeight := CalculateNodeHeight;
end;

//------------------------------------------------------------------------------

function TErrorReportForm.CalculateNodeHeight : Integer;
var OldFont : HFONT;
    DC : HDC;
    xysize : Size;
begin
  Result := 0;
  DC := vtvErrorList.Canvas.Handle;
  Result := Result + NodeTopMargin;
  OldFont := SelectObject(DC, BoldFont);
  GetTextExtentPoint32W(DC, 'W', 1, xysize);
  Result := Round(Result + (xysize.cy * NodeInterline));
  SelectObject(DC, NormalFont);
  GetTextExtentPoint32W(DC, 'W', 1, xysize);
  Result := Round(Result + (xysize.cy * NodeInterline));
  GetTextExtentPoint32W(DC, 'W', 1, xysize);
  Result := Round(Result + (xysize.cy * NodeInterline));
  SelectObject(DC, OldFont);
  Result := Result + NodeTopMargin;
end;

//------------------------------------------------------------------------------

procedure TErrorReportForm.Clear;
begin
  vtvErrorList.Clear;
end;

//------------------------------------------------------------------------------

procedure TErrorReportForm.pmiClearAllClick(Sender: TObject);
begin
  Clear;
end;

//------------------------------------------------------------------------------

procedure TErrorReportForm.CreateFont;
var FontLOG : LOGFONT;
begin
  if BoldFont <> 0 then
    DeleteObject(BoldFont);
  if NormalFont <> 0 then
    DeleteObject(NormalFont);
  ZeroMemory(@FontLOG, SizeOf(FontLOG));
  StrCopy(FontLOG.lfFaceName,PChar(vtvErrorList.Canvas.Font.Name));
  FontLOG.lfHeight := vtvErrorList.Canvas.Font.Height;
  FontLOG.lfCharSet := vtvErrorList.Canvas.Font.Charset;
  FontLOG.lfWeight := FW_BOLD;
  BoldFont := CreateFontIndirect(FontLOG);
  FontLOG.lfWeight := FW_NORMAL;
  NormalFont := CreateFontIndirect(FontLOG);
end;

//------------------------------------------------------------------------------

procedure TErrorReportForm.vtvErrorListDrawNode(Sender: TBaseVirtualTree;
  const PaintInfo: TVTPaintInfo);
var x, y : Integer;
    OldColor : Cardinal;
    OldFont : HFONT;
    DC : HDC;
    Msg : WideString;
    xysize : Size;
    pErrorData : PErrorTreeData;
    OldBrush, CurrentBrush : HBRUSH;
    OldPen, CurrentPen : HPEN;
begin
  pErrorData := vtvErrorList.GetNodeData(PaintInfo.Node);
  
  vtvErrorList.Canvas.Lock;
  DC := PaintInfo.Canvas.Handle;
  OldColor := GetTextColor(DC);

  x := NodeLeftMargin;
  y := NodeTopMargin;

  if (vtvErrorList.Selected[PaintInfo.Node]) then
    SetTextColor(DC, ColorToRGB(clHighlightText))
  else
    SetTextColor(DC, ColorToRGB(clWindowText));
  SetBKMode(DC, TRANSPARENT);

  OldFont := SelectObject(DC, BoldFont);

  Msg := pErrorData.GlobalMsg;
  if pErrorData.ContextMsg <> '' then
    Msg := Msg + ' (' + pErrorData.ContextMsg +')';
  GetTextExtentPoint32W(DC, PWideChar(Msg), Length(Msg), xysize);
  TextOutW(DC, x + xysize.cy + 2, y, PWideChar(Msg), Length(Msg));

  CurrentPen := CreatePen(PS_SOLID,1,0);
  OldPen := SelectObject(DC, CurrentPen);

  CurrentBrush := CreateSolidBrush(pErrorData.Color);
  OldBrush := SelectObject(DC, CurrentBrush);
  Ellipse(DC, x, y+2, x+xysize.cy-4, y+xysize.cy-2);
  SelectObject(DC, OldBrush);
  DeleteObject(CurrentBrush);
  SelectObject(DC, OldPen);
  DeleteObject(CurrentPen);
  y := Round(y + (xysize.cy * NodeInterline));

  SelectObject(DC, NormalFont);
  Msg := TimeMsToString(pErrorData.Range.StartTime) + ' -> ' + TimeMsToString(pErrorData.Range.StopTime);
  TextOutW(DC, x, y, PWideChar(Msg), Length(Msg));
  GetTextExtentPoint32W(DC, PWideChar(Msg), Length(Msg), xysize);
  y := Round(y + (xysize.cy * NodeInterline));

  Msg := StringConvertCRLFToPipe(pErrorData.Range.Text);
  TextOutW(DC, x, y, PWideChar(Msg), Length(Msg));

  if (vtvErrorList.FocusedNode = PaintInfo.Node) then
    DrawFocusRect(DC, PaintInfo.CellRect);

  SelectObject(DC, OldFont);
  SetTextColor(DC, OldColor);
  vtvErrorList.Canvas.Unlock;  
end;

//------------------------------------------------------------------------------

procedure TErrorReportForm.FormDestroy(Sender: TObject);
begin
  if BoldFont <> 0 then
    DeleteObject(BoldFont);
  if NormalFont <> 0 then
    DeleteObject(NormalFont);
end;

//------------------------------------------------------------------------------

procedure TErrorReportForm.AddError(Range : TSubtitleRange; Color : TColor;
  GlobalMsg, ContextMsg, Filename, PluginName : WideString);
var Node : PVirtualNode;
    pError : PErrorTreeData;
begin
  Node := vtvErrorList.AddChild(nil);
  pError := vtvErrorList.GetNodeData(Node);
  pError.Color := Color;
  pError.Range := Range;
  pError.GlobalMsg := GlobalMsg;
  pError.ContextMsg := ContextMsg;
  pError.Filename := Filename;
  pError.PluginName := PluginName;
end;

//------------------------------------------------------------------------------

procedure TErrorReportForm.vtvErrorListDblClick(Sender: TObject);
var pError : PErrorTreeData;
begin
  if Assigned(vtvErrorList.FocusedNode) then
  begin
    pError := vtvErrorList.GetNodeData(vtvErrorList.FocusedNode);
    MainForm.SelectNode(pError.Range.Node);
  end
end;

//------------------------------------------------------------------------------

procedure TErrorReportForm.DeleteError(Range : TSubtitleRange);
var Node : PVirtualNode;
    NodeData : PErrorTreeData;
    DeleteList : TList;
    i : integer;
begin
  DeleteList := TList.Create;
  Node := vtvErrorList.GetFirst;
  while Assigned(Node) do
  begin
    NodeData := vtvErrorList.GetNodeData(Node);
    if Range = NodeData.Range then
      DeleteList.Add(Node);
    Node := vtvErrorList.GetNext(Node);
  end;
  for i:=0 to DeleteList.Count-1 do
    vtvErrorList.DeleteNode(PVirtualNode(DeleteList[i]));
  DeleteList.Free;
end;

//------------------------------------------------------------------------------

procedure TErrorReportForm.miCloseClick(Sender: TObject);
begin
  Close;
end;

//------------------------------------------------------------------------------

procedure TErrorReportForm.miClearAllClick(Sender: TObject);
begin
  Clear;
end;

//------------------------------------------------------------------------------

procedure TErrorReportForm.pmiFixErrorClick(Sender: TObject);
var ErrorLst : TList;
    Node : PVirtualNode;
begin
  ErrorLst := TList.Create;
  Node := vtvErrorList.GetFirstSelected;
  while Assigned(Node) do
  begin
    ErrorLst.Add(vtvErrorList.GetNodeData(Node));
    Node := vtvErrorList.GetNextSelected(Node);
  end;
  MainForm.FixErrorInList(ErrorLst);
  ErrorLst.Free;
  vtvErrorList.Repaint;
end;

//------------------------------------------------------------------------------

procedure TErrorReportForm.ErrorListPopupMenuPopup(Sender: TObject);
var Node : PVirtualNode;
    NodeData : PErrorTreeData;
    pmiFixErrorEnabled : Boolean;
begin
  pmiFixErrorEnabled := False;
  Node := vtvErrorList.GetFirstSelected;
  while Assigned(Node) and (not pmiFixErrorEnabled) do
  begin
    NodeData := vtvErrorList.GetNodeData(Node);
    pmiFixErrorEnabled := pmiFixErrorEnabled or MainForm.CanFixError(NodeData.Filename);
    Node := vtvErrorList.GetNextSelected(Node);
  end;
  pmiFixError.Enabled := pmiFixErrorEnabled;

  pmiFixAllXXX.Enabled := False;
  pmiFixAllYYY.Enabled := False;
  if pmiFixErrorEnabled and (vtvErrorList.SelectedCount = 1) then
  begin
    pmiFixAllXXX.Enabled := True;
    NodeData := vtvErrorList.GetNodeData(vtvErrorList.FocusedNode);
    pmiFixAllYYY.Enabled := (NodeData.ContextMsg <> '');
  end;
end;

//------------------------------------------------------------------------------

procedure TErrorReportForm.pmiFixAllXXXClick(Sender: TObject);
var ErrorLst : TList;
    Node : PVirtualNode;
    FocusedNodeData, NodeData : PErrorTreeData;
    CM : ICursorManager;
begin
  CM := TCursorManager.Create(crHourGlass);
  if Assigned(vtvErrorList.FocusedNode) then
  begin
    FocusedNodeData := vtvErrorList.GetNodeData(vtvErrorList.FocusedNode);
    ErrorLst := TList.Create;
    Node := vtvErrorList.GetFirst;
    while Assigned(Node) do
    begin
      NodeData := vtvErrorList.GetNodeData(Node);
      if (NodeData.PluginName = FocusedNodeData.PluginName) then
        ErrorLst.Add(NodeData);
      Node := vtvErrorList.GetNext(Node);
    end;
    MainForm.FixErrorInList(ErrorLst);
    ErrorLst.Free;
    vtvErrorList.Repaint;
  end;
end;

//------------------------------------------------------------------------------

procedure TErrorReportForm.pmiFixAllYYYClick(Sender: TObject);
var ErrorLst : TList;
    Node : PVirtualNode;
    FocusedNodeData, NodeData : PErrorTreeData;
    CM : ICursorManager;
begin
  CM := TCursorManager.Create(crHourGlass);
  if Assigned(vtvErrorList.FocusedNode) then
  begin
    FocusedNodeData := vtvErrorList.GetNodeData(vtvErrorList.FocusedNode);
    ErrorLst := TList.Create;
    Node := vtvErrorList.GetFirst;
    while Assigned(Node) do
    begin
      NodeData := vtvErrorList.GetNodeData(Node);
      if (NodeData.PluginName = FocusedNodeData.PluginName) and
         (NodeData.ContextMsg = FocusedNodeData.ContextMsg) then
      begin
        ErrorLst.Add(NodeData);
      end;
      Node := vtvErrorList.GetNext(Node);
    end;
    MainForm.FixErrorInList(ErrorLst);
    ErrorLst.Free;
    vtvErrorList.Repaint;
  end;
end;

//------------------------------------------------------------------------------

procedure TErrorReportForm.vtvErrorListFocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var pError : PErrorTreeData;
begin
  if Assigned(vtvErrorList.FocusedNode) then
  begin
    pError := vtvErrorList.GetNodeData(vtvErrorList.FocusedNode);
    MainForm.SelectNode(pError.Range.Node);
  end
end;

//------------------------------------------------------------------------------

procedure TErrorReportForm.GoToNextError;
var NextNode : PVirtualNode;

begin
  if Assigned(vtvErrorList.FocusedNode) then
  begin
    NextNode := vtvErrorList.GetNext(vtvErrorList.FocusedNode);
    if Assigned(NextNode) then
    begin
      vtvErrorList.Selected[vtvErrorList.FocusedNode] := False;
      vtvErrorList.Selected[NextNode] := True;
      vtvErrorList.FocusedNode := NextNode;
    end;
  end
end;

//------------------------------------------------------------------------------

procedure TErrorReportForm.GoToPreviousError;
var PreviousNode : PVirtualNode;

begin
  if Assigned(vtvErrorList.FocusedNode) then
  begin
    PreviousNode := vtvErrorList.GetPrevious(vtvErrorList.FocusedNode);
    if Assigned(PreviousNode) then
    begin
      vtvErrorList.Selected[vtvErrorList.FocusedNode] := False;
      vtvErrorList.Selected[PreviousNode] := True;
      vtvErrorList.FocusedNode := PreviousNode;
    end;
  end
end;

//------------------------------------------------------------------------------

procedure TErrorReportForm.miClearClick(Sender: TObject);
var FirstSel, Cursor : PVirtualNode;
begin
  FirstSel := vtvErrorList.GetFirstSelected;
  Cursor := FirstSel;
  while Assigned(Cursor) do
  begin
    if (not (vsSelected in Cursor.States)) then
      Break;
    Cursor := vtvErrorList.GetNext(Cursor);
  end;

  if not Assigned(Cursor) and Assigned(FirstSel) then
  begin
    Cursor := vtvErrorList.GetPrevious(FirstSel);
  end;
  vtvErrorList.DeleteSelectedNodes;
  if Assigned(Cursor) then
  begin
    vtvErrorList.FocusedNode := Cursor;
    vtvErrorList.Selected[Cursor] := True;
  end;
end;

//------------------------------------------------------------------------------
end.
//------------------------------------------------------------------------------
