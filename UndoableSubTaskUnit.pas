// -----------------------------------------------------------------------------
//  VisualSubSync
// -----------------------------------------------------------------------------
//  Copyright (C) 2007 Christophe Paris
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
unit UndoableSubTaskUnit;

interface

uses UndoableTaskUnit, SubStructUnit, Contnrs, Graphics, TntComCtrls;

type
  TUndoableTaskIndexed = class(TUndoableTask)
  private
    FIndexes : array of Integer; // Indexes of subtitles in FWavDisplayer
    FCount : Integer; // Number of indexes
  public
    destructor Destroy; override;
    procedure AddSubtitleIndex(Index : Integer);
    procedure SetCapacity(Capacity : Integer);
  end;

  TUndoableDelayTask = class(TUndoableTaskIndexed)
  private
    FDelayInMs : Integer;
    FDelayShiftType : TDelayShiftType;
  public
    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;

    procedure SetDelayInMs(DelayInMs : Integer);
    procedure SetDelayShiftType(DelayShiftType : TDelayShiftType);
  end;

  TUndoableAddTask = class(TUndoableTask)
  private
    FStartTime, FStopTime : Integer;
    FText : WideString;
    FAutoSelect : Boolean;
    FIndex : Integer;
  public
    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;

    procedure SetTime(StartTime, StopTime : Integer);
    procedure SetText(Text : WideString);
    procedure SetAutoSelect(AutoSelect : Boolean);
  end;

  TUndoableDeleteTask = class(TUndoableTaskIndexed)
  private
    FDeletedSubs : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;
  end;

  TUndoableMultiAddTask = class(TUndoableTaskIndexed)
  private
    FDeletedSubs : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;
  end;

  TUndoableSetTimeTask = class(TUndoableTask)
  private
    FNewStartTime, FNewStopTime : Integer;
    FOldStartTime, FOldStopTime : Integer;
    FIndex : Integer;
  public
    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;

    procedure SetData(Index, OldStartTime, OldStopTime,
      NewStartTime, NewStopTime : Integer);
  end;

  TUndoableSplitTask = class(TUndoableTask)
  private
    FIndex : Integer;
    FStartTime, FStopTime, FSplitTime : Integer;
  public
    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;

    procedure SetData(Index, StartTime, StopTime, SplitTime : Integer);
  end;

  TUndoableMergeTask = class(TUndoableTaskIndexed)
  private
    FDeletedSubs : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;
  end;

  TUndoableSubTextTask = class(TUndoableTask)
  private
    FUndoableTextTask : TUndoableTask;
    FIndex : Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;

    procedure SetData(Index : Integer; UndoableTextTask : TUndoableTask);
  end;

  TUndoableCompositeTask = class(TUndoableTask)
  private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;

    procedure AddTask(UndoableTask : TUndoableTask);
  end;

  TChangeSubData = class
    FIndex : Integer;
    FNewStartTime, FOldStartTime : Integer;
    FNewStopTime, FOldStopTime : Integer;
    FNewText, FOldText : WideString;

    constructor Create(Index : Integer);
    function StartChanged : Boolean;
    function StopChanged : Boolean;
    function TextChanged : Boolean;
    function GetStart(ForUndo : Boolean) : Integer;
    function GetStop(ForUndo : Boolean) : Integer;
    function GetText(ForUndo : Boolean) : WideString;
  end;

  // Used for error correction
  TUndoableMultiChangeTask = class(TUndoableTask)
  private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;

    procedure AddData(ChangeSubData : TChangeSubData);
    function GetData(Index : Integer) : TChangeSubData;
    function GetCount : Integer;
  end;

  TUndoablePipeTask = class(TUndoableTask)
  private
    FActionType, FOldSelStart, FOldSelLength : Integer;
    FOldSelText : WideString;
    FNewSelColor, FOldSelColor : TColor;
  public
    procedure DoTask; override;
    function GetName : WideString; override;
    procedure UndoTask; override;

    procedure SetData(ActionType : Integer; RichEdit : TTntRichEdit; NewColor : TColor);
  end;

implementation

uses Main, MiscToolsUnit, VirtualTrees;

//==============================================================================

destructor TUndoableTaskIndexed.Destroy;
begin
  SetLength(FIndexes, 0);
  FIndexes := nil;
  inherited;
end;

procedure TUndoableTaskIndexed.AddSubtitleIndex(Index : Integer);
begin
  FIndexes[FCount] := Index;
  Inc(FCount);
end;

procedure TUndoableTaskIndexed.SetCapacity(Capacity : Integer);
begin
  SetLength(FIndexes, Capacity);
end;

//-------------------------------------------------------------------------------

procedure TUndoableDelayTask.UndoTask;
begin
  MainForm.ApplyDelay(FIndexes, -FDelayInMs, FDelayShiftType);
end;

procedure TUndoableDelayTask.DoTask;
begin
  MainForm.ApplyDelay(FIndexes, FDelayInMs, FDelayShiftType);
end;

function TUndoableDelayTask.GetName : WideString;
begin
  Result := 'Delay ' + TimeMsToString(FDelayInMs);
end;

procedure TUndoableDelayTask.SetDelayInMs(DelayInMs : Integer);
begin
  FDelayInMs := DelayInMs;
end;

procedure TUndoableDelayTask.SetDelayShiftType(DelayShiftType : TDelayShiftType);
begin
  FDelayShiftType := DelayShiftType;
end;

//------------------------------------------------------------------------------

procedure TUndoableAddTask.DoTask;
var NewNode : PVirtualNode;
begin
  NewNode := MainForm.AddSubtitle(FStartTime, FStopTime, FText, FAutoSelect);
  FIndex := NewNode.Index;
end;

function TUndoableAddTask.GetName : WideString;
begin
  Result := 'Add subtitle';
end;

procedure TUndoableAddTask.UndoTask;
begin
  MainForm.DeleteSubtitle(FIndex);
end;

procedure TUndoableAddTask.SetTime(StartTime, StopTime : Integer);
begin
  FStartTime := StartTime;
  FStopTime := StopTime;
end;

procedure TUndoableAddTask.SetText(Text : WideString);
begin
  FText := Text;
end;

procedure TUndoableAddTask.SetAutoSelect(AutoSelect : Boolean);
begin
  FAutoSelect := AutoSelect;
end;

//------------------------------------------------------------------------------

constructor TUndoableMultiAddTask.Create;
begin
  inherited;
  FDeletedSubs := TObjectList.Create;
  FDeletedSubs.OwnsObjects := True;
end;

destructor TUndoableMultiAddTask.Destroy;
begin
  FDeletedSubs.Free;
  inherited;
end;

procedure TUndoableMultiAddTask.DoTask;
begin
  // Lazy task
  MainForm.RestoreSubtitles(FDeletedSubs);
  FDeletedSubs.Clear;
end;

function TUndoableMultiAddTask.GetName : WideString;
begin
  Result := 'MultiAdd';
end;

procedure TUndoableMultiAddTask.UndoTask;
begin
  // First clone subtitles
  MainForm.CloneSubtitles(FIndexes, FDeletedSubs);
  // Now delete them
  MainForm.DeleteSubtitles(FIndexes);
end;

//------------------------------------------------------------------------------

constructor TUndoableDeleteTask.Create;
begin
  inherited;
  FDeletedSubs := TObjectList.Create;
  FDeletedSubs.OwnsObjects := True;
end;

destructor TUndoableDeleteTask.Destroy;
begin
  FDeletedSubs.Free;
  inherited;
end;

procedure TUndoableDeleteTask.DoTask;
begin
  // First clone subtitles
  MainForm.CloneSubtitles(FIndexes, FDeletedSubs);
  // Now delete them
  MainForm.DeleteSubtitles(FIndexes);
end;

function TUndoableDeleteTask.GetName : WideString;
begin
  Result := 'Delete';
end;

procedure TUndoableDeleteTask.UndoTask;
begin
  MainForm.RestoreSubtitles(FDeletedSubs);
  FDeletedSubs.Clear;
end;

//------------------------------------------------------------------------------

procedure TUndoableSetTimeTask.DoTask;
begin
  FIndex := MainForm.SetSubtitleTime(FIndex, FNewStartTime, FNewStopTime);
end;

function TUndoableSetTimeTask.GetName : WideString;
begin
  Result := 'Set time';
end;

procedure TUndoableSetTimeTask.UndoTask;
begin
  FIndex := MainForm.SetSubtitleTime(FIndex, FOldStartTime, FOldStopTime);
end;

procedure TUndoableSetTimeTask.SetData(Index, OldStartTime, OldStopTime,
  NewStartTime, NewStopTime : Integer);
begin
  FIndex := Index;
  FOldStartTime := OldStartTime;
  FOldStopTime := OldStopTime;
  FNewStartTime := NewStartTime;
  FNewStopTime := NewStopTime;
end;

//------------------------------------------------------------------------------

procedure TUndoableSplitTask.DoTask;
begin
  MainForm.SplitSubtitle(FIndex, FSplitTime);
end;

function TUndoableSplitTask.GetName : WideString;
begin
  Result := 'Split';
end;

procedure TUndoableSplitTask.UndoTask;
begin
  MainForm.DeleteSubtitle(FIndex + 1);
  MainForm.SetSubtitleTime(FIndex, FStartTime, FStopTime);
end;

procedure TUndoableSplitTask.SetData(Index, StartTime, StopTime, SplitTime : Integer);
begin
  FIndex := Index;
  FStartTime := StartTime;
  FStopTime := StopTime;
  FSplitTime := SplitTime;
end;

//------------------------------------------------------------------------------

constructor TUndoableMergeTask.Create;
begin
  inherited;
  FDeletedSubs := TObjectList.Create;
  FDeletedSubs.OwnsObjects := True;
end;

destructor TUndoableMergeTask.Destroy;
begin
  FDeletedSubs.Free;
  inherited;
end;

procedure TUndoableMergeTask.DoTask;
var SubRange : TSubtitleRange;
    Node : PVirtualNode;
begin
  // Calculate the merged subtitle
  SubRange := MainForm.MergeSubtitles(FIndexes);
  // Clone subtitles
  MainForm.CloneSubtitles(FIndexes, FDeletedSubs);
  // Now delete them
  MainForm.DeleteSubtitles(FIndexes);
  // Add back the merged subtitle
  Node := MainForm.AddSubtitle(SubRange);
  // Focus the merged node
  MainForm.FocusNode(Node, False);
  MainForm.ClearWAVSelection;
  // Free temporarily created merged sub
  SubRange.Free;
end;

function TUndoableMergeTask.GetName : WideString;
begin
  Result := 'Merge';
end;

procedure TUndoableMergeTask.UndoTask;
begin
  // Delete the merged subtitle
  MainForm.DeleteSubtitle(FIndexes[0]);
  // Restaure deleted subtitles
  MainForm.RestoreSubtitles(FDeletedSubs);
  FDeletedSubs.Clear;
end;

// -----------------------------------------------------------------------------

constructor TUndoableSubTextTask.Create;
begin
  FUndoableTextTask := nil;
end;

destructor TUndoableSubTextTask.Destroy;
begin
  if Assigned(FUndoableTextTask) then
  begin
    FUndoableTextTask.Free;
  end
end;

procedure TUndoableSubTextTask.DoTask;
begin
  MainForm.FocusNodeAt(FIndex);
  FUndoableTextTask.DoTask;
end;

function TUndoableSubTextTask.GetName : WideString;
begin
  FUndoableTextTask.GetName;
end;

procedure TUndoableSubTextTask.UndoTask;
begin
  MainForm.FocusNodeAt(FIndex);
  FUndoableTextTask.UndoTask;
end;

procedure TUndoableSubTextTask.SetData(Index : Integer; UndoableTextTask : TUndoableTask);
begin
  FIndex := Index;
  FUndoableTextTask := UndoableTextTask;
end;

// -----------------------------------------------------------------------------

constructor TUndoableCompositeTask.Create;
begin
  FList := TObjectList.Create;
  FList.OwnsObjects := True;
end;

destructor TUndoableCompositeTask.Destroy;
begin
  FList.Free;
end;

procedure TUndoableCompositeTask.DoTask;
var i : Integer;
begin
  for i := 0 to FList.Count-1 do
  begin
    TUndoableTask(FList[i]).DoTask;
  end;
end;

function TUndoableCompositeTask.GetName : WideString;
begin
  Result := 'TUndoableCompositeTask';
end;

procedure TUndoableCompositeTask.UndoTask;
var i : Integer;
begin
  for i := FList.Count-1 downto 0 do
  begin
    TUndoableTask(FList[i]).UndoTask;
  end;
end;

procedure TUndoableCompositeTask.AddTask(UndoableTask : TUndoableTask);
begin
  FList.Add(UndoableTask);
end;

// -----------------------------------------------------------------------------

constructor TChangeSubData.Create(Index : Integer);
begin
  FIndex := Index;
  FNewStartTime := -1; FOldStartTime := -1;
  FNewStopTime := -1; FOldStopTime := -1;
  FNewText := ''; FOldText := '';
end;

function TChangeSubData.StartChanged : Boolean;
begin
  Result := (FOldStartTime <> -1)
end;

function TChangeSubData.StopChanged : Boolean;
begin
  Result := (FOldStopTime <> -1)
end;

function TChangeSubData.TextChanged : Boolean;
begin
  Result := (FNewText <> FOldText)
end;

function TChangeSubData.GetStart(ForUndo : Boolean) : Integer;
begin
  if ForUndo then Result := FOldStartTime else Result := FNewStartTime;
end;

function TChangeSubData.GetStop(ForUndo : Boolean) : Integer;
begin
  if ForUndo then Result := FOldStopTime else Result := FNewStopTime;
end;

function TChangeSubData.GetText(ForUndo : Boolean) : WideString;
begin
  if ForUndo then Result := FOldText else Result := FNewText;
end;

constructor TUndoableMultiChangeTask.Create;
begin
  FList := TObjectList.Create;
  FList.OwnsObjects := True;
end;

destructor TUndoableMultiChangeTask.Destroy;
begin
  FList.Free;
end;

procedure TUndoableMultiChangeTask.DoTask;
begin
  MainForm.ProcessMultiChangeSub(FList, False);
end;

function TUndoableMultiChangeTask.GetName : WideString;
begin
  Result := 'TUndoableMultiChangeTask';
end;

procedure TUndoableMultiChangeTask.UndoTask;
begin
  MainForm.ProcessMultiChangeSub(FList, True);
end;

procedure TUndoableMultiChangeTask.AddData(ChangeSubData : TChangeSubData);
begin
  FList.Add(ChangeSubData);
end;

function TUndoableMultiChangeTask.GetData(Index : Integer) : TChangeSubData;
var i : Integer;
    ChangeSubData : TChangeSubData;
begin
  for i:=0 to FList.Count-1 do
  begin
    ChangeSubData := TChangeSubData(FList[i]);
    if (ChangeSubData.FIndex = Index) then
    begin
      Result := ChangeSubData;
      Exit;
    end;
  end;
  Result := nil;
end;

function TUndoableMultiChangeTask.GetCount : Integer;
begin
  Result := FList.Count;
end;

// -----------------------------------------------------------------------------

procedure TUndoablePipeTask.DoTask;
var NewSelLen : Integer;
begin
  if (FActionType = 0) then
  begin
    MainForm.MemoTextPipe.SelStart := FOldSelStart;
    MainForm.MemoTextPipe.SelLength := FOldSelLength;
    MainForm.MemoTextPipe.SelAttributes.Color := FNewSelColor;
  end
  else if (FActionType = 1) then
  begin
    MainForm.MemoTextPipe.SelStart := FOldSelStart;
    MainForm.MemoTextPipe.SelLength := FOldSelLength;
    MainForm.MemoTextPipe.ClearSelection;
  end
  else if (FActionType = 2) then
  begin
    MainForm.MemoTextPipe.SelStart := 0;
    MainForm.MemoTextPipe.SelLength := FOldSelStart + FOldSelLength;
    FOldSelText := MainForm.MemoTextPipe.SelText;
    MainForm.MemoTextPipe.ClearSelection;
  end
end;

function TUndoablePipeTask.GetName : WideString;
begin
  Result := 'TUndoablePipeTask';
end;

procedure TUndoablePipeTask.UndoTask;
begin
  if FActionType = 0 then
  begin
    MainForm.MemoTextPipe.SelStart := FOldSelStart;
    MainForm.MemoTextPipe.SelLength := FOldSelLength;
    MainForm.MemoTextPipe.SelAttributes.Color := FOldSelColor;
  end
  else if FActionType = 1 then
  begin
    MainForm.MemoTextPipe.SelStart := FOldSelStart;
    MainForm.MemoTextPipe.SelLength := 0;
    MainForm.MemoTextPipe.SelText := FOldSelText;
    MainForm.MemoTextPipe.SelStart := FOldSelStart;
    MainForm.MemoTextPipe.SelLength := FOldSelLength;
  end
  else if FActionType = 2 then
  begin
    MainForm.MemoTextPipe.SelStart := 0;
    MainForm.MemoTextPipe.SelLength := 0;
    MainForm.MemoTextPipe.SelText := FOldSelText;
    MainForm.MemoTextPipe.SelStart := FOldSelStart;
    MainForm.MemoTextPipe.SelLength := FOldSelLength;
  end;
end;

procedure TUndoablePipeTask.SetData(ActionType : Integer;
  RichEdit : TTntRichEdit; NewColor : TColor);
begin
  FActionType := ActionType;
  FOldSelStart := RichEdit.SelStart;
  FOldSelLength := RichEdit.SelLength;
  FOldSelText := RichEdit.SelText;
  FOldSelColor := RichEdit.SelAttributes.Color;
  FNewSelColor := NewColor;
end;

// -----------------------------------------------------------------------------

end.
