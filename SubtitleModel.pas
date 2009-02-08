unit SubtitleModel;

interface

uses Classes, Contnrs, DeCAL;
  
type
  // Forward Declarations
  TSubtitleModel = class;

  TSubtitleItem = class(TObject)
  private
    Index : Integer;   
    Start : Integer;
    Stop : Integer;
    Text : WideString;
  public
    constructor Create;
      
    function GetIndex : Integer;
    function GetStart : Integer;
    function GetStop : Integer;
    function GetText : WideString;
  end;

  
  TUndoableSubTask = class(TObject)
  private
    FSubtitleModel : TSubtitleModel;
  public
    constructor Create(SubtitleModel : TSubtitleModel); virtual;
    
    procedure DoTask; virtual; abstract;
    procedure UndoTask; virtual; abstract;
  end;

  TCreateSubTask = class(TUndoableSubTask)
  private
    FSubtitleIndex : Integer;
  public
    procedure DoTask; override;
    procedure UndoTask; override;
  end;

  TSetStartTask = class(TUndoableSubTask)
  private
    FSubtitleIndex : Integer;
    FOldStart, FNewStart : Integer;
  public
    procedure DoTask; override;
    procedure UndoTask; override;
  end;

  TSetStopTask = class(TUndoableSubTask)
  private
    FSubtitleIndex : Integer;
    FOldStop, FNewStop : Integer;
  public
    procedure DoTask; override;
    procedure UndoTask; override;
  end;

  TSetTextTask = class(TUndoableSubTask)
  private
    FSubtitleIndex : Integer;
    FOldText, FNewText : WideString;
  public
    procedure DoTask; override;
    procedure UndoTask; override;
  end;

  TDeleteSubTask = class(TUndoableSubTask)
  private
    FSubtitleIndex : Integer;
    FOldSubtitle : TSubtitleItem;
  public
    destructor Destroy; override;
    
    procedure DoTask; override;
    procedure UndoTask; override;
  end;

  TCompositeSubTask = class(TUndoableSubTask)
  private
    FTasks : TObjectList;
  public
    constructor Create(SubtitleModel : TSubtitleModel); override;
    destructor Destroy; override;

    procedure Add(Task : TUndoableSubTask);

    procedure DoTask; override;
    procedure UndoTask; override;
  end;



  TSubtitleModel = class(TObject)
  private
    FSortedSubs : DArray;
    FTransaction : TCompositeSubTask; // Current transaction

  protected
    function InternalCreateSubtitle : TSubtitleItem;
    procedure InternalDeleteSubtitle(Index : Integer; FreeMemory : Boolean = True);
    procedure InternalInsertSubtitle(SubtitleItem : TSubtitleItem);
    procedure InternalSetStart(Sub : TSubtitleItem; Value : Integer);
    procedure InternalSetStop(Sub : TSubtitleItem; Value : Integer);
    procedure InternalSetText(Sub : TSubtitleItem; Value : WideString);
    procedure InternalReindex(FromIndex : Integer = 0);

  public
    constructor Create;
    destructor Destroy; override;
    
    function CreateSubtitle : TSubtitleItem;
    procedure DeleteSubtitle(Sub : TSubtitleItem); overload;
    procedure DeleteSubtitle(Idx : Integer); overload;
    
    procedure BeginTransaction;
    function EndTransaction : TCompositeSubTask;
    
    procedure SetSubtitleStart(Sub : TSubtitleItem; Value : Integer);
    procedure SetSubtitleStop(Sub : TSubtitleItem; Value : Integer);
    procedure SetSubtitleText(Sub : TSubtitleItem; Value : WideString);

    function GetCount : Integer;
    function GetAt(Idx : Integer) : TSubtitleItem;

    function GetFirst : TSubtitleItem;
    function GetNext(Sub : TSubtitleItem) : TSubtitleItem;
    function GetPrevious(Sub : TSubtitleItem) : TSubtitleItem;
  end;

  function SubtitleTimeComparator(const Item1, Item2 : TSubtitleItem) : Integer;

implementation

uses SysUtils;

var SubIdCounter : Integer = 10;

// -----------------------------------------------------------------------------

function SubtitleTimeDComparator(ptr : Pointer; const obj1, obj2 : DObject) : Integer;
begin
  Result := SubtitleTimeComparator(TSubtitleItem(asObject(obj1)), TSubtitleItem(asObject(obj2))); 
end;

function SubtitleTimeComparator(const Item1, Item2 : TSubtitleItem) : Integer;
begin
  if (Item1.Start < Item2.Start ) then
    Result := -1
  else if (Item1.Start > Item2.Start) then
    Result := 1
  else
  begin
    if (Item1.Stop < Item2.Stop) then
      Result := -1
    else if (Item1.Stop > Item2.Stop) then
      Result := 1
    else
      Result := 0
  end
end;

// -----------------------------------------------------------------------------

constructor TSubtitleItem.Create;
begin
  Start := 0;
  Stop := 0;
end;

function TSubtitleItem.GetIndex : Integer;
begin
  Result := Index;
end;

function TSubtitleItem.GetStart : Integer;
begin
  Result := Start;
end;

function TSubtitleItem.GetStop : Integer;
begin
  Result := Stop;
end;

function TSubtitleItem.GetText : WideString;
begin
  Result := Text;
end;

// -----------------------------------------------------------------------------

constructor TUndoableSubTask.Create(SubtitleModel : TSubtitleModel);
begin
  FSubtitleModel := SubtitleModel;
end;

// ------

procedure TCreateSubTask.DoTask;
var Sub : TSubtitleItem;
begin
  Sub := FSubtitleModel.InternalCreateSubtitle;
  FSubtitleIndex := Sub.GetIndex;
end;

procedure TCreateSubTask.UndoTask;
begin
  FSubtitleModel.InternalDeleteSubtitle(FSubtitleIndex);
end;

// ------

procedure TSetStartTask.DoTask;
var Sub : TSubtitleItem;
begin
  Sub := FSubtitleModel.GetAt(FSubtitleIndex);
  FSubtitleModel.InternalSetStart(Sub, FNewStart);
end;

procedure TSetStartTask.UndoTask;
var Sub : TSubtitleItem;
begin
  Sub := FSubtitleModel.GetAt(FSubtitleIndex);
  FSubtitleModel.InternalSetStart(Sub, FOldStart);
end;

// ------

procedure TSetStopTask.DoTask;
var Sub : TSubtitleItem;
begin
  Sub := FSubtitleModel.GetAt(FSubtitleIndex);
  FSubtitleModel.InternalSetStop(Sub, FNewStop);
end;

procedure TSetStopTask.UndoTask;
var Sub : TSubtitleItem;
begin
  Sub := FSubtitleModel.GetAt(FSubtitleIndex);
  FSubtitleModel.InternalSetStop(Sub, FOldStop);
end;

// ------

procedure TSetTextTask.DoTask;
var Sub : TSubtitleItem;
begin
  Sub := FSubtitleModel.GetAt(FSubtitleIndex);
  FSubtitleModel.InternalSetText(Sub, FNewText);
end;

procedure TSetTextTask.UndoTask;
var Sub : TSubtitleItem;
begin
  Sub := FSubtitleModel.GetAt(FSubtitleIndex);
  FSubtitleModel.InternalSetText(Sub, FOldText);
end;

// ------

procedure TDeleteSubTask.DoTask;
begin
  FOldSubtitle := FSubtitleModel.GetAt(FSubtitleIndex);
  FSubtitleModel.InternalDeleteSubtitle(FSubtitleIndex, False);
end;

procedure TDeleteSubTask.UndoTask;
begin
  FSubtitleModel.InternalInsertSubtitle(FOldSubtitle);
  FOldSubtitle := nil;
end;

destructor TDeleteSubTask.Destroy;
begin
  inherited;
  if Assigned(FOldSubtitle) then
  begin
    FreeAndNil(FOldSubtitle);
  end;
end;

// -----------------------------------------------------------------------------

constructor TCompositeSubTask.Create(SubtitleModel : TSubtitleModel);
begin
  inherited Create(SubtitleModel);
  FTasks := TObjectList.Create(True);
end;

destructor TCompositeSubTask.Destroy;
begin
  FreeAndNil(FTasks);
end;

procedure TCompositeSubTask.Add(Task : TUndoableSubTask);
begin
  FTasks.Add(Task);
end;

procedure TCompositeSubTask.DoTask;
var I : Integer;
begin
  for I := 0 to FTasks.Count-1 do
  begin
    TUndoableSubTask(FTasks[i]).DoTask;
  end;
end;

procedure TCompositeSubTask.UndoTask;
var I : Integer;
begin
  for I := FTasks.Count-1 downto 0 do
  begin
    TUndoableSubTask(FTasks[i]).UndoTask;
  end;
end;

// -----------------------------------------------------------------------------

constructor TSubtitleModel.Create;
var SubComparator : DComparator; 
begin
  SubComparator := MakeComparator(SubtitleTimeDComparator);
  FSortedSubs := DArray.CreateWith(SubComparator);
  FTransaction := nil;
end;

destructor TSubtitleModel.Destroy;
begin
  FTransaction.Free;
  if (FTransaction <> nil) then
    FreeAndNil(FTransaction);
  inherited;
end;

function TSubtitleModel.InternalCreateSubtitle : TSubtitleItem;
begin
  Result := TSubtitleItem.Create;
  InternalInsertSubtitle(Result);
end;

procedure TSubtitleModel.InternalInsertSubtitle(SubtitleItem : TSubtitleItem);
begin
  FSortedSubs.add([SubtitleItem]);
  InternalReindex;
end;

procedure TSubtitleModel.InternalDeleteSubtitle(Index : Integer; FreeMemory : Boolean = True);
var Sub : TSubtitleItem;
begin
  Sub := GetAt(Index);
  if Assigned(Sub) then
  begin
    FSortedSubs.removeAt(Index);
    InternalReindex(Sub.Index);
    if (FreeMemory) then
    begin
      FreeAndNil(Sub);
    end;
  end;
end;

procedure TSubtitleModel.InternalSetStart(Sub : TSubtitleItem; Value : Integer);
begin
  Sub.Start := Value;
  // TODO optimize sort only the modified subtitle ?
  sort(FSortedSubs);
  InternalReindex;  
end;

procedure TSubtitleModel.InternalSetStop(Sub : TSubtitleItem; Value : Integer);
begin
  Sub.Stop := Value;
  // TODO optimize sort only the modified subtitle ?
  sort(FSortedSubs);
  InternalReindex;   
end;

procedure TSubtitleModel.InternalSetText(Sub : TSubtitleItem; Value : WideString);
begin
  Sub.Text := Value;
end;

procedure TSubtitleModel.InternalReindex(FromIndex : Integer = 0);
var I : Integer;
    Sub : TSubtitleItem;
begin
  for I := FromIndex to FSortedSubs.size-1 do
  begin
    Sub := TSubtitleItem(FSortedSubs.atAsObject(I));
    Sub.Index := I;
  end;
end;

function TSubtitleModel.CreateSubtitle : TSubtitleItem;
var Task : TCreateSubTask;
begin
  if (FTransaction = nil) then
    raise Exception.Create('Missing transaction');
  Task := TCreateSubTask.Create(Self);
  Task.DoTask;
  FTransaction.Add(Task);
  Result := GetAt(Task.FSubtitleIndex);
end;

procedure TSubtitleModel.DeleteSubtitle(Sub : TSubtitleItem);
var Task : TDeleteSubTask;
begin
  if (FTransaction = nil) then
    raise Exception.Create('Missing transaction');
  Task := TDeleteSubTask.Create(Self);
  Task.FSubtitleIndex := Sub.Index;
  Task.DoTask;
  FTransaction.Add(Task);
end;

procedure TSubtitleModel.DeleteSubtitle(Idx : Integer);
var Sub : TSubtitleItem;
begin
  Sub := GetAt(Idx);
  DeleteSubtitle(Sub);
end;

procedure TSubtitleModel.BeginTransaction;
begin
  if Assigned(FTransaction) then
    raise Exception.Create('Nested transaction is not suported.');
  FTransaction := TCompositeSubTask.Create(Self);
end;

function TSubtitleModel.EndTransaction : TCompositeSubTask;
begin
  if (FTransaction = nil) then
    raise Exception.Create('Calling EndTransaction without BeginTransaction.');
  Result := FTransaction;
  FTransaction := nil;
end;

procedure TSubtitleModel.SetSubtitleStart(Sub : TSubtitleItem; Value : Integer);
var Task : TSetStartTask;
begin
  if (FTransaction = nil) then
    raise Exception.Create('Missing transaction');
  Task := TSetStartTask.Create(Self);
  Task.FSubtitleIndex := Sub.Index;
  Task.FNewStart := Value;
  Task.FOldStart := Sub.Start;
  Task.FSubtitleModel := Self;
  Task.DoTask;
  FTransaction.Add(Task);
end;

procedure TSubtitleModel.SetSubtitleStop(Sub : TSubtitleItem; Value : Integer);
var Task : TSetStopTask;
begin
  if (FTransaction = nil) then
    raise Exception.Create('Missing transaction');
  Task := TSetStopTask.Create(Self);
  Task.FSubtitleIndex := Sub.Index;
  Task.FNewStop := Value;
  Task.FOldStop := Sub.Stop;
  Task.FSubtitleModel := Self;
  Task.DoTask;
  FTransaction.Add(Task);
end;

procedure TSubtitleModel.SetSubtitleText(Sub : TSubtitleItem; Value : WideString);
var Task : TSetTextTask;
begin
  if (FTransaction = nil) then
    raise Exception.Create('Missing transaction');
  Task := TSetTextTask.Create(Self);
  Task.FSubtitleIndex := Sub.Index;
  Task.FNewText := Value;
  Task.FOldText := Sub.Text;
  Task.FSubtitleModel := Self;
  Task.DoTask;
  FTransaction.Add(Task);
end;

function TSubtitleModel.GetCount : Integer;
begin
  Result := FSortedSubs.size;
end;

function TSubtitleModel.GetAt(Idx : Integer) : TSubtitleItem;
begin
  if FSortedSubs.legalIndex(Idx) then
    Result := TSubtitleItem(FSortedSubs.atAsObject(Idx))
  else
    Result := nil
end;

function TSubtitleModel.GetFirst : TSubtitleItem;
begin
  Result := GetAt(0);
end;

function TSubtitleModel.GetNext(Sub : TSubtitleItem) : TSubtitleItem;
var NextIndex : Integer;
begin
  if (Sub <> nil) then
  begin
    NextIndex := Sub.Index + 1;
    Result := GetAt(NextIndex);
  end
  else
    Result := nil
end;

function TSubtitleModel.GetPrevious(Sub : TSubtitleItem) : TSubtitleItem;
var PreviousIndex : Integer;
begin
  if (Sub <> nil) then
  begin
    PreviousIndex := Sub.Index - 1;
    Result := GetAt(PreviousIndex);
  end
  else
    Result := nil
end;


end.
