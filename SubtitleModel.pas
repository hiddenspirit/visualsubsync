unit SubtitleModel;

interface

uses Classes, Contnrs, DeCAL;

const
  INVALID_SUB_ID : Integer = -1;
  
type
  // Forward Declarations
  TSubtitleModel = class;

  TSubtitleItem = class(TObject)
  private
    Id : Integer;
    Start : Integer;
    Stop : Integer;
    Text : WideString;
  public
    function GetId : Integer;
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
    FSubtitleId : Integer;
  public
    procedure DoTask; override;
    procedure UndoTask; override;
  end;

  TSetStartTask = class(TUndoableSubTask)
  private
    FSubtitleId : Integer;
    FOldStart, FNewStart : Integer;
  public
    procedure DoTask; override;
    procedure UndoTask; override;
  end;

  TSetStopTask = class(TUndoableSubTask)
  private
    FSubtitleId : Integer;
    FOldStop, FNewStop : Integer;
  public
    procedure DoTask; override;
    procedure UndoTask; override;
  end;

  TSetTextTask = class(TUndoableSubTask)
  private
    FSubtitleId : Integer;
    FOldText, FNewText : WideString;
  public
    procedure DoTask; override;
    procedure UndoTask; override;
  end;

  TDeleteSubTask = class(TUndoableSubTask)
  private
    FSubtitleId : Integer;
    FOldSubtitle : TSubtitleItem;
  public
    destructor Destroy; override;
    
    procedure DoTask; override;
    procedure UndoTask; override;
  end;

  TCompositeSubTask = class(TUndoableSubTask)
  private
    FTasks : TObjectList;
    FTasks2 : DHashMap;
  public
    constructor Create(SubtitleModel : TSubtitleModel); override;
    destructor Destroy; override;

    procedure Add(Task : TUndoableSubTask);
    
    procedure DoTask; override;
    procedure UndoTask; override;    
  end;



  TSubtitleModel = class(TObject)
  private
    FSubs : DHashMap; // Subtitle map (Map<Sub.Id, Sub>)
    FTransaction : TCompositeSubTask; // Current transaction

    // TODO
    // Sorted by time container

  protected
    function InternalCreateSubtitle(Id : Integer = -1) : TSubtitleItem;
    procedure InternalDeleteSubtitle(Id : Integer; FreeMemory : Boolean = True);
    procedure InternalInsertSubtitle(SubtitleItem : TSubtitleItem);
    
  public
    constructor Create;
    destructor Destroy; override;
    
    function CreateSubtitle : TSubtitleItem;
    procedure DeleteSubtitle(Sub : TSubtitleItem);
    
    procedure BeginTransaction;
    function EndTransaction : TCompositeSubTask;
    
    procedure SetSubtitleStart(Sub : TSubtitleItem; Value : Integer);
    procedure SetSubtitleStop(Sub : TSubtitleItem; Value : Integer);
    procedure SetSubtitleText(Sub : TSubtitleItem; Value : WideString);

    function Get(Id : Integer) : TSubtitleItem;
    function GetCount : Integer;
  end;

  function SubtitleTimeComparator(const Item1, Item2 : TSubtitleItem) : Integer;  

implementation

uses SysUtils;

var SubIdCounter : Integer = 10;

// -----------------------------------------------------------------------------

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

function TSubtitleItem.GetId : Integer;
begin
  Result := Id;
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
  Sub := FSubtitleModel.InternalCreateSubtitle(FSubtitleId);
  FSubtitleId := Sub.GetId;
end;

procedure TCreateSubTask.UndoTask;
begin
  FSubtitleModel.InternalDeleteSubtitle(FSubtitleId);
end;

// ------

procedure TSetStartTask.DoTask;
var Sub : TSubtitleItem;
begin
  Sub := FSubtitleModel.Get(FSubtitleId);
  Sub.Start := FNewStart;
end;

procedure TSetStartTask.UndoTask;
var Sub : TSubtitleItem;
begin
  Sub := FSubtitleModel.Get(FSubtitleId);
  Sub.Start := FOldStart;
end;

// ------

procedure TSetStopTask.DoTask;
var Sub : TSubtitleItem;
begin
  Sub := FSubtitleModel.Get(FSubtitleId);
  Sub.Stop := FNewStop;
end;

procedure TSetStopTask.UndoTask;
var Sub : TSubtitleItem;
begin
  Sub := FSubtitleModel.Get(FSubtitleId);
  Sub.Stop := FOldStop;
end;

// ------

procedure TSetTextTask.DoTask;
var Sub : TSubtitleItem;
begin
  Sub := FSubtitleModel.Get(FSubtitleId);
  Sub.Text := FNewText;
end;

procedure TSetTextTask.UndoTask;
var Sub : TSubtitleItem;
begin
  Sub := FSubtitleModel.Get(FSubtitleId);
  Sub.Text := FOldText;
end;

// ------

procedure TDeleteSubTask.DoTask;
begin
  FOldSubtitle := FSubtitleModel.Get(FSubtitleId);
  FSubtitleModel.InternalDeleteSubtitle(FSubtitleId, False);
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
begin
  FSubs := DHashMap.Create;
  FTransaction := nil;
end;

destructor TSubtitleModel.Destroy;
begin
  FSubs.Free;
  if (FTransaction <> nil) then
    FreeAndNil(FTransaction);
  inherited;
end;

function TSubtitleModel.InternalCreateSubtitle(Id : Integer) : TSubtitleItem;
begin
  Result := TSubtitleItem.Create;
  if (Id = INVALID_SUB_ID) then
  begin
    Result.Id := SubIdCounter;
    Inc(SubIdCounter);
  end
  else
  begin
    Result.Id := Id;
  end;
  FSubs.putPair([Result.Id, Result]);
end;

procedure TSubtitleModel.InternalInsertSubtitle(SubtitleItem : TSubtitleItem);
begin
  FSubs.putPair([SubtitleItem.Id, SubtitleItem]);
end;

procedure TSubtitleModel.InternalDeleteSubtitle(Id : Integer; FreeMemory : Boolean);
var Sub : TSubtitleItem;
    Iter : DIterator;
begin
  Iter := FSubs.locate([Id]);
  if (not atEnd(Iter)) then
  begin
    Sub := TSubtitleItem(getObject(Iter));
    if (FreeMemory) then
    begin
      FreeAndNil(Sub);
    end;
    FSubs.removeAt(Iter);
  end;
end;

function TSubtitleModel.CreateSubtitle : TSubtitleItem;
var Task : TCreateSubTask;
begin
  if (FTransaction = nil) then
    raise Exception.Create('Missing transaction');
  Task := TCreateSubTask.Create(Self);
  Task.FSubtitleId := INVALID_SUB_ID;
  Task.DoTask;
  FTransaction.Add(Task);
  Result := Get(Task.FSubtitleId);
end;

procedure TSubtitleModel.DeleteSubtitle(Sub : TSubtitleItem);
var Task : TDeleteSubTask;
begin
  if (FTransaction = nil) then
    raise Exception.Create('Missing transaction');
  Task := TDeleteSubTask.Create(Self);
  Task.FSubtitleId := Sub.Id;
  Task.DoTask;
  FTransaction.Add(Task);
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
  Task.FSubtitleId := Sub.Id;
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
  Task.FSubtitleId := Sub.Id;
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
  Task.FSubtitleId := Sub.Id;
  Task.FNewText := Value;
  Task.FOldText := Sub.Text;
  Task.FSubtitleModel := Self;
  Task.DoTask;
  FTransaction.Add(Task);
end;

function TSubtitleModel.Get(Id : Integer) : TSubtitleItem;
var Iter : DIterator;
begin
  Iter := FSubs.locate([Id]);
  if (atEnd(Iter)) then
    Result := nil
  else
    Result := TSubtitleItem(getObject(Iter));
end;

function TSubtitleModel.GetCount : Integer;
begin
  Result := FSubs.size;
end;

end.
