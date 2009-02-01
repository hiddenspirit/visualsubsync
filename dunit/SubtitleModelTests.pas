unit SubtitleModelTests;

interface

uses
  SubtitleModel,
  TestFrameWork;

type
  TSubtitleModelTests = class(TTestCase)
  private
    FSubtitleModel : TSubtitleModel;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published

    // Test methods
    procedure TestCreateSubtitle;
    procedure TestGet;
    procedure TestSetStart;
    procedure TestSetStop;
    procedure TestSetText;
    procedure TestDeleteSubtitle;
    procedure TestSubtitleTimeComparator;

  end;

  TSubtitleItemHack = class(TObject)
  private
    Id : Integer;
    Start : Integer;
    Stop : Integer;
    Text : WideString;
  end;

implementation

uses Classes, SysUtils;

procedure TSubtitleModelTests.SetUp;
begin
  FSubtitleModel := TSubtitleModel.Create;
end;

procedure TSubtitleModelTests.TearDown;
begin
  FreeAndNil(FSubtitleModel);
end;

procedure TSubtitleModelTests.TestCreateSubtitle;
var Sub : TSubtitleItem;
    Transaction : TCompositeSubTask;
    SubId : Integer;
begin
  CheckEquals(0, FSubtitleModel.GetCount, 'No subtitle yet.');
  FSubtitleModel.BeginTransaction;
  try
    Sub := FSubtitleModel.CreateSubtitle;
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;
  CheckNotNull(Sub, 'Subtitle exists.');
  CheckEquals(1, FSubtitleModel.GetCount, '1 subtitle.');
  
  SubId := Sub.GetId;
  Transaction.UndoTask;
  CheckEquals(0, FSubtitleModel.GetCount, '0 subtitle.');
  Sub := FSubtitleModel.Get(SubId);
  CheckNull(Sub, 'No subtitle anymore.');

  Transaction.DoTask;
  CheckEquals(1, FSubtitleModel.GetCount, '1 subtitle again.');
  Sub := FSubtitleModel.Get(SubId);
  CheckNotNull(Sub, 'Subtitle exists again.');
  CheckEquals(SubId, Sub.GetId, 'Same id.');
    
  FreeAndNil(Transaction);
end;

procedure TSubtitleModelTests.TestGet;
var Sub, Sub2 : TSubtitleItem;
    Transaction : TCompositeSubTask;
begin
  Sub := FSubtitleModel.Get(100);
  CheckNull(Sub, 'Subtitle doesnt exists yet.');
  FSubtitleModel.BeginTransaction;
  try
    Sub := FSubtitleModel.CreateSubtitle;
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;
  FreeAndNil(Transaction);  
  CheckNotNull(Sub, 'Subtitle created.');
  Sub2 := FSubtitleModel.Get(Sub.GetId);
  CheckNotNull(Sub2, 'Subtitle not null.');
  CheckEquals(Sub.GetId, Sub2.GetId, 'Subtitle is same.');
end;

procedure TSubtitleModelTests.TestSetStart;
var Sub : TSubtitleItem;
    Transaction : TCompositeSubTask;
begin
  FSubtitleModel.BeginTransaction;
  try
    Sub := FSubtitleModel.CreateSubtitle;
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;
  FreeAndNil(Transaction);
  
  FSubtitleModel.BeginTransaction;
  try
    FSubtitleModel.SetSubtitleStart(Sub, 10);
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;
  
  CheckEquals(10, Sub.GetStart, 'Start = 10.');
  Transaction.UndoTask;
  CheckEquals(0, Sub.GetStart, 'Start = 0.');

  FreeAndNil(Transaction);
end;

procedure TSubtitleModelTests.TestSetStop;
var Sub : TSubtitleItem;
    Transaction : TCompositeSubTask;
begin
  FSubtitleModel.BeginTransaction;
  try
    Sub := FSubtitleModel.CreateSubtitle;
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;
  FreeAndNil(Transaction);
  
  FSubtitleModel.BeginTransaction;
  try
    FSubtitleModel.SetSubtitleStop(Sub, 10);
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;
  
  CheckEquals(10, Sub.GetStop, 'Stop = 10.');
  Transaction.UndoTask;
  CheckEquals(0, Sub.GetStop, 'Stop = 0.');

  FreeAndNil(Transaction);
end;

procedure TSubtitleModelTests.TestSetText;
var Sub : TSubtitleItem;
    Transaction : TCompositeSubTask;
begin
  FSubtitleModel.BeginTransaction;
  try
    Sub := FSubtitleModel.CreateSubtitle;
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;
  FreeAndNil(Transaction);
  
  FSubtitleModel.BeginTransaction;
  try
    FSubtitleModel.SetSubtitleText(Sub, '1234 text');
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;
  
  CheckEquals('1234 text', Sub.GetText, 'Stop = 1234 text.');
  Transaction.UndoTask;
  CheckEquals('', Sub.GetText, 'Stop = .');

  FreeAndNil(Transaction);
end;

procedure TSubtitleModelTests.TestDeleteSubtitle;
var Sub : TSubtitleItem;
    Transaction : TCompositeSubTask;
    SubId : Integer;
begin
  CheckEquals(0, FSubtitleModel.GetCount, 'No subtitle yet.');
  FSubtitleModel.BeginTransaction;
  try
    Sub := FSubtitleModel.CreateSubtitle;
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;
  FreeAndNil(Transaction);
  CheckNotNull(Sub, 'Subtitle exists.');
  CheckEquals(1, FSubtitleModel.GetCount, 'One subtitle.');

  SubId := Sub.GetId;
  FSubtitleModel.BeginTransaction;
  try
    FSubtitleModel.DeleteSubtitle(Sub);
  finally
    Transaction := FSubtitleModel.EndTransaction;
  end;
  CheckEquals(0, FSubtitleModel.GetCount, 'No subtitle.');

  Transaction.UndoTask;

  CheckEquals(1, FSubtitleModel.GetCount, 'One subtitle again.');
  Sub := FSubtitleModel.Get(SubId);
  CheckNotNull(Sub, 'Subtitle exists again.');
  CheckEquals(SubId, Sub.GetId, 'Same id.');
end;

procedure TSubtitleModelTests.TestSubtitleTimeComparator;
var SubItem1, SubItem2 : TSubtitleItem;
begin
  SubItem1 := TSubtitleItem.Create;
  SubItem2 := TSubtitleItem.Create;

  TSubtitleItemHack(SubItem1).Start := 0;
  TSubtitleItemHack(SubItem1).Stop := 0;
  TSubtitleItemHack(SubItem2).Start := 0;
  TSubtitleItemHack(SubItem2).Stop := 0;
  CheckEquals(0, SubtitleTimeComparator(SubItem1, SubItem2), 'strictly =');

  TSubtitleItemHack(SubItem1).Start := 10;
  TSubtitleItemHack(SubItem1).Stop := 15;
  CheckEquals(1, SubtitleTimeComparator(SubItem1, SubItem2), 'strictly >');

  TSubtitleItemHack(SubItem2).Start := 20;
  TSubtitleItemHack(SubItem2).Stop := 25;
  CheckEquals(-1, SubtitleTimeComparator(SubItem1, SubItem2), 'strictly <');

  TSubtitleItemHack(SubItem1).Start := 20;
  TSubtitleItemHack(SubItem1).Stop := 22;
  CheckEquals(-1, SubtitleTimeComparator(SubItem1, SubItem2), 'stop <');

  TSubtitleItemHack(SubItem1).Start := 20;
  TSubtitleItemHack(SubItem1).Stop := 28;
  CheckEquals(1, SubtitleTimeComparator(SubItem1, SubItem2), 'stop >');
end;

initialization
  TestFramework.RegisterTest('SubtitleModelTests Suite',
    TSubtitleModelTests.Suite);

end.
