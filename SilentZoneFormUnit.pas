unit SilentZoneFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ExtCtrls, TntExtCtrls, Contnrs, WAVDisplayerUnit;

type
  TSilentZoneForm = class(TForm)
    lbSilentZones: TTntListBox;
    TntGroupBox1: TTntGroupBox;
    edThreshold: TTntEdit;
    TntLabel1: TTntLabel;
    TntBevel1: TTntBevel;
    bttUpdate: TTntButton;
    TntLabel2: TTntLabel;
    edDuration: TTntEdit;
    procedure lbSilentZonesDblClick(Sender: TObject);
    procedure bttUpdateClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FWAVDisplayer : TWAVDisplayer;
    FZoneList : TObjectList;

    procedure FillZone(ZoneList : TList);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; WAVDisplayer : TWAVDisplayer); reintroduce;

    procedure Clear;
    function GetThreshold : Integer;
    function GetDuration : Integer;
    procedure UpdateData;

  end;

var
  SilentZoneForm: TSilentZoneForm;

implementation

uses MiscToolsUnit, main;

{$R *.dfm}

constructor TSilentZoneForm.Create(AOwner: TComponent;
  WAVDisplayer : TWAVDisplayer);
begin
  inherited Create(AOwner);
  FWAVDisplayer := WAVDisplayer;
  FZoneList := TObjectList.Create(True);
end;

procedure TSilentZoneForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FZoneList);
end;

procedure TSilentZoneForm.Clear;
begin
  lbSilentZones.Clear;
  FZoneList.Clear;
end;

procedure TSilentZoneForm.FillZone(ZoneList : TList);
var i : Integer;
    SilentRange : TSilentRangeInfo;
    Str : WideString;
    SilentDuration, MinDuration : Integer;
begin
  MinDuration := GetDuration;
  lbSilentZones.Items.BeginUpdate;
  for i:=0 to ZoneList.Count-1 do
  begin
    SilentRange := TSilentRangeInfo(ZoneList[i]);
    SilentDuration := SilentRange.Stop - SilentRange.Start;
    if (SilentDuration >= MinDuration) then
    begin
      Str := Format('%s -> %s (%.1f)', [TimeMsToString(SilentRange.Start),
        TimeMsToString(SilentRange.Stop),
        (SilentRange.RmsSum / SilentRange.RmsCount)]);
      lbSilentZones.Items.AddObject(Str, SilentRange);
    end;
  end;
  lbSilentZones.Items.EndUpdate;
end;

procedure TSilentZoneForm.lbSilentZonesDblClick(Sender: TObject);
var SilentRange : TSilentRangeInfo;
begin
  if (lbSilentZones.ItemIndex <> -1) then
  begin
    SilentRange := TSilentRangeInfo(lbSilentZones.Items.Objects[lbSilentZones.ItemIndex]);
    MainForm.SetSelection(SilentRange.Start, SilentRange.Stop);
  end;
end;

function TSilentZoneForm.GetThreshold : Integer;
begin
  Result := StrToIntDef(edThreshold.Text, 300);
end;

function TSilentZoneForm.GetDuration : Integer;
begin
  Result := StrToIntDef(edDuration.Text, 500);
end;

procedure TSilentZoneForm.bttUpdateClick(Sender: TObject);
begin
  UpdateData;
end;

procedure TSilentZoneForm.UpdateData;
begin
  Clear;
  FWAVDisplayer.DetectSilentZone(FZoneList, GetThreshold, 200);
  SilentZoneForm.FillZone(FZoneList);
end;

end.
