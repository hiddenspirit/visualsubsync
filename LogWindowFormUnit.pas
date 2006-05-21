unit LogWindowFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, TntStdCtrls;

type
  TLogForm = class(TForm)
    MainMenu1: TMainMenu;
    Window1: TMenuItem;
    miStayHidden: TMenuItem;
    miClose: TMenuItem;
    miClear: TMenuItem;
    MemoLog: TTntMemo;
    procedure miCloseClick(Sender: TObject);
    procedure miStayHiddenClick(Sender: TObject);
    procedure miClearClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure LogMsg(const Msg : WideString);
    procedure SilentLogMsg(const Msg : WideString);
    procedure Clear;
  end;

var
  LogForm: TLogForm;

implementation

{$R *.dfm}

procedure TLogForm.miCloseClick(Sender: TObject);
begin
  Visible := False;
end;

procedure TLogForm.LogMsg(const Msg : WideString);
begin
  MemoLog.Lines.Add(Msg);
  if (not miStayHidden.Checked) and (Visible = False) then
    Visible := True;
end;

procedure TLogForm.SilentLogMsg(const Msg : WideString);
begin
  MemoLog.Lines.Add(Msg);
end;

procedure TLogForm.miStayHiddenClick(Sender: TObject);
begin
  Visible := False;
end;

procedure TLogForm.miClearClick(Sender: TObject);
begin
  Clear;
end;

procedure TLogForm.Clear;
begin
  MemoLog.Clear;
end;

end.
