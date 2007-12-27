unit DetachedVideoFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, TntMenus;

type
  TDetachedVideoForm = class(TForm)
    VideoPopupMenu: TTntPopupMenu;
    pmiVideoFullscreen: TTntMenuItem;
    pmiVideoNormalSize: TTntMenuItem;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDblClick(Sender: TObject);
    procedure pmiVideoFullscreenClick(Sender: TObject);
    procedure pmiVideoNormalSizeClick(Sender: TObject);
    procedure VideoPopupMenuPopup(Sender: TObject);
  private
    { Private declarations }
    FNormalLeft, FNormalTop, FNormalWidth, FNormalHeight : Integer;
    function IsFullscreen : Boolean;

  public
    { Public declarations }
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetNormalPos(NormalLeft, NormalTop, NormalWidth, NormalHeight: Integer);

    property NormalLeft : Integer read FNormalLeft;
    property NormalTop : Integer read FNormalTop;
    property NormalWidth : Integer read FNormalWidth;
    property NormalHeight : Integer read FNormalHeight;
  end;

var
  DetachedVideoForm: TDetachedVideoForm;

implementation

{$R *.dfm}

uses Main;

procedure TDetachedVideoForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := (Params.Style or WS_THICKFRAME);
  FNormalLeft := 0;
  FNormalTop := 0;
  FNormalWidth := 320;
  FNormalHeight := 240;
end;

procedure TDetachedVideoForm.FormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  SC_DRAGMOVE = $F012;
begin
  if (Button = mbLeft) then
  begin
    ReleaseCapture;
    (Self as TControl).Perform(WM_SYSCOMMAND, SC_DRAGMOVE, 0);
  end;
end;

procedure TDetachedVideoForm.FormDblClick(Sender: TObject);
begin
  MainForm.ActionDetachVideo.Execute;
end;

procedure TDetachedVideoForm.pmiVideoFullscreenClick(Sender: TObject);
var Monitor : TMonitor;
begin
  if (not IsFullscreen) then
  begin
    FNormalLeft := Self.Left;
    FNormalTop := Self.Top;
    FNormalWidth := Self.Width;
    FNormalHeight := Self.Height;
    Monitor := Screen.MonitorFromWindow(Self.Handle);
    Self.SetBounds(Monitor.Left, Monitor.Top, Monitor.Width, Monitor.Height);
  end;
end;

procedure TDetachedVideoForm.pmiVideoNormalSizeClick(Sender: TObject);
begin
  if IsFullscreen then
  begin
    if FNormalWidth = 0 then
      FNormalWidth := 320;
    if (FNormalHeight = 0) then
      FNormalHeight := 240;
    Self.SetBounds(FNormalLeft, FNormalTop, FNormalWidth, FNormalHeight);
  end;
end;

function TDetachedVideoForm.IsFullscreen : Boolean;
var Monitor : TMonitor;
begin
  Monitor := Screen.MonitorFromWindow(Self.Handle);
  Result := (Monitor.Left = Self.Left) and (Monitor.Top = Self.Top) and
    (Monitor.Width = Self.Width) and (Monitor.Height = Self.Height);
end;

procedure TDetachedVideoForm.VideoPopupMenuPopup(Sender: TObject);
begin
  pmiVideoNormalSize.Checked := not IsFullscreen;
  pmiVideoNormalSize.Enabled := IsFullscreen;
  pmiVideoFullscreen.Checked := IsFullscreen;
  pmiVideoFullscreen.Enabled := not IsFullscreen;
end;

procedure TDetachedVideoForm.SetNormalPos(NormalLeft, NormalTop, NormalWidth, NormalHeight : Integer);
begin
  FNormalLeft := NormalLeft;
  FNormalTop := NormalTop;
  FNormalWidth := NormalWidth;
  FNormalHeight := NormalHeight;
end;

end.
