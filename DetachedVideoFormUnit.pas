unit DetachedVideoFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TDetachedVideoForm = class(TForm)
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure CreateParams(var Params: TCreateParams); override;
  end;

var
  DetachedVideoForm: TDetachedVideoForm;

implementation

{$R *.dfm}

procedure TDetachedVideoForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := (Params.Style or WS_THICKFRAME);
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

end.
