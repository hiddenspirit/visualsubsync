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

unit FindFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls;

type
  TFindForm = class(TForm)
    TntLabel1: TTntLabel;
    bttClose: TTntButton;
    chkMatchCase: TTntCheckBox;
    ComboTextToFind: TTntComboBox;
    bttOk: TTntButton;
    chkFromCursor: TCheckBox;
    procedure bttCloseClick(Sender: TObject);
    procedure TntButton1Click(Sender: TObject);
    procedure ComboTextToFindKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function GetFindWord : WideString;
    function MatchCase : Boolean;
    function FromCursor : Boolean;
  end;

var
  FindForm: TFindForm;

implementation

uses TntClasses;

{$R *.dfm}

//------------------------------------------------------------------------------

procedure TFindForm.bttCloseClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

//------------------------------------------------------------------------------

procedure TFindForm.TntButton1Click(Sender: TObject);
var i, Idx : integer;
begin
  // Search in historic
  Idx := -1;
  for i:=0 to ComboTextToFind.Items.Count-1 do
  begin
    if (ComboTextToFind.Text = ComboTextToFind.Items[i]) then
    begin
      Idx := i;
      Break;
    end;
  end;
  if (Idx = -1) then
  begin
    // Add it to the historic
    ComboTextToFind.Items.Insert(0,ComboTextToFind.Text);
  end
  else
  begin
    // Move item to top og historic
    ComboTextToFind.Items.Move(Idx,0);
    ComboTextToFind.Text := ComboTextToFind.Items[0];
  end;
  ModalResult := mrOk;  
end;

//------------------------------------------------------------------------------

function TFindForm.GetFindWord : WideString;
begin
  Result := ComboTextToFind.Text;
end;

//------------------------------------------------------------------------------

function TFindForm.MatchCase : Boolean;
begin
  Result := chkMatchCase.Checked;
end;

//------------------------------------------------------------------------------

function TFindForm.FromCursor : Boolean;
begin
  Result := chkFromCursor.Checked;
end;

//------------------------------------------------------------------------------

procedure TFindForm.ComboTextToFindKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    bttOk.Click
  else if Key = VK_ESCAPE then
    ModalResult := mrCancel
end;

//------------------------------------------------------------------------------

procedure TFindForm.FormActivate(Sender: TObject);
begin
  ComboTextToFind.SetFocus;
end;

//------------------------------------------------------------------------------
end.
//------------------------------------------------------------------------------
