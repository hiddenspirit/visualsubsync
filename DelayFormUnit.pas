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

unit DelayFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, TntExtCtrls, Mask;

type
  TDelayForm = class(TForm)
    meDelay: TMaskEdit;
    rgApplyTo: TTntRadioGroup;
    rgType: TTntRadioGroup;
    bttApply: TTntButton;
    bttCancel: TTntButton;
    procedure bttApplyClick(Sender: TObject);
    procedure bttCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DelayForm: TDelayForm;

implementation

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TDelayForm.bttApplyClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

// -----------------------------------------------------------------------------

procedure TDelayForm.bttCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

// -----------------------------------------------------------------------------
end.
// -----------------------------------------------------------------------------
