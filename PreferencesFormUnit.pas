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

unit PreferencesFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ComCtrls, TntComCtrls, CheckLst,
  TntCheckLst, IniFiles, ExtCtrls, TntExtCtrls;

type
  TErrorListElemType = (eletNoParam, eletIntParam);
  TErrorListElem = record
    Name : string;
    ElemType : TErrorListElemType;
    Value : Integer;
    UnitName : string;
    Description : string;
    Msg: string;
    Color : TColor;
  end;
  PErrorListElem = ^TErrorListElem;

const
  ErrorListElemTab : array[0..3] of TErrorListElem = (
    (Name: 'Overlapping';
     ElemType: eletNoParam;
     Description: 'An error is detected when the subtitle overlap on next subtitle.';
     Msg: 'Subtitle overlap on next subtitle :';
     Color: $003737FF),
    (Name: 'Too short display time';
     ElemType: eletIntParam;
     UnitName: 'Char/s';
     Description: 'An error is detected when the number of Char/s is strictly superior to the specified value.';
     Msg: 'Subtitle display time is too short :';
     Color: $0037C4FF),
    (Name: 'Too long display time';
     ElemType: eletIntParam;
     UnitName: 'Char/s';
     Description: 'An error is detected when the number of Char/s is strictly inferior to the specified value.';
     Msg: 'Subtitle display time is too long :';
     Color: $00FF8737),
    (Name: 'Too long line';
     ElemType: eletIntParam;
     UnitName: 'Characters';
     Description: 'An error is detected when the line length in a subtitle is strictly superior to the specified value.';
     Msg: 'Subtitle has a too long line :';
     Color: $0037FFFF)
  );

type
  TConfigObject = class
    // Web server
    ServerPort : Integer;
    EnableCompression : Boolean;
    // Error
    ErrorOverlappingEnabled : Boolean;
    ErrorTooShortDisplayTimeEnabled : Boolean;
    ErrorTooShortDisplayTimeValue : Integer;
    ErrorTooLongDisplayTimeEnabled : Boolean;
    ErrorTooLongDisplayTimeValue : Integer;
    ErrorTooLongLineEnabled : Boolean;
    ErrorTooLongLineValue : Integer;
    // Misc
    SwapSubtitlesList : Boolean;

    constructor Create;
    procedure SetDefault;
    procedure SaveIni(IniFile : TIniFile);
    procedure LoadIni(IniFile : TIniFile);
  end;

  TPreferencesForm = class(TForm)
    TntPageControl1: TTntPageControl;
    tsGeneral: TTntTabSheet;
    bttOk: TTntButton;
    bttCancel: TTntButton;
    tsErrorChecking: TTntTabSheet;
    TntGroupBox1: TTntGroupBox;
    EditServerPort: TTntEdit;
    TntLabel1: TTntLabel;
    ListErrorChecking: TTntCheckListBox;
    EditErrorValue: TTntEdit;
    TntLabel2: TTntLabel;
    UpDownServerPort: TTntUpDown;
    lbErrorUnit: TTntLabel;
    lbErrorDescription: TTntLabel;
    TntLabel3: TTntLabel;
    TntBevel1: TTntBevel;
    GroupBox1: TGroupBox;
    chkAssociateExt: TCheckBox;
    ShapeErrorColor: TShape;
    TntLabel4: TTntLabel;
    chkEnableCompression: TCheckBox;
    chkSwapSubList: TCheckBox;
    tsHotKeys: TTntTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure bttOkClick(Sender: TObject);
    procedure bttCancelClick(Sender: TObject);
    procedure ListErrorCheckingClick(Sender: TObject);
    procedure EditErrorValueChange(Sender: TObject);
    procedure EditDigitOnlyKeyPress(Sender: TObject; var Key: Char);
    procedure chkAssociateExtClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure LoadConfig(Config : TConfigObject);
    procedure SaveConfig(Config : TConfigObject);
    function GetErrorListElem(Idx : Integer) : PErrorListElem;
  end;

var
  PreferencesForm: TPreferencesForm;

implementation

uses MiscToolsUnit, GlobalUnit;

{$R *.dfm}

// =============================================================================

constructor TConfigObject.Create;
begin
  SetDefault;
end;

//------------------------------------------------------------------------------

procedure TConfigObject.SetDefault;
begin
  // Misc
  SwapSubtitlesList := False;
  // Web server
  ServerPort := 80;
  EnableCompression := True;
  // Error
  ErrorOverlappingEnabled := True;
  ErrorTooShortDisplayTimeEnabled := True;
  ErrorTooShortDisplayTimeValue := 25;
  ErrorTooLongDisplayTimeEnabled := True;
  ErrorTooLongDisplayTimeValue := 5;
  ErrorTooLongLineEnabled := True;
  ErrorTooLongLineValue := 60;
end;

//------------------------------------------------------------------------------

procedure TConfigObject.SaveIni(IniFile : TIniFile);
begin
  // Misc
  IniFile.WriteBool('Misc','SwapSubtitlesList',SwapSubtitlesList);
  // Web server
  IniFile.WriteInteger('WebServer','Port',ServerPort);
  IniFile.WriteBool('WebServer','EnableCompression',EnableCompression);

  // Error
  IniFile.WriteBool('ErrorChecking','OverlappingEnabled',ErrorOverlappingEnabled);

  IniFile.WriteBool('ErrorChecking','TooShortDisplayTimeEnabled',ErrorTooShortDisplayTimeEnabled);
  IniFile.WriteInteger('ErrorChecking','TooShortDisplayTimeValue',ErrorTooShortDisplayTimeValue);

  IniFile.WriteBool('ErrorChecking','TooLongDisplayTimeEnabled',ErrorTooLongDisplayTimeEnabled);
  IniFile.WriteInteger('ErrorChecking','TooLongDisplayTimeValue',ErrorTooLongDisplayTimeValue);

  IniFile.WriteBool('ErrorChecking','TooLongLineEnabled',ErrorTooLongLineEnabled);
  IniFile.WriteInteger('ErrorChecking','TooLongLineValue',ErrorTooLongLineValue);
end;

//------------------------------------------------------------------------------

procedure TConfigObject.LoadIni(IniFile : TIniFile);
begin
  // Misc
  SwapSubtitlesList := IniFile.ReadBool('Misc','SwapSubtitlesList',SwapSubtitlesList);

  // Web server
  ServerPort := IniFile.ReadInteger('WebServer','Port',ServerPort);
  EnableCompression := IniFile.ReadBool('WebServer','EnableCompression',EnableCompression);

  // Error
  ErrorOverlappingEnabled := IniFile.ReadBool('ErrorChecking','OverlappingEnabled',ErrorOverlappingEnabled);

  ErrorTooShortDisplayTimeEnabled := IniFile.ReadBool('ErrorChecking','TooShortDisplayTimeEnabled',ErrorTooShortDisplayTimeEnabled);
  ErrorTooShortDisplayTimeValue := IniFile.ReadInteger('ErrorChecking','TooShortDisplayTimeValue',ErrorTooShortDisplayTimeValue);

  ErrorTooLongDisplayTimeEnabled := IniFile.ReadBool('ErrorChecking','TooLongDisplayTimeEnabled',ErrorTooLongDisplayTimeEnabled);
  ErrorTooLongDisplayTimeValue := IniFile.ReadInteger('ErrorChecking','TooLongDisplayTimeValue',ErrorTooLongDisplayTimeValue);

  ErrorTooLongLineEnabled := IniFile.ReadBool('ErrorChecking','TooLongLineEnabled',ErrorTooLongLineEnabled);
  ErrorTooLongLineValue := IniFile.ReadInteger('ErrorChecking','TooLongLineValue',ErrorTooLongLineValue);
end;

// =============================================================================

procedure TPreferencesForm.FormCreate(Sender: TObject);
var i : integer;
begin
  for i:=0 to Length(ErrorListElemTab)-1 do
  begin
    ListErrorChecking.AddItem(ErrorListElemTab[i].Name,@ErrorListElemTab[i]);
  end;
  ListErrorChecking.ItemIndex := 0;
  ListErrorCheckingClick(Self);
  TntPageControl1.ActivePage := tsGeneral;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.FormActivate(Sender: TObject);
begin
  chkAssociateExt.Checked := ShellIsExtensionRegistered('vssprj',ApplicationName,Application.ExeName);
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.LoadConfig(Config : TConfigObject);
begin
  // Misc
  chkSwapSubList.Checked := Config.SwapSubtitlesList;

  // Web server
  UpDownServerPort.Position := Config.ServerPort;
  chkEnableCompression.Checked := Config.EnableCompression;

  // Error
  ListErrorChecking.Checked[0] := Config.ErrorOverlappingEnabled;

  ListErrorChecking.Checked[1] := Config.ErrorTooShortDisplayTimeEnabled;
  PErrorListElem(ListErrorChecking.Items.Objects[1]).Value := Config.ErrorTooShortDisplayTimeValue;

  ListErrorChecking.Checked[2] := Config.ErrorTooLongDisplayTimeEnabled;
  PErrorListElem(ListErrorChecking.Items.Objects[2]).Value := Config.ErrorTooLongDisplayTimeValue;

  ListErrorChecking.Checked[3] := Config.ErrorTooLongLineEnabled;
  PErrorListElem(ListErrorChecking.Items.Objects[3]).Value := Config.ErrorTooLongLineValue;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.SaveConfig(Config : TConfigObject);
begin
  // Misc
  Config.SwapSubtitlesList := chkSwapSubList.Checked;
  
  // Web server
  Config.ServerPort := UpDownServerPort.Position;
  Config.EnableCompression := chkEnableCompression.Checked;

  // Error
  Config.ErrorOverlappingEnabled := ListErrorChecking.Checked[0];

  Config.ErrorTooShortDisplayTimeEnabled := ListErrorChecking.Checked[1];
  Config.ErrorTooShortDisplayTimeValue := PErrorListElem(ListErrorChecking.Items.Objects[1]).Value;

  Config.ErrorTooLongDisplayTimeEnabled := ListErrorChecking.Checked[2];
  Config.ErrorTooLongDisplayTimeValue := PErrorListElem(ListErrorChecking.Items.Objects[2]).Value;

  Config.ErrorTooLongLineEnabled := ListErrorChecking.Checked[3];
  Config.ErrorTooLongLineValue := PErrorListElem(ListErrorChecking.Items.Objects[3]).Value;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.bttOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.bttCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.ListErrorCheckingClick(Sender: TObject);
var ErrorListElem : PErrorListElem;
begin
  if ListErrorChecking.ItemIndex <> -1 then
  begin
    ErrorListElem := PErrorListElem(ListErrorChecking.Items.Objects[ListErrorChecking.ItemIndex]);
    EditErrorValue.Enabled := (ErrorListElem.ElemType <> eletNoParam);
    if (EditErrorValue.Enabled) then
    begin
      EditErrorValue.Text := IntToStr(ErrorListElem.Value);
      EditErrorValue.Color := clWindow;
      lbErrorUnit.Caption := ErrorListElem.UnitName;
    end
    else
    begin
      EditErrorValue.Text := '';
      EditErrorValue.Color := cl3DLight;
      lbErrorUnit.Caption := '';
    end;
    ShapeErrorColor.Brush.Color := ErrorListElem.Color;    
    lbErrorDescription.Caption := ErrorListElem.Description;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.EditErrorValueChange(Sender: TObject);
var ErrorListElem : PErrorListElem;
begin
  if ListErrorChecking.ItemIndex <> -1 then
  begin
    ErrorListElem := PErrorListElem(ListErrorChecking.Items.Objects[ListErrorChecking.ItemIndex]);
    ErrorListElem.Value := StrToIntDef(EditErrorValue.Text,ErrorListElem.Value);
  end;  
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.EditDigitOnlyKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then // #8 is Backspace
    Key := #0; // Discard the key
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.chkAssociateExtClick(Sender: TObject);
begin
  if chkAssociateExt.Checked then
  begin
    ShellRegisterExtension('vssprj',ApplicationName,Application.ExeName);
  end
  else
  begin
    ShellUnRegisterExtension('vssprj',ApplicationName,Application.ExeName);
  end;
end;

//------------------------------------------------------------------------------

function TPreferencesForm.GetErrorListElem(Idx : Integer) : PErrorListElem;
begin
  Result := nil;
  if (Idx >= 0) and (Idx < ListErrorChecking.Count) then
  begin
    Result := PErrorListElem(ListErrorChecking.Items.Objects[Idx]);
  end;
end;

//------------------------------------------------------------------------------
end.
//------------------------------------------------------------------------------

