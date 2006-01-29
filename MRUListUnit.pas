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

// TODO : unicode friendly

unit MRUListUnit;

interface

uses Classes, Menus, IniFiles;

type
  TMRUList = class
  private
    FFileList : TStringList;
    FLimit : Integer;
    FRootMenuItem : TMenuItem;
    FOnRecentMenuItemClick : TNotifyEvent;

    procedure FillMenuItem(MenuItem : TMenuItem);
    procedure OnClearItemClick(Sender: TObject);
    procedure OnClearDeadEntriesItemClick(Sender: TObject);
  public
    constructor Create(RootMenuItem : TMenuItem);
    destructor Destroy; override;
    procedure AddFile(Filename : string);
    procedure SaveIni(IniFile : TIniFile; SectionName : string);
    procedure LoadIni(IniFile : TIniFile; SectionName : string);
  published
    property OnRecentMenuItemClick : TNotifyEvent read FOnRecentMenuItemClick write FOnRecentMenuItemClick;
  end;

implementation

uses SysUtils;

// -----------------------------------------------------------------------------

constructor TMRUList.Create(RootMenuItem : TMenuItem);
begin
  FFileList := TStringList.Create;
  FLimit := 8;
  FRootMenuItem := RootMenuItem;
end;

// -----------------------------------------------------------------------------

destructor TMRUList.Destroy;
begin
  FFileList.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TMRUList.AddFile(Filename : string);
var Idx : Integer;
begin
  // Search if file is already present
  Idx := FFileList.IndexOf(Filename);
  if (Idx = -1) then
    FFileList.Insert(0,Filename)
  else
    FFileList.Move(Idx,0);
  while (FFileList.Count > FLimit) do
    FFileList.Delete(FLimit);
  FillMenuItem(FRootMenuItem);
end;

// -----------------------------------------------------------------------------

procedure TMRUList.FillMenuItem(MenuItem : TMenuItem);
var NewItem : TMenuItem;
    i : Integer;
begin
  MenuItem.Clear;
  MenuItem.Enabled := (FFileList.Count > 0);
  for i:=0 to FFileList.Count-1 do
  begin
    NewItem := TMenuItem.Create(MenuItem);
    NewItem.Caption := FFileList[i];
    NewItem.OnClick := FOnRecentMenuItemClick;
    MenuItem.Add(NewItem);
  end;
  // Add special "Clear list" menu
  NewItem := TMenuItem.Create(MenuItem);
  NewItem.Caption := '-';
  MenuItem.Add(NewItem);

  NewItem := TMenuItem.Create(MenuItem);
  NewItem.Caption := 'Clear list';
  NewItem.OnClick := OnClearItemClick;
  MenuItem.Add(NewItem);

  NewItem := TMenuItem.Create(MenuItem);
  NewItem.Caption := 'Clear dead entries';
  NewItem.OnClick := OnClearDeadEntriesItemClick;
  MenuItem.Add(NewItem);
end;

// -----------------------------------------------------------------------------

procedure TMRUList.SaveIni(IniFile : TIniFile; SectionName : string);
var i : Integer;
begin
  for i:=0 to FLimit-1 do
  begin
    if i < FFileList.Count then
      IniFile.WriteString(SectionName,'Recent'+IntToStr(i),FFileList[i])
    else
      IniFile.WriteString(SectionName,'Recent'+IntToStr(i),'')
  end;
end;

// -----------------------------------------------------------------------------

procedure TMRUList.LoadIni(IniFile : TIniFile; SectionName : string);
var i : Integer;
    s : string;
begin
  FFileList.Clear;
  for i:=0 to FLimit-1 do
  begin
    s := IniFile.ReadString(SectionName, 'Recent'+IntToStr(i),'');
    s := Trim(s);
    if (s <> '') then
      FFileList.Add(s);
  end;
  FillMenuItem(FRootMenuItem);
end;

// -----------------------------------------------------------------------------

procedure TMRUList.OnClearItemClick(Sender: TObject);
begin
  FFileList.Clear;
  FillMenuItem(FRootMenuItem);
end;

// -----------------------------------------------------------------------------

procedure TMRUList.OnClearDeadEntriesItemClick(Sender: TObject);
var i : Integer;
begin
  for i:=FFileList.Count-1 downto 0 do
  begin
    if not FileExists(FFileList[i]) then
    begin
      FFileList.Delete(i);
    end;
  end;
  FillMenuItem(FRootMenuItem);
end;

// -----------------------------------------------------------------------------
end.
// -----------------------------------------------------------------------------
