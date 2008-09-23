unit SpellCheckFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ComCtrls, TntComCtrls, SubStructUnit,
  LibHunspellUnit, UndoableSubTaskUnit;

type
  TSpellCheckForm = class(TForm)
    reSubtitleText: TTntRichEdit;
    lbSuggestions: TTntListBox;
    TntLabel1: TTntLabel;
    TntLabel2: TTntLabel;
    bttReplace: TTntButton;
    bttReplaceAll: TTntButton;
    bttIgnore: TTntButton;
    bttIgnoreAll: TTntButton;
    bttCancel: TTntButton;
    bttAdd: TTntButton;
    procedure bttCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bttIgnoreClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bttIgnoreAllClick(Sender: TObject);
    procedure bttAddClick(Sender: TObject);
    procedure bttReplaceClick(Sender: TObject);
  private
    { Private declarations }
    Sub : TSubtitleRange;
    TextToSpellOffset : Integer;
    TextToSpellWithTagOffset : Integer;
    WordIdx : Integer;
    WordInfo : TWordInfo;
    CurrentWord : WideString;
    MultiChangeTask : TUndoableMultiChangeTask;
    
    procedure NextError;
  public
    { Public declarations }
    function GetUndoableTask : TUndoableMultiChangeTask;
    procedure Reset;
  end;

var
  SpellCheckForm: TSpellCheckForm;

implementation

uses main, MiscToolsUnit, Types, TntClasses;

{$R *.dfm}

procedure TSpellCheckForm.bttCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSpellCheckForm.Reset;
begin
  TextToSpellOffset := 1;
  TextToSpellWithTagOffset := 0;
  WordIdx := 0;
  Sub := nil;
  FreeAndNil(MultiChangeTask);
  MultiChangeTask := TUndoableMultiChangeTask.Create;
end;

procedure TSpellCheckForm.NextError;
var WordArray : TWideStringDynArray;
    TextToSpell : WideString;
    SpellOk : Boolean;
    Suggestions: TTntStrings;
begin
  if not Assigned(Sub) then
  begin
    Sub := MainForm.GetFirst;
  end;
  while Assigned(Sub) do
  begin
    TagSplit(Sub.Text, WordArray);
    while (WordIdx < Length(WordArray)) do
    begin
      if (WordIdx mod 2) = 0 then
      begin
        TextToSpell := WordArray[WordIdx];
        while (TextToSpellOffset <= Length(TextToSpell)) do
        begin
          GetNextWordPos(TextToSpellOffset, TextToSpell, WordInfo);
          CurrentWord := Copy(TextToSpell, WordInfo.Position, WordInfo.Length);
          SpellOk := ContainDigit(CurrentWord) or IsUpperCase(CurrentWord)
            or MainForm.GetSpellChecker.Spell(CurrentWord);
          if (not SpellOk) then
          begin
            reSubtitleText.Text := TextToSpell;
            // Remove all colors
            reSubtitleText.SelectAll;
            reSubtitleText.SelAttributes.Color := clWindowText;
            // Put mispelled word in red
            reSubtitleText.SelStart := TextToSpellWithTagOffset + WordInfo.Position - 1;
            reSubtitleText.SelLength := WordInfo.Length;
            reSubtitleText.SelAttributes.Color := clRed;
            reSubtitleText.SelLength := 0;
            // Fill suggestions list
            Suggestions := TTntStringList.Create;
            MainForm.GetSpellChecker.Suggest(CurrentWord, Suggestions);
            lbSuggestions.Items.Assign(Suggestions);
            Suggestions.Free;
            // Select first suggestion
            if (lbSuggestions.Items.Count > 0) then
              lbSuggestions.ItemIndex := 0;
            Exit;
          end;
          Inc(TextToSpellOffset);
        end;
        TextToSpellOffset := 1;
      end;
      Inc(TextToSpellWithTagOffset, Length(WordArray[WordIdx]));
      Inc(WordIdx);
    end;
    WordIdx := 0;
    TextToSpellWithTagOffset := 0;
    Sub := MainForm.GetNext(Sub);
  end;
end;

procedure TSpellCheckForm.FormShow(Sender: TObject);
begin
  Reset;
  NextError;
end;

procedure TSpellCheckForm.bttIgnoreClick(Sender: TObject);
begin
  NextError;
end;

procedure TSpellCheckForm.FormCreate(Sender: TObject);
begin
  reSubtitleText.Font.Assign(MainForm.MemoSubtitleText.Font);
end;

procedure TSpellCheckForm.bttIgnoreAllClick(Sender: TObject);
begin
  MainForm.GetSpellChecker.Ignore(CurrentWord);
  NextError;
end;

procedure TSpellCheckForm.bttAddClick(Sender: TObject);
begin
  MainForm.GetSpellChecker.Add(CurrentWord);
  NextError;
end;

procedure TSpellCheckForm.bttReplaceClick(Sender: TObject);
var ChangeSubData : TChangeSubData;
    TextBegin, TextEnd : WideString;
begin
  ChangeSubData := TChangeSubData.Create(Sub.Node.Index);
  ChangeSubData.OldText := Sub.Text;
  TextBegin := Copy(Sub.Text, 1, WordInfo.Position - 1);
  TextEnd := Copy(Sub.Text, WordInfo.Position + WordInfo.Length, MaxInt);
  Sub.Text := TextBegin +
    lbSuggestions.Items.Strings[lbSuggestions.ItemIndex] + TextEnd;
  ChangeSubData.NewText := Sub.Text;
  MultiChangeTask.AddData(ChangeSubData);
  NextError;
end;

function TSpellCheckForm.GetUndoableTask : TUndoableMultiChangeTask;
begin
  Result := MultiChangeTask;
end;

end.
