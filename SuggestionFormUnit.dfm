object SuggestionForm: TSuggestionForm
  Left = 209
  Top = 115
  Width = 273
  Height = 436
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSizeToolWin
  BorderWidth = 4
  Caption = 'Suggestions'
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 100
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    257
    401)
  PixelsPerInch = 96
  TextHeight = 13
  object vtvSuggestionsLst: TVirtualDrawTree
    Left = 0
    Top = 0
    Width = 257
    Height = 321
    Anchors = [akLeft, akTop, akRight, akBottom]
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    ParentBackground = False
    PopupMenu = TntPopupMenu1
    TabOrder = 0
    OnChange = vtvSuggestionsLstChange
    OnDblClick = vtvSuggestionsLstDblClick
    OnDrawNode = vtvSuggestionsLstDrawNode
    Columns = <>
  end
  object MemoSuggPreview: TTntMemo
    Left = 0
    Top = 328
    Width = 257
    Height = 73
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
  end
  object TntPopupMenu1: TTntPopupMenu
    Left = 8
    Top = 8
    object pmiClear: TTntMenuItem
      Caption = 'Clear'
      OnClick = pmiClearClick
    end
  end
end
