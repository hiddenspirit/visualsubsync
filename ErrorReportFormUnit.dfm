object ErrorReportForm: TErrorReportForm
  Left = 515
  Top = 110
  Width = 321
  Height = 425
  BorderStyle = bsSizeToolWin
  Caption = 'Error report'
  Color = clBtnFace
  Constraints.MinHeight = 100
  Constraints.MinWidth = 100
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Menu = TntMainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    313
    379)
  PixelsPerInch = 96
  TextHeight = 13
  object vtvErrorList: TVirtualDrawTree
    Left = 4
    Top = 4
    Width = 305
    Height = 350
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
    PopupMenu = ErrorListPopupMenu
    TabOrder = 0
    OnDblClick = vtvErrorListDblClick
    OnDrawNode = vtvErrorListDrawNode
    Columns = <>
  end
  object TntStatusBar1: TTntStatusBar
    Left = 0
    Top = 358
    Width = 313
    Height = 21
    Panels = <
      item
        Width = 50
      end>
  end
  object ErrorListPopupMenu: TTntPopupMenu
    Left = 16
    Top = 48
    object pmiClear: TTntMenuItem
      Caption = 'Clear'
      OnClick = pmiClearClick
    end
  end
  object TntMainMenu1: TTntMainMenu
    Left = 16
    Top = 16
    object miFile: TTntMenuItem
      Caption = 'File'
      SubMenuImages = MainForm.ImageList1
      object miRecheck: TTntMenuItem
        Action = MainForm.ActionCheckErrors
      end
      object Clear1: TTntMenuItem
        Caption = 'Clear'
        OnClick = Clear1Click
      end
      object N2: TTntMenuItem
        Caption = '-'
      end
      object miPreferences: TTntMenuItem
        Action = MainForm.ActionErrorPreferences
      end
      object N1: TTntMenuItem
        Caption = '-'
      end
      object miClose: TTntMenuItem
        Caption = 'Close'
        OnClick = miCloseClick
      end
    end
  end
end
