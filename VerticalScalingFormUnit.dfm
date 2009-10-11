object VerticalScalingForm: TVerticalScalingForm
  Left = 215
  Top = 103
  BorderStyle = bsToolWindow
  BorderWidth = 3
  Caption = 'Vertical scaling'
  ClientHeight = 98
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 321
    Height = 98
    Align = alClient
    Caption = ' Scaling : XXX% '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object Label2: TLabel
      Left = 15
      Top = 70
      Width = 12
      Height = 13
      Caption = '50'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 50
      Top = 70
      Width = 18
      Height = 13
      Caption = '100'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 129
      Top = 69
      Width = 18
      Height = 13
      Caption = '200'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 209
      Top = 70
      Width = 18
      Height = 13
      Caption = '300'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 286
      Top = 70
      Width = 18
      Height = 13
      Caption = '400'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object ScalingTrackBar: TTrackBar
      Left = 10
      Top = 35
      Width = 297
      Height = 33
      Max = 400
      Min = 50
      PageSize = 10
      Frequency = 25
      Position = 50
      TabOrder = 0
      OnChange = ScalingTrackBarChange
    end
    object bttReset: TButton
      Left = 232
      Top = 0
      Width = 67
      Height = 25
      Caption = 'Reset'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = bttResetClick
    end
  end
end
