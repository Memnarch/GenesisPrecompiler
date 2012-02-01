object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Genesis Precompiler'
  ClientHeight = 290
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LogEdit: TRichEdit
    Left = 0
    Top = 0
    Width = 554
    Height = 290
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 0
  end
  object MainMenu1: TMainMenu
    Left = 392
    Top = 216
    object File1: TMenuItem
      Caption = 'File'
      object Open1: TMenuItem
        Action = OpenAction
      end
    end
    object About1: TMenuItem
      Action = AboutAction
    end
  end
  object ActionList1: TActionList
    Left = 448
    Top = 96
    object OpenAction: TAction
      Caption = 'Open'
      OnExecute = OpenActionExecute
    end
    object AboutAction: TAction
      Caption = 'About'
      OnExecute = AboutActionExecute
    end
  end
  object OpenDialog1: TOpenDialog
    FileName = 'D:\Delphi\GenesisPreCompiler\testfiles\Rectangle.class'
    Filter = 'SourceFile(*.cxx)|*.cxx'
    Left = 272
    Top = 160
  end
end
