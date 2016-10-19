object Form1: TForm1
  Left = 190
  Top = 105
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = #1054#1087#1088#1077#1076#1077#1083#1077#1085#1080#1077' '#1082#1086#1076#1080#1088#1086#1074#1082#1080
  ClientHeight = 85
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 103
    Height = 13
    Caption = #1042#1074#1077#1076#1080#1090#1077' '#1080#1084#1103' '#1092#1072#1081#1083#1072':'
  end
  object Label2: TLabel
    Left = 201
    Top = 75
    Width = 142
    Height = 10
    Hint = #1050#1088#1072#1089#1085#1080#1082#1086#1074' '#1045#1074#1075#1077#1085#1080#1081#13'    jin_x@pisem.net'
    Caption = 'Charset Detect, v1.00 (c) 2003 by Jin X'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 192
    Font.Height = -8
    Font.Name = 'Small Fonts'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object Edit1: TEdit
    Left = 128
    Top = 13
    Width = 201
    Height = 21
    TabOrder = 0
    Text = 'CSDetect.pas'
  end
  object Button1: TButton
    Left = 88
    Top = 48
    Width = 169
    Height = 25
    Caption = #1054#1087#1088#1077#1076#1077#1083#1080#1090#1100' '#1082#1086#1076#1080#1088#1086#1074#1082#1091
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
end
