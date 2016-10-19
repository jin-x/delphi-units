unit MemoBoxInput;  { v1.00 (c) 2013-2016 by Jin X (jin.x@sources.ru), http://xk7.ru/p/d/u }
// Модуль многострочного ввода текста из диалогового окна.
// Создан на основе открытых исходных кодов функций InputBox и InputQuery для Delphi 7.

interface

// Выводит диалоговое окно с вводом текста в несколько строк, возвращая введённую строку или ADefault в случае отмены
function InputMemoBox(const ACaption, APrompt, ADefault: String): String;

// Выводит диалоговое окно с вводом текста в несколько строк, возвращая True в случае подтверждения и False в случае отмены
// Введённая строка записывается в переменную Value, которая изначально содержит значение по умолчанию
function InputMemoQuery(const ACaption, APrompt: String; var Value: String): Boolean;

implementation

uses Windows, Graphics, Controls, Forms, StdCtrls, Consts, Dialogs;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

function InputMemoQuery(const ACaption, APrompt: String; var Value: String): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Memo: TMemo;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      Position := poScreenCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        Caption := APrompt;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
        WordWrap := True;
      end;
      Memo := TMemo.Create(Form);
      with Memo do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := Prompt.Top + Prompt.Height + 5;
        Width := MulDiv(164, DialogUnits.X, 4);
//        MaxLength := 65535;
        WantReturns := False;
        Text := Value;
        SelectAll;
      end;
      ButtonTop := Memo.Top + Memo.Height + 15;
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgOK;
        ModalResult := mrOk;
        Default := True;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgCancel;
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(MulDiv(92, DialogUnits.X, 4), Memo.Top + Memo.Height + 15,
          ButtonWidth, ButtonHeight);
        Form.ClientHeight := Top + Height + 13;          
      end;
      if ShowModal = mrOk then
      begin
        Value := Memo.Text;
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;

function InputMemoBox(const ACaption, APrompt, ADefault: String): String;
begin
  Result := ADefault;
  InputMemoQuery(ACaption, APrompt, Result);
end;

end.
