unit ProcessMsgBox;  { v1.00 (c) 2016 by Jin X (jin.x@sources.ru), http://xk7.ru/p/d/u }
// Модуль предназначен для вывода немодальных сообщений с 1 или 2 кнопками либо без кнопок (в данной версии), т.е. сообщений, которые не останавливают работу, а "висят" параллельно дальнейшей работы программы.
// Кроме того, имеется возможность устанавливать таймаут, по истечении которого окно закрывается автоматически.
// Использует VCL-модули, поэтому размер получаемого файла будет большим...

{$B-}
interface

uses Windows, Forms;

type
  // Иконка окна сообщения (предупреждение, ошибка, информация (по умолчанию), вопрос).
  TProcessMsgBoxIcon = (pmbWarning, pmbError, pmbInformation, pmbQuestion);
  // Статус диалогового окна (всё ещё показывается, окно закрыто, время вышло, нажата кнопка).
  TProcessMsgBoxStatus = (pmbDisplaying, pmbClosed, pmbTimedout, pmbButton1, pmbButton2);

  // Основной класс для работы с немодальными сообщениями.
  TProcessMsgBox = class
  protected
    // Внутренние переменные и методы.
    MsgForm: TForm;
    FClosed: Boolean;
    FStatus: TProcessMsgBoxStatus;
    TickStart, TickCount: DWord;
    procedure ClickEvent(Sender: TObject);
    procedure CloseEvent(Sender: TObject; var Action: TCloseAction);

  public
    // Создать экземпляр класса без вывода сообщения.
    constructor Create; overload;

    // Вывести сообщение с текстом Text, заголовком Title и кнопками Buttons (если в тексте нет символов '||', используется 1 кнопка, если есть, то 2, текст на которых разделяется этими символами;
    // текст каждой кнопки должен быть ~ до 10 символов, иначе будет обрезан; если задана пустая строка, кнопки будут отсутствовать; если параметр не задан, используется текст "Cancel").
    // Icon задаёт тип иконки, Timeout - количество миллисекунд, по истечении которых сообщение будет автоматически закрыто (при значении = 0 автоматическое закрытие окна отключается),
    // AlwaysOnTop определяет, будет ли сообщение поверх всех окон, а XButton = False удаляет крестик закрытия окна (но это не значит, что его нельзя будет закрыть через Alt+F4 или завершение работы системы).
    constructor Create(const Text, Title, Buttons: String; Icon: TProcessMsgBoxIcon = pmbInformation; Timeout: DWord = 0; AlwaysOnTop: Boolean = False; XButton: Boolean = True); overload;
    constructor Create(const Text, Title, Buttons: String; Icon: TProcessMsgBoxIcon; AlwaysOnTop: Boolean; XButton: Boolean = True); overload;
    constructor Create(const Text, Title, Buttons: String; Timeout: DWord; AlwaysOnTop: Boolean = False; XButton: Boolean = True); overload;
    constructor Create(const Text, Title, Buttons: String; AlwaysOnTop: Boolean; XButton: Boolean = True); overload;
    constructor Create(const Text, Title: String; Icon: TProcessMsgBoxIcon = pmbInformation; Timeout: DWord = 0; AlwaysOnTop: Boolean = False; XButton: Boolean = True); overload;
    constructor Create(const Text, Title: String; Icon: TProcessMsgBoxIcon; AlwaysOnTop: Boolean; XButton: Boolean = True); overload;
    constructor Create(const Text, Title: String; Timeout: DWord; AlwaysOnTop: Boolean = False; XButton: Boolean = True); overload;
    constructor Create(const Text, Title: String; AlwaysOnTop: Boolean; XButton: Boolean = True); overload;

    // Вывести сообщение аналогино конструктору Create.
    // Если сообщение уже отображается данным экземпляром класса, оно будет закрыто.
    procedure Show(const Text, Title, Buttons: String; Icon: TProcessMsgBoxIcon = pmbInformation; Timeout: DWord = 0; AlwaysOnTop: Boolean = False; XButton: Boolean = True); overload;
    procedure Show(const Text, Title, Buttons: String; Icon: TProcessMsgBoxIcon; AlwaysOnTop: Boolean; XButton: Boolean = True); overload;
    procedure Show(const Text, Title, Buttons: String; Timeout: DWord; AlwaysOnTop: Boolean = False; XButton: Boolean = True); overload;
    procedure Show(const Text, Title, Buttons: String; AlwaysOnTop: Boolean; XButton: Boolean = True); overload;
    procedure Show(const Text, Title: String; Icon: TProcessMsgBoxIcon = pmbInformation; Timeout: DWord = 0; AlwaysOnTop: Boolean = False; XButton: Boolean = True); overload;
    procedure Show(const Text, Title: String; Icon: TProcessMsgBoxIcon; AlwaysOnTop: Boolean; XButton: Boolean = True); overload;
    procedure Show(const Text, Title: String; Timeout: DWord; AlwaysOnTop: Boolean = False; XButton: Boolean = True); overload;
    procedure Show(const Text, Title: String; AlwaysOnTop: Boolean; XButton: Boolean = True); overload;

    // Дать окну сообщения "вдохнуть свежего воздуха" (обработать сообщения Windows) и проверить таймаут.
    // Этот метод нужно использовать внутри циклов вместо Application.ProcessMessages (поскольку он его и так содержит)!
    procedure Breathe;

    // Принудительно закрыть сообщение, установив указанный статус (при задании статуса pmbDisplaying будет установлен статус pmbClosed).
    // Если окно уже закрыто, ничего страшного. После обработки статуса необходимо освободить память с помощью метода Free.
    procedure Close(Status: TProcessMsgBoxStatus = pmbClosed);

    // Закрыть окно и освободить память. Если окно уже закрыто, ничего страшного.
    procedure Free;

    // Окно уже закрыто?
    property Closed: Boolean read FClosed;

    // Статус диалогового окна (в т.ч. причина закрытия).
    property Status: TProcessMsgBoxStatus read FStatus;
  end;

  // Закрыть окно, освободить память и обнулить переменную (если она уже обнулена, ничего страшного).
  procedure PMBFreeAndNil(var MsgBox: TProcessMsgBox);

  // Вызвать метод Breathe или Application.ProcessMessages, если переменная пуста.
  procedure PMBSafelyBreathe(MsgBox: TProcessMsgBox);

  // Вызвать метод Breathe или Sleep(Time), если переменная пуста.
  procedure PMBBreatheOrSleep(MsgBox: TProcessMsgBox; Time: Dword = 0);


implementation

uses SysUtils, Messages, StdCtrls, Dialogs;

constructor TProcessMsgBox.Create;
begin
  FClosed := True;
  FStatus := pmbClosed
end;

constructor TProcessMsgBox.Create(const Text, Title, Buttons: String; Icon: TProcessMsgBoxIcon = pmbInformation; Timeout: DWord = 0; AlwaysOnTop: Boolean = False; XButton: Boolean = True);
begin
  Create;
  Show(Text, Title, Buttons, Icon, Timeout, AlwaysOnTop, XButton)
end;

constructor TProcessMsgBox.Create(const Text, Title, Buttons: String; Icon: TProcessMsgBoxIcon; AlwaysOnTop: Boolean; XButton: Boolean = True);
begin
  Create(Text, Title, Buttons, Icon, 0, AlwaysOnTop, XButton)
end;

constructor TProcessMsgBox.Create(const Text, Title, Buttons: String; Timeout: DWord; AlwaysOnTop: Boolean = False; XButton: Boolean = True);
begin
  Create(Text, Title, Buttons, pmbInformation, Timeout, AlwaysOnTop, XButton)
end;

constructor TProcessMsgBox.Create(const Text, Title, Buttons: String; AlwaysOnTop: Boolean; XButton: Boolean = True);
begin
  Create(Text, Title, Buttons, pmbInformation, 0, AlwaysOnTop, XButton)
end;

constructor TProcessMsgBox.Create(const Text, Title: String; Icon: TProcessMsgBoxIcon = pmbInformation; Timeout: DWord = 0; AlwaysOnTop: Boolean = False; XButton: Boolean = True);
begin
  Create(Text, Title, 'Cancel', Icon, Timeout, AlwaysOnTop, XButton)
end;

constructor TProcessMsgBox.Create(const Text, Title: String; Icon: TProcessMsgBoxIcon; AlwaysOnTop: Boolean; XButton: Boolean = True);
begin
  Create(Text, Title, 'Cancel', Icon, 0, AlwaysOnTop, XButton)
end;

constructor TProcessMsgBox.Create(const Text, Title: String; Timeout: DWord; AlwaysOnTop: Boolean = False; XButton: Boolean = True);
begin
  Create(Text, Title, 'Cancel', pmbInformation, Timeout, AlwaysOnTop, XButton)
end;

constructor TProcessMsgBox.Create(const Text, Title: String; AlwaysOnTop: Boolean; XButton: Boolean = True);
begin
  Create(Text, Title, 'Cancel', pmbInformation, 0, AlwaysOnTop, XButton)
end;

procedure TProcessMsgBox.Show(const Text, Title, Buttons: String; Icon: TProcessMsgBoxIcon = pmbInformation; Timeout: DWord = 0; AlwaysOnTop: Boolean = False; XButton: Boolean = True);
var
  DlgType: TMsgDlgType;
  DlgButtons: TMsgDlgButtons;
  DivPos, N: Integer;
begin
  if not FClosed then Close;
  
  case Icon of
    pmbWarning: DlgType := mtWarning;
    pmbError: DlgType := mtError;
    pmbQuestion: DlgType := mtConfirmation
    else DlgType := mtInformation
  end;
  DivPos := Pos('||', Buttons);
  if Buttons = '' then DlgButtons := []
  else if DivPos = 0 then DlgButtons := [mbCancel]
  else DlgButtons := [mbOK, mbCancel];
  MsgForm := CreateMessageDialog(Text, DlgType, DlgButtons);
  with MsgForm do
  begin
    Caption := Title;
    if Buttons <> '' then
    begin
      N := 1;
      if DivPos > 0 then
        with TButton(FindChildControl('OK')) do
        begin
          Caption := Copy(Buttons, 1, DivPos-1);
          OnClick := ClickEvent;
          Tag := N;
          Inc(N)
        end // with
      else Dec(DivPos);
      with TButton(FindChildControl('Cancel')) do
      begin
        Caption := Copy(Buttons, DivPos+2, MaxInt);
        OnClick := ClickEvent;
        Tag := N
      end // with
    end; // if Button <> ''
    if not XButton then BorderIcons := [];
    if AlwaysOnTop then FormStyle := fsStayOnTop; // SetWindowPos(MsgForm.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);

    FClosed := False;
    FStatus := pmbDisplaying;
    OnClose := CloseEvent;
    TickCount := Timeout;
    TickStart := GetTickCount;
    Show;
    Breathe
  end // with MsgForm
end; // Show

procedure TProcessMsgBox.Show(const Text, Title, Buttons: String; Icon: TProcessMsgBoxIcon; AlwaysOnTop: Boolean; XButton: Boolean = True);
begin
  Show(Text, Title, Buttons, Icon, 0, AlwaysOnTop, XButton)
end;

procedure TProcessMsgBox.Show(const Text, Title, Buttons: String; Timeout: DWord; AlwaysOnTop: Boolean = False; XButton: Boolean = True);
begin
  Show(Text, Title, Buttons, pmbInformation, Timeout, AlwaysOnTop, XButton)
end;

procedure TProcessMsgBox.Show(const Text, Title, Buttons: String; AlwaysOnTop: Boolean; XButton: Boolean = True);
begin
  Show(Text, Title, Buttons, pmbInformation, 0, AlwaysOnTop, XButton)
end;

procedure TProcessMsgBox.Show(const Text, Title: String; Icon: TProcessMsgBoxIcon = pmbInformation; Timeout: DWord = 0; AlwaysOnTop: Boolean = False; XButton: Boolean = True);
begin
  Show(Text, Title, 'Cancel', Icon, Timeout, AlwaysOnTop, XButton)
end;

procedure TProcessMsgBox.Show(const Text, Title: String; Icon: TProcessMsgBoxIcon; AlwaysOnTop: Boolean; XButton: Boolean = True);
begin
  Show(Text, Title, 'Cancel', Icon, 0, AlwaysOnTop, XButton)
end;

procedure TProcessMsgBox.Show(const Text, Title: String; Timeout: DWord; AlwaysOnTop: Boolean = False; XButton: Boolean = True);
begin
  Show(Text, Title, 'Cancel', pmbInformation, Timeout, AlwaysOnTop, XButton)
end;

procedure TProcessMsgBox.Show(const Text, Title: String; AlwaysOnTop: Boolean; XButton: Boolean = True);
begin
  Show(Text, Title, 'Cancel', pmbInformation, 0, AlwaysOnTop, XButton)
end;

procedure TProcessMsgBox.Breathe;
begin
  Application.ProcessMessages;
  if FClosed then Exit;
  if (TickCount <> 0) and (GetTickCount - TickStart >= TickCount) then
  begin
    FClosed := True;
    FStatus := pmbTimedout;
    MsgForm.Free
  end
end;

procedure TProcessMsgBox.ClickEvent(Sender: TObject);
begin
  if (Sender as TButton).Tag = 1 then FStatus := pmbButton1
  else FStatus := pmbButton2;
  MsgForm.Close
end;

procedure TProcessMsgBox.CloseEvent(Sender: TObject; var Action: TCloseAction);
begin
  FClosed := True;
  if FStatus = pmbDisplaying then FStatus := pmbClosed;
  Action := caFree
end;

procedure TProcessMsgBox.Close(Status: TProcessMsgBoxStatus = pmbClosed);
begin
  FStatus := Status;
  if FStatus = pmbDisplaying then FStatus := pmbClosed;
  if not FClosed then MsgForm.Close;
  Breathe
end;

procedure TProcessMsgBox.Free;
begin
  Close;
  inherited Free
end;

procedure PMBFreeAndNil(var MsgBox: TProcessMsgBox);
begin
  if MsgBox <> nil then FreeAndNil(MsgBox)
end;

procedure PMBSafelyBreathe(MsgBox: TProcessMsgBox);
begin
  if MsgBox <> nil then MsgBox.Breathe
  else Application.ProcessMessages
end;

procedure PMBBreatheOrSleep(MsgBox: TProcessMsgBox; Time: Dword = 0);
begin
  if MsgBox <> nil then MsgBox.Breathe
  else Sleep(Time)
end;

end.
