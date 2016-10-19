uses Windows, MemoBoxInput;
begin
  MessageBox(0, PChar('Your message is '+InputMemoBox('Hello!', 'Please write your message', 'No message')+'!'), 'Hello!', MB_OK or MB_ICONINFORMATION or MB_TASKMODAL)
end.
