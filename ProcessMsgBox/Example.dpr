uses ProcessMsgBox, Windows, Forms;
var
  MsgBox: TProcessMsgBox;
  S: String;
  i: Integer;
begin
  Sleep(1000);
  MsgBox := TProcessMsgBox.Create('Just wait...', 'Break test', '', pmbWarning, 5000, True, False);
  for i := 1 to 10 do
  begin
    Sleep(100);
    MsgBox.Breathe
  end;
  MsgBox.Show('Please wait...', 'Processing', 'OK||Cancel', pmbInformation, 5000, True);
  repeat
    MsgBox.Breathe
  until MsgBox.Closed;

  case MsgBox.Status of
    pmbClosed: S := 'Dialog is closed!';
    pmbTimedout: S := 'Timed out!';
    pmbButton1: S := '"OK" button is pressed!';
    pmbButton2: S := '"Cancel" button is pressed!';
    else S := 'Still displaying??? 8-(  )';
  end;
  MessageBox(0, PChar(S), 'Result', MB_OK or MB_ICONINFORMATION or MB_SYSTEMMODAL);
  MsgBox.Free;
  MessageBox(0, 'Finished!', '', MB_OK or MB_ICONINFORMATION or MB_SYSTEMMODAL);
end.
