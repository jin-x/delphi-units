program CSDTest;

uses
  Forms,
  CSDMain in 'CSDMain.pas' {Form1};

begin
  Application.Initialize;
  Application.Title := 'Определение кодировки';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
