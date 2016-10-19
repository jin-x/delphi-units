unit CSDMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CSDetect;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
  F: File;
  S: String;
  i: DWord;
  CloseIt: Boolean;
begin
  CloseIt := False;
  try
    AssignFile(F, Edit1.Text);
    Reset(F, 1);
    CloseIt := True;
    i := FileSize(F);
    SetLength(S, i);
    BlockRead(F, PChar(S)^, i);
    MessageBox(0, PChar('DetectCharset: '+CSName[DetectCharset(S)]+
                     #13'DetectCharsetFast: '+CSName[DetectCharsetFast(S)]), 'Кодировка', MB_OK or MB_ICONINFORMATION or MB_TASKMODAL)
  except
    MessageBox(0, 'Не удалось прочитать указанный файл.', 'Ошибка!', MB_OK or MB_ICONERROR or MB_TASKMODAL)
  end;
  {$I-} if CloseIt then CloseFile(F) {$I+}
end;

end.
