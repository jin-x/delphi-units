program DelphiVerExample2;
{$APPTYPE CONSOLE}
// This is a simple example of "DelphiVerDef7x.inc" include using.

{$DEFINE NeedDelphi7}  // You need at least Delphi 7 compiler version,
{$DEFINE NeedWindows}  // This code can be compiled only for Windows.
{$I DelphiVerDef7x.inc}
{$IFNDEF DelphiVerDef7x100}{$MESSAGE FATAL 'DelphiVerDef7x.inc file is not included or incompatible version !!!'}{$ENDIF}

{$IFDEF UnitScopeNames}
uses System.SysUtils;
{$ELSE}
uses SysUtils;
{$ENDIF}

procedure WriteRes(const S: String); {$IFDEF Inline}inline;{$ENDIF}
begin
  WriteLn('You''ve entered ' + S)
end;  // AddN

var
  S: String;
  N: Extended;
  D: TDateTime;
{$IFDEF ForIn}
  Ch: Char;
  Ok: Boolean;
{$ENDIF}
begin
  Write('Enter number, date/time or some string: ');
  ReadLn(S);

{$IFDEF ForIn}
  Ok := False;
  for Ch in '., ' do
  begin
    Ok := TryStrToFloat(S, N);
    if Ok then Break;
    {$IFDEF DelphiXE}FormatSettings.{$ENDIF}DecimalSeparator := Ch
  end;
  if Ok then
{$ELSE}
  DecimalSeparator := '.';
  if TryStrToFloat(S, N) then
{$ENDIF}
    if Frac(N) = 0 then WriteRes('an integer number.')
    else WriteRes('a floating point number.')
  else

  begin
{$IFDEF ForIn}
    Ok := False;
    for Ch in '.-/ ' do
    begin
      Ok := TryStrToDateTime(S, D);
      if Ok then Break;
      {$IFDEF DelphiXE}FormatSettings.{$ENDIF}DateSeparator := Ch
    end;
    if Ok then
{$ELSE}
    if TryStrToDateTime(S, D) then
{$ENDIF}
      WriteRes('a date/time.')
      else WriteRes('some string.')
  end
end.
