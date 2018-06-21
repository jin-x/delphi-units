unit Measure7x;  { v1.00 (c) 2018 by Jin X (jin.x@sources.ru), http://xk7.ru/p/d/u }
// ������ ��� ��������� �������� �������� � ��������� �������� ����
// ��� ������ �������� ������������ ������������ ������� WinAPI QueryPerformanceCounter � QueryPerformanceFrequency

{$IF CompilerVersion >= 17} // Delphi 2005+
  {$DEFINE INLINE}
  {$DEFINE STATIC}
{$IFEND}

interface

type
  // ��� ��������� ��� ����������
  TProcedure = procedure;
  // ��� ��������� � 1-� ���������� ���� Integer
  TProcedureInt = procedure(N: Integer);
  // ������ �������� ��� ����������
  TProcedureArray = array of TProcedure;

  // ����� ������ �������� � ����� ���������
  TMeasure = class
    protected
      FCounter: Int64;
    public
      // ������ ������ �������
      procedure Start;
      // ���������� ������ ������� � ������� ������� ����� ������� � ��������� ���������� �������� �������
      function Stop: Int64;
      // �������� �������� ������ ���������
      function MeasureProc(Proc: TProcedure): Int64;
      // �������� �������� ������ ��������� � ���������� N
      function MeasureCall(Proc: TProcedureInt; N: Integer): Int64;
      // �������� �������� �������� (��������� �����)
      // ���� ������� �� ��� �������, ����� ���������� ������� ��������, ���� �� ���� ����������, ����� ���������� �������� ������� ������
      property Counter: Int64 read FCounter;
      // �������� �������� �������� (��������� �����) � ��������
      function Sec: Double;
      // �������� �������� �������� (��������� �����) � �������������
      function Millisec: Int64;
      // �������� �������� �������� (��������� �����) � �������������
      function Microsec: Int64;
      // �������� ������� �������� (���-�� �������� � �������)
      class function Frequency: Int64; {$IFDEF STATIC}static;{$ENDIF}
  end;

// ������ ������ �������, ��������� ������� ����� Idx ��� ������� Counter
// ���� Idx < 0, ��������� ���������� ��� ������� ������� �������
procedure StartMeasure(Idx: Integer = 0); overload;
procedure StartMeasure(var Counter: Int64); overload; {$IFDEF INLINE}inline;{$ENDIF}

// ���������� ������ �������, ��������� ������� ����� Idx ��� ������� Counter
// ���������� ������� ����� ������� � ��������� ���������� �������� �������
// ���� �������� �� ���������� (���� Idx < 0), ����� ���������� �������� -1
function StopMeasure(Idx: Integer = 0): Int64; overload;
function StopMeasure(var Counter: Int64): Int64; overload;

// �������� �������� �������� (��������� �����), ��������� ������� ����� Idx
// ���� ������� �� ��� ����������, ����� ���������� �������� ������� ������
// ���� �������� �� ���������� (���� Idx < 0), ����� ���������� �������� -1
function LastMeasure(Idx: Integer = 0): Int64; overload;
// �������� �������� �������� (��������� �����), ��������� ������� ����� Idx, � ��������, ������������� ��� �������������
function LastMeasureSec(Idx: Integer = 0): Double; {$IFDEF INLINE}inline;{$ENDIF}
function LastMeasureMillisec(Idx: Integer = 0): Int64; {$IFDEF INLINE}inline;{$ENDIF}
function LastMeasureMicrosec(Idx: Integer = 0): Int64; {$IFDEF INLINE}inline;{$ENDIF}

// �������� �������� ������ ���������, ��������� ������� ����� Idx
// ���� Idx < 0, ����� ���������� �������� -1, �� ��������� Proc �� ����� ����� ���������
function MeasureProc(Proc: TProcedure; Idx: Integer = 0): Int64;
// �������� �������� ������ ��������� � ���������� N, ��������� ������� ����� Idx (�� ��������� 0, � �� N !!!)
// ���� Idx < 0, ����� ���������� �������� -1, �� ��������� Proc �� ����� ����� ���������
function MeasureCall(Proc: TProcedureInt; N: Integer; Idx: Integer = 0): Int64;

// �������� �������� ������ 2-�, 3-�, 4-� ��� 5-� �������� � ������� ����� ����� ������� �� ��� (�� 0 �� 4)
// �������� �������� ������� ����� �������� � ������� ������� LastMeasure, ������ � �������� ��������� ����� ���������
function MeasureFastestProc(Proc1, Proc2: TProcedure): Integer; overload;
function MeasureFastestProc(Proc1, Proc2, Proc3: TProcedure): Integer; overload;
function MeasureFastestProc(Proc1, Proc2, Proc3, Proc4: TProcedure): Integer; overload;
function MeasureFastestProc(Proc1, Proc2, Proc3, Proc4, Proc5: TProcedure): Integer; overload;
// �������� �������� ������ ��������������� ���-�� �������� � ������� ����� (������ �������) ����� ������� �� ���
// ���� ������ ����, ����������� ����� �������� -1
// �������� �������� ������� ����� �������� � ������� ������� LastMeasure, ������ � �������� ��������� ����� ���������
function MeasureFastestProc(ProcList: TProcedureArray): Integer; overload;
// �������� �������� ������ ��������� �� ���������� ��������� �� 0 �� Count-1 � ������� ����� ������ �������� ������ (������� � 0)
// ���� Count <= 0, ����� ���������� �������� -1
// �������� �������� ������� ����� �������� � ������� ������� LastMeasure, ������ � �������� ��������� �������� �� 0 �� Count-1
function MeasureFastestCall(Proc: TProcedureInt; Count: Integer): Integer;

// �������� �������� ������ 2-�, 3-�, 4-� ��� 5-� �������� � ������� ����� ����� ��������� �� ��� (�� 0 �� 4)
// �������� �������� ������� ����� �������� � ������� ������� LastMeasure, ������ � �������� ��������� ����� ���������
function MeasureSlowestProc(Proc1, Proc2: TProcedure): Integer; overload;
function MeasureSlowestProc(Proc1, Proc2, Proc3: TProcedure): Integer; overload;
function MeasureSlowestProc(Proc1, Proc2, Proc3, Proc4: TProcedure): Integer; overload;
function MeasureSlowestProc(Proc1, Proc2, Proc3, Proc4, Proc5: TProcedure): Integer; overload;
// �������� �������� ������ ��������������� ���-�� �������� � ������� ����� (������ �������) ����� ��������� �� ���
// ���� ������ ����, ����������� ����� �������� -1
// �������� �������� ������� ����� �������� � ������� ������� LastMeasure, ������ � �������� ��������� ����� ���������
function MeasureSlowestProc(ProcList: TProcedureArray): Integer; overload;
// �������� �������� ������ ��������� �� ���������� ��������� �� 0 �� Count-1 � ������� ����� ������ ���������� ������ (������� � 0)
// ���� Count <= 0, ����� ���������� �������� -1
// �������� �������� ������� ����� �������� � ������� ������� LastMeasure, ������ � �������� ��������� �������� �� 0 �� Count-1
function MeasureSlowestCall(Proc: TProcedureInt; Count: Integer): Integer;

// ��������� �������� �������� � �������, ������������ ��� ������������
function MeasureToSec(Counter: Int64): Double;
function MeasureToMillisec(Counter: Int64): Int64;
function MeasureToMicrosec(Counter: Int64): Int64;

// �������� ������� �������� (���-�� �������� � �������)
function MeasureFrequency: Int64;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

implementation

{$IF CompilerVersion >= 23} // Delphi XE2+
uses Winapi.Windows;
{$ELSE}
uses Windows;
{$IFEND}

var
  CounterFrequency: Int64;
  Counters: array of Int64;

////////////////////
// ����� TMeasure //
////////////////////

procedure TMeasure.Start;
begin
  QueryPerformanceCounter(FCounter);
end;

function TMeasure.Stop: Int64;
begin
  QueryPerformanceCounter(Result);
  Dec(Result, FCounter);
  FCounter := Result;
end;

function TMeasure.MeasureProc(Proc: TProcedure): Int64;
begin
  Start;
  Proc;
  Result := Stop;
end;

function TMeasure.MeasureCall(Proc: TProcedureInt; N: Integer): Int64;
begin
  Start;
  Proc(N);
  Result := Stop;
end;

function TMeasure.Sec: Double;
begin
  Result := MeasureToSec(FCounter);
end;

function TMeasure.Millisec: Int64;
begin
  Result := MeasureToMillisec(FCounter);
end;

function TMeasure.Microsec: Int64;
begin
  Result := MeasureToMicrosec(FCounter);
end;

class function TMeasure.Frequency: Int64;
begin
  Result := CounterFrequency;
end;

///////////////////////////////
// ����� ��������� � ������� //
///////////////////////////////

procedure StartMeasure(Idx: Integer = 0);
begin
  if Idx < 0 then Exit;
  if Idx+1 > Length(Counters) then SetLength(Counters, Idx+1);
  QueryPerformanceCounter(Counters[Idx]);
end;

procedure StartMeasure(var Counter: Int64);
begin
  QueryPerformanceCounter(Counter);
end;

function StopMeasure(Idx: Integer = 0): Int64;
begin
  if (Idx < 0) or (Idx > Length(Counters)-1) then
  begin
    Result := -1;
    Exit;
  end;
  QueryPerformanceCounter(Result);
  Result := Result - Counters[Idx];
  Counters[Idx] := Result;
end;

function StopMeasure(var Counter: Int64): Int64;
begin
  QueryPerformanceCounter(Result);
  Result := Result - Counter;
  Counter := Result;
end;

function LastMeasure(Idx: Integer = 0): Int64;
begin
  if (Idx < 0) or (Idx > Length(Counters)-1) then
  begin
    Result := -1;
    Exit;
  end;
  Result := Counters[Idx];
end;

function LastMeasureSec(Idx: Integer = 0): Double;
begin
  Result := MeasureToSec(LastMeasure(Idx));
end;

function LastMeasureMillisec(Idx: Integer = 0): Int64;
begin
  Result := MeasureToMillisec(LastMeasure(Idx));
end;

function LastMeasureMicrosec(Idx: Integer = 0): Int64;
begin
  Result := MeasureToMicrosec(LastMeasure(Idx));
end;

function MeasureProc(Proc: TProcedure; Idx: Integer = 0): Int64;
begin
  StartMeasure(Idx);
  Proc;
  Result := StopMeasure(Idx);
end;

function MeasureCall(Proc: TProcedureInt; N: Integer; Idx: Integer = 0): Int64;
begin
  StartMeasure(Idx);
  Proc(N);
  Result := StopMeasure(Idx);
end;

function MeasureFastestProc(Proc1, Proc2: TProcedure): Integer;
begin
  MeasureProc(Proc1, 0);
  MeasureProc(Proc2, 1);
  if Counters[0] < Counters[1] then Result := 0
  else Result := 1;
end;

function MeasureFastestProc(Proc1, Proc2, Proc3: TProcedure): Integer; overload;
var
  i: Integer;
  MinVal: Int64;
begin
  MeasureProc(Proc1, 0);
  MeasureProc(Proc2, 1);
  MeasureProc(Proc3, 2);
  Result := 0;
  MinVal := Counters[0];
  for i := 1 to 2 do
    if Counters[i] < MinVal then
    begin
      Result := i;
      MinVal := Counters[i];
    end;
end;

function MeasureFastestProc(Proc1, Proc2, Proc3, Proc4: TProcedure): Integer; overload;
var
  i: Integer;
  MinVal: Int64;
begin
  MeasureProc(Proc1, 0);
  MeasureProc(Proc2, 1);
  MeasureProc(Proc3, 2);
  MeasureProc(Proc4, 3);
  Result := 0;
  MinVal := Counters[0];
  for i := 1 to 3 do
    if Counters[i] < MinVal then
    begin
      Result := i;
      MinVal := Counters[i];
    end;
end;

function MeasureFastestProc(Proc1, Proc2, Proc3, Proc4, Proc5: TProcedure): Integer; overload;
var
  i: Integer;
  MinVal: Int64;
begin
  MeasureProc(Proc1, 0);
  MeasureProc(Proc2, 1);
  MeasureProc(Proc3, 2);
  MeasureProc(Proc4, 3);
  MeasureProc(Proc5, 4);
  Result := 0;
  MinVal := Counters[0];
  for i := 1 to 4 do
    if Counters[i] < MinVal then
    begin
      Result := i;
      MinVal := Counters[i];
    end;
end;

function MeasureFastestProc(ProcList: TProcedureArray): Integer; overload;
var
  i: Integer;
  MinVal: Int64;
begin
  if Length(ProcList) = 0 then
  begin
    Result := -1;
    Exit;
  end;
  for i := 0 to Length(ProcList)-1 do
    MeasureProc(ProcList[i], i);
  Result := 0;
  MinVal := Counters[0];
  for i := 1 to Length(ProcList)-1 do
    if Counters[i] < MinVal then
    begin
      Result := i;
      MinVal := Counters[i];
    end;
end;

function MeasureFastestCall(Proc: TProcedureInt; Count: Integer): Integer; overload;
var
  i: Integer;
  MinVal: Int64;
begin
  if Count <= 0 then
  begin
    Result := -1;
    Exit;
  end;
  for i := 0 to Count-1 do
    MeasureCall(Proc, i, i);
  Result := 0;
  MinVal := Counters[0];
  for i := 1 to Count-1 do
    if Counters[i] < MinVal then
    begin
      Result := i;
      MinVal := Counters[i];
    end;
end;

function MeasureSlowestProc(Proc1, Proc2: TProcedure): Integer; overload;
begin
  MeasureProc(Proc1, 0);
  MeasureProc(Proc2, 1);
  if Counters[0] > Counters[1] then Result := 0
  else Result := 1;
end;

function MeasureSlowestProc(Proc1, Proc2, Proc3: TProcedure): Integer; overload;
var
  i: Integer;
  MaxVal: Int64;
begin
  MeasureProc(Proc1, 0);
  MeasureProc(Proc2, 1);
  MeasureProc(Proc3, 2);
  Result := 0;
  MaxVal := Counters[0];
  for i := 1 to 2 do
    if Counters[i] > MaxVal then
    begin
      Result := i;
      MaxVal := Counters[i];
    end;
end;

function MeasureSlowestProc(Proc1, Proc2, Proc3, Proc4: TProcedure): Integer; overload;
var
  i: Integer;
  MaxVal: Int64;
begin
  MeasureProc(Proc1, 0);
  MeasureProc(Proc2, 1);
  MeasureProc(Proc3, 2);
  MeasureProc(Proc4, 3);
  Result := 0;
  MaxVal := Counters[0];
  for i := 1 to 3 do
    if Counters[i] > MaxVal then
    begin
      Result := i;
      MaxVal := Counters[i];
    end;
end;

function MeasureSlowestProc(Proc1, Proc2, Proc3, Proc4, Proc5: TProcedure): Integer; overload;
var
  i: Integer;
  MaxVal: Int64;
begin
  MeasureProc(Proc1, 0);
  MeasureProc(Proc2, 1);
  MeasureProc(Proc3, 2);
  MeasureProc(Proc4, 3);
  MeasureProc(Proc5, 4);
  Result := 0;
  MaxVal := Counters[0];
  for i := 1 to 4 do
    if Counters[i] > MaxVal then
    begin
      Result := i;
      MaxVal := Counters[i];
    end;
end;

function MeasureSlowestProc(ProcList: TProcedureArray): Integer; overload;
var
  i: Integer;
  MaxVal: Int64;
begin
  if Length(ProcList) = 0 then
  begin
    Result := -1;
    Exit;
  end;
  for i := 0 to Length(ProcList)-1 do
    MeasureProc(ProcList[i], i);
  Result := 0;
  MaxVal := Counters[0];
  for i := 1 to Length(ProcList)-1 do
    if Counters[i] > MaxVal then
    begin
      Result := i;
      MaxVal := Counters[i];
    end;
end;

function MeasureSlowestCall(Proc: TProcedureInt; Count: Integer): Integer; overload;
var
  i: Integer;
  MaxVal: Int64;
begin
  if Count <= 0 then
  begin
    Result := -1;
    Exit;
  end;
  for i := 0 to Count-1 do
    MeasureCall(Proc, i, i);
  Result := 0;
  MaxVal := Counters[0];
  for i := 1 to Count-1 do
    if Counters[i] > MaxVal then
    begin
      Result := i;
      MaxVal := Counters[i];
    end;
end;

function MeasureToSec(Counter: Int64): Double;
begin
  Result := Counter / CounterFrequency;
end;

function MeasureToMillisec(Counter: Int64): Int64;
begin
  Result := Round(Counter * 1000.0 / CounterFrequency);
end;

function MeasureToMicrosec(Counter: Int64): Int64;
begin
  Result := Round(Counter * 1000000.0 / CounterFrequency);
end;

function MeasureFrequency: Int64;
begin
  Result := CounterFrequency;
end;

initialization
  QueryPerformanceFrequency(CounterFrequency);

end.
