unit Measure7x;  { v1.00 (c) 2018 by Jin X (jin.x@sources.ru), http://xk7.ru/p/d/u }
// Модуль для измерения скорости процедур и отдельных участков кода
// Для замера скорости используются высокоточные функции WinAPI QueryPerformanceCounter и QueryPerformanceFrequency

{$IF CompilerVersion >= 17} // Delphi 2005+
  {$DEFINE INLINE}
  {$DEFINE STATIC}
{$IFEND}

interface

type
  // Тип процедуры без параметров
  TProcedure = procedure;
  // Тип процедуры с 1-м параметром типа Integer
  TProcedureInt = procedure(N: Integer);
  // Массив процедур без параметров
  TProcedureArray = array of TProcedure;

  // Класс замера скорости с одним счётчиком
  TMeasure = class
    protected
      FCounter: Int64;
    public
      // Начать отсчёт времени
      procedure Start;
      // Остановить отсчёт времени и вернуть разницу между текущим и начальным значениями счётчика времени
      function Stop: Int64;
      // Получить скорость работы процедуры
      function MeasureProc(Proc: TProcedure): Int64;
      // Получить скорость работы процедуры с параметром N
      function MeasureCall(Proc: TProcedureInt; N: Integer): Int64;
      // Получить значение счётчика (последний замер)
      // Если счётчик не был запущен, будет возвращено нулевое значение, если не было остановлен, будет возвращено значение времени старта
      property Counter: Int64 read FCounter;
      // Получить значение счётчика (последний замер) в секундах
      function Sec: Double;
      // Получить значение счётчика (последний замер) в миллисекундах
      function Millisec: Int64;
      // Получить значение счётчика (последний замер) в микросекундах
      function Microsec: Int64;
      // Получить частоту счётчика (кол-во отсчётов в секунду)
      class function Frequency: Int64; {$IFDEF STATIC}static;{$ENDIF}
  end;

// Начать отсчёт времени, используя счётчик номер Idx или счётчик Counter
// Если Idx < 0, процедура завершится без запуска отсчёта времени
procedure StartMeasure(Idx: Integer = 0); overload;
procedure StartMeasure(var Counter: Int64); overload; {$IFDEF INLINE}inline;{$ENDIF}

// Остановить отсчёт времени, используя счётчик номер Idx или счётчик Counter
// Возвращает разницу между текущим и начальным значениями счётчика времени
// Если счётчика не существует (либо Idx < 0), будет возвращено значение -1
function StopMeasure(Idx: Integer = 0): Int64; overload;
function StopMeasure(var Counter: Int64): Int64; overload;

// Получить значение счётчика (последний замер), используя счётчик номер Idx
// Если счётчик не был остановлен, будет возвращено значение времени старта
// Если счётчика не существует (либо Idx < 0), будет возвращено значение -1
function LastMeasure(Idx: Integer = 0): Int64; overload;
// Получить значение счётчика (последний замер), используя счётчик номер Idx, в секундах, миллисекундах или микросекундах
function LastMeasureSec(Idx: Integer = 0): Double; {$IFDEF INLINE}inline;{$ENDIF}
function LastMeasureMillisec(Idx: Integer = 0): Int64; {$IFDEF INLINE}inline;{$ENDIF}
function LastMeasureMicrosec(Idx: Integer = 0): Int64; {$IFDEF INLINE}inline;{$ENDIF}

// Получить скорость работы процедуры, используя счётчик номер Idx
// Если Idx < 0, будет возвращено значение -1, но процедура Proc всё равно будет выполнена
function MeasureProc(Proc: TProcedure; Idx: Integer = 0): Int64;
// Получить скорость работы процедуры с параметром N, используя счётчик номер Idx (по умолчанию 0, а не N !!!)
// Если Idx < 0, будет возвращено значение -1, но процедура Proc всё равно будет выполнена
function MeasureCall(Proc: TProcedureInt; N: Integer; Idx: Integer = 0): Int64;

// Сравнить скорость работы 2-х, 3-х, 4-х или 5-и процедур и вернуть номер самой быстрой из них (от 0 до 4)
// Значение счётчика времени можно получить с помощью функции LastMeasure, указав в качестве параметра номер процедуры
function MeasureFastestProc(Proc1, Proc2: TProcedure): Integer; overload;
function MeasureFastestProc(Proc1, Proc2, Proc3: TProcedure): Integer; overload;
function MeasureFastestProc(Proc1, Proc2, Proc3, Proc4: TProcedure): Integer; overload;
function MeasureFastestProc(Proc1, Proc2, Proc3, Proc4, Proc5: TProcedure): Integer; overload;
// Сравнить скорость работы неограниченного кол-ва процедур и вернуть номер (индекс массива) самой быстрой из них
// Если массив пуст, результатом будет значение -1
// Значение счётчика времени можно получить с помощью функции LastMeasure, указав в качестве параметра номер процедуры
function MeasureFastestProc(ProcList: TProcedureArray): Integer; overload;
// Сравнить скорость работы процедуры со значениями параметра от 0 до Count-1 и вернуть номер самого быстрого вызова (начиная с 0)
// Если Count <= 0, будет возвращено значение -1
// Значение счётчика времени можно получить с помощью функции LastMeasure, указав в качестве параметра значение от 0 до Count-1
function MeasureFastestCall(Proc: TProcedureInt; Count: Integer): Integer;

// Сравнить скорость работы 2-х, 3-х, 4-х или 5-и процедур и вернуть номер самой медленной из них (от 0 до 4)
// Значение счётчика времени можно получить с помощью функции LastMeasure, указав в качестве параметра номер процедуры
function MeasureSlowestProc(Proc1, Proc2: TProcedure): Integer; overload;
function MeasureSlowestProc(Proc1, Proc2, Proc3: TProcedure): Integer; overload;
function MeasureSlowestProc(Proc1, Proc2, Proc3, Proc4: TProcedure): Integer; overload;
function MeasureSlowestProc(Proc1, Proc2, Proc3, Proc4, Proc5: TProcedure): Integer; overload;
// Сравнить скорость работы неограниченного кол-ва процедур и вернуть номер (индекс массива) самой медленной из них
// Если массив пуст, результатом будет значение -1
// Значение счётчика времени можно получить с помощью функции LastMeasure, указав в качестве параметра номер процедуры
function MeasureSlowestProc(ProcList: TProcedureArray): Integer; overload;
// Сравнить скорость работы процедуры со значениями параметра от 0 до Count-1 и вернуть номер самого медленного вызова (начиная с 0)
// Если Count <= 0, будет возвращено значение -1
// Значение счётчика времени можно получить с помощью функции LastMeasure, указав в качестве параметра значение от 0 до Count-1
function MeasureSlowestCall(Proc: TProcedureInt; Count: Integer): Integer;

// Перевести значение счётчика в секунды, миллисекунды или микросекунды
function MeasureToSec(Counter: Int64): Double;
function MeasureToMillisec(Counter: Int64): Int64;
function MeasureToMicrosec(Counter: Int64): Int64;

// Получить частоту счётчика (кол-во отсчётов в секунду)
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
// Класс TMeasure //
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
// Общие процедуры и функции //
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
