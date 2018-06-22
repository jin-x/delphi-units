unit Measure7x;  { v1.10 (c) 2018 by Jin X (jin.x@sources.ru), http://xk7.ru/p/d/u }
// Модуль для измерения скорости процедур/методов и отдельных участков кода
// Для замера скорости используются высокоточные функции WinAPI QueryPerformanceCounter и QueryPerformanceFrequency, если они
// поддерживаются системой (в Windows XP и более старших версиях поддерживаются всегда), либо GetTickCount в противном случае

{$IF CompilerVersion >= 17} // Delphi 2005+
  {$DEFINE INLINE}
  {$DEFINE STATIC}
{$IFEND}

{$IF CompilerVersion >= 20} // Delphi 2009+
  {$DEFINE ANONYMMETHODS}
{$IFEND}

interface

{$IF CompilerVersion >= 23} // Delphi XE2+
uses System.Types, Winapi.Windows;
{$ELSE}
uses Types, Windows;
{$IFEND}

const
  // Версия модуля
  Measure7xVersion = 1.10;

type
  // Процедура без параметров
  TProcedure = procedure;
  // Процедура с 1-м параметром типа Integer
  TProcedureInt = procedure(N: Integer);

  // Объектный метода без параметров
  TObjectProc = procedure of object;
  // Объектный метода с 1-м параметром типа Integer
  TObjectProcInt = procedure(N: Integer) of object;

{$IFDEF ANONYMMETHODS}
  // Анонимный метод без параметров
  TReferenceProc = reference to procedure;
  // Анонимный метод с 1-м параметром типа Integer
  TReferenceProcInt = reference to procedure(N: Integer);
{$ENDIF} // {$IFDEF ANONYMMETHODS}

  // Класс замера скорости (с одним счётчиком)
  TMeasure = class
    protected
      FCounter: Int64;
    public
      // Начать отсчёт времени
      procedure Start; {$IFDEF INLINE}inline;{$ENDIF}
      // Остановить отсчёт времени (начатый/продолженный методом Start/Continue) и вернуть разницу между текущим и начальным значениями счётчика времени
      function Stop: Int64;
      // Продолжить прерванный методом Stop отсчёт времени и вернуть текущее значение счётчика времени
      function Continue: Int64;

      // Получить скорость работы процедуры или метода
      function Proc(Proc: TProcedure): Int64;
      function ObjProc(Proc: TObjectProc): Int64;
{$IFDEF ANONYMMETHODS}
      function RefProc(Proc: TReferenceProc): Int64;
{$ENDIF}
      // Получить скорость работы процедуры или метода с параметром N
      function ProcInt(Proc: TProcedureInt; N: Integer): Int64;
      function ObjProcInt(Proc: TObjectProcInt; N: Integer): Int64;
{$IFDEF ANONYMMETHODS}
      function RefProcInt(Proc: TReferenceProcInt; N: Integer): Int64;
{$ENDIF}

      // Получить значение счётчика (последний замер)
      // Если счётчик не был запущен, будет возвращено нулевое значение, если не было остановлен, будет возвращено значение времени старта
      property Last: Int64 read FCounter;
      // Получить значение счётчика времени (последний замер) в секундах, миллисекундах или микросекундах
      function Sec: Double; {$IFDEF INLINE}inline;{$ENDIF}
      function Millisec: Int64; {$IFDEF INLINE}inline;{$ENDIF}
      function Microsec: Int64; {$IFDEF INLINE}inline;{$ENDIF}
      // Перевести значение счётчика времени в секунды, миллисекунды или микросекунды
      class function ToSec(Counter: Int64): Double; {$IFDEF STATIC}static; inline;{$ENDIF}
      class function ToMillisec(Counter: Int64): Int64; {$IFDEF STATIC}static; inline;{$ENDIF}
      class function ToMicrosec(Counter: Int64): Int64; {$IFDEF STATIC}static; inline;{$ENDIF}
      // Получить текущее значение системного счётчика
      class function Counter: Int64; {$IFDEF STATIC}static; inline;{$ENDIF}
      // Получить частоту счётчика (кол-во отсчётов в секунду)
      class function Frequency: Int64; {$IFDEF STATIC}static; inline;{$ENDIF}
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Начать отсчёт времени, используя счётчик номер Idx или пользовательский счётчик Counter
// Если отсчёт уже начат, он будет перезапущен заново
// Если Idx < 0, процедура завершится без запуска отсчёта времени
procedure StartMeasure(Idx: Integer = 0);
procedure StartMeasureX(var Counter: Int64); {$IFDEF INLINE}inline;{$ENDIF}

// Остановить отсчёт времени (начатый/продолженный функцией StartMeasure[X]/ContinueMeasure[X]), используя счётчик номер Idx или пользовательский счётчик Counter
// Возвращает разницу между текущим и начальным значениями счётчика времени
// Если счётчика не существует (либо Idx < 0), будет возвращено значение -1
function StopMeasure(Idx: Integer = 0): Int64;
function StopMeasureX(var Counter: Int64): Int64;

// Продолжить прерванный функцией StopMeasure[X] отсчёт времени, используя счётчик номер Idx или пользовательский счётчик Counter
// Возвращает текущее значение счётчика времени
// Если счётчика не существует (либо Idx < 0), будет возвращено значение -1
function ContinueMeasure(Idx: Integer = 0): Int64;
function ContinueMeasureX(var Counter: Int64): Int64;

// Получить значение счётчика времени номер Idx (последний замер)
// Если счётчик не был остановлен, будет возвращено значение времени старта
// Если счётчика не существует (либо Idx < 0), будет возвращено значение -1
function LastMeasure(Idx: Integer = 0): Int64;

// Получить значение счётчика времени номер Idx (последний замер) в секундах, миллисекундах или микросекундах
function LastMeasureSec(Idx: Integer = 0): Double; {$IFDEF INLINE}inline;{$ENDIF}
function LastMeasureMillisec(Idx: Integer = 0): Int64; {$IFDEF INLINE}inline;{$ENDIF}
function LastMeasureMicrosec(Idx: Integer = 0): Int64; {$IFDEF INLINE}inline;{$ENDIF}

// Получить скорость работы процедуры или метода, используя счётчик номер Idx или пользовательский счётчик Counter
// Если Idx < 0, будет возвращено значение -1, но процедура/метода Proc всё равно будет выполнен(а)
function MeasureProc(Proc: TProcedure; Idx: Integer = 0): Int64;
function MeasureProcX(Proc: TProcedure; var Counter: Int64): Int64;
function MeasureObjProc(Proc: TObjectProc; Idx: Integer = 0): Int64;
function MeasureObjProcX(Proc: TObjectProc; var Counter: Int64): Int64;
{$IFDEF ANONYMMETHODS}
function MeasureRefProc(Proc: TReferenceProc; Idx: Integer = 0): Int64;
function MeasureRefProcX(Proc: TReferenceProc; var Counter: Int64): Int64;
{$ENDIF}

// Получить скорость работы процедуры или метода с параметром N, используя счётчик номер Idx (по умолчанию 0, а не N !!!) или пользовательский счётчик Counter
// Если Idx < 0, будет возвращено значение -1, но процедура/метод Proc всё равно будет выполнен(а)
function MeasureProcInt(Proc: TProcedureInt; N: Integer; Idx: Integer = 0): Int64;
function MeasureProcIntX(Proc: TProcedureInt; N: Integer; var Counter: Int64): Int64;
function MeasureObjProcInt(Proc: TObjectProcInt; N: Integer; Idx: Integer = 0): Int64;
function MeasureObjProcIntX(Proc: TObjectProcInt; N: Integer; var Counter: Int64): Int64;
{$IFDEF ANONYMMETHODS}
function MeasureRefProcInt(Proc: TReferenceProcInt; N: Integer; Idx: Integer = 0): Int64;
function MeasureRefProcIntX(Proc: TReferenceProcInt; N: Integer; var Counter: Int64): Int64;
{$ENDIF}

// Замерить скорость работы нескольких процедур или методов
// Значения счётчиков времени можно получить с помощью функции LastMeasure, указав в качестве параметра номер процедуры/метода
procedure MeasureProcs(const ProcList: array of TProcedure);
procedure MeasureObjProcs(const ProcList: array of TObjectProc);
{$IFDEF ANONYMMETHODS}
procedure MeasureRefProcs(const ProcList: array of TReferenceProc);
{$ENDIF}

// Замерить скорость работы процедуры или метода со значениями параметра от 0 до Count-1
// Значения счётчиков времени можно получить с помощью функции LastMeasure, указав в качестве параметра значение от 0 до Count-1
procedure MeasureProcCalls(Proc: TProcedureInt; Count: Integer);
procedure MeasureObjProcCalls(Proc: TObjectProcInt; Count: Integer);
{$IFDEF ANONYMMETHODS}
procedure MeasureRefProcCalls(Proc: TReferenceProcInt; Count: Integer);
{$ENDIF}

// Сравнить скорость работы нескольких процедур или методов и вернуть номер (индекс массива) самой быстрой из них
// Если массив пуст, результатом будет значение -1
// Значения счётчиков времени можно получить с помощью функции LastMeasure, указав в качестве параметра номер процедуры/метода
function MeasureFastestProc(const ProcList: array of TProcedure): Integer;
function MeasureFastestObjProc(const ProcList: array of TObjectProc): Integer;
{$IFDEF ANONYMMETHODS}
function MeasureFastestRefProc(const ProcList: array of TReferenceProc): Integer;
{$ENDIF}

// Сравнить скорость работы процедуры или метода со значениями параметра от 0 до Count-1 и вернуть номер самого быстрого вызова (начиная с 0)
// Если Count <= 0, будет возвращено значение -1
// Значения счётчиков времени можно получить с помощью функции LastMeasure, указав в качестве параметра значение от 0 до Count-1
function MeasureFastestProcCalls(Proc: TProcedureInt; Count: Integer): Integer;
function MeasureFastestObjProcCalls(Proc: TObjectProcInt; Count: Integer): Integer;
{$IFDEF ANONYMMETHODS}
function MeasureFastestRefProcCalls(Proc: TReferenceProcInt; Count: Integer): Integer;
{$ENDIF}

// Сравнить скорость работы нескольких процедур или методов и вернуть номер (индекс массива) самой медленной из них
// Если массив пуст, результатом будет значение -1
// Значения счётчиков времени можно получить с помощью функции LastMeasure, указав в качестве параметра номер процедуры/метода
function MeasureSlowestProc(const ProcList: array of TProcedure): Integer;
function MeasureSlowestObjProc(const ProcList: array of TObjectProc): Integer;
{$IFDEF ANONYMMETHODS}
function MeasureSlowestRefProc(const ProcList: array of TReferenceProc): Integer;
{$ENDIF}

// Сравнить скорость работы процедуры или метода со значениями параметра от 0 до Count-1 и вернуть номер самого медленного вызова (начиная с 0)
// Если Count <= 0, будет возвращено значение -1
// Значения счётчиков времени можно получить с помощью функции LastMeasure, указав в качестве параметра значение от 0 до Count-1
function MeasureSlowestProcCalls(Proc: TProcedureInt; Count: Integer): Integer;
function MeasureSlowestObjProcCalls(Proc: TObjectProcInt; Count: Integer): Integer;
{$IFDEF ANONYMMETHODS}
function MeasureSlowestRefProcCalls(Proc: TReferenceProcInt; Count: Integer): Integer;
{$ENDIF}

// Сравнить скорость работы двух процедур или методов с учётом погрешности и вернуть результат сравнения (см. описание функции CompareMeasures)
// Значения счётчиков времени можно получить с помощью функции LastMeasure, указав в качестве параметра номер процедуры/метода (0 или 1)
function MeasureAndCompareProcs(Proc1, Proc2: TProcedure; RelError: Double = 0.01): TValueRelationship;
function MeasureAndCompareObjProcs(Proc1, Proc2: TObjectProc; RelError: Double = 0.01): TValueRelationship;
{$IFDEF ANONYMMETHODS}
function MeasureAndCompareRefProcs(Proc1, Proc2: TReferenceProc; RelError: Double = 0.01): TValueRelationship;
{$ENDIF} // {$IFDEF ANONYMMETHODS}

// Вернуть номер счётчика с минимальным значением среди первых Count счётчиков (с номерами от 0 до Count-1)
// Если существующих счётчиков меньше, чем Count, будет возвращён результат сравнения существующих счётчиков
// Если Count <= 0 либо не существует ни одного счётчика, будет возвращено значение -1
// !!! Помните, что все счётчики объединены в обычный массив, поэтому, например, при создании счётчиков №0 и №2 будет создан и счётчик №1, но с нулевым
// значением, поэтому при сравнении 3-х счётчиков функция вернёт в качестве результата 1, т.к. этот счётчик имеет минимальное (нулевое) значение
// (разумеется, если только счётчик №0 не имеет тоже нулевое значение) !!!
function FastestMeasure(Count: Integer): Integer;
// Вернуть номер счётчика с максимальным значением среди первых Count счётчиков (с номерами от 0 до Count-1)
// Если существующих счётчиков меньше, чем Count, будет возвращён результат сравнения существующих счётчиков
// Если Count <= 0 либо не существует ни одного счётчика, будет возвращено значение -1
function SlowestMeasure(Count: Integer): Integer;

// Сравнить два значения времени с учётом погрешности и вернуть результат сравнения:
// -1, если A < B (A быстрее); 0, если A примерно равно B; 1, A > B (B быстрее)
// RelError указывает долю допустимой разницы (от среднего значения A и B) до которой измерения считаются одинаковыми (0.01 = 1%)
function CompareMeasures(A, B: Int64; RelError: Double = 0.01): TValueRelationship;

// Перевести значение счётчика времени в секунды, миллисекунды или микросекунды
function MeasureToSec(Counter: Int64): Double;
function MeasureToMillisec(Counter: Int64): Int64;
function MeasureToMicrosec(Counter: Int64): Int64;

// Сбросить (удалить) все счётчики
// Эту процедуру можно использовать после завершения всех измерений для экономии памяти, выделенного под массив счётчиков
// Тем не менее, сброс ни коим образом не ограничивает повторное выполнение измерений
procedure ResetMeasures;

// Получить текущее значение системного счётчика
function MeasureCounter: Int64;

// Получить частоту счётчика (кол-во отсчётов в секунду)
// Если высокоточные функции WinAPI QueryPerformanceCounter и QueryPerformanceFrequency не поддерживаются системой, будет возвращено значение 1000
function MeasureFrequency: Int64;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

implementation

var
  Counters: array of Int64;
  Frequency: Int64;
  LPCounterOnly: Boolean;

////////////////////
// Класс TMeasure //
////////////////////

procedure TMeasure.Start;
begin
  FCounter := MeasureCounter;
end;

function TMeasure.Stop: Int64;
begin
  Result := MeasureCounter - FCounter;
  FCounter := Result;
end;

function TMeasure.Continue: Int64;
begin
  Result := FCounter;
  FCounter := MeasureCounter - Result;
end;

function TMeasure.Proc(Proc: TProcedure): Int64;
begin
  Start;
  Proc;
  Result := Stop;
end;

function TMeasure.ObjProc(Proc: TObjectProc): Int64;
begin
  Start;
  Proc;
  Result := Stop;
end;

{$IFDEF ANONYMMETHODS}
function TMeasure.RefProc(Proc: TReferenceProc): Int64;
begin
  Start;
  Proc;
  Result := Stop;
end;
{$ENDIF} // {$IFDEF ANONYMMETHODS}

function TMeasure.ProcInt(Proc: TProcedureInt; N: Integer): Int64;
begin
  Start;
  Proc(N);
  Result := Stop;
end;

function TMeasure.ObjProcInt(Proc: TObjectProcInt; N: Integer): Int64;
begin
  Start;
  Proc(N);
  Result := Stop;
end;

{$IFDEF ANONYMMETHODS}
function TMeasure.RefProcInt(Proc: TReferenceProcInt; N: Integer): Int64;
begin
  Start;
  Proc(N);
  Result := Stop;
end;
{$ENDIF} // {$IFDEF ANONYMMETHODS}

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

class function TMeasure.ToSec(Counter: Int64): Double;
begin
  Result := MeasureToSec(Counter);
end;

class function TMeasure.ToMillisec(Counter: Int64): Int64;
begin
  Result := MeasureToMillisec(Counter);
end;

class function TMeasure.ToMicrosec(Counter: Int64): Int64;
begin
  Result := MeasureToMicrosec(Counter);
end;

class function TMeasure.Counter: Int64;
begin
  Result := MeasureCounter;
end;

class function TMeasure.Frequency: Int64;
begin
  Result := MeasureFrequency;
end;

///////////////////////////////
// Общие процедуры и функции //
///////////////////////////////

procedure StartMeasure(Idx: Integer = 0);
var OldLen, i: Integer;
begin
  if Idx < 0 then Exit;
  OldLen := Length(Counters);
  if Idx > OldLen-1 then
  begin
    SetLength(Counters, Idx+1);
    for i := OldLen to Idx-1 do
      Counters[i] := -1;
  end;
  Counters[Idx] := MeasureCounter;
end;

procedure StartMeasureX(var Counter: Int64);
begin
  Counter := MeasureCounter;
end;

function StopMeasure(Idx: Integer = 0): Int64;
begin
  if (Idx < 0) or (Idx > High(Counters)) then
  begin
    Result := -1;
    Exit;
  end;
  Result := MeasureCounter - Counters[Idx];
  Counters[Idx] := Result;
end;

function StopMeasureX(var Counter: Int64): Int64;
begin
  Result := MeasureCounter - Counter;
  Counter := Result;
end;

function ContinueMeasure(Idx: Integer = 0): Int64;
begin
  if (Idx < 0) or (Idx > High(Counters)) then
  begin
    Result := -1;
    Exit;
  end;
  Result := Counters[Idx];
  Counters[Idx] := MeasureCounter - Result;
end;

function ContinueMeasureX(var Counter: Int64): Int64;
begin
  Result := Counter;
  Counter := MeasureCounter - Result;
end;

function LastMeasure(Idx: Integer = 0): Int64;
begin
  if (Idx < 0) or (Idx > High(Counters)) then
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

function MeasureProcX(Proc: TProcedure; var Counter: Int64): Int64;
begin
  StartMeasure(Counter);
  Proc;
  Result := StopMeasure(Counter);
end;

function MeasureObjProc(Proc: TObjectProc; Idx: Integer = 0): Int64;
begin
  StartMeasure(Idx);
  Proc;
  Result := StopMeasure(Idx);
end;

function MeasureObjProcX(Proc: TObjectProc; var Counter: Int64): Int64;
begin
  StartMeasure(Counter);
  Proc;
  Result := StopMeasure(Counter);
end;

{$IFDEF ANONYMMETHODS}
function MeasureRefProc(Proc: TReferenceProc; Idx: Integer = 0): Int64;
begin
  StartMeasure(Idx);
  Proc;
  Result := StopMeasure(Idx);
end;

function MeasureRefProcX(Proc: TReferenceProc; var Counter: Int64): Int64;
begin
  StartMeasure(Counter);
  Proc;
  Result := StopMeasure(Counter);
end;
{$ENDIF} // {$IFDEF ANONYMMETHODS}

function MeasureProcInt(Proc: TProcedureInt; N: Integer; Idx: Integer = 0): Int64;
begin
  StartMeasure(Idx);
  Proc(N);
  Result := StopMeasure(Idx);
end;

function MeasureProcIntX(Proc: TProcedureInt; N: Integer; var Counter: Int64): Int64;
begin
  StartMeasure(Counter);
  Proc(N);
  Result := StopMeasure(Counter);
end;

function MeasureObjProcInt(Proc: TObjectProcInt; N: Integer; Idx: Integer = 0): Int64;
begin
  StartMeasure(Idx);
  Proc(N);
  Result := StopMeasure(Idx);
end;

function MeasureObjProcIntX(Proc: TObjectProcInt; N: Integer; var Counter: Int64): Int64;
begin
  StartMeasure(Counter);
  Proc(N);
  Result := StopMeasure(Counter);
end;

{$IFDEF ANONYMMETHODS}
function MeasureRefProcInt(Proc: TReferenceProcInt; N: Integer; Idx: Integer = 0): Int64;
begin
  StartMeasure(Idx);
  Proc(N);
  Result := StopMeasure(Idx);
end;

function MeasureRefProcIntX(Proc: TReferenceProcInt; N: Integer; var Counter: Int64): Int64;
begin
  StartMeasure(Counter);
  Proc(N);
  Result := StopMeasure(Counter);
end;
{$ENDIF} // {$IFDEF ANONYMMETHODS}

procedure MeasureProcs(const ProcList: array of TProcedure);
var i: Integer;
begin
  for i := 0 to High(ProcList) do
    MeasureProc(ProcList[i], i);
end;

procedure MeasureObjProcs(const ProcList: array of TObjectProc);
var i: Integer;
begin
  for i := 0 to High(ProcList) do
    MeasureObjProc(ProcList[i], i);
end;

{$IFDEF ANONYMMETHODS}
procedure MeasureRefProcs(const ProcList: array of TReferenceProc);
var i: Integer;
begin
  for i := 0 to High(ProcList) do
    MeasureRefProc(ProcList[i], i);
end;
{$ENDIF} // {$IFDEF ANONYMMETHODS}

procedure MeasureProcCalls(Proc: TProcedureInt; Count: Integer);
var i: Integer;
begin
  for i := 0 to Count-1 do
    MeasureProcInt(Proc, i, i);
end;

procedure MeasureObjProcCalls(Proc: TObjectProcInt; Count: Integer);
var i: Integer;
begin
  for i := 0 to Count-1 do
    MeasureObjProcInt(Proc, i, i);
end;

{$IFDEF ANONYMMETHODS}
procedure MeasureRefProcCalls(Proc: TReferenceProcInt; Count: Integer);
var i: Integer;
begin
  for i := 0 to Count-1 do
    MeasureRefProcInt(Proc, i, i);
end;
{$ENDIF} // {$IFDEF ANONYMMETHODS}

function MeasureFastestProc(const ProcList: array of TProcedure): Integer;
begin
  MeasureProcs(ProcList);
  Result := FastestMeasure(Length(ProcList));
end;

function MeasureFastestObjProc(const ProcList: array of TObjectProc): Integer;
begin
  MeasureObjProcs(ProcList);
  Result := FastestMeasure(Length(ProcList));
end;

{$IFDEF ANONYMMETHODS}
function MeasureFastestRefProc(const ProcList: array of TReferenceProc): Integer;
begin
  MeasureRefProcs(ProcList);
  Result := FastestMeasure(Length(ProcList));
end;
{$ENDIF} // {$IFDEF ANONYMMETHODS}

function MeasureFastestProcCalls(Proc: TProcedureInt; Count: Integer): Integer;
begin
  MeasureProcCalls(Proc, Count);
  Result := FastestMeasure(Count);
end;

function MeasureFastestObjProcCalls(Proc: TObjectProcInt; Count: Integer): Integer;
begin
  MeasureObjProcCalls(Proc, Count);
  Result := FastestMeasure(Count);
end;

{$IFDEF ANONYMMETHODS}
function MeasureFastestRefProcCalls(Proc: TReferenceProcInt; Count: Integer): Integer;
begin
  MeasureRefProcCalls(Proc, Count);
  Result := FastestMeasure(Count);
end;
{$ENDIF} // {$IFDEF ANONYMMETHODS}

function MeasureSlowestProc(const ProcList: array of TProcedure): Integer;
begin
  MeasureProcs(ProcList);
  Result := SlowestMeasure(Length(ProcList));
end;

function MeasureSlowestObjProc(const ProcList: array of TObjectProc): Integer;
begin
  MeasureObjProcs(ProcList);
  Result := SlowestMeasure(Length(ProcList));
end;

{$IFDEF ANONYMMETHODS}
function MeasureSlowestRefProc(const ProcList: array of TReferenceProc): Integer;
begin
  MeasureRefProcs(ProcList);
  Result := SlowestMeasure(Length(ProcList));
end;
{$ENDIF} // {$IFDEF ANONYMMETHODS}

function MeasureSlowestProcCalls(Proc: TProcedureInt; Count: Integer): Integer;
begin
  MeasureProcCalls(Proc, Count);
  Result := SlowestMeasure(Count);
end;

function MeasureSlowestObjProcCalls(Proc: TObjectProcInt; Count: Integer): Integer;
begin
  MeasureObjProcCalls(Proc, Count);
  Result := SlowestMeasure(Count);
end;

{$IFDEF ANONYMMETHODS}
function MeasureSlowestRefProcCalls(Proc: TReferenceProcInt; Count: Integer): Integer;
begin
  MeasureRefProcCalls(Proc, Count);
  Result := SlowestMeasure(Count);
end;
{$ENDIF} // {$IFDEF ANONYMMETHODS}

function MeasureAndCompareProcs(Proc1, Proc2: TProcedure; RelError: Double = 0.01): TValueRelationship;
begin
  MeasureProc(Proc1, 0);
  MeasureProc(Proc2, 1);
  Result := CompareMeasures(Counters[0], Counters[1], RelError);
end;

function MeasureAndCompareObjProcs(Proc1, Proc2: TObjectProc; RelError: Double = 0.01): TValueRelationship;
begin
  MeasureObjProc(Proc1, 0);
  MeasureObjProc(Proc2, 1);
  Result := CompareMeasures(Counters[0], Counters[1], RelError);
end;

{$IFDEF ANONYMMETHODS}
function MeasureAndCompareRefProcs(Proc1, Proc2: TReferenceProc; RelError: Double = 0.01): TValueRelationship;
begin
  MeasureRefProc(Proc1, 0);
  MeasureRefProc(Proc2, 1);
  Result := CompareMeasures(Counters[0], Counters[1], RelError);
end;
{$ENDIF} // {$IFDEF ANONYMMETHODS}

{$WARNINGS OFF} // Иначе будет ругаться, что MinVal не инициализирован
function FastestMeasure(Count: Integer): Integer;
var
  i: Integer;
  Value, MinVal: Int64;
begin
  Result := -1;
  if Count > Length(Counters) then Count := Length(Counters);
  if Count <= 0 then Exit;
  for i := 1 to Count-1 do
  begin
    Value := Counters[i];
    if (Value > 0) and ((Result < 0) or (Value < MinVal)) then
    begin
      Result := i;
      MinVal := Value;
    end;
  end;
end;
{$WARNINGS ON}

function SlowestMeasure(Count: Integer): Integer;
var
  i: Integer;
  Value, MinVal: Int64;
begin
  Result := -1;
  if Count > Length(Counters) then Count := Length(Counters);
  if Count <= 0 then Exit;
  MinVal := -1;
  for i := 1 to Count-1 do
  begin
    Value := Counters[i];
    if Value > MinVal then
    begin
      Result := i;
      MinVal := Value;
    end;
  end;
end;

function CompareMeasures(A, B: Int64; RelError: Double = 0.01): TValueRelationship;
begin
  RelError := (A+B)/2*RelError;
  if Abs(A-B) <= RelError then Result := 0
  else if A < B then Result := -1
  else Result := 1;
end;

function MeasureToSec(Counter: Int64): Double;
begin
  Result := Counter / MeasureFrequency;
end;

function MeasureToMillisec(Counter: Int64): Int64;
begin
  Result := Round(Counter * 1000.0 / MeasureFrequency);
end;

function MeasureToMicrosec(Counter: Int64): Int64;
begin
  Result := Round(Counter * 1000000.0 / MeasureFrequency);
end;

procedure ResetMeasures;
begin
  SetLength(Counters, 0);
end;

function MeasureCounter: Int64;
begin
  if not LPCounterOnly then LPCounterOnly := not QueryPerformanceCounter(Result);
  if LPCounterOnly then Result := GetTickCount;
end;

function MeasureFrequency: Int64;
begin
  if Frequency = 0 then
  begin
    if not LPCounterOnly then LPCounterOnly := not QueryPerformanceFrequency(Frequency);
    if LPCounterOnly then Frequency := 1000;
  end;
  Result := Frequency;
end;

end.
