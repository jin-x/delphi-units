program Measure7xDemo;
{$APPTYPE CONSOLE}

uses Measure7x,
{$IF CompilerVersion >= 23} // Delphi XE2+
  System.Types, System.Math, System.SysUtils, Winapi.Windows;
{$ELSE}
  Types, Math, SysUtils, Windows;
{$IFEND}

{$IF CompilerVersion >= 20} // Delphi 2009+
  {$DEFINE ANONYMMETHODS}
{$IFEND}

{$IF CompilerVersion >= 24} // Delphi XE3+
  {$DEFINE ATOMIC}
{$IFEND}

const
  SLEEP_FACTOR = 200;
  CYCLE_COUNT = 1000*1000*1000;
  IncDecCompareResult: array [TValueRelationship] of String = ('Inc is faster!', 'Speed of Inc and Dec is about the same!', 'Dec is faster!');
  SqrCompareResult: array [0..2] of String = ('X*X', 'Sqr', 'IntPower');
  SlowestResult: array [Boolean] of String = ('', ' (slowest)');

var
  X, Y: Integer;
  Z: Double;

procedure IncProc;
var i: Integer;
begin
  X := 1;
  for i := 1 to CYCLE_COUNT do
    Inc(X);
end;

procedure DecProc;
var i: Integer;
begin
  Y := CYCLE_COUNT;
  for i := 1 to CYCLE_COUNT do
    Dec(Y);
end;

procedure Sqr1;
var i: Integer;
begin
  for i := 1 to CYCLE_COUNT do
    Z := i*i;
end;

procedure Sqr2;
var i: Integer;
begin
  for i := 1 to CYCLE_COUNT do
    Z := Sqr(i);
end;

procedure Sqr3;
var i: Integer;
begin
  for i := 1 to CYCLE_COUNT do
    Z := IntPower(i, 2);
end;

procedure Delay(N: Integer);
begin
  Sleep(N*SLEEP_FACTOR);
end;

var Result, i: Integer;
begin
  with TMeasure.Create do
  begin
    WriteLn('Starting...');
    WriteLn(Format('Unit version: %f', [Measure7xVersion]));
    WriteLn(Format('Measure counter frequency: %.0n ticks/sec', [MeasureFrequency*1.0]));  // *1.0 для перевода в тип Double
    Start;

    WriteLn;
    WriteLn('Comparing Inc and Dec...');
    Result := MeasureAndCompareProcs(IncProc, DecProc);
    WriteLn(IncDecCompareResult[Result]);
    WriteLn(Format('Inc time: %.6f sec', [LastMeasureSec(0)]));
    WriteLn(Format('Dec time: %.6f sec', [LastMeasureSec(1)]));
  
    WriteLn;
    WriteLn('Comparing X*X, Sqr and IntPower...');
    Result := MeasureFastestProc([Sqr1, Sqr2, Sqr3]);
    WriteLn(Format('%s is faster', [SqrCompareResult[Result]]));
    WriteLn(Format('X*X time: %.6f sec', [LastMeasureSec(0)]));
    WriteLn(Format('Sqr time: %.6f sec', [LastMeasureSec(1)]));
    WriteLn(Format('IntPower time: %.6f sec', [LastMeasureSec(2)]));

{$IFDEF ANONYMMETHODS}
    WriteLn;
    WriteLn('Comparing simple and atomic increment (using anonymous procedures)...');
    X := 0; Y := 0;
    MeasureRefProcs([procedure
      var i: Integer;
      begin
        for i := 1 to CYCLE_COUNT do
          Inc(X);
      end,
      procedure
      var i: Integer;
      begin
        for i := 1 to CYCLE_COUNT do
        {$IFDEF ATOMIC}
          AtomicIncrement(Y);
        {$ELSE}
          InterlockedIncrement(Y);
        {$ENDIF}
      end]);
    WriteLn(Format('Simple increment time: %.6f sec', [LastMeasureSec(0)]));
    WriteLn(Format('Atomic increment time: %.6f sec (%fx slower)', [LastMeasureSec(1), LastMeasure(1)/LastMeasure(0)]));
{$ENDIF}

    WriteLn;
    WriteLn('Measuring Sleep function...');
    Result := MeasureSlowestProcCalls(Delay, 6);
    for i := 0 to 5 do
      WriteLn(Format('Sleep(%d) time: %.6f sec%s', [i*SLEEP_FACTOR, LastMeasureSec(i), SlowestResult[i = Result]]));

    WriteLn;
    WriteLn(Format('Total elapsed time: %.6f sec', [ToSec(Stop)]));
    WriteLn('Press enter to test Continue method...');
    ReadLn;

    WriteLn('Waiting for 1 sec...');
    Continue;
    Sleep(1000);
    Stop;
    WriteLn(Format('Total time including 1 sec delay: %.6f sec (%.0n millisec, %.0n microsec)', [Sec, Millisec*1.0, Microsec*1.0]));

    WriteLn;
    WriteLn('Press enter to quit...');
    ReadLn;
    Free;
  end;
end.
