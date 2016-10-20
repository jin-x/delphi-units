program DelphiVerExample;
{$APPTYPE CONSOLE}
// This is an example of "DelphiVerDef7x.inc" include using.
// It may contain superfluous define declarations but it's made for various possibilities illustration.

{$DEFINE DesignedForDelphi2005}  // This code is best designed for Delphi 2005.
{$DEFINE NeedDelphi7}            // You need at least Delphi 7 compiler version,
{$DEFINE NotAboveDelphiBerlin}   // ...but not above than Delphi 10.1 Berlin.
{$DEFINE NotForDelphi2009}       // You cannot compile this with Delphi 2009 (for example).
{$DEFINE TestWarnings}           // Show "not tested" warnings.
{$DEFINE TestedDelphi101}        // This code is well-tested with Delphi 10.1 Berlin,
{$DEFINE TestedDelphi2006}       // ...and with Delphi 2006,
{$DEFINE TestedDelphi2005}       // ...and with Delphi 2005,
{$DEFINE TestedDelphi7}          // ...and with Delphi 7.
{$DEFINE NeedWindows}            // This code can be compiled only for Windows.
{$DEFINE NeedAssembler}          // This code requires Assembler support.
{$DEFINE RecommendAnsi}          // It's recommended to use ANSI Strings defaults (not Unicode).
{$I DelphiVerDef7x.inc}
{$IFNDEF DelphiVerDef7x100}{$MESSAGE FATAL 'DelphiVerDef7x.inc file is not included or incompatible version !!!'}{$ENDIF}

//{$DEFINE ManualInput}  // Uncomment this if you want to use only manual input.
{$DEFINE DoSort}  // Comment this if you don't want to sort strings.

{$IFDEF UnitScopeNames}
uses System.SysUtils;
{$ELSE}
uses SysUtils;
{$ENDIF}

type
  TStrArr = array of String;
var
  StrArr: TStrArr;
  Count: Integer;
  Ticks: Int64;

function rdtsc: Int64;  // {$IFDEF Inline}inline;{$ENDIF}
asm
  rdtsc
end;  // rdtsc

procedure SortStrings(var S: TStrArr);

 procedure QSort(L, R: Integer);
 var
   i, j: Integer;
   M, T: String;
 begin
   i := L; j := R;
   M := S[(L+R) div 2];
   repeat
     while (S[i] < M) do Inc(i);
     while (M < S[j]) do Dec(j);
     if (i <= j) then
     begin
       T := S[i];
       S[i] := S[j];
       S[j] := T;
       Inc(i);
       Dec(j)
     end
   until (i > j);
   if (L < j) then QSort(L, j);
   if (i < R) then QSort(i, R);
 end;  // QSort

begin
  QSort(Low(S), High(S))
end;  // SortStrings

procedure SetNumbers;
var i: Integer;
begin
  for i := 0 to Count-1 do
    Insert(IntToStr(i+1)+'. ', StrArr[i], 1)
end;  // SetNumbers

procedure ProcessStrings(Sort: Boolean = False); {$IFDEF Inline}inline;{$ENDIF}
begin
  if Sort then SortStrings(StrArr);
  SetNumbers;
end;  // ProcessStrings

var
{$IFNDEF ForIn}i: Integer;{$ENDIF}
  S: String;
begin
{$IF Defined(DynArrayInit) and not Defined(ManualInput)}
  StrArr := TStrArr.Create('First', 'Second', 'Third', 'Fourth', 'Fifth', 'Sixth', 'Seventh');
  Count := Length(StrArr);
{$ELSE}
  WriteLn('Please enter some strings (finish by empty string):');
  repeat
    ReadLn(S);
    if S = '' then Break;
  {$IFDEF NewDynArrayOps}
    Insert(S, StrArr, Count);
  {$ENDIF}
    Inc(Count);
  {$IFDEF NoNewDynArrayOps}
    SetLength(StrArr, Count);
    StrArr[Count-1] := S
  {$ENDIF}
  until False;
{$IFEND DynArraysInit and not ManualInput}

  if Count = 0 then
  begin
    WriteLn('Nothing to do.');
    WriteLn('Good bye!');
    Exit
  end;

  Ticks := rdtsc;
  ProcessStrings{$IFDEF DoSort}(True){$ENDIF};
  Ticks := rdtsc-Ticks;
  
  WriteLn('Result (ready for ', Ticks, ' CPU cycles):');
{$IFDEF ForIn}
  for S in StrArr do WriteLn(S)
{$ELSE}
  for i := 0 to Count-1 do
    WriteLn(StrArr[i])
{$ENDIF}
end.
