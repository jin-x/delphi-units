///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//  This unit is written by Eugene P. Krasnikov (aka Jin X) (c) 22-sep-2003  //
//  Any offers and wishes about this unit send to e-mail: jin.x@sources.ru   //
//                                                                           //
//    ƒанный модуль написан  расниковым ≈.ѕ. (aka Jin X) (c) 22-сен-2003     //
//   Ћюбые предложени€ и пожелани€, касающиес€ данного модул€ направл€йте    //
//                        на e-mail: jin.x@sources.ru                        //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

unit CSDetect;

interface

type
  TCharset = (csASC, csWIN, csDOS, csKOI, csISO, csMAC);
  TChars = set of #128..#255;
  DWord = LongWord;

const
  Charsets = DWord(High(TCharset));
  CSName: array [TCharset] of String = ('ASC', 'WIN', 'DOS', 'KOI', 'ISO', 'MAC');
  Charset: array [1..Charsets*2] of TChars = (
    [#192..#223,#168], [#224..#255,#184],            {win}
    [#128..#159,#240], [#160..#175,#224..#239,#241], {dos}
    [#224..#255,#179], [#192..#223,#163],            {koi}
    [#176..#207,#240], [#208..#239,#241],            {iso}
    [#128..#159,#221], [#223..#254,#222]);           {mac}
    { заглавные }      { строчные }

function DetectCharsetFast(const S: String): TCharset;
{ ќпредел€ет кодировку на основе статистики попадани€ символов в ту или иную кодировку. }
{ ≈сли кол-во строчных букв в кодировке с максимальным кол-вом попавших символов меньше }
{ или  равно кол-ву заглавных, то берЄтс€ кодировка с предмаксимальным кол-вом попавших }
{ символов (при условии, что в ней кол-во заглавных букв меньше кол-ва строчных).       }

function DetectCharset(const S: String): TCharset;
{ Ѕолее точный (но и более медленный) способ определени€ кодировки.                     }
{ ‘ункци€ ведЄт статистику _слов_, написанных _полностью_ в той или иной кодировке.     }
{ ѕри этом каждое слово получает от 1 до 3 баллов по следующим правилам:                }
{  - слова, состо€щие из 1 буквы получают по 1 баллу;                                   }
{  - слова, написанные в смешанном регистре получают по 1 баллу;                        }
{  - слова, написанные полностью заглавными буквами получают по 2 балла;                }
{  - слова, написанные строчными буквами или с заглавной буквы получают по 3 балла.     }
{ ‘ункци€ возвращает кодировку с максимальным кол-вом баллов.  ≈сли максимальное кол-во }
{ баллов получили сразу несколько кодировок, вызываетс€ функци€ DetectCharsetFast. ≈сли }
{ данна€  функци€ возвращает одну  из  кодировок,  котора€  набрала максимальное кол-во }
{ баллов,  то  возвращаетс€  именно  эта кодировка, иначе - перва€ кодировка по списку, }
{ набравша€ максимальное кол-во баллов :)                                               }

implementation

function DetectCharsetFast(const S: String): TCharset;
var
  i, j, k, Idx, PreIdx, PreMax: DWord;
  AscOnly: Boolean;
  Cnt: array [1..High(Charset)] of DWord;
begin
  FillChar(Cnt, SizeOf(Cnt), 0);
  AscOnly := True;
  for i := 1 to Length(S) do
    if S[i] >= #128 then
    begin
      AscOnly := False;
      for j := 1 to High(Charset) do
        if S[i] in Charset[j] then Inc(Cnt[j])
    end;
  if AscOnly then Result := csASC
  else
  begin
    Idx := 0;
    for i := 1 to 2 do
    begin
      PreIdx := 0;
      PreMax := 0;
      for j := 1 to Charsets do
      begin
        k := Cnt[j*2-1]+Cnt[j*2];
        if (k > PreMax) and (j <> Idx) then
        begin
          PreIdx := j;
          PreMax := k
        end
      end;
      if i = 1 then Idx := PreIdx
    end;
    k := Idx;
    if (Cnt[Idx*2-1] >= Cnt[Idx*2]) and
       (PreIdx <> 0) and (Cnt[PreIdx*2-1] < Cnt[PreIdx*2]) then
      k := PreIdx;
    Result := TCharset(k)
  end
end;

function DetectCharset(const S: String): TCharset;
var
  i, j, Max: DWord;
  AscOnly: Boolean;
  Cnt: array [1..Charsets] of DWord;
  CS: TCharset;
  W: String;

 function CheckWord(const S: String; CBig, CSmall: TChars): Integer;
 var
   i: DWord;
   LastBig, Big, Small, Mixed: Boolean;
 begin
   Result := 0;
   if Length(S) = 1 then
     if S[1] in CBig + CSmall then Result := 1
     else
   else if Length(S) > 0 then
   begin
     Mixed := False;
     for i := 1 to Length(S) do
     begin
       LastBig := Big;
       Big := S[i] in CBig;
       Small := S[i] in CSmall;
       if (not Big) and (not Small) then Exit;
       if ((Big and not LastBig)) or ((i > 2) and (Small and LastBig)) then
         Mixed := True
     end;
     if Mixed then Result := 1
     else if Big then Result := 2
     else Result := 3
   end
 end;

 procedure CheckWordAll(const S: String);
 var i: DWord;
 begin
   for i := 1 to Charsets do
     Inc(Cnt[i], CheckWord(S, Charset[i*2-1], Charset[i*2]))
 end;

begin
  FillChar(Cnt, SizeOf(Cnt), 0);
  AscOnly := True;
  W := '';
  for i := 1 to Length(S) do
  begin
    if S[i] >= #128 then
    begin
      AscOnly := False;
      W := W + S[i]
    end
    else
    begin
      CheckWordAll(W);
      W := ''
    end
  end;
  CheckWordAll(W);
  if AscOnly then Result := csAsc
  else
  begin
    Max := 0;
    for i := 1 to Charsets do
      if Max < Cnt[i] then
      begin
        Result := TCharset(i);
        Max := Cnt[i]
      end;
    j := 0;
    for i := 1 to Charsets do
      if Max = Cnt[i] then Inc(j);
    if j > 1 then
    begin
      CS := DetectCharsetFast(S);
      if Max = Cnt[Ord(CS)] then Result := CS
    end
  end
end;

end.
