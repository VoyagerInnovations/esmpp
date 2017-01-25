%%% Copyright (c) 2011 Aleksey Yeschenko <aleksey@yeschenko.com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

-module(gsm0338).

-export([from_utf8/1, to_utf8/1]).

%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------

-spec from_utf8(binary()) -> binary() | {error, binary(), RestData :: binary()}
                           | {incomplete, binary(), IncompleteSeq :: binary()}.
from_utf8(UTF8) ->
    case unicode:characters_to_list(UTF8, utf8) of
        CodePoints when is_list(CodePoints) ->
            codepoints_to_gsm(CodePoints, []);
        {error, CodePoints, RestData} ->
            {error, codepoints_to_gsm(CodePoints, []), RestData};
        {incomplete, CodePoints, IncompleteSeq} ->
            {incomplete, codepoints_to_gsm(CodePoints, []), IncompleteSeq}
    end.

-spec to_utf8(binary()) -> binary() | {error, binary(), RestData :: binary()}.
to_utf8(GSM) ->
    gsm_to_codepoints(GSM, []).

%% -------------------------------------------------------------------------
%% private functions
%% -------------------------------------------------------------------------

codepoints_to_gsm([], Acc) ->
    list_to_binary(lists:reverse(Acc));
codepoints_to_gsm([CP|Rest], Acc) ->
    case gsm(CP) of
        N when N =< 16#FF ->
            codepoints_to_gsm(Rest, [N|Acc]);
        N ->
            codepoints_to_gsm(Rest, [N rem 256,N div 256|Acc])
    end.

gsm_to_codepoints(<<>>, Acc) ->
    unicode:characters_to_binary(lists:reverse(Acc), utf8);
gsm_to_codepoints(<<C, _/binary>> = GSM, Acc) when C > 16#7F ->
    {error, gsm_to_codepoints(<<>>, Acc), GSM};
gsm_to_codepoints(<<16#1B, Ext, Rest/binary>>, Acc) ->
    case cp_ext(Ext) of
        undefined ->
            gsm_to_codepoints(<<Ext, Rest/binary>>, [cp(16#1B)|Acc]);
        CP ->
            gsm_to_codepoints(Rest, [CP|Acc])
    end;
gsm_to_codepoints(<<C, Rest/binary>>, Acc) ->
    gsm_to_codepoints(Rest, [cp(C)|Acc]).

gsm(16#0040) -> 16#00;
gsm(16#00A3) -> 16#01;
gsm(16#0024) -> 16#02;
gsm(16#00A5) -> 16#03;
gsm(16#00E8) -> 16#04;
gsm(16#00E9) -> 16#05;
gsm(16#00F9) -> 16#06;
gsm(16#00EC) -> 16#07;
gsm(16#00F2) -> 16#08;
gsm(16#00E7) -> 16#09;
gsm(16#00C7) -> 16#09;
gsm(16#000A) -> 16#0A;
gsm(16#00D8) -> 16#0B;
gsm(16#00F8) -> 16#0C;
gsm(16#000D) -> 16#0D;
gsm(16#00C5) -> 16#0E;
gsm(16#00E5) -> 16#0F;
gsm(16#0394) -> 16#10;
gsm(16#005F) -> 16#11;
gsm(16#03A6) -> 16#12;
gsm(16#0393) -> 16#13;
gsm(16#039B) -> 16#14;
gsm(16#03A9) -> 16#15;
gsm(16#03A0) -> 16#16;
gsm(16#03A8) -> 16#17;
gsm(16#03A3) -> 16#18;
gsm(16#0398) -> 16#19;
gsm(16#039E) -> 16#1A;
gsm(16#00A0) -> 16#1B;
gsm(16#000C) -> 16#1B0A;
gsm(16#005E) -> 16#1B14;
gsm(16#007B) -> 16#1B28;
gsm(16#007D) -> 16#1B29;
gsm(16#005C) -> 16#1B2F;
gsm(16#005B) -> 16#1B3C;
gsm(16#007E) -> 16#1B3D;
gsm(16#005D) -> 16#1B3E;
gsm(16#007C) -> 16#1B40;
gsm(16#20AC) -> 16#1B65;
gsm(16#00C6) -> 16#1C;
gsm(16#00E6) -> 16#1D;
gsm(16#00DF) -> 16#1E;
gsm(16#00C9) -> 16#1F;
gsm(CP) when CP >= 16#0020,
             CP =< 16#0023 -> CP;
gsm(16#00A4) -> 16#24;
gsm(CP) when CP >= 16#0025,
             CP =< 16#003F -> CP;
gsm(16#00A1) -> 16#40;
gsm(16#0391) -> 16#41;
gsm(16#0392) -> 16#42;
gsm(16#0395) -> 16#45;
gsm(16#0397) -> 16#48;
gsm(16#0399) -> 16#49;
gsm(16#039A) -> 16#4B;
gsm(16#039C) -> 16#4D;
gsm(16#039D) -> 16#4E;
gsm(16#039F) -> 16#4F;
gsm(16#03A1) -> 16#50;
gsm(16#03A4) -> 16#54;
gsm(16#03A7) -> 16#58;
gsm(16#03A5) -> 16#59;
gsm(16#0396) -> 16#5A;
gsm(CP) when CP >= 16#0041,
             CP =< 16#005A -> CP;
gsm(16#00C4) -> 16#5B;
gsm(16#00D6) -> 16#5C;
gsm(16#00D1) -> 16#5D;
gsm(16#00DC) -> 16#5E;
gsm(16#00A7) -> 16#5F;
gsm(16#00BF) -> 16#60;
gsm(CP) when CP >= 16#0061,
             CP =< 16#007A -> CP;
gsm(16#00E4) -> 16#7B;
gsm(16#00F6) -> 16#7C;
gsm(16#00F1) -> 16#7D;
gsm(16#00FC) -> 16#7E;
gsm(16#00E0) -> 16#7F;
gsm(_)       -> 16#3F. % '?'

cp(16#00) -> 16#0040;
cp(16#01) -> 16#00A3;
cp(16#02) -> 16#0024;
cp(16#03) -> 16#00A5;
cp(16#04) -> 16#00E8;
cp(16#05) -> 16#00E9;
cp(16#06) -> 16#00F9;
cp(16#07) -> 16#00EC;
cp(16#08) -> 16#00F2;
cp(16#09) -> 16#00E7;
cp(16#0A) -> 16#000A;
cp(16#0B) -> 16#00D8;
cp(16#0C) -> 16#00F8;
cp(16#0D) -> 16#000D;
cp(16#0E) -> 16#00C5;
cp(16#0F) -> 16#00E5;
cp(16#10) -> 16#0394;
cp(16#11) -> 16#005F;
cp(16#12) -> 16#03A6;
cp(16#13) -> 16#0393;
cp(16#14) -> 16#039B;
cp(16#15) -> 16#03A9;
cp(16#16) -> 16#03A0;
cp(16#17) -> 16#03A8;
cp(16#18) -> 16#03A3;
cp(16#19) -> 16#0398;
cp(16#1A) -> 16#039E;
cp(16#1B) -> 16#00A0;
cp(16#1C) -> 16#00C6;
cp(16#1D) -> 16#00E6;
cp(16#1E) -> 16#00DF;
cp(16#1F) -> 16#00C9;
cp(GSM) when GSM >= 16#20,
             GSM =< 16#23 -> GSM;
cp(16#24) -> 16#00A4;
cp(GSM) when GSM >= 16#25,
             GSM =< 16#3F -> GSM;
cp(16#40) -> 16#00A1;
cp(GSM) when GSM >= 16#41,
             GSM =< 16#5A -> GSM;
cp(16#5B) -> 16#00C4;
cp(16#5C) -> 16#00D6;
cp(16#5D) -> 16#00D1;
cp(16#5E) -> 16#00DC;
cp(16#5F) -> 16#00A7;
cp(16#60) -> 16#00BF;
cp(GSM) when GSM >= 16#61,
             GSM =< 16#7A -> GSM;
cp(16#7B) -> 16#00E4;
cp(16#7C) -> 16#00F6;
cp(16#7D) -> 16#00F1;
cp(16#7E) -> 16#00FC;
cp(16#7F) -> 16#00E0.

cp_ext(16#0A) -> 16#000C;
cp_ext(16#14) -> 16#005E;
cp_ext(16#28) -> 16#007B;
cp_ext(16#29) -> 16#007D;
cp_ext(16#2F) -> 16#005C;
cp_ext(16#3C) -> 16#005B;
cp_ext(16#3D) -> 16#007E;
cp_ext(16#3E) -> 16#005D;
cp_ext(16#40) -> 16#007C;
cp_ext(16#65) -> 16#20AC;
cp_ext(_)     -> undefined.
