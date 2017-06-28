%%%===================================================================
%% @author Erdem Aksu
%% @copyright 2017 Pundun Labs AB
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
%% implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% -------------------------------------------------------------------
%% @doc
%% Unicode Text Segmentation
%% http://www.unicode.org/reports/tr29
%% @end
%%%===================================================================


-module(term_prep_uts).

-export([word_boundaries/1]).

-spec word_boundaries(unicode:chardata()) ->
    [unicode:chardata()].
word_boundaries(Chardata) when is_binary(Chardata) -> %% WB1
    wb(binary_to_list(Chardata), [], [], undefined);
word_boundaries(Chardata) when is_list(Chardata) -> %% WB1
    wb(Chardata, [], [], undefined). 
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
wb([], Word, Acc, _) -> %% WB2
    lists:reverse(acc_word(Word, Acc));
wb([13, 10 | Rest], Word,  Acc, _) -> %%WB3
    wb(Rest, [], acc_word(Word, Acc), 'LF');
wb([16#000A | Rest], Word,  Acc, _) -> %%WB3a, WB3b
    wb(Rest, [], acc_word(Word, Acc), 'LF');
wb([16#000B | Rest], Word,  Acc, _) -> %%WB3a, WB3b
    wb(Rest, [], acc_word(Word, Acc), 'Newline');
wb([16#000C | Rest], Word,  Acc, _) -> %%WB3a, WB3b
    wb(Rest, [], acc_word(Word, Acc), 'Newline');
wb([16#000D | Rest], Word,  Acc, _) -> %%WB3a, WB3b
    wb(Rest, [], acc_word(Word, Acc), 'Newline');
wb([16#0085 | Rest], Word,  Acc, _) -> %%WB3a, WB3b
    wb(Rest, [], acc_word(Word, Acc), 'Newline');
wb([16#2028 | Rest], Word,  Acc, _) -> %%WB3a, WB3b
    wb(Rest, [], acc_word(Word, Acc), 'Newline');
wb([16#2029 | Rest], Word,  Acc, _) -> %%WB3a, WB3b
    wb(Rest, [], acc_word(Word, Acc), 'Newline');
wb([A | Rest], Word,  Acc, undefined) -> %% first char after boundary
    case term_prep_uwb_lib:prop(A) of
	undefined ->
	    wb(Rest, Word, Acc, undefined);
	WB4 when WB4 == 'Extend' orelse
		 WB4 == 'Format' orelse
		 WB4 == 'ZWJ' ->
	    wb(Rest, Word, Acc, WB4);
	Next ->
	    wb(Rest, [A | Word], Acc, Next)
    end;
wb([A], Word, Acc, Last) ->
    case {Last, term_prep_uwb_lib:prop(A)} of
	{_, undefined} ->
	    wb([], Word, Acc, undefined);
	{_, WB4} when WB4 == 'Extend' orelse
		      WB4 == 'Format' orelse
		      WB4 == 'ZWJ' ->
	    wb([], Word, Acc, WB4);
	{_, Else} ->
	    wb([], [A | Word], Acc, Else)
    end;
wb([A | Rest], Word, Acc, Last) -> 
    case {Last, term_prep_uwb_lib:prop(A)} of
	{'ZWJ', 'Glue_After_Zwj'} -> %%WB3c
	    wb(Rest, [A | Word], Acc, 'Glue_After_Zwj');
	{'ZWJ', 'E_Base_GAZ'} -> %%WB3c
	    wb(Rest, [A | Word], Acc, 'E_Base_GAZ');
	{_, WB4} when WB4 == 'Extend' orelse
	              WB4 == 'Format' orelse
		      WB4 == 'ZWJ' -> %%WB4
	    wb(Rest, Word, Acc, WB4);
	{'ALetter', 'ALetter'} -> %%WB5
	    wb(Rest, [A | Word], Acc, 'ALetter');
	{'ALetter', 'Hebrew_Letter'} -> %%WB5
	    wb(Rest, [A | Word], Acc, 'Hebrew_Letter');
	{'Hebrew_Letter', 'ALetter'} -> %%WB5
	    wb(Rest, [A | Word], Acc, 'ALetter');
	{'Hebrew_Letter', 'Hebrew_Letter'} -> %%WB5
	    wb(Rest, [A | Word], Acc, 'Hebrew_Letter');
	{AHLetter, WB6} when ((AHLetter == 'ALetter' orelse
			       AHLetter == 'Hebrew_Letter') andalso
			      (WB6 == 'MidLetter' orelse
			       WB6 == 'MidNumLet' orelse
			       WB6 == 'Single_Quote')) ->
	    [H|T] = Rest,
	    case term_prep_uwb_lib:prop(H) of
		'ALetter' -> %%WB6/7
		    wb(T, [H, A | Word], Acc, 'ALetter');
		'Hebrew_Letter' -> %%WB6/7
		    wb(T, [H, A | Word], Acc, 'Hebrew_Letter');
		_ ->
		    wb(Rest, [], acc_word(Word, Acc), WB6)
	    end;
	{'Hebrew_Letter', 'Single_Quote'} -> %%WB7a
	    wb(Rest, [A | Word], Acc, 'Single_Quote');
	{'Hebrew_Letter', 'Double_Quote'} ->
	    [H|T] = Rest,
	    case term_prep_uwb_lib:prop(H) of
		'Hebrew_Letter' ->%%WB7b/c
		    wb(T, [H, A | Word], Acc, 'Hebrew_Letter');
		_ ->
		    wb(Rest, [], acc_word(Word, Acc), 'Double_Quote')
	    end;
	{'Numeric', 'Numeric'} -> %WB8
	    wb(Rest, [A | Word], Acc, 'Numeric');
	{AHLetter, 'Numeric'} when AHLetter == 'ALetter';
	                           AHLetter == 'Hebrew_Letter'-> %WB9
	    wb(Rest, [A | Word], Acc, 'Numeric');
	{'Numeric', AHLetter} when AHLetter == 'ALetter';
	                           AHLetter == 'Hebrew_Letter'-> %WB10
	    wb(Rest, [A | Word], Acc, AHLetter);
	{'Numeric', WB11} when WB11 == 'MidNum' orelse
	                       WB11 == 'MidNumLet' orelse
			       WB11 == 'Single_Quote' -> %%WB11/12
	    [H|T] = Rest,
	    case term_prep_uwb_lib:prop(H) of
		'Numeric' ->
		    wb(T, [H, A | Word], Acc, 'Numeric');
		_ ->
		    wb(Rest, [], acc_word(Word, Acc), WB11)
	    end;
	{'Katakana', 'Katakana'} -> %% WB13
	    wb(Rest, [A | Word], Acc, 'Katakana');
	{WB13a, 'ExtendNumLet'} when WB13a == 'ALetter' orelse
				     WB13a == 'Hebrew_Letter' orelse
				     WB13a == 'Numeric' orelse
				     WB13a == 'Katakana' orelse
				     WB13a == 'ExtendNumLet' -> %%WB13a
	    wb(Rest, [A | Word], Acc, 'ExtendNumLet');
	{'ExtendNumLet', WB13b} when WB13b == 'ALetter'orelse
				     WB13b == 'Hebrew_Letter'orelse
				     WB13b == 'Numeric' orelse
				     WB13b == 'Katakana' -> %%WB13b
	    wb(Rest, [A | Word], Acc, WB13b);
	{WB14, 'E_Modifier'} when WB14 == 'E_Base' orelse
				  WB14 == 'E_Base_GAZ' -> %%WB14
	    wb(Rest, [A | Word], Acc, 'E_Modifier');
	{sot, 'Regional_Indicator'} -> %%WB15	
	    wb(Rest, [A | Word], Acc, 'Regional_Indicator');
	{'Regional_Indicator', 'Regional_Indicator'} -> %%WB16
	    wb(Rest, [A | Word], Acc, 'Regional_Indicator');
	{_Last, _Current} ->
	    wb(Rest, [], acc_word(Word, Acc), undefined)
    end.

acc_word([], Acc) ->
    Acc;
acc_word(Word, Acc) ->
    [lists:reverse(Word) | Acc].
