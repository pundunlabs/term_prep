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
%% Module Description:
%% @end
%%%===================================================================

-module(term_prep).

%% Exported functions
-export([init/0,
	 config/1,
	 analyze/1,
	 analyze/2]).

-include_lib("gb_log/include/gb_log.hrl").

-type char_filter() :: nfc | nfd | nfkc | nfkd |
		       {M :: module(), F :: atom(), A :: [any()]} |
		       undefined.

-type tokenizer() :: unicode_word_boundaries |
		     {M :: module(), F :: atom(), A :: [any()]} |
		     undefined.

-type token_transform() :: lowercase | uppercase | casefold |
			   {M :: module(), F :: atom(), A :: [any()]} |
			   undefined.

-type token_add() :: {M :: module(), F :: atom(), A :: [any()]}.

-type token_delete() :: english_stopwords |
			lucene_stopwords |
			wikipages_stopwords |
			{M :: module(), F :: atom(), A :: [any()]}.

-type token_stats() :: unique |
		       freqs |
		       positions |
		       undefined.

-type token_filter() :: #{transform := token_transform(),
			  add := [token_add()],
			  delete := [token_delete()],
			  stats := token_stats()} |
			undefined.

-type config() :: #{char_filter := char_filter(),
		    tokenizer := tokenizer(),
		    token_filter := token_filter()}.
%%%===================================================================
%%% API
%%%===================================================================
-spec init() ->
    ok | {error, Reason :: term()}.
init() ->
    ResL = term_prep_code_gen:init(),
    case lists:usort(ResL) of
	[ok] -> ok;
	Else ->
	    {error, [R || R <- Else, R =/= ok]}
    end.

-spec config(term()) ->
    config().
config(unicode_nfc_words) ->
    #{char_filter => nfc,
      tokenizer => unicode_word_boundaries,
      token_filter => #{transform => lowercase,
			add => [],
			delete => [english_stopwords],
			stats => unique}};
config(unicode_nfd_words) ->
    C = config(unicode_nfc_words),
    C#{char_filter => nfd};
config(unicode_nfkc_words) ->
    C = config(unicode_nfc_words),
    C#{char_filter => nfkc};
config(unicode_nfkd_words) ->
    C = config(unicode_nfc_words),
    C#{char_filter => nfkd};
config(wiki_analysis) ->
    #{char_filter => nfc,
      tokenizer => unicode_word_boundaries,
      token_filter => #{transform => lowercase,
			add => [],
			delete => [english_stopwords, wikipages_stopwords]}};
config(_) ->
    #{}.

-spec analyze(Data :: unicode:chardata()) ->
    [unicode:charlist()] |
    [{unicode:charlist(), Freq ::pos_integer()}] |
    [{unicode:charlist(), Freq ::pos_integer(), Pos :: integer()}] .
analyze(Data)  ->
    analyze(config(unicode_nfc_words), Data).

-spec analyze(Config :: term_prep:config(),
	      Data :: unicode:chardata()) ->
    [unicode:charlist()] |
    [{unicode:charlist(), Freq ::pos_integer()}] |
    [{unicode:charlist(), Freq ::pos_integer(), Pos :: integer()}] .
analyze(Config, Bin) when is_binary(Bin) ->
    analyze(Config, binary_to_list(Bin));
analyze(Config, Data) when is_map(Config) ->
    Normalized = normalize(maps:get(char_filter, Config, undefined), Data),
    Tokenized = tokenize(maps:get(tokenizer, Config, undefined), Normalized),
    token_filter(maps:get(token_filter, Config, undefined), Tokenized).

normalize(nfc, Data) ->
    unicode:characters_to_nfc_list(Data);
normalize(nfd, Data) ->
    unicode:characters_to_nfc_list(Data);
normalize(nfkc, Data) ->
    unicode:characters_to_nfkc_list(Data);
normalize(nfkd, Data) ->
    unicode:characters_to_nfkd_list(Data);
normalize({Mod, Fun, Args}, Data) ->
    apply(Mod, Fun, [Data | Args]);
normalize(undefined, Data) ->
    Data.

tokenize(unicode_word_boundaries, Data) ->
    term_prep_uts:word_boundaries(Data);
tokenize({Mod, Fun, Args}, Data) ->
    apply(Mod, Fun, [Data | Args]);
tokenize(undefined, Data) ->
    [Data].

token_filter(Filter, Data) when is_map(Filter) ->
    T = token_transform(maps:get(transform, Filter, undefined), Data),
    D = token_delete(maps:get(delete, Filter, []), T),
    S = token_stats(maps:get(stats, Filter, undefined), D),
    token_add(maps:get(add, Filter, []), S);
token_filter(undefined, Data) ->
    Data.

token_transform(lowercase, Data) ->
    [string:lowercase(S) || S <-  Data];
token_transform(casefold, Data) ->
    [string:casefold(S) || S <-  Data];
token_transform(uppercase, Data) ->
    [string:uppercase(S) || S <-  Data];
token_transform({Mod, Fun, Args}, Data) ->
    apply(Mod, Fun, [Data | Args]);
token_transform(undefined, Data) ->
    Data.

token_delete([Rule | Rest], Data) ->
    Interm = token_delete_(Rule, Data),
    token_delete(Rest, Interm);
token_delete([], Data) ->
    Data.

token_delete_(english_stopwords, Data) ->
    [S || S <- Data, not term_prep_english_stopwords:contains(S)];
token_delete_(lucene_stopwords, Data) ->
    [S || S <- Data, not term_prep_lucene_stopwords:contains(S)];
token_delete_(wikipages_stopwords, Data) ->
    [S || S <- Data, not term_prep_wikipages_stopwords:contains(S)];
token_delete_({Mod, Fun, Args}, Data) ->
    apply(Mod, Fun, [Data | Args]);
token_delete_(String, Data) ->
    case io_lib:printable_unicode_list(String) of
	true -> [S || S <- Data, not string:equal(String, S)];
	false -> Data
    end.

-spec token_add(AddRules :: {M :: atom(), F :: atom(), A :: [term()]} |
			    [string()],
		Data :: [Term :: unicode:chardata()] |
			[{Term :: unicode:chardata(), Freq :: integer()}] |
			[{Term :: unicode:chardata(), Freq :: integer(), Pos :: integer()}]
			) ->
    [Term :: unicode:chardata()] |
    [{Term :: unicode:chardata(), Freq :: integer()}] |
    [{Term :: unicode:chardata(), Freq :: integer(), Pos :: integer()}].
token_add({Mod, Fun, Args}, Data) ->
    apply(Mod, Fun, [Data | Args]);
token_add([Rule | Rest], Data) ->
    Interm =
	case io_lib:printable_unicode_list(Rule) of
	    true ->
		Synonyms = get_synonyms(Rule),
		lists:append(Data, tokens_to_add(Data, Synonyms, []));
	    false ->
		Data
	end,
    token_add(Rest, Interm);
token_add([], Data) ->
    Data.

tokens_to_add([Term | Rest], Synonyms, Acc) ->
    case make_add_terms(Term, Synonyms) of
	[] -> tokens_to_add(Rest, Synonyms, Acc);
	Add -> tokens_to_add(Rest, Synonyms, Acc ++ Add)
    end;
tokens_to_add([], _, Acc) ->
    Acc.

make_add_terms(TermStats, Synonyms) ->
    Term = get_term_(TermStats),
    case maps:is_key(Term, Synonyms) of
	true ->
	    [make_add_term(TermStats, S) || S <- maps:get(Term, Synonyms)];
	false ->
	    []
    end.

get_term_({Term, _F, _P}) ->
    Term;
get_term_({Term, _F}) ->
    Term;
get_term_(Term) ->
    Term.

make_add_term({_T, F, P}, S) ->
    {S, F, P};
make_add_term({_T, F}, S) ->
    {S, F};
make_add_term(_T, S) ->
    S.

-spec token_stats(Stat :: token_stats(),
		  Data :: [unicode:chardata()]) ->
    [Term :: unicode:chardata()] |
    [{Term :: unicode:chardata(), Freq :: integer()}] |
    [{Term :: unicode:chardata(), Freq :: integer(), Pos :: integer()}].
token_stats(unique, Data) ->
    lists:usort(Data);
token_stats(freqs, Data) ->
    token_freqs(Data);
token_stats(positions, Data) ->
    token_positions(Data);
token_stats(undefined, Data) ->
    Data.

token_freqs(Data) ->
    Fun = fun(V) -> V + 1 end,
    token_freqs(Data, Fun, #{}).

token_freqs([Token | Rest], Fun, Map) ->
    NewMap = maps:update_with(Token, Fun, 1, Map),
    token_freqs(Rest, Fun, NewMap);
token_freqs([], _Fun, Map) ->
    maps:to_list(Map).

token_positions(Data) ->
    Fun = fun({V, P}) -> {V + 1, P} end,
    token_positions(Data, Fun, #{}, 1).

token_positions([Token | Rest], Fun, Map, Pos) ->
    NewMap = maps:update_with(Token, Fun, {1, Pos}, Map),
    token_positions(Rest, Fun, NewMap, Pos+1);
token_positions([], _Fun, Map, _Pos) ->
    [{T,F,P} || {T, {F,P}} <- maps:to_list(Map)].


get_synonyms(String) ->
    Tokens = term_prep_uts:word_boundaries(String),
    Terms = sets:to_list(sets:from_list(Tokens)),
    {_, Synonyms} = lists:foldl(fun get_synonyms/2, {Terms, #{}}, Terms),
    Synonyms.

get_synonyms(Term, {Terms, Synonyms}) ->
    {Terms, maps:put(Term, lists:delete(Term, Terms), Synonyms)}.
