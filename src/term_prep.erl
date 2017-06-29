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

-type token_add() :: {M :: module(), F :: atom(), A :: [any()]} |
		     undefined.

-type token_delete() :: english_stopwords |
			lucene_stopwords |
			wikipages_stopwords |
			{M :: module(), F :: atom(), A :: [any()]} |
			undefined.

-type token_filter() :: #{transform := token_transform(),
			  add := token_add() | [token_add()],
			  delete := token_delete() | [token_delete()]} |
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
			add => undefined,
			delete => [english_stopwords]}};
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
			add => undefined,
			delete => [english_stopwords, wikipages_stopwords]}};
config(_) ->
    #{char_filter => undefined,
      tokenizer => undefined,
      token_filter => undefined}.

-spec analyze(Data :: unicode:chardata()) ->
    [unicode:charlist()] | unicode:chardata().
analyze(Data)  ->
    analyze(config(unicode_nfc_words), Data).

-spec analyze(Config :: term_prep:config(),
	      Data :: unicode:chardata()) ->
    [unicode:charlist()] | unicode:chardata().
analyze(Config, Bin) when is_binary(Bin) ->
    analyze(Config, binary_to_list(Bin));
analyze(#{char_filter := CharFilter,
	  tokenizer := Tokenizer,
	  token_filter := TokenFilter}, Data) ->
    Normalized = normalize(CharFilter, Data),
    Tokenized = tokenize(Tokenizer, Normalized),
    token_filter(TokenFilter, Tokenized).

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
    Data.

token_filter(#{transform := Transform,
	       add := Add,
	       delete := Delete}, Data) ->
    T = token_transform(Transform, Data),
    D = token_delete(Delete, T),
    token_add(Add, D);
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

token_delete(english_stopwords, Data) ->
    [S || S <- Data, term_prep_english_stopwords:prop(S) == undefined];
token_delete(lucene_stopwords, Data) ->
    [S || S <- Data, term_prep_lucene_stopwords:prop(S) == undefined];
token_delete(wikipages_stopwords, Data) ->
    [S || S <- Data, term_prep_wikipages_stopwords:prop(S) == undefined];
token_delete({Mod, Fun, Args}, Data) ->
    apply(Mod, Fun, [Data | Args]);
token_delete(undefined, Data) ->
    Data;
token_delete([Rule | Rest], Data) ->
    Interm = token_delete(Rule, Data),
    token_delete(Rest, Interm);
token_delete([], Data) ->
    Data.

token_add({Mod, Fun, Args}, Data) ->
    apply(Mod, Fun, [Data | Args]);
token_add(undefined, Data) ->
    Data;
token_add([Rule | Rest], Data) ->
    Interm = token_add(Rule, Data),
    token_add(Rest, Interm);
token_add([], Data) ->
    Data.
