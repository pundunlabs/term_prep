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

-module(term_prep_code_gen).

%% Exported functions
-export([init/0,
	 init/1]).
-include_lib("gb_log/include/gb_log.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
init() ->
    Config = [
	#{parser_mod=>term_prep_wbp,
	  input_source=>"WordBreakProperty.txt",
	  out_mod=>term_prep_uwb_lib,
	  func => {property, 1}},
	#{parser_mod=>term_prep_stopwords,
	  input_source=>"_english_stopwords",
	  out_mod=>term_prep_english_stopwords,
	  func => {contains, 1}},
	#{parser_mod=>term_prep_stopwords,
	  input_source=>"_lucene_stopwords",
	  out_mod=>term_prep_lucene_stopwords,
	  func => {contains, 1}},
	#{parser_mod=>term_prep_stopwords,
	  input_source=>"_wikipages_stopwords",
	  out_mod=>term_prep_wikipages_stopwords,
	  func => {contains, 1}}
    ],
    init(Config).

init(Config)->
    [init_(C) || C <- Config].

init_(#{out_mod := OutMod} = C) ->
    case code:which(OutMod) of
	non_existing ->
	    create_code(C);
	_ ->
	    ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a module by parsing given file.
%% @end
%%--------------------------------------------------------------------
create_code(#{parser_mod := ParserMod,
	      input_source := Filename,
	      out_mod := OutMod,
	      func := Func}) ->
    PrivDir = code:priv_dir(term_prep),
    File = filename:join(PrivDir, Filename),
    {ok, Bin} = file:read_file(File),
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Bin)),
    {ok, Properties} = ParserMod:parse(Tokens),
    CEForms = make_mod(Func, OutMod, Properties),
    {ok, _, Beam } = compile:forms(CEForms, [from_core, binary]),
    code:load_binary(OutMod, [], Beam).

%%--------------------------------------------------------------------
%% @doc
%% Make module Mod with Fun/Arity function.
%% @end
%%--------------------------------------------------------------------
make_mod({Fun, Arity}, Mod, Properties) ->
    ModuleName = cerl:c_atom(Mod),
    cerl:c_module(ModuleName,
		  [cerl:c_fname(Fun, Arity),
		   cerl:c_fname(module_info, 0),
		   cerl:c_fname(module_info, 1)],
		  [make_fun(Fun, Arity, Properties) | mod_info(ModuleName)]).

make_fun(property, 1, Mappings) ->
    make_property_fun(Mappings);
make_fun(contains, 1, Mappings) ->
    make_contains_fun(Mappings).

%%--------------------------------------------------------------------
%% @doc
%% Make property/1 function.
%% @end
%%--------------------------------------------------------------------
make_property_fun(Mappings) ->
    Arg1 = cerl:c_var('FuncArg1'),
    Else = cerl:c_var('Else'),

    Clauses = make_property_clauses(Arg1, Mappings),

    LastClause = cerl:c_clause([Else],
			       cerl:c_atom(true),
			       cerl:c_atom(undefined)),
    Case = cerl:c_case(Arg1, Clauses ++ [LastClause]),
    {cerl:c_fname(property,1), cerl:c_fun([Arg1], Case)}.

%%--------------------------------------------------------------------
%% @doc
%% Make case clauses for property/1 function.
%% @end
%%--------------------------------------------------------------------
make_property_clauses(Arg1, Mappings) ->
    make_property_clauses(Arg1, Mappings,[]).

%%--------------------------------------------------------------------
%% @doc
%% Make case clauses for property/1 function.
%% @end
%%--------------------------------------------------------------------
make_property_clauses(_Arg1, [], Acc) ->
    lists:reverse(Acc);
make_property_clauses(Arg1, [[{integer, _Line, Val},
			      {var, _Line, Prop}] | Rest], Acc) ->
    Clause = cerl:c_clause([cerl:c_int(Val)],
			   cerl:c_atom(true),
			   cerl:c_atom(Prop)),
    make_property_clauses(Arg1, Rest, [Clause | Acc]);
make_property_clauses(Arg1, [[{{integer, _Line, Val1},
			       {integer, _Line, Val2}},
			      {var, _Line, Prop} ] | Rest], Acc) ->
    Arg = cerl:c_var('Char'),
    GE = make_call(erlang, '>=', [Arg, cerl:c_int(Val1)]),
    LE = make_call(erlang, '=<', [Arg, cerl:c_int(Val2)]),
    Guard = make_call(erlang, 'and', [GE, LE]),
    Clause = cerl:c_clause([Arg],
			    Guard,
			    cerl:c_atom(Prop)),
    make_property_clauses(Arg1, Rest, [Clause | Acc]);
make_property_clauses(_Arg1, [[{_, Line, _}] | _Rest], _Acc) ->
    {error, io_lib:format("Syntax error: ~p", [Line])}.

%%--------------------------------------------------------------------
%% @doc
%% Make contains/1 function.
%% @end
%%--------------------------------------------------------------------
make_contains_fun(Mappings) ->
    Arg1 = cerl:c_var('FuncArg1'),
    Else = cerl:c_var('Else'),

    Clauses = make_contains_clauses(Arg1, Mappings),

    LastClause = cerl:c_clause([Else],
			       cerl:c_atom(true),
			       cerl:c_atom(false)),
    Case = cerl:c_case(Arg1, Clauses ++ [LastClause]),
    {cerl:c_fname(contains,1), cerl:c_fun([Arg1], Case)}.

%%--------------------------------------------------------------------
%% @doc
%% Make case clauses for contains/1 function.
%% @end
%%--------------------------------------------------------------------
make_contains_clauses(Arg1, Mappings) ->
    make_contains_clauses(Arg1, Mappings,[]).

%%--------------------------------------------------------------------
%% @doc
%% Make case clauses for contains/1 function.
%% @end
%%--------------------------------------------------------------------
make_contains_clauses(_Arg1, [], Acc) ->
    lists:reverse(Acc);
make_contains_clauses(Arg1, [[{string, _Line, Val}] | Rest], Acc) ->
    Clause = cerl:c_clause([cerl:c_string(Val)],
			   cerl:c_atom(true),
			   cerl:c_atom(true)),
    make_contains_clauses(Arg1, Rest, [Clause | Acc]);
make_contains_clauses(_Arg1, [[{_, Line, _}] | _Rest], _Acc) ->
    {error, io_lib:format("Syntax error: ~p", [Line])}.

%%--------------------------------------------------------------------
%% @doc
%% Make module_info/1 function.
%% @end
%%--------------------------------------------------------------------
mod_info(Name) ->
    M = cerl:c_atom(erlang),
    F = cerl:c_atom(get_module_info),
    Info0 = {cerl:c_fname(module_info, 0),
	     cerl:c_fun([], cerl:c_call(M, F, [Name]))},
    Key = cerl:c_var('Key'),
    Info1 = {cerl:c_fname(module_info, 1),
	     cerl:c_fun([Key], cerl:c_call(M, F, [Name, Key]))},
    [Info0, Info1].

%%--------------------------------------------------------------------
%% @doc
%% Make a function call cerl with given Mod, Fun, and Args.
%% @end
%%--------------------------------------------------------------------
make_call(Mod0, Fun0, Args) ->
    Mod = cerl:c_atom(Mod0),
    Fun = cerl:c_atom(Fun0),
    cerl:c_call(Mod, Fun, Args).
