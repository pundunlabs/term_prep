-module(term_prep).

%% Exported functions
-export([init/0]).
-include_lib("gb_log/include/gb_log.hrl").
-define(UWB_MOD, term_prep_uwb_lib).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
init() ->
    init([?UWB_MOD]).

init(Modules)->
    [init_(Mod) || Mod <- Modules].

init_(Mod) ->
    case code:which(Mod) of
	non_existing ->
	    create_code(Mod);
	_ ->
	    ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a module from a generated parser.
%% @end
%%--------------------------------------------------------------------
create_code(?UWB_MOD) ->
    PrivDir = code:priv_dir(term_prep),
    create_code_(filename:join(PrivDir,"WordBreakProperty.txt"));
create_code(_Else) ->
    ?debug("Unknown module name: ~p", [_Else]).

%%--------------------------------------------------------------------
%% @doc
%% Creates a module by parsing given file.
%% @end
%%--------------------------------------------------------------------
create_code_(File) ->
    {ok, Bin} = file:read_file(File),
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Bin)),
    {ok, Properties} = term_prep_wbp:parse(Tokens),
    CEForms = make_mod(Properties),
    io:format("~p~n", [CEForms]),
    {ok, _, Beam } = compile:forms(CEForms, [from_core, binary]),
    code:load_binary(?UWB_MOD, [], Beam).

 

%%--------------------------------------------------------------------
%% @doc
%% Make module 'stringprep_lib' with map/1 and prohibit/1 functions.
%% @end
%%--------------------------------------------------------------------
make_mod(Properties) ->
    ModuleName = cerl:c_atom(?UWB_MOD),
    cerl:c_module(ModuleName,
		  [cerl:c_fname(prop, 1),
		   cerl:c_fname(module_info, 0),
		   cerl:c_fname(module_info, 1)],
		  [make_prop_fun(Properties) | mod_info(ModuleName)]).

%%--------------------------------------------------------------------
%% @doc
%% Make prop/1 function.
%% @end
%%--------------------------------------------------------------------
make_prop_fun(Mappings) ->
    Arg1 = cerl:c_var('FuncArg1'),
    Else = cerl:c_var('Else'),

    Clauses = make_prop_clauses(Arg1, Mappings),
    
    LastClause = cerl:c_clause([Else], cerl:c_atom(undefined), Arg1),
    Case = cerl:c_case(Arg1, Clauses ++ [LastClause]),
    {cerl:c_fname(prop,1), cerl:c_fun([Arg1], Case)}.

%%--------------------------------------------------------------------
%% @doc
%% Make case clauses for prop/1 function.
%% @end
%%--------------------------------------------------------------------
make_prop_clauses(Arg1, Mappings) ->
    make_prop_clauses(Arg1, Mappings,[]).
    
%%--------------------------------------------------------------------
%% @doc
%% Make case clauses for prop/1 function.
%% @end
%%--------------------------------------------------------------------
make_prop_clauses(_Arg1, [], Acc) ->
    lists:reverse(Acc);
make_prop_clauses(Arg1, [[{integer, _Line, Val},
			  {var, _Line, Prop} ] | Rest], Acc) ->
    Clause = cerl:c_clause([cerl:c_int(Val)],
			   cerl:c_atom(true),
			   cerl:c_atom(Prop)),
    make_prop_clauses(Arg1, Rest, [Clause | Acc]);
make_prop_clauses(Arg1, [[{{integer, _Line, Val1},
			   {integer, _Line, Val2}},
			  {var, _Line, Prop} ] | Rest], Acc) ->
    Arg = cerl:c_var('Char'),
    GE = make_call(erlang, '>=', [Arg, cerl:c_int(Val1)]),
    LE = make_call(erlang, '=<', [Arg, cerl:c_int(Val2)]),
    Guard = make_call(erlang, 'and', [GE, LE]),
    Clause = cerl:c_clause([Arg],
			    Guard,
			    cerl:c_atom(Prop)),
    make_prop_clauses(Arg1, Rest, [Clause | Acc]);
make_prop_clauses(_Arg1, [[{_, Line, _}] | _Rest], _Acc) ->
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
