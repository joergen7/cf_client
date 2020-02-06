%% -*- erlang -*-
%%
%% cf_client: Cuneiform client implementation
%%
%% Copyright 2015-2019 Jörgen Brandt <joergen@cuneiform-lang.org>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author Jörgen Brandt <joergen@cuneiform-lang.org>
%% @version 0.1.7
%% @copyright 2015-2019
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cuneiform_format ).

-export( [format_expr/1, format_type/1, format_pattern/1, format_info/1,
          format_extended_script/1, format_error/1] ).

-include( "cuneiform_lang.hrl" ).
-include( "cuneiform_shell.hrl" ).

-define( LAM, "*lam*" ).
-define( CLOSE, "*close*" ).
-define( APP, "*app*" ).
-define( FIX, "*fix*" ).
-define( FUT, "*fut*" ).


-spec format_expr( E :: e() ) -> string().

format_expr( {var, _, X} ) ->
  atom_to_list( X );

format_expr( {lam, _, _, _} ) ->
  ?LAM;

format_expr( {app, _, {var, _, X}, []} ) ->
  lists:flatten( io_lib:format( "~s()", [X] ) );

format_expr( {app, _, {var, _, X}, EBindLst} ) ->
  L = [io_lib:format( "~s = ~s", [X1, format_expr( E1 )] ) ||{X1, E1} <- EBindLst],
  S = string:join( L, ", " ),
  lists:flatten( io_lib:format( "~s( ~s )", [X, S] ) );

%format_expr( {app, _, {lam, _, [{X, T}], EBody}, [{X, E}]} ) ->
%  io_lib:format( "let ~s : ~s = ~s; ~s",
%                 [X,
%                  format_type( T ),
%                  format_expr( E ),
%                  format_expr( EBody )] );

format_expr( {app, _, _, _} ) ->
  ?APP;

format_expr( {fix, _, _} )    -> ?FIX;
format_expr( {fut, _, _, _} ) -> ?FUT;

format_expr( {str, _, B} ) ->
  lists:flatten( io_lib:format( "\"~s\"", [B] ) );

format_expr( {file, _, B} ) ->
  lists:flatten( io_lib:format( "'~s'", [B] ) );

format_expr( {true, _} )      -> "true";
format_expr( {false, _} )     -> "false";

format_expr( {cmp, _, E1, E2} ) ->
  S1 = format_expr( E1 ),
  S2 = format_expr( E2 ),
  lists:flatten( io_lib:format( "( ~s == ~s )", [S1, S2] ) );

format_expr( {conj, _, A, B} ) ->
  S1 = format_expr( A ),
  S2 = format_expr( B ),
  lists:flatten( io_lib:format( "( ~s and ~s )", [S1, S2] ) );

format_expr( {disj, _, A, B} ) ->
  S1 = format_expr( A ),
  S2 = format_expr( B ),
  lists:flatten( io_lib:format( "( ~s or ~s )", [S1, S2] ) );

format_expr( {neg, _, E} ) ->
  lists:flatten( io_lib:format( "not ~s", [format_expr( E )] ) );

format_expr( {isnil, _, E} ) ->
  lists:flatten( io_lib:format( "isnil ~s", [format_expr( E )] ) );

format_expr( {cnd, _, A, B, C} ) ->
  S1 = format_expr( A ),
  S2 = format_expr( B ),
  S3 = format_expr( C ),
  lists:flatten( io_lib:format( "if ~s then ~s else ~s end", [S1, S2, S3] ) );



format_expr( {null, _, T} ) ->
  io_lib:format( "[: ~s]", [format_type( T )] );

format_expr( Cons = {cons, _, _, _} ) ->

  ToExprLst =
    fun
      ToExprLst( {cons, _, Hd, Tl} ) ->
        {L, T} = ToExprLst( Tl ),
        {[Hd|L], T};
      ToExprLst( {null, _, T} )      ->
        {[], T}
    end,

  {ELst, T} = ToExprLst( Cons ),
  S = string:join( [format_expr( E ) || E <- ELst], ", " ),

  io_lib:format( "[~s : ~s]", [S, format_type( T )] );

format_expr( {append, _, A, B} ) ->
  io_lib:format( "(~s + ~s)", [format_expr( A ), format_expr( B )] );

format_expr( {err, _, T, {user, Msg}} ) ->
  io_lib:format( "error ~s : ~s", [Msg, format_type( T )] );

format_expr( {proj, _, X, E} ) ->
  io_lib:format( "(~s | ~s)", [format_expr( E ), X] );

format_expr( {rcd, _, EBindLst} ) ->
  L = [io_lib:format( "~s = ~s", [X, format_expr( E )] ) || {X, E} <- EBindLst],
  S = string:join( L, ", " ),
  io_lib:format( "<~s>", [S] );

format_expr( {for, _, T, EBindLst, EBody} ) ->
  L = [io_lib:format( "~s <- ~s", [X, format_expr( E )] ) || {X, E} <- EBindLst],
  S = string:join( L, ", " ),
  io_lib:format( "for ~s do ~s : ~s end",
                 [S,
                  format_expr( EBody ),
                  format_type( T )] );

format_expr( {fold, _, {XAcc, EAcc}, {XLst, ELst}, EBody} ) ->
  io_lib:format( "fold ~s = ~s, ~s <- ~s do ~s end",
                 [XAcc,
                  format_expr( EAcc ),
                  XLst,
                  format_expr( ELst ),
                  format_expr( EBody )] ).


-spec format_type( T :: t() ) -> string().

format_type( 'Str' )  -> "Str";
format_type( 'File' ) -> "File";
format_type( 'Bool' ) -> "Bool";

format_type( {'Fn', [], TRet} ) ->
  lists:flatten( io_lib:format( "Fn() -> ~s", [format_type( TRet )] ) );

format_type( {'Fn', TBindLst, RetType} ) ->
  L = [io_lib:format( "~s : ~s", [X, format_type( T )] ) || {X, T} <- TBindLst],
  S = lists:join( ", ", L ),
  lists:flatten( io_lib:format( "Fn( ~s ) -> ~s", [S, format_type( RetType )] ) );

format_type( {'Lst', T} ) ->
  lists:flatten( io_lib:format( "[~s]", [format_type( T )] ) );

format_type( {'Rcd', TBindLst} ) ->
  L = [io_lib:format( "~s : ~s", [X, format_type( T )] ) || {X, T} <- TBindLst],
  S = lists:join( ", ", L ),
  lists:flatten( io_lib:format( "<~s>", [S] ) ).




-spec format_pattern( R :: r() ) -> string().

format_pattern( {r_var, X, T} ) ->
  io_lib:format( "~s : ~s", [X, format_type( T )] );

format_pattern( {r_rcd, RBindLst} ) ->
  SLst = [io_lib:format( "~s = ~s", [X, format_pattern( R )] )
          || {X, R} <- RBindLst],
  S = string:join( SLst, ", " ),
  io_lib:format( "<~s>", [S] ).


-spec format_info( info() ) -> string().

format_info( na ) ->
  "[na]";

format_info( N )
when is_integer( N ), N > 0 ->
  lists:flatten( io_lib:format( "line ~b", [N] ) );

format_info( {File, Line} ) ->
  lists:flatten( io_lib:format( "in ~s line ~b", [File, Line] ) ).


-spec format_extended_script( ExtendedScript :: binary() ) -> string().

format_extended_script( ExtendedScript ) ->
  {_, S} = lists:foldl( fun( Line, {N, S} ) ->
                          {N+1, io_lib:format( "~s~4.B  ~s~n", [S, N, Line] )}
                        end,
                        {1, []}, re:split( ExtendedScript, "\n" ) ),
lists:flatten( S ).



-spec format_error( {error, Stage :: stage(), Reason :: _} ) -> string().

format_error( {error, scan, {Info, cuneiform_scan, {illegal, S}}} ) ->
  io_lib:format( "scan error ~s: illegal symbol ~s", [format_info( Info ), S] );

format_error( {error, parse, {Info, cuneiform_parse, Msg}} ) ->
  io_lib:format( "parse error ~s: ~s", [format_info( Info ), Msg] );

format_error( {error, type, {unbound_var, Info, VarName}} ) ->
  io_lib:format( "type error ~s: unbound variable ~p",
                 [format_info( Info ), VarName] );

format_error( {error, type, {type_mismatch, Info, {T1, T2}}} ) ->
  io_lib:format( "type error ~s: type mismatch, expected ~s got ~s",
                 [format_info( Info ),
                  format_type( T1 ),
                  format_type( T2 )] );

format_error( {error, type, {ambiguous_name, Info, Name}} ) ->
  io_lib:format( "type error ~s: ambiguous argument or field name ~p",
                 [format_info( Info ), Name] );

format_error( {error, type, {key_missing, Info, Name}} ) ->
  io_lib:format( "type error ~s: application argument missing ~p",
                 [format_info( Info ), Name] );

format_error( {error, type, {superfluous_key, Info, Name}} ) ->
  io_lib:format( "type error ~s: application superfluous argument ~p",
                 [format_info( Info ), Name] );

format_error( {error, type, {no_record_type, Info, T}} ) ->
  io_lib:format( "type error ~s: record expected, got ~s",
                 [format_info( Info ), format_type( T )] );

format_error( {error, type, {no_native_function_type, Info, T}} ) ->
  io_lib:format( "type error ~s: native function expected, got ~s",
                 [format_info( Info ), format_type( T )] );

format_error( {error, type, {no_list_type, Info, T}} ) ->
  io_lib:format( "type error ~s: list expected, got ~s",
                 [format_info( Info ), format_type( T )] );

format_error( {error, type, {no_comparable_type, Info, T}} ) ->
  io_lib:format( "type error ~s: type not comparable ~s",
                 [format_info( Info ), format_type( T )] );

format_error( {error,
               runtime,
               {err, Info,
                     RetType,
                     {run, Node, AppId, LamName, ExtendedScript, Output}}} ) ->

  S =   "~n~s~n"
      ++"~s~n"
      ++"runtime error ~s: executing foreign function ~s~n"
      ++"  return type: ~s~n"
      ++"  node:        ~s~n"
      ++"  app id:      ~s~n",

  io_lib:format( S, [Output,
                     format_extended_script( ExtendedScript ),
                     format_info( Info ),
                     LamName,
                     format_type( RetType ),
                     Node,
                     AppId] );

format_error( {error, runtime,
                      {err, Info,
                            RetType,
                            {stagein, Node, AppId, LamName, FileLst}}} ) ->

  S =   "runtime error ~s: staging in data designated for foreign function ~s~n"
      ++"  return type:         ~s~n"
      ++"  node:                ~s~n"
      ++"  app id:              ~s~n"
      ++"  missing input files: ~s~n",

  io_lib:format( S, [format_info( Info ),
                     LamName,
                     format_type( RetType ),
                     Node,
                     AppId,
                     string:join( lists:map( fun binary_to_list/1, FileLst ),
                                  ", " )] );

format_error( {error, runtime,
                      {err, Info,
                            RetType,
                            {stageout, Node, AppId, LamName, FileLst}}} ) ->

  S =   "runtime error ~s: staging out data produced by foreign function ~s~n"
      ++"  return type:          ~s~n"
      ++"  node:                 ~s~n"
      ++"  app id:               ~s~n"
      ++"  missing output files: ~s~n",

  io_lib:format( S, [format_info( Info ),
                     LamName,
                     format_type( RetType ),
                     Node,
                     AppId,
                     string:join( lists:map( fun binary_to_list/1, FileLst ),
                                  ", " )] );

format_error( {error, runtime, {err, Info, RetType, {user, Msg}}} ) ->

  S =   "runtime error ~s: user error~n"
      ++"  return type:   ~s~n"
      ++"  error message: ~s~n",

  io_lib:format( S, [format_info( Info ),
                     format_type( RetType ),
                     Msg] );


format_error( {error, Stage, Reason} ) ->
  io_lib:format( "~p error: ~p", [Stage, Reason] ).

