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

-module( cuneiform_cek ).

-export( [ev/1, extract_outbox/1, is_finished/1, load/1, recv_result/3,
          unload/1] ).

-include( "cuneiform_lang.hrl" ).
-include( "cuneiform_cek.hrl" ).

-import( cuneiform_lang, [alet/3, drop_info/1, impose_info/2] ).




-spec step( P :: prog() ) -> prog().


% variables
%----------

% meaning of variable comes from the environment
step( {Comm, {var, _, X}, Env, K, unknown} ) ->
  #{ X := {E, Env1} } = Env,
  {Comm, E, Env1, K, unknown};

% lam
%----

% native lam closes
step( {Comm, E = {lam, Info, _, {ntv, _}}, Env, K, unknown} ) ->
  E1 = {close, Info, E, Env},
  {Comm, E1, #{}, K, value};

% foreign lam deflects
step( {Comm, E = {lam, _, _, {frn, _, _, _, _}}, _, K, unknown} ) ->
  {Comm, E, #{}, K, value};

% applications
%-------------

% application resolves function first
step( {Comm, {app, Info, ELam, XeLst}, Env, K, unknown} ) ->
  {Comm, ELam, Env, {app_fn, Info, XeLst, Env, K}, unknown};

% application of closure: call by name
step( {Comm, {close, _, {lam, _, _, {ntv, EBody}}, EnvLam}, _,
       {app_fn, _, XeLst, EnvApp, K}, _} ) ->
  EnvArg = maps:from_list( [{X, {E, EnvApp}} || {X, E} <- XeLst] ),
  Env1 = maps:merge( EnvLam, EnvArg ),
  {Comm, EBody, Env1, K, unknown};

% application of foreign function no arguments: send it
step( {Comm, ELam = {lam, _, [], {frn, _, _, _, _}}, _,
       {app_fn, Info, [], _, K}, _} ) ->
  ESnd = {app, Info, ELam, []},
  send( Comm, ESnd, K );

% application of foreign function with arguments: call by value
step( {Comm, ELam = {lam, _, _, {frn, _, _, _, _}}, _,
       {app_fn, Info, [{X1, E1}|XeLst], Env, K}, _} ) ->
  K1 = {app_arg, Info, ELam, [], [], X1, XeLst, Env, K},
  {Comm, E1, Env, K1, unknown};

% application of foreign function all arguments visited: check if we can send
step( {Comm, E1, _,
       {app_arg, Info, ELam, XeLst1, PropLst1, X1, [], _, K}, Prop1} )
when Prop1 =/= unknown ->
  XeLst = lists:reverse( [{X1, E1}|XeLst1] ),
  PropLst = lists:reverse( [Prop1|PropLst1] ),
  ESnd = {app, Info, ELam, XeLst},
  case is_all_value( PropLst ) of

    % foreign function all arguments values: send it
    true ->
      send( Comm, ESnd, K );

    % foreign function stalled argument: stall
    false ->
      {Comm, ESnd, #{}, K, stalled}
  end;

% application of foreign function some arguments unvisited: visit next
step( {Comm, E1, _,
       {app_arg, Info, ELam, XeLst1, PropLst1, X1, [{X2, E2}|XeLst2], Env, K},
       Prop1} )
when Prop1 =/= unknown ->
  XeLst = [{X1, E1}|XeLst1],
  PropLst = [Prop1|PropLst1],
  K1 = {app_arg, Info, ELam, XeLst, PropLst, X2, XeLst2, Env, K},
  {Comm, E2, Env, K1, unknown};

% stalled function: stall
step( {Comm, ELam, _, {app_fn, Info, XeLst, _, K}, stalled} ) ->
  E = {app, Info, ELam, XeLst},
  {Comm, E, #{}, K, stalled};

% closure replaces current environment
step( {Comm, {close, _, E, Env}, _, K, unknown} ) ->
  {Comm, E, Env, K, unknown};

% fixpoint
%---------

% fixpoint: resolve operand
step( {Comm, {fix, Info, EFix}, Env, K, unknown} ) ->
  {Comm, EFix, Env, {fix_op, Info, K}, unknown};

% fixpoint operand function: defer
step( {Comm, C = {lam, InfoLam, [{X, T}|XtLst], {ntv, EBody}}, EnvLam,
       {fix_op, InfoFix, K}, value} ) ->
  EBody1 = alet( InfoLam, [{X, T, {fix, InfoFix, C}}], EBody ),
  {Comm, {lam, InfoLam, XtLst, {ntv, EBody1}}, EnvLam, K, value};

% fixpoint operand stalled: stall
step( {Comm, EFix, _, {fix_op, Info, K}, stalled} ) ->
  {Comm, {fix, Info, EFix}, #{}, K, stalled};


% fut
%----

step( {Comm, EFut = {fut, Info, _, H}, _, K, unknown} ) ->
  {_, _, Inbox} = Comm,
  case maps:is_key( H, Inbox ) of
    false ->
      {Comm, EFut, #{}, K, stalled};
    true ->
      #{ H := E1 } = Inbox,
      E2 = impose_info( Info, E1 ),
      {Comm, E2, #{}, K, unknown} % result could be an error
  end;

% str
%----

% str deflects
step( {Comm, C = {str, _, _}, _, K, unknown} ) ->
  {Comm, C, #{}, K, value};

% file
%-----

% file deflects
step( {Comm, E = {file, _, _}, _, K, unknown} ) ->
  {Comm, E, #{}, K, value};

% Booleans
%---------

% true deflects
step( {Comm, E = {true, _}, _, K, unknown} ) ->
  {Comm, E, #{}, K, value};

% false deflects
step( {Comm, E = {false, _}, _, K, unknown} ) ->
  {Comm, E, #{}, K, value};

% comparison
%-----------

% comparison: resolve left-hand operand
step( {Comm, {cmp, Info, E1, E2}, Env, K, unknown} ) ->
  {Comm, E1, Env, {cmp_lhs, Info, E2, Env, K}, unknown};

% comparison lhs stalled or value: proceed to rhs
step( {Comm, E1, _, {cmp_lhs, Info, E2, Env, K}, Prop1} )
when Prop1 =/= unknown ->
  {Comm, E2, Env, {cmp_rhs, Info, E1, Prop1, Env, K}, unknown};

% compare Booleans
step( {Comm, {B1, _}, _, {cmp_rhs, Info, {B1, _}, _, _, K}, _} )
when is_boolean( B1 ) ->
  {Comm, {true, Info}, #{}, K, value};

% compare strings
step( {Comm, {str, _, B1}, _, {cmp_rhs, Info, {str, _, B1}, _, _, K}, _} ) ->
  {Comm, {true, Info}, #{}, K, value};

% compare lists
step( {Comm, {null, _, _}, _, {cmp_rhs, Info, {null, _, _}, _, _, K}, _} ) ->
  {Comm, {true, Info}, #{}, K, value};

step( {Comm, {cons, _, E21, E22}, _, {cmp_rhs, Info, {cons, _, E11, E12}, _, Env, K}, _} ) ->
  E = {conj, Info, {cmp, Info, E11, E21}, {cmp, Info, E12, E22}},
  {Comm, E, Env, K, unknown};

% compare records
step( {Comm, {rcd, _, [{X, E2}]}, {cmp_rhs, Info, {rcd, _, [X, E1]}, _, Env, K}, _} ) ->
  {Comm, E2, #{}, {cmp_rhs, Info, E1, Env, K}, value};

step( {Comm, {rcd, I2, [{X, E2}|XeLst2]}, {cmp_rhs, Info, {rcd, I1, XeLst1}, _, Env, K}, _} ) ->
  case lists:keyfind( X, 1, XeLst1 ) of
    {X, E1} ->
      E = {conj, Info, {cmp, Info, E1, E2}, {cmp, Info, {rcd, I1, XeLst1--[{X, E1}]}, {rcd, I2, XeLst2}}},
      {Comm, E, Env, K, unknown};
    false ->
      {Comm, {false, Info}, #{}, K, value}
  end;

% if both operands are values and there is no match, they must be unequal
step( {Comm, _, _, {cmp_rhs, Info, _, value, _, K}, value} ) ->
  {Comm, {false, Info}, #{}, K, value};

% comparison either E1 or E2 is no value: stall
step( {Comm, E2, _, {cmp_rhs, Info, E1, _, _, K}, Prop2} )
when Prop2 =/= unknown ->
  ECmp = {cmp, Info, E1, E2},
  {Comm, ECmp, #{}, K, stalled};

% conjunction
%------------

% conjunction: resolve left-hand operand
step( {Comm, {conj, Info, E1, E2}, Env, K, unknown} ) ->
  {Comm, E1, Env, {conj_lhs, Info, E2, Env, K}, unknown};

step( {Comm, E1 = {false, _}, _, {conj_lhs, _, _, _, K}, _} ) ->
  {Comm, E1, #{}, K, value};

step( {Comm, {true, _}, _, {conj_lhs, _, E2, Env, K}, _} ) ->
  {Comm, E2, Env, K, unknown};

step( {Comm, E1, _, {conj_lhs, Info, E2, Env, K}, stalled} ) ->
  {Comm, E2, Env, {conj_rhs, Info, E1, K}, unknown};

step( {Comm, E2, _, {conj_rhs, Info, E1, K}, stalled} ) ->
  {Comm, {conj, Info, E1, E2}, #{}, K, stalled};

step( {Comm, E2 = {false, _}, _, {conj_rhs, _, _, K}, _} ) ->
  {Comm, E2, #{}, K, value};

step( {Comm, {true, _}, _, {conj_rhs, _, E1, K}, _} ) ->
  {Comm, E1, #{}, K, stalled};

% disjunction
%------------

% disjunction: resolve left-hand operand
step( {Comm, {disj, Info, E1, E2}, Env, K, unknown} ) ->
  {Comm, E1, Env, {disj_lhs, Info, E2, Env, K}, unknown};

step( {Comm, {false, _}, _, {disj_lhs, _, E2, Env, K}, _} ) ->
  {Comm, E2, Env, K, unknown};

step( {Comm, E1 = {true, _}, _, {disj_lhs, _, _, _, K}, _} ) ->
  {Comm, E1, #{}, K, value};

step( {Comm, E1, _, {disj_lhs, Info, E2, Env, K}, stalled} ) ->
  {Comm, E2, Env, {disj_rhs, Info, E1, K}, unknown};

step( {Comm, E2, _, {disj_rhs, Info, E1, K}, stalled} ) ->
  {Comm, {disj, Info, E1, E2}, #{}, K, stalled};

step( {Comm, E2 = {true, _}, _, {disj_rhs, _, _, K}, _} ) ->
  {Comm, E2, #{}, K, value};

step( {Comm, {false, _}, _, {disj_rhs, _, E1, K}, _} ) ->
  {Comm, E1, #{}, K, stalled};

% negation
%---------

step( {Comm, {neg, Info, EOp}, Env, K, unknown} ) ->
  {Comm, EOp, Env, {neg_op, Info, K}, unknown};

step( {Comm, {B, _}, _, {neg_op, Info, K}, _} )
when is_boolean( B ) ->
  {Comm, {not B, Info}, #{}, K, value};

step( {Comm, EOp, _, {neg_op, Info, K}, stalled} ) ->
  {Comm, {neg, Info, EOp}, #{}, K, stalled};

% isnil
%------

step( {Comm, {isnil, Info, EOp}, Env, K, unknown} ) ->
  {Comm, EOp, Env, {isnil_op, Info, K}, unknown};

step( {Comm, {cons, _, _, _}, _, {isnil_op, Info, K}, _} ) ->
  {Comm, {false, Info}, #{}, K, value};

step( {Comm, {null, _, _}, {isnil_op, Info, K}, _} ) ->
  {Comm, {true, Info}, #{}, K, value};

step( {Comm, EOp, _, {isnil_op, Info, K}, stalled} ) ->
  {Comm, {isnil, Info, EOp}, #{}, K, stalled};

% cnd
%----

step( {Comm, {cnd, Info, E1, E2, E3}, Env, K, unknown} ) ->
  {Comm, E1, Env, {cnd_if, Info, E2, E3, Env, K}, unknown};

step( {Comm, {true, _}, _, {cnd_if, _, E2, _, Env, K}, _} ) ->
  {Comm, E2, Env, K, unknown};

step( {Comm, {false, _}, _, {cnd_if, _, _, E3, Env, K}, _} ) ->
  {Comm, E3, Env, K, unknown};

step( {Comm, E1, _, {cnd_if, Info, E2, E3, Env, K}, stalled} ) ->
  Close = {close, Info, {cnd, Info, E1, E2, E3}, Env},
  {Comm, Close, #{}, K, stalled};


% null
%-----

% null deflects
step( {Comm, C = {null, _, _}, _, K, unknown} ) ->
  {Comm, C, #{}, K, value};

% cons
%-----

step( {Comm, {cons, Info, E1, E2}, Env, K, unknown} ) ->
  {Comm, E1, Env, {cons_hd, Info, E2, Env, K}, unknown};

step( {Comm, E1, _, {cons_hd, Info, E2, Env, K}, Prop1} )
when Prop1 =/= unknown ->
  {Comm, E2, Env, {cons_tl, Info, E1, Prop1, K}, unknown};

step( {Comm, E2, _, {cons_tl, Info, E1, value, K}, value} ) ->
  {Comm, {cons, Info, E1, E2}, #{}, K, value};

step( {Comm, E2, _, {cons_tl, Info, E1, _, K}, Prop2} )
when Prop2 =/= unknown ->
  {Comm, {cons, Info, E1, E2}, #{}, K, stalled};

% hd
%---

step( {Comm, {hd, Info, E1, E2}, Env, K, unknown} ) ->
  {Comm, E1, Env, {hd_op, Info, E2, Env, K}, unknown};

step( {Comm, {null, _, _}, _, {hd_op, _, E2, Env, K}, _} ) ->
  {Comm, E2, Env, K, unknown};

step( {Comm, {cons, _, E11, _}, _, {hd_op, _, _, Env, K}, _} ) ->
  {Comm, E11, Env, K, unknown};

step( {Comm, E1, _, {hd_op, Info, E2, Env, K}, stalled} ) ->
  E = {close, Info, {hd, Info, E1, E2}, Env},
  {Comm, E, #{}, K, stalled};

% tl
%---

step( {Comm, {tl, Info, E1, E2}, Env, K, unknown} ) ->
  {Comm, E1, Env, {tl_op, Info, E2, Env, K}, unknown};

step( {Comm, {null, _, _}, _, {tl_op, _, E2, Env, K}, _} ) ->
  {Comm, E2, Env, K, unknown};

step( {Comm, {cons, _, _, E12}, _, {tl_op, _, _, Env, K}, _} ) ->
  {Comm, E12, Env, K, unknown};

step( {Comm, E1, _, {tl_op, Info, E2, Env, K}, stalled} ) ->
  E = {close, Info, {tl, Info, E1, E2}, Env},
  {Comm, E, #{}, K, stalled};

% append
%-------

step( {Comm, {append, Info, E1, E2}, Env, K, unknown} ) ->
  {Comm, E1, Env, {append_lhs, Info, E2, Env, K}, unknown};

step( {Comm, {null, _, _}, _, {append_lhs, _, E2, Env, K}, _} ) ->
  {Comm, E2, Env, K, unknown};

step( {Comm, {cons, InfoCons, E11, E12}, _, {append_lhs, InfoAppend, E2, Env, K}, value} ) ->
  E3 = {append, InfoAppend, E12, E2},
  K1 = {cons_tl, InfoCons, E11, value, K},
  {Comm, E3, Env, K1, unknown};

step( {Comm, {cons, InfoCons, E11, E12}, _, {append_lhs, InfoAppend, E2, Env, K}, _} ) ->
  E3 = {cons, InfoCons, E11, {append, InfoAppend, E12, E2}},
  {Comm, E3, Env, K, unknown};

step( {Comm, E1, _, {append_lhs, InfoAppend, E2, Env, K}, stalled} ) ->
  {Comm, E2, Env, {append_rhs, InfoAppend, E1, K}, unknown};

step( {Comm, E2, _, {append_rhs, InfoAppend, E1, K}, Prop} )
when Prop =/= unknown ->
  {Comm, {append, InfoAppend, E1, E2}, #{}, K, stalled};

% for
%----

step( {Comm, {for, Info, TRet, [{X1, T1, E1}|XteLst], EBody}, Env, K, unknown} ) ->
  K1 = {for_arg, Info, TRet, [], X1, T1, XteLst, EBody, Env, K},
  {Comm, E1, Env, K1, unknown};

step( {Comm, E1, _,
       {for_arg, Info, TRet, XteLst1, X1, T1, [], EBody, Env, K}, Prop1} )
when Prop1 =/= unknown ->

  ConsPred =
    fun
      ( {cons, _, _, _} ) -> true;
      ( _ )               -> false
    end,

  NullPred =
    fun
      ( {null, _, _} ) -> true;
      ( _ )            -> false
    end,

  XteLst = lists:reverse( [{X1, T1, E1}|XteLst1] ),
  ELst = [E || {_, _, E} <- XteLst],

  case lists:any( NullPred, ELst ) of
    true ->
      {Comm, {null, Info, TRet}, #{}, K, value};
    false ->
      case lists:all( ConsPred, ELst ) of
        true ->
          HdLst = [{X, T, E} || {X, T, {cons, _, E, _}} <- XteLst],
          TlLst = [{X, T, E} || {X, T, {cons, _, _, E}} <- XteLst],
          E21 = alet( Info, HdLst, EBody ),
          E22 = {for, Info, TRet, TlLst, EBody},
          {Comm, {cons, Info, E21, E22}, Env, K, unknown};
        false ->
          E2 = {close, Info, {for, Info, TRet, XteLst, EBody}, Env},
          {Comm, E2, #{}, K, stalled}
      end
  end;

step( {Comm, E1, _,
       {for_arg, Info, TRet, XteLst1, X1, T1, [{X2, T2, E2}|XteLst2], EBody, Env, K},
       Prop1} )
when Prop1 =/= unknown ->
  K1 = {for_arg, Info, TRet, [{X1, T1, E1}|XteLst1], X2, T2, XteLst2, EBody, Env, K},
  {Comm, E2, Env, K1, unknown};

% fold
%-----

step( {Comm, {fold, Info, XteAcc = {_, _, _}, {X2, T2, E2}, EBody}, Env, K, unknown} ) ->
  {Comm, E2, Env, {fold_arg, Info, XteAcc, X2, T2, EBody, Env, K}, unknown};

step( {Comm, {null, _, _}, _, {fold_arg, _, {_, _, E1}, _, _, _, Env, K}, _} ) ->
  {Comm, E1, Env, K, unknown};

step( {Comm, {cons, _, E21, E22}, _, {fold_arg, Info, XteAcc = {X1, T1, _}, X2, T2, EBody, Env, K}, _} ) ->
  EAcc = alet( Info, [XteAcc, {X2, T2, E21}], EBody ),
  EFold = {fold, Info, {X1, T1, EAcc}, {X2, T2, E22}, EBody},
  {Comm, EFold, Env, K, unknown};

step( {Comm, E2, _, {fold_arg, Info, XteAcc = {_, _, _}, X2, T2, EBody, Env, K}, stalled} ) ->
  EFold = {fold, Info, XteAcc, {X2, T2, E2}, EBody},
  Close = {close, Info, EFold, Env},
  {Comm, Close, #{}, K, stalled};
  
% rcd
%----

step( {Comm, {rcd, Info, [{X1, E1}|XeLst]}, Env, K, unknown} ) ->
  {Comm, E1, Env, {rcd_field, Info, [], [], X1, XeLst, Env, K}, unknown};

step( {Comm, E1, _, {rcd_field, Info, XeLst1, PropLst1, X1, [], _, K}, Prop1} )
when Prop1 =/= unknown ->
  XeLst = lists:reverse( [{X1, E1}|XeLst1] ),
  PropLst = lists:reverse( [Prop1|PropLst1] ),
  Prop =
    case is_all_value( PropLst ) of
      true -> value;
      false -> stalled
    end,
  {Comm, {rcd, Info, XeLst}, #{}, K, Prop};

step( {Comm, E1, _, {rcd_field, Info, XeLst1, PropLst1, X1, [{X2, E2}|XeLst2], Env, K}, Prop1} )
when Prop1 =/= unknown ->
  K1 = {rcd_field, Info, [{X1, E1}|XeLst1], [Prop1|PropLst1], X2, XeLst2, Env, K},
  {Comm, E2, Env, K1, unknown};

% proj
%-----

step( {Comm, {proj, Info, X, E1}, Env, K, unknown} ) ->
  {Comm, E1, Env, {proj_op, Info, X, K}};

step( {Comm, {rcd, _, XeLst}, _, {proj_op, _, X, K}, value} ) ->
  {_, E} = lists:keyfind( X, 1, XeLst ),
  {Comm, E, #{}, K, value};

step( {Comm, {rcd, _, XeLst}, _, {proj_op, _, X, K}, _} ) ->
  {_, E} = lists:keyfind( X, 1, XeLst ),
  {Comm, E, #{}, K, unknown};

step( {Comm, E1, _, {proj_op, Info, X, K}, stalled} ) ->
  {Comm, {proj, Info, X, E1}, #{}, K, stalled};

% err
%----

step( {_, E = {err, _, _, _}, _, K, unknown} )
when K =/= mt ->
  {[], #{}, E, #{}, mt, unknown};


step( _ ) ->
  throw( norule ).


-spec ev( P :: prog() ) -> prog().

ev( P ) ->
  try
    ev( step( P ) )
  catch
    throw:norule -> P
  end.


-spec is_finished( P :: prog() ) -> boolean().

is_finished( {_, _, _, mt, value} ) -> true;
is_finished( _ )                    -> false.


-spec load( E :: e() ) -> prog().

load( E ) ->
  Comm = {[], sets:new(), #{}},
  {Comm, E, #{}, mt, unknown}.


-spec unload( P :: prog() ) -> e().

unload( {_, E, _, mt, value} ) ->
  E;

unload( {_, _, _, K, _} ) when K =/= mt ->
  error( continuation_not_empty );

unload( {_, _, _, _, P} ) when P =/= value ->
  error( {bad_control_string_prop, P} ).


-spec recv_result( P, H, E ) -> prog()
when P :: prog(),
     H :: binary(),
     E :: e().

recv_result( {{Outbox, Awaybox, Inbox}, E, Env, K, Prop}, H1, E1 ) ->
  Inbox1 = Inbox#{ H1 => E1 },
  {{Outbox, Awaybox, Inbox1}, E, Env, K, Prop}.

-spec extract_outbox( P :: prog() ) -> {[{binary(), e()}], prog()}.

extract_outbox( {{Outbox, Awaybox, Inbox}, E, Env, K, Prop} ) ->
  {Outbox, {{[], Awaybox, Inbox}, E, Env, K, Prop}}.


%%====================================================================
%% Internal functions
%%====================================================================

-spec hash( T :: _ ) -> binary().

hash( T ) ->
  B = term_to_binary( T ),
  H = crypto:hash( sha3_224, B ),
  S = lists:flatten( [io_lib:format( "~2.16.0b", [X] ) || <<X>> <= H] ),
  list_to_binary( S ).

-spec send( Comm, ESnd, K ) -> prog()
when Comm    :: comm(),
     ESnd    :: e(),
     K       :: k().

send( Comm, ESnd, K ) ->
  {Outbox, Awaybox, Inbox} = Comm,
  {app, Info, {lam, _, _, {frn, _, TRet, _, _}}, _} = ESnd,
  ESnd1 = drop_info( ESnd ),
  H = hash( ESnd1 ),
  EFut = {fut, Info, TRet, H},
  case sets:is_element( H, Awaybox ) of
    true ->
      {Comm, EFut, #{}, K, unknown};
    false ->
      Comm1 = {[{H, ESnd1}|Outbox], sets:add_element( H, Awaybox ), Inbox},
      {Comm1, EFut, #{}, K, stalled}
  end.


-spec is_all_value( L :: [cprop()] ) -> boolean().

is_all_value( L ) ->
  lists:all( fun( value ) -> true; ( _ ) -> false end, L ).