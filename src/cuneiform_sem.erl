%% -*- erlang -*-
%%
%% cf_client
%%
%% Copyright 2015-2018 Jörgen Brandt
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
%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>
%% @version 0.1.6
%% @copyright 2015-2018 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cuneiform_sem ).

-export( [is_value/1, subst_fut/3, set_info/2] ).

-include_lib( "cuneiform.hrl" ).

-import( cuneiform_lang, [
                          true/1, false/1, app/3, cmp/3, cnd/4, neg/2, conj/3,
                          disj/3, var/2, lam_ntv/3, lst/3, append/3, isnil/2,
                          for/4, fold/4, rcd/2, proj/3, fix/2, cons/3, null/2,
                          str/2, file/3, e_bind/2
                         ] ).

%%====================================================================
%% Callback function definition
%%====================================================================

-callback step( E :: e() ) -> {ok, e()} | {ok_send, e(), e()} | norule.


%%====================================================================
%% Determining values
%%====================================================================

-spec is_value( E :: e() ) -> boolean().

is_value( {str, _, _} )                 -> true;
is_value( {cmp, _, _, _} )              -> false;
is_value( {file, _, _, _} )             -> true;
is_value( {true, _} )                   -> true;
is_value( {false, _} )                  -> true;
is_value( {cnd, _, _, _, _} )           -> false;
is_value( {neg, _, _} )                 -> false;
is_value( {conj, _, _, _} )             -> false;
is_value( {disj, _, _, _} )             -> false;
is_value( {var, _, _} )                 -> false;
is_value( {lam_ntv, _, _, _} )          -> true;
is_value( {lam_frn, _, _, _, _, _, _} ) -> true;
is_value( {app, _, _, _} )              -> false;
is_value( {fut, _, _, _} )              -> false;
is_value( {null, _, _} )                -> true;
is_value( {cons, _, E1, E2} )           -> is_value( E1 ) andalso is_value( E2 );
is_value( {append, _, _, _} )           -> false;
is_value( {isnil, _, _} )               -> false;
is_value( {for, _, _, _, _} )           -> false;
is_value( {fold, _, _, _, _} )          -> false;
is_value( {rcd, _, EBindLst} )          -> lists:all( fun is_value/1, [E || {_, E} <- EBindLst] );
is_value( {proj, _, _, _} )             -> false;
is_value( {fix, _, _} )                 -> false;
is_value( {err, _, _, _} )              -> true.


%%====================================================================
%% Substituting futures
%%====================================================================

-spec subst_fut( E, A, Delta ) -> e()
when E     :: e(),
     A     :: e(),
     Delta :: e().

subst_fut( {fut, Info, _, H}, H, ES )              -> set_info( ES, Info );
subst_fut( E = {fut, _, _, _}, _, _ )              -> E;
subst_fut( E = {str, _, _}, _, _ )                 -> E;
subst_fut( E = {file, _, _, _}, _, _ )             -> E;
subst_fut( E = {true, _}, _, _ )                   -> E;
subst_fut( E = {false, _}, _, _ )                  -> E;
subst_fut( E = {var, _, _}, _, _ )                 -> E;
subst_fut( E = {lam_ntv, _, _, _}, _, _ )          -> E;
subst_fut( E = {lam_frn, _, _, _, _, _, _}, _, _ ) -> E;
subst_fut( E = {null, _, _}, _, _ )                -> E;
subst_fut( E = {err, _, _, _}, _, _ )              -> E;

subst_fut( {cmp, Info, E1, E2}, H, ES ) ->
  cmp( Info, subst_fut( E1, H, ES ),
             subst_fut( E2, H, ES ) );

subst_fut( {cnd, Info, E1, E2, E3}, H, ES ) ->
  cnd( Info, subst_fut( E1, H, ES ),
             subst_fut( E2, H, ES ),
             subst_fut( E3, H, ES ) );

subst_fut( {neg, Info, E1}, H, ES ) ->
  neg( Info, subst_fut( E1, H, ES ) );

subst_fut( {conj, Info, E1, E2}, H, ES ) ->
  conj( Info, subst_fut( E1, H, ES ),
              subst_fut( E2, H, ES ) );

subst_fut( {disj, Info, E1, E2}, H, ES ) ->
  disj( Info, subst_fut( E1, H, ES ),
              subst_fut( E2, H, ES ) );

subst_fut( {app, Info, EF, EBindLst}, H, ES ) ->
  app( Info, subst_fut( EF, H, ES ),
             [e_bind( X, subst_fut( E, H, ES ) ) || {X, E} <- EBindLst] );

subst_fut( {cons, Info, E1, E2}, H, ES ) ->
  cons( Info, subst_fut( E1, H, ES ),
              subst_fut( E2, H, ES ) );

subst_fut( {append, Info, E1, E2}, H, ES ) ->
  append( Info, subst_fut( E1, H, ES ),
                subst_fut( E2, H, ES ) );

subst_fut( {isnil, Info, E1}, H, ES ) ->
  isnil( Info, subst_fut( E1, H, ES ) );

subst_fut( {rcd, Info, EBindLst}, H, ES ) ->
  rcd( Info, [e_bind( X, subst_fut( E, H, ES ) ) || {X, E} <- EBindLst] );

subst_fut( {proj, Info, X, E1}, H, ES ) ->
  proj( Info, X, subst_fut( E1, H, ES ) );

subst_fut( {fix, Info, E1}, H, ES ) ->
  fix( Info, subst_fut( E1, H, ES ) );

subst_fut( {for, Info, TRet, EBindLst, EBody}, H, ES ) ->
  for( Info, TRet, [e_bind( X, subst_fut( E, H, ES )) || {X, E} <- EBindLst],
       EBody );

subst_fut( {fold, Info, AccBind, {X, E}, EBody}, H, ES ) ->
  fold( Info, AccBind, e_bind( X, subst_fut( E, H, ES ) ), EBody ).


-spec set_info( E, Info ) -> e()
when E    :: e(),
     Info :: info().

set_info( {true, _}, Info )            -> true( Info );
set_info( {false, _}, Info )           -> false( Info );
set_info( {str, _, S}, Info )          -> str( Info, S );
set_info( {file, _, S, H}, Info )      -> file( Info, S, H );
set_info( {null, _, T}, Info )         -> null( Info, T );
set_info( {cons, _, E1, E2}, Info )    -> cons( Info, set_info( E1, Info ), set_info( E2, Info ) );
set_info( {rcd, _, EBindLst}, Info )   -> rcd( Info, [e_bind( X, set_info( E, Info ) ) || {X, E} <- EBindLst] );
set_info( {err, _, T, Reason}, Info )  -> {err, Info, T, Reason}.