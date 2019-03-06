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

%%====================================================================
%% Symbol Declaration
%%====================================================================

Nonterminals
  define e imp l t_arg_lst t_arg r script t stat e_bind_lst e_bind r_bind_lst
  r_bind e_lst define_lst from_lst from.

Terminals
  l_bash l_elixir l_erlang l_java l_javascript l_matlab l_octave
  l_perl l_python l_r l_racket
  t_str t_file t_bool t_fn_frn t_fn_ntv
  assign bar wedge cmp cnd colon
  comma def do doublertag else eq err false fold for halt import in
  isnil larrow lbrace lparen lsquarebr ltag neg vee plus
  rarrow rbrace rparen rsquarebr rtag semicolon then true id
  intlit strlit filelit body.

%%====================================================================
%% Syntax Definition
%%====================================================================

Rootsymbol script.

script          -> stat                       : '$1'.
script          -> stat script                : join_stat( '$1', '$2' ).

stat            -> imp                        : {['$1'], [], []}.
stat            -> define                     : {[], ['$1'], []}.
stat            -> e semicolon                : {[], [], ['$1']}.

imp             -> import filelit semicolon   : visit_import( '$2' ).

define          -> assign r eq e semicolon                                             : visit_assign( '$1', '$2', '$4' ).
define          -> def id lparen rparen rarrow ltag t_arg_lst rtag in l body           : visit_def_frn( '$1', '$2', [], '$7', '$10', '$11' ).
define          -> def id lparen t_arg_lst rparen rarrow ltag t_arg_lst rtag in l body : visit_def_frn( '$1', '$2', '$4', '$8', '$11', '$12' ).
define          -> def id lparen rparen rarrow t lbrace e rbrace                       : visit_def_ntv( '$1', '$2', [], '$6', [], '$8' ).
define          -> def id lparen rparen rarrow t lbrace define_lst e rbrace            : visit_def_ntv( '$1', '$2', [], '$6', '$8', '$9' ).
define          -> def id lparen t_arg_lst rparen rarrow t lbrace e rbrace             : visit_def_ntv( '$1', '$2', '$4', '$7', [], '$9' ).
define          -> def id lparen t_arg_lst rparen rarrow t lbrace define_lst e rbrace  : visit_def_ntv( '$1', '$2', '$4', '$7', '$9', '$10' ).

define_lst      -> define                     : ['$1'].
define_lst      -> define define_lst          : ['$1'|'$2'].

r               -> id colon t                 : visit_r_var( '$1', '$3' ).
r               -> ltag r_bind_lst rtag       : visit_r_rcd( '$1', '$2' ).

l               -> l_bash                     : l_bash().
l               -> l_elixir                   : l_elixir().
l               -> l_erlang                   : l_erlang().
l               -> l_java                     : l_java().
l               -> l_matlab                   : l_matlab().
l               -> l_octave                   : l_octave().
l               -> l_perl                     : l_perl().
l               -> l_python                   : l_python().
l               -> l_r                        : l_r().
l               -> l_racket                   : l_racket().
l               -> l_javascript               : l_javascript().

t               -> t_str                                     : t_str().
t               -> t_file                                    : t_file().
t               -> t_bool                                    : t_bool().
t               -> t_fn_ntv lparen rparen rarrow t           : t_fn( ntv, [], '$5' ).
t               -> t_fn_ntv lparen t_arg_lst rparen rarrow t : t_fn( ntv, '$3', '$6' ).
t               -> t_fn_frn lparen rparen rarrow t           : t_fn( frn, [], '$5' ).
t               -> t_fn_frn lparen t_arg_lst rparen rarrow t : t_fn( frn, '$3', '$6' ).
t               -> lsquarebr t rsquarebr                     : t_lst( '$2' ).
t               -> ltag t_arg_lst rtag                       : t_rcd( '$2' ).

t_arg           -> id colon t                 : visit_t_arg( '$1', '$3' ).

t_arg_lst       -> t_arg                      : ['$1'].
t_arg_lst       -> t_arg comma t_arg_lst      : ['$1'|'$3'].


e               -> id                                                   : visit_var( '$1' ).
e               -> strlit                                               : visit_str( '$1' ).
e               -> intlit                                               : visit_str( '$1' ).
e               -> filelit                                              : visit_file( '$1' ).
e               -> true                                                 : visit_true( '$1' ).
e               -> false                                                : visit_false( '$1' ).
e               -> lparen e cmp e rparen                                : visit_cmp( '$2', '$3', '$4' ).
e               -> cnd e then e else e halt                             : visit_cnd( '$1', '$2', [], '$4', [], '$6' ).
e               -> cnd e then define_lst e else e halt                  : visit_cnd( '$1', '$2', '$4', '$5', [], '$7' ).
e               -> cnd e then e else define_lst e halt                  : visit_cnd( '$1', '$2', [], '$4', '$6', '$7' ).
e               -> cnd e then define_lst e else define_lst e halt       : visit_cnd( '$1', '$2', '$4', '$5', '$7', '$8' ).
e               -> neg e                                                : visit_neg( '$1', '$2' ).
e               -> lparen e wedge e rparen                              : visit_conj( '$2', '$3', '$4' ).
e               -> lparen e vee e rparen                                : visit_disj( '$2', '$3', '$4' ).
e               -> id lparen rparen                                     : visit_app( '$1', [] ).
e               -> id lparen e_bind_lst rparen                          : visit_app( '$1', '$3' ).
e               -> ltag e_bind_lst rtag                                 : visit_rcd( '$1', '$2' ).
e               -> lparen e bar id rparen                               : visit_proj( '$2', '$4' ).
e               -> lparen e plus e rparen                               : visit_append( '$2', '$3', '$4' ).
e               -> lsquarebr colon t rsquarebr                          : visit_lst( [], '$2', '$3' ).
e               -> lparen e doublertag e rparen                         : visit_cons( '$3', '$2', '$4' ).
e               -> lsquarebr e_lst colon t rsquarebr                    : visit_lst( '$2', '$3', '$4' ).
e               -> isnil e                                              : visit_isnil( '$1', '$2' ).
e               -> for from_lst do e colon t halt                       : visit_for( '$1', '$2', [], '$4', '$6' ).
e               -> for from_lst do define_lst e colon t halt            : visit_for( '$1', '$2', '$4', '$5', '$7' ).
e               -> fold id colon t eq e comma from do e halt            : visit_fold( '$1', '$2', '$4', '$6', '$8', [], '$10' ).
e               -> fold id colon t eq e comma from do define_lst e halt : visit_fold( '$1', '$2', '$4', '$6', '$8', '$10', '$11' ).
e               -> err strlit colon t                                   : visit_err( '$1', '$2', '$4' ).

from_lst        -> from                       : ['$1'].
from_lst        -> from comma from_lst        : ['$1'|'$3'].

from            -> id colon t larrow e        : visit_from( '$1', '$3', '$5' ).

e_lst           -> e                          : ['$1'].
e_lst           -> e comma e_lst              : ['$1'|'$3'].

e_bind_lst      -> e_bind                     : ['$1'].
e_bind_lst      -> e_bind comma e_bind_lst    : ['$1'|'$3'].

e_bind          -> id eq e                    : visit_e_bind( '$1', '$3' ).

r_bind_lst      -> r_bind                     : ['$1'].
r_bind_lst      -> r_bind comma r_bind_lst    : ['$1'|'$3'].

r_bind          -> id eq r                    : visit_r_bind( '$1', '$3' ).


%%====================================================================
%% Erlang Code
%%====================================================================

Erlang code.

-import( cuneiform_lang, [
                          l_bash/0, l_elixir/0, l_erlang/0, l_java/0,
                          l_javascript/0, l_matlab/0, l_octave/0,
                          l_perl/0, l_python/0, l_r/0, l_racket/0
                         ] ).

-import( cuneiform_lang, [
                          t_str/0, t_file/0, t_bool/0, t_fn/3, t_lst/1,
                          t_rcd/1
                         ] ).

-import( cuneiform_preproc, [join_stat/2, visit_from/3, visit_fold/7,
                             visit_cnd/6, visit_for/5, visit_import/1,
                             visit_r_var/2, visit_var/1, visit_file/1,
                             visit_str/1, visit_assign/3, visit_def_frn/6,
                             visit_def_ntv/6, visit_r_rcd/2, visit_t_arg/2,
                             visit_true/1, visit_false/1, visit_cmp/3,
                             visit_conj/3, visit_disj/3, visit_neg/2,
                             visit_app/2, visit_rcd/2, visit_proj/2,
                             visit_append/3, visit_lst/3, visit_cons/3,
                             visit_isnil/2, visit_err/3, visit_e_bind/2,
                             visit_r_bind/2
                             ] ).



