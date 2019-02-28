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

-module( cuneiform_lang_test ).

-include_lib( "eunit/include/eunit.hrl" ).

-import( cuneiform_lang, [r_var/2, r_rcd/1, r_bind/2] ).
-import( cuneiform_lang, [t_str/0] ).
-import( cuneiform_lang, [assign/2, create_closure/2] ).
-import( cuneiform_lang, [e_bind/2, t_arg/2] ).
-import( cuneiform_lang, [var/1, app/2, lam_ntv/2, proj/2] ).
-import( cuneiform_lang, [l_bash/0, l_elixir/0, l_erlang/0, l_java/0,
                          l_javascript/0, l_matlab/0, l_octave/0, l_perl/0,
                          l_python/0, l_r/0, l_racket/0] ).

create_closure_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"assign variable pattern",
     fun assign_variable_pattern/0},
    {"last assignment binds innermost",
     fun last_assignment_binds_innermost/0},
    {"empty record pattern is neutral",
     fun empty_record_pattern_is_neutral/0},
    {"assignment resolution propagates to record fields",
     fun assignment_resolution_propagates_to_record_fields/0},
    {"ambiguous variable binding returns error",
     fun ambiguous_variable_binding_returns_error/0}
   ]
  }.

assign_variable_pattern() ->
  AssignLst = [assign( r_var( x, t_str() ), var( y ) )],
  EBody = var( z ),
  Closure = app( lam_ntv( [t_arg( x, t_str() )],
                          var( z ) ),
                 [e_bind( x, var( y ) )] ),
  ?assertEqual( {ok, Closure}, create_closure( AssignLst, EBody ) ).

last_assignment_binds_innermost() ->
  AssignLst = [assign( r_var( x1, t_str() ), var( y1 ) ),
               assign( r_var( x2, t_str() ), var( y2 ) )],
  EBody = var( z ),
  Closure = app( lam_ntv( [t_arg( x1, t_str() )],
                          app( lam_ntv( [t_arg( x2, t_str() )],
                                        var( z ) ),
                               [e_bind( x2, var( y2 ) )] ) ),
                 [e_bind( x1, var( y1 ) )] ),
  ?assertEqual( {ok, Closure}, create_closure( AssignLst, EBody ) ).

empty_record_pattern_is_neutral() ->
  AssignLst = [assign( r_rcd( [] ), var( x ) )],
  EBody = var( y ),
  ?assertEqual( {ok, EBody}, create_closure( AssignLst, EBody ) ).

assignment_resolution_propagates_to_record_fields() ->
  AssignLst = [assign( r_rcd( [r_bind( a, r_var( x, t_str() ) )] ),
               var( y ) )],
  EBody = var( z ),
  Closure = app( lam_ntv( [t_arg( x, t_str() )],
                          var( z ) ),
                 [e_bind( x, proj( a, var( y ) ) )] ),
  ?assertEqual( {ok, Closure}, create_closure( AssignLst, EBody ) ).

ambiguous_variable_binding_returns_error() ->
  AssignLst = [assign( r_rcd( [r_bind( a, r_var( x, t_str() ) ),
                               r_bind( b, r_var( x, t_str() ) )] ),
                       var( m ) )],
  EBody = var( x ),
  ?assertEqual( {error, {ambiguous_name, na, x}},
                create_closure( AssignLst, EBody ) ).



lang_test_() ->
  {foreach,
   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"l_bash returns atom",
     fun l_bash_returns_atom/0},
    {"l_elixir returns atom",
     fun l_elixir_returns_atom/0},
    {"l_erlang returns atom",
     fun l_erlang_returns_atom/0},
    {"l_java returns atom",
     fun l_java_returns_atom/0},
    {"l_javascript returns atom",
     fun l_javascript_returns_atom/0},
    {"l_matlab returns atom",
     fun l_matlab_returns_atom/0},
    {"l_octave returns atom",
     fun l_octave_returns_atom/0},
    {"l_perl returns atom",
     fun l_perl_returns_atom/0},
    {"l_python returns atom",
     fun l_python_returns_atom/0},
    {"l_r returns atom",
     fun l_r_returns_atom/0},
    {"l_racket returns atom",
     fun l_racket_returns_atom/0}
   ]
  }.

l_bash_returns_atom() ->
  ?assertEqual( 'Bash', l_bash() ).

l_elixir_returns_atom() ->
  ?assertEqual( 'Elixir', l_elixir() ).

l_erlang_returns_atom() ->
  ?assertEqual( 'Erlang', l_erlang() ).

l_java_returns_atom() ->
  ?assertEqual( 'Java', l_java() ).

l_javascript_returns_atom() ->
  ?assertEqual( 'Javascript', l_javascript() ).

l_matlab_returns_atom() ->
  ?assertEqual( 'Matlab', l_matlab() ).

l_octave_returns_atom() ->
  ?assertEqual( 'Octave', l_octave() ).

l_perl_returns_atom() ->
  ?assertEqual( 'Perl', l_perl() ).

l_python_returns_atom() ->
  ?assertEqual( 'Python', l_python() ).

l_r_returns_atom() ->
  ?assertEqual( 'R', l_r() ).

l_racket_returns_atom() ->
  ?assertEqual( 'Racket', l_racket() ).

