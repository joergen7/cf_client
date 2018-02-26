%% -*- erlang -*-
%%
%% A Cuneiform client implementation.
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
%% @version 0.1.0
%% @copyright 2015-2018 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------


-module( cuneiform_shell ).

-include_lib( "cf_client/include/cuneiform.hrl" ).

-export( [shell/1, shell_eval_oneshot/1, process_reply_lst/3] ).


-define( RED( Str ), "\e[31m" ++ Str ++ "\e[0m" ).
-define( BRED( Str ), "\e[1;31m" ++ Str ++ "\e[0m" ).
-define( GRN( Str ), "\e[32m" ++ Str ++ "\e[0m" ).
-define( YLW( Str ), "\e[33m" ++ Str ++ "\e[0m" ).
-define( BYLW( Str ), "\e[1;33m" ++ Str ++ "\e[0m" ).
-define( BLU( Str ), "\e[34m" ++ Str ++ "\e[0m" ).
-define( BBLU( Str ), "\e[1;34m" ++ Str ++ "\e[0m" ).

-record( shell_state, {line       = 1,
                       string_buf = "",
                       token_buf  = [],
                       token_lst  = [],
                       import_buf = [],
                       import_lst = [],
                       def_buf    = [],
                       def_lst    = [],
                       query_lst  = [],
                       reply_lst  = []} ).


-type reply() :: {query, e()}
               | {error, stage(), _}
               | {parrot, e(), t()}.






-spec shell( ClientName :: _ ) -> ok.

shell( ClientName ) ->


  io:format( "~s~n~n~n", [get_banner()] ),
  shell_repl( ClientName, #shell_state{} ).


-spec shell_repl( ClientName :: _, ShellState :: #shell_state{} ) -> ok.

shell_repl( ClientName, ShellState = #shell_state{ def_lst = DefLst } ) ->

  Prompt = get_prompt( ShellState ),
  
  case io:get_line( Prompt ) of

    "quit\n" ->
      ok;

    "help\n" ->
      io:format( "~s~n", [get_help()] ),
      shell_repl( ClientName, ShellState );

    "hist\n" ->
      G =
        fun( {assign, _, R, E} ) ->
          SR = cuneiform_format:format_pattern( R ),
          SE = cuneiform_format:format_expr( E ),
          io:format( "let ~s =~n  ~s;~n~n", [SR, SE] )
        end,
      lists:foreach( G,
                     DefLst ),
      shell_repl( ClientName, ShellState );

    Input    ->
      {ReplyLst, ShellState1} = shell_eval( Input, ShellState ),
      process_reply_lst( ReplyLst, ClientName, verbose ),
      shell_repl( ClientName, ShellState1 )
  end.


-spec process_reply_lst( ReplyLst, ClientName, Verbosity ) -> ok
when ReplyLst   :: [reply()],
     ClientName :: _,
     Verbosity  :: verbose | silent.

process_reply_lst( ReplyLst, ClientName, Verbosity )
when is_list( ReplyLst ),
     Verbosity =:= verbose orelse Verbosity =:= silent ->

  F =
    fun

      ( {query, E} ) ->
        V = cre_client:eval( ClientName, E ),
        case V of

          {err, _, _, _} ->
            SE = cuneiform_format:format_error( {error, runtime, V} ),
            io:format( ?RED( "~s" )++"~n", [SE] );

          _ ->
            {ok, T} = cuneiform_type:type( V ),
            SV = cuneiform_format:format_expr( V ),
            ST = cuneiform_format:format_type( T ),
            io:format( ?GRN( "~s" )++"~n"++?BLU( ": " )++?BBLU( "~s" )++"~n", [SV, ST] )
        end;

      ( {parrot, E, T} ) ->
        case Verbosity of
          silent  -> ok;
          verbose ->
            SE = cuneiform_format:format_expr( E ),
            ST = cuneiform_format:format_type( T ),
            io:format( "~s~n"++?BLU( ": " )++?BBLU( "~s" )++"~n", [SE, ST] )
        end;

      ( Reply = {error, _Stage, _Reason} ) ->
        S = cuneiform_format:format_error( Reply ),
        io:format( ?RED( "~s" )++"~n", [S] )

    end,

  ok = lists:foreach( F, ReplyLst ).


-spec shell_eval_oneshot( Input ) -> [reply()]
when Input :: string().

shell_eval_oneshot( Input ) ->

  {ReplyLst, ShellState} = shell_eval( Input, #shell_state{} ),

  case ShellState of

    #shell_state{ string_buf = [_|_] } ->
      [{error, scan, "input stream ended inside foreign code block"}];

    #shell_state{ token_buf = [_|_] } ->
      [{error, parse, "premature end of input stream"}];

    _ ->
      ReplyLst
      
  end.


-spec shell_eval( Input, ShellState ) -> {[reply()], #shell_state{}}
when Input      :: string(),
     ShellState :: #shell_state{}.

shell_eval( Input, ShellState = #shell_state{ string_buf = StringBuf } )
when is_list( Input ) ->

  Eval =
    fun Eval( S ) ->
      case shell_step( S ) of
        norule   -> S;
        {ok, S1} -> Eval( S1 )
      end
    end,

  StringBuf1 = StringBuf++Input,
  ShellState1 = ShellState#shell_state{ string_buf = StringBuf1 },
  ShellState2 = Eval( ShellState1 ),
  #shell_state{ reply_lst = ReplyLst } = ShellState2,
  ShellState3 = ShellState2#shell_state{ reply_lst = [] },

  {ReplyLst, ShellState3}.



-spec shell_step( ShellState ) -> norule | {ok, #shell_state{}}
when ShellState :: #shell_state{}.

% return query if it is typable
shell_step( ShellState = #shell_state{ string_buf = "",
                                       token_buf  = [],
                                       token_lst  = [],
                                       import_buf = [],
                                       def_buf    = [],
                                       query_lst  = [_|_] } ) ->

  #shell_state{ def_lst = DefLst,
                query_lst = [Q1|QLst],
                reply_lst = ReplyLst } = ShellState,

  % TODO: catch error
  {ok, C} = cuneiform_lang:create_closure( DefLst, Q1 ),

  ShellState1 = 
    case cuneiform_type:type( C ) of

      {ok, _} ->

        ReplyLst1 = ReplyLst++[{query, C}],

        ShellState#shell_state{ query_lst = QLst,
                                reply_lst = ReplyLst1 };

      {error, Reason} ->

        ReplyLst1 = ReplyLst++[{error, type, Reason}],

        ShellState#shell_state{ query_lst = QLst,
                                reply_lst = ReplyLst1 }

    end,

  {ok, ShellState1};




% TODO: clear import buffer

% type untyped definition
shell_step( ShellState = #shell_state{ string_buf = "",
                                       token_buf  = [],
                                       token_lst  = [],
                                       def_buf    = [_|_] } ) ->

  #shell_state{ def_buf   = [D1|DBuf],
                def_lst   = DefLst,
                reply_lst = ReplyLst } = ShellState,

  {assign, _, _, E} = D1,

  % TODO: catch error
  {ok, C} = cuneiform_lang:create_closure( DefLst++[D1], E ),

  ShellState1 = 
    case cuneiform_type:type( C ) of

      {ok, T} ->

        ReplyLst1 = ReplyLst++[{parrot, E, T}],
        DefLst1 = DefLst++[D1],

        ShellState#shell_state{ def_buf   = DBuf,
                                def_lst   = DefLst1,
                                reply_lst = ReplyLst1 };

      {error, Reason} ->

        ReplyLst1 = ReplyLst++[{error, type, Reason}],

        ShellState#shell_state{ def_buf = DBuf, reply_lst = ReplyLst1 }

    end,

  {ok, ShellState1};




% extract imports, definitions, and queries if token list can be parsed
shell_step( ShellState = #shell_state{ string_buf = "",
                                       token_buf  = [],
                                       token_lst  = [_|_] } ) ->

  #shell_state{ token_lst  = TokenLst,
                import_buf = ImportBuf,
                def_buf    = DefBuf,
                query_lst  = QueryLst,
                reply_lst  = ReplyLst } = ShellState,

  ShellState1 = 
    case cuneiform_parse:parse( TokenLst ) of

      {ok, {ILst, DLst, QLst}} ->

        ImportBuf1 = ImportBuf++ILst,
        DefBuf1 = DefBuf++DLst,
        QueryLst1 = QueryLst++QLst,

        ShellState#shell_state{ token_lst  = [],
                                import_buf = ImportBuf1,
                                def_buf    = DefBuf1,
                                query_lst  = QueryLst1 };

      {error, Reason} ->

        ReplyLst1 = ReplyLst++[{error, parse, Reason}],

        ShellState#shell_state{
          token_lst = [],
          reply_lst = ReplyLst1 }

    end,

  {ok, ShellState1};

% mark token list for parsing if all mentioned files can be found
shell_step( ShellState = #shell_state{ string_buf = "",
                                       token_buf  = [_|_] } ) ->

  #shell_state{ token_buf = TokenBuf,
                token_lst = TokenLst } = ShellState,

  case lists:last( TokenBuf ) of

    {A, _, _} when A =:= semicolon orelse A =:= rbrace orelse A =:= body ->

      case brace_level( TokenBuf, 0 ) > 0 of

        true ->
          norule;

        false ->

          % TODO: update file info

          TokenLst1 = TokenLst++TokenBuf,
          ShellState1 = ShellState#shell_state{ token_buf = [],
                                                token_lst = TokenLst1},

          {ok, ShellState1}

      end;

    _ ->
      norule

  end;


% scan string if we can be sure that all foreign code blocks are terminated
shell_step( ShellState = #shell_state{ string_buf = [_|_] } ) ->

  #shell_state{ line       = Line,
                string_buf = StringBuf,
                token_buf  = TokenBuf,
                reply_lst  = ReplyLst } = ShellState,

  case is_ready( StringBuf ) of

    false ->
      norule;

    true ->

      ShellState1 = 
        case cuneiform_scan:string( StringBuf, Line ) of

          {ok, TLst, Line1} ->

            % TODO: augment token information with source file name

            TokenBuf1 = TokenBuf++TLst,
            
            ShellState#shell_state{ line       = Line1,
                                    string_buf = "",
                                    token_buf  = TokenBuf1 };
      
          {error, Reason, _} ->
            
            ReplyLst1 = ReplyLst++[{error, scan, Reason}],
            
            ShellState#shell_state{
              string_buf = "",
              reply_lst  = ReplyLst1 }

        end,

      {ok, ShellState1}

  end;


shell_step( _ ) -> norule.





-spec get_banner() -> string().

get_banner() ->
  string:join(
    ["            ___",
     "           @@WB      Cuneiform",
     "          @@E_____",
     "     _g@@@@@WWWWWWL  "++?YLW( "Type " )++?BYLW( "help" )++?YLW( " for usage info" ),
     "   g@@#*`3@B              "++?BYLW( "quit" )++?YLW( " to exit shell" ),
     "  @@P    3@B",
     "  @N____ 3@B         "++?BLU( "http://www.cuneiform-lang.org" ),
     "  \"W@@@WF3@B"
], "\n" ).


-spec get_help() -> iolist().

get_help() ->
  string:join(
    [?BYLW( "help" )++?YLW( " show this usage info" ),
     ?BYLW( "hist" )++?YLW( " show definition history" ),
     ?BYLW( "quit" )++?YLW( " quit the shell" )
], "\n" ).


-spec is_ready( Buf :: string() ) -> boolean().
      is_ready( Buf )             -> is_ready( Buf, true ).

-spec is_ready( Buf :: string(), S :: boolean() ) -> boolean().

is_ready( [], S )                -> S;
is_ready( [$*, ${|Rest], true )  -> is_ready( Rest, false );
is_ready( [$}, $*|Rest], false ) -> is_ready( Rest, true );
is_ready( [_|Rest], S )          -> is_ready( Rest, S ).


-spec get_prompt( ShellState :: #shell_state{} ) -> string().

get_prompt( ShellState ) ->

  case ShellState of

    #shell_state{ line = Line, string_buf = "", token_buf = [] } ->
      io_lib:format( "~p> ", [Line] );

    _ ->
      ""

  end.


-spec brace_level( TokenLst, L ) -> integer()
when TokenLst :: [{atom(), info(), string()}],
     L        :: integer().

brace_level( [], L )                 -> L;
brace_level( [{lbrace, _, _}|R], L ) -> brace_level( R, L+1 );
brace_level( [{rbrace, _, _}|R], L ) -> brace_level( R, L-1 );
brace_level( [_|R], L )              -> brace_level( R, L ).


