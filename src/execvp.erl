-module(execvp).

-on_load(init/0).
-export([cmd/2, 'cmd!'/2]).

init() ->
  PrivDir =
    case code:priv_dir(?MODULE) of
      {error, _} -> "priv";
      Dir -> Dir
    end,
  erlang:load_nif(filename:join(PrivDir, "execvp"), 0).

-spec cmd(Command :: string() | binary(), Args :: [string() | binary()]) ->
  {error, Reason :: binary()} | no_return().
cmd(_Cmd, _Args) ->
  erlang:nif_error(not_loaded).

'cmd!'(Cmd, Args) ->
  execvp:cmd(Cmd, Args),
  erlang:halt(1).

