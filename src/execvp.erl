-module(execvp).

-if(?OTP_RELEASE >= 28).
-moduledoc("""
`execvp` is the module that hosts the `cmd` command. For mode information see

    erl> h(execvp, cmd).

and

    erl> h(execvp, 'cmd!').

""").
-endif.

-on_load(init/0).
-export([cmd/2, 'cmd!'/2]).

init() ->
  PrivDir =
    case code:priv_dir(?MODULE) of
      {error, _} -> "priv";
      Dir -> Dir
    end,
  erlang:load_nif(filename:join(PrivDir, "execvp"), 0).

-if(?OTP_RELEASE >= 28).
-doc("""
`execvp:cmd/2` is like `exec` in bash. It replaces the entire BEAM with the
shell command supplied in the arguments.

The arguments can be binaries, Erlang strings (char lists), and these can also
be mixed.

The name comes from the POSIX system call `execvp`.

## Example:

    erl> execvp:cmd("echo", ["Hello"]).
    Hello
    bash$ █

## Another example:

    erl> execvp:cmd("nonexistent shell command", []).
    {error,<<"No such file or directory">>}
    erl> █

""").
-endif.
-spec cmd(Command :: string() | binary(), Args :: [string() | binary()]) ->
  {error, Reason :: binary()} | no_return().
cmd(_Cmd, _Args) ->
  erlang:nif_error(not_loaded).

-if(?OTP_RELEASE >= 28).
-doc("""
`execvp:cmd/2` may return an error tuple in case of an error, like the
executable does not exists, but `execvp:'cmd!'` always leaves the BEAM, even if
the command could not be executed.
""").
-endif.
-spec 'cmd!'(Command :: string() | binary(), Args :: [string() | binary()]) -> no_return().
'cmd!'(Cmd, Args) ->
  execvp:cmd(Cmd, Args),
  erlang:halt(1).

