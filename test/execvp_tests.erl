-module(execvp_tests).

-include_lib("eunit/include/eunit.hrl").

missing_command_test() ->
    Peer = start_peer(),
    Result = peer:call(Peer, execvp, cmd, ["i-am-definitely-not-a-valid-command", []]),
    peer:stop(Peer),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ?assertMatch({error, _NotFoundMessage}, Result).
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

non_executable_test() ->
    FileName = "/tmp/non_exec_test_file",
    {ok, Tmp} = file:open(FileName, [write]),
    ok = file:write(Tmp, "#!/bin/sh\necho hello\n"),
    ok = file:close(Tmp),
    ok = file:change_mode(FileName, 8#644),
    Peer = start_peer(),
    Result = peer:call(Peer, execvp, cmd, [FileName, []]),
    peer:stop(Peer),
    ok = file:delete(FileName),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ?assertMatch({error, _PermissionDeinedMessage}, Result).
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

exec_successful_call_test() ->
    FileName = "/tmp/temp_file_for_execvp_testing",
    {ok, Tmp} = file:open(FileName, [write]),
    file:close(Tmp),
    ?assertEqual(0, filelib:file_size(FileName)),
    Peer = start_peer(),
    Ref = erlang:monitor(process, Peer),
    spawn(
      fun() ->
        peer:call(Peer, execvp, cmd, ["bash", ["-c", "echo '1234.' > " ++ FileName]])
      end
    ),
    ok =
      receive
        {'DOWN', Ref, process, Peer, _Reason} -> ok
      after 5_000 ->
        error(timeout)
      end,
    Evidence = file:consult(FileName),
    ok = file:delete(FileName),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ?assertEqual(Evidence, {ok, [1234]}).
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_peer() ->
    {ok, Peer, _Node} =
      peer:start_link(#{
        name => peer:random_name(),
        connection => 0,
        longnames => true,
        env => [{"ERL_LIBS", string:join(code:get_path(), ":")}]
      }),
    [peer:call(Peer, code, add_path, [Path])
      || Path <- code:get_path()],
    Peer.

