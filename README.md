# execvp

`execvp` is a small library that allows a running BEAM instance to hand over
control to another OS process. Just like `exec` replaces the current process
with another command in bash. This is particularly useful for **escripts or CLI
tools** that need to replace themselves with another process as their final
step. This means when that other process exits, execution does not return to
the BEAM.

## Erlang:

```erlang
  execvp:cmd("git", ["help", "rebase"]).
  execvp:'cmd!'("git", ["help", "rebase"]).
```

## Elixir:

```elixir
  :execvp.cmd("git", ["help", "rebase"])
  :execvp.cmd!("git", ["help", "rebase"])
```

The difference between `cmd` and `cmd!` is that the formar may return an error
tuple in case of an error (like a non existent command), but the latter always
crashes the entire BEAM in case of an error.

> ⚠️  **Warning:** Calling `execvp:cmd` or `cmd!` **replaces the current BEAM
> process**.  Any further Erlang/Elixir code will not run. Concurrent BEAM
> processes are brutally killed. Use this only if you know what you are doing.
> Otherwise consider using ports or `os:cmd`.

---

## Installation

### Mix

Add this to your `mix.exs` dependencies:

```elixir
  def deps do
    [
      {:execvp, "~> 1.0.0"}
    ]
  end
```

Then run:

```bash
  mix deps.get
  mix deps.compile
```

### Rebar3

Add this to your rebar.config:

```erlang
  {deps, [
    {execvp, "~> 1.0.0"}
  ]}.
```

Compile with:

```bash
  rebar3 compile
```

The NIF is built automatically and placed in `priv/`.

## Usage

### Erlang

```erlang
  % Replace current VM with `ls -l /etc`
  execvp:cmd("ls", ["-l", "/etc"]).
```

### Elixir

```elixir
  # Replace current VM with `ls -l /etc`
  :execvp.cmd("ls", ["-l", "/etc"])
```

## Behavior

* On success, the BEAM VM is replaced; your script ends.
* On failure (e.g., command not found), returns:

```erlang
  {error, Reason} % Reason is a binary describing the failure
```

Examples:

```erlang
  bash$ rebar3 shell
  erl> execvp:cmd("nosuchcommand", []).
  {error, <<"No such file or directory">>}
  erl> execvp:cmd("echo", ["1", "2", "3"]).
  1 2 3
  bash$ _
```

```elixir
  bash$ iex -S mix
  iex> :execvp.cmd("nosuchcommand", [])
  {:error, "No such file or directory"}
  erl> :execvp.cmd("echo", ["1", "2", "3"])
  1 2 3
  bash$ _
```

## Intended Use Case

`execvp` is designed for:

* Designed for escripts that need to execute a final system command.
* May be useful in CLI tools written in Erlang or Elixir that wish to "handoff"
  to another executable without leaving a lingering BEAM process.
* Intended use is in scripts where the BEAM process is only a bootstrap or
  wrapper.
* It is not intended for general runtime use inside a long-running VM.
* Once executed successfully, `execvp:cmd/2` cannot return.
* Terminal settings are automatically restored to "sane" mode before executing
  the external command.
* Parts of your code _after_ `execvp:cmd` may not run, unless the execution of
  the external command was not possible (missing executable, no execution
  right, etc.).

## Development

* `c_src/` contains the NIF source code.
* `src/execvp.erl` contains the Erlang wrapper module.
* The `.so` NIF is built in `priv/` automatically by Rebar3.
* Execute `rebar3 eunit` to run the tests.

