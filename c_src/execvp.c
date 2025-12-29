#include <erl_nif.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>

static void reset_terminal() {
  struct termios tio;

  if (tcgetattr(STDOUT_FILENO, &tio) == 0) {
    tio.c_oflag |= (OPOST | ONLCR);
    tcsetattr(STDOUT_FILENO, TCSANOW, &tio);
  }

  if (tcgetattr(STDIN_FILENO, &tio) == 0) {
    tio.c_oflag |= (OPOST | ONLCR);
    tcsetattr(STDIN_FILENO, TCSANOW, &tio);
  }
}

static int term_to_cstring(ErlNifEnv *env, ERL_NIF_TERM term, char **out) {
  ErlNifBinary bin;

  if (enif_inspect_binary(env, term, &bin)) {
    *out = strndup((char *)bin.data, bin.size);
    return 1;
  }

  unsigned len;
  if (enif_get_list_length(env, term, &len)) {
    *out = malloc(len + 1);
    if (!*out) {
      return 0;
    }
    if (!enif_get_string(env, term, *out, len + 1, ERL_NIF_LATIN1)) {
      return 0;
    }
    return 1;
  }

  return 0;
}

static ERL_NIF_TERM cmd_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  if (argc != 2) {
    return enif_make_badarg(env);
  }

  char *cmd = NULL;

  if (!term_to_cstring(env, argv[0], &cmd)) {
    return enif_make_badarg(env);
  }

  unsigned arg_count;
  if (!enif_get_list_length(env, argv[1], &arg_count)) {
    free(cmd);
    return enif_make_badarg(env);
  }

  char **exec_argv = malloc(sizeof(char *) * (arg_count + 2));
  if (!exec_argv) {
    free(cmd);
    return enif_make_badarg(env);
  }

  exec_argv[0] = cmd;

  ERL_NIF_TERM list = argv[1];
  for (unsigned i = 0; i < arg_count; i++) {
    ERL_NIF_TERM head;
    enif_get_list_cell(env, list, &head, &list);

    if (!term_to_cstring(env, head, &exec_argv[i + 1])) {
      return enif_make_badarg(env);
    }
  }

  exec_argv[arg_count + 1] = NULL;

  if (isatty(STDOUT_FILENO)) {
    reset_terminal();
  }
  execvp(cmd, exec_argv);

  ErlNifBinary reason;
  enif_alloc_binary(strlen(strerror(errno)), &reason);
  memcpy(reason.data, strerror(errno), reason.size);

  ERL_NIF_TERM error_reason_tuple =
    enif_make_tuple2(env,
      enif_make_atom(env, "error"),
      enif_make_binary(env, &reason)
    );

  return error_reason_tuple;
}

static ErlNifFunc nif_funcs[] = {{"cmd", 2, cmd_2}};

ERL_NIF_INIT(execvp, nif_funcs, NULL, NULL, NULL, NULL)
