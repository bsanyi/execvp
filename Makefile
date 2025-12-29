all:
	gcc -fPIC -shared \
	  -o execvp.so execvp.c \
	  -I`erl -eval 'io:format("~s", [code:root_dir()]), init:stop()' -noshell)`/usr/include
	erlc execvp.erl

clean:
	rm -f *.so *.beam erl_crash.dump

format:
	[ -x "`which clang-format`" ] || sudo apt install clang-format
	clang-format -i execvp.c


