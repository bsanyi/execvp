CFLAGS ?= -fPIC -O3 -Wall
LDFLAGS ?= -shared
SOURCE = execvp.c
TARGET = execvp.so
ERL ?= erl
ERL_INCLUDE := $(shell $(ERL) -eval 'io:format("~s",[code:root_dir()]), init:stop()' -noshell)/usr/include

all:
	$(CC) $(CFLAGS) -I$(ERL_INCLUDE) -o ../priv/$(TARGET) $(SOURCE) $(LDFLAGS)

clean:
	rm -f ../priv/$(TARGET)

format:
	[ -x "`which clang-format`" ] || sudo apt install clang-format
	clang-format -i $(SOURCE)

