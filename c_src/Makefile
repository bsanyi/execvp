CFLAGS ?= -fPIC -O3 -Wall

ifeq ($(shell uname),Darwin)
	LDFLAGS += -dynamiclib -undefined dynamic_lookup
else
	LDFLAGS += -shared
endif

SOURCE = execvp.c
TARGET = execvp.so
ERL ?= erl
ERL_INCLUDE := $(shell $(ERL) -eval 'io:format("~s",[code:root_dir()]), init:stop()' -noshell)/usr/include

all:
	mkdir -p ../priv/
	$(CC) $(CFLAGS) $(LDFLAGS) -I$(ERL_INCLUDE) -o ../priv/$(TARGET) $(SOURCE)

clean:
	rm -f ../priv/$(TARGET)

format:
	[ -x "`which clang-format`" ] || sudo apt install clang-format
	clang-format -i $(SOURCE)

