backupdate := $(shell date +%Y-%m-%d_%H-%M-%S)

all:
	if [ -f ~/.gdbinit ]; then mv ~/.gdbinit ~/.gdbinit.$(backupdate); fi
	cp gdbinit ~/.gdbinit
