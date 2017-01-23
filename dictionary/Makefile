backupdate := $(shell date +%Y-%m-%d_%H-%M-%S)

all:
	if [ -f ~/.aspell.en.pws ]; then mv ~/.aspell.en.pws ~/.aspell.en.pws.$(backupdate); fi
	cp aspell.en.pws ~/.aspell.en.pws
