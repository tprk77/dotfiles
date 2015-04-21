backupdate := $(shell date +%Y-%m-%d_%H-%M-%S)

all:
	if [ -f ~/.gitconfig ]; then mv ~/.gitconfig ~/.gitconfig.$(backupdate); fi
	cp gitconfig ~/.gitconfig
