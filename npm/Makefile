backupdate := $(shell date +%Y-%m-%d_%H-%M-%S)

all:
	if [ -f ~/.npmrc ]; then mv ~/.npmrc ~/.npmrc.$(backupdate); fi
	cp npmrc ~/.npmrc
