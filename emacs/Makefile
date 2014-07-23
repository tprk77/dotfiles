backupdate := $(shell date +%Y-%m-%d_%H-%M-%S)

all:
	cd ~ && tar czf .emacs.d.$(backupdate).tar.gz .emacs.d/
	rm -rf ~/.emacs.d/
	mkdir ~/.emacs.d/
	cp init.el -t ~/.emacs.d/
	cp -R use-package/ -t ~/.emacs.d/
