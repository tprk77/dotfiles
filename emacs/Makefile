backupdate := $(shell date +%Y-%m-%d_%H-%M-%S)

all:
	cd ~ && tar czf .emacs.d.$(backupdate).tar.gz .emacs.d/
	if [ -f ~/.emacs.d/custom.el ]; then mv ~/.emacs.d/custom.el /tmp/custom.el; fi
	rm -rf ~/.emacs.d/
	mkdir ~/.emacs.d/
	cp init.el -t ~/.emacs.d/
	cp -R use-package/ -t ~/.emacs.d/
	cp -R plugins/ -t ~/.emacs.d/
	cp -R snippets/ -t ~/.emacs.d/
	if [ -f /tmp/custom.el ]; then mv /tmp/custom.el ~/.emacs.d/custom.el; else touch ~/.emacs.d/custom.el; fi
	emacs --refresh-packages
