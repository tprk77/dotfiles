backupdate := $(shell date +%Y-%m-%d_%H-%M-%S)

all:
	if [ -d ~/.emacs.d/ ]; then cd ~ && tar czf .emacs.d.$(backupdate).tar.gz .emacs.d/; fi
	if [ -f ~/.emacs.d/custom.el ]; then mv ~/.emacs.d/custom.el /tmp/custom.el; fi
	rm -rf ~/.emacs.d/
	mkdir ~/.emacs.d/
	cp init.el -t ~/.emacs.d/
	cp -R plugins/ -t ~/.emacs.d/
	cp -R snippets/ -t ~/.emacs.d/
	if [ -f /tmp/custom.el ]; then mv /tmp/custom.el ~/.emacs.d/custom.el; else touch ~/.emacs.d/custom.el; fi
	emacs --refresh-packages
