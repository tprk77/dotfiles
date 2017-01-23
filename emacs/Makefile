backupdate := $(shell date +%Y-%m-%d_%H-%M-%S)
tmpconfdir := $(shell mktemp -d)

all:
	if [ -d ~/.emacs.d/ ]; then cd ~ && tar czf .emacs.d.$(backupdate).tar.gz .emacs.d/; fi
	if [ -f ~/.emacs.d/custom.el ]; then mv ~/.emacs.d/custom.el -t $(tmpconfdir); fi
	if [ -f ~/.emacs.d/irony/cdb-json-projects ]; then \
	    mv ~/.emacs.d/irony/cdb-json-projects -t $(tmpconfdir); fi
	if [ -f ~/.emacs.d/smex-items ]; then mv ~/.emacs.d/smex-items -t $(tmpconfdir); fi
	if [ -f ~/.emacs.d/ido.last ]; then mv ~/.emacs.d/ido.last -t $(tmpconfdir); fi
	rm -rf ~/.emacs.d/
	mkdir ~/.emacs.d/
	cp init.el -t ~/.emacs.d/
	cp -R plugins/ -t ~/.emacs.d/
	cp -R snippets/ -t ~/.emacs.d/
	if [ -f $(tmpconfdir)/custom.el ]; then mv $(tmpconfdir)/custom.el -t ~/.emacs.d/; else \
	    touch ~/.emacs.d/custom.el; fi
	if [ -f $(tmpconfdir)/cdb-json-projects ]; then mkdir -p ~/.emacs.d/irony; \
	    mv $(tmpconfdir)/cdb-json-projects -t ~/.emacs.d/irony/; fi
	if [ -f $(tmpconfdir)/smex-items ]; then mv $(tmpconfdir)/smex-items -t ~/.emacs.d/; fi
	if [ -f $(tmpconfdir)/ido.last ]; then mv $(tmpconfdir)/ido.last -t ~/.emacs.d/; fi
	emacs --refresh-packages
