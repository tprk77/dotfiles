# TODO #

Stuff I had to remove from 16.04, to get up-to-date ELPA packages.

> 2017-01-23 16:26:13 remove magit:all 2.5.0-2 <none>
> 2017-01-23 16:26:13 remove elpa-magit:all 2.5.0-2 <none>
> 2017-01-23 16:26:13 remove elpa-git-commit:all 2.5.0-2 <none>
> 2017-01-23 16:26:14 remove elpa-magit-popup:all 2.5.0-2 <none>
> 2017-01-23 16:26:14 remove elpa-markdown-mode:all 2.1-1 <none>
> 2017-01-23 16:26:14 remove elpa-with-editor:all 2.5.0-1 <none>
> 2017-01-23 16:27:51 remove dash-el:all 2.12.1-1 <none>
> 2017-01-23 16:27:52 remove yaml-mode:all 0.0.9-2 <none>
> 2017-01-23 16:28:13 remove emacs:all 46.1 <none>
> 2017-01-23 16:31:26 remove emacs24-el:all 24.5+1-6ubuntu1 <none>

...

tperkins@starscream:~/dotfiles$ dpkg -l | grep -i emacs
ii  dash-el              2.12.1-1        all             Modern list manipulation library for Emacs
ii  elpa-magit           2.5.0-2         all             Emacs interface for Git
ii  elpa-with-editor     2.5.0-1         all             Call program using Emacs as $EDITOR
ii  emacs                46.1            all             GNU Emacs editor (metapackage)
ii  emacs-goodies-el     35.12ubuntu2    all             Miscellaneous add-ons for Emacs
ii  emacs24              24.5+1-6ubuntu1 amd64           GNU Emacs editor (with GTK+ GUI support)
ii  emacs24-bin-common   24.5+1-6ubuntu1 amd64           GNU Emacs editor's shared, architecture depen
ii  emacs24-common       24.5+1-6ubuntu1 all             GNU Emacs editor's shared, architecture indep
ii  emacs24-common-non-d 24.4+1-2        all             GNU Emacs common non-DFSG items, including th
ii  emacsen-common       2.0.8           all             Common facilities for all emacsen
ii  yaml-mode            0.0.9-2         all             emacs mode for YAML files

...
 
 The following packages will be REMOVED:
  dash-el elpa-git-commit elpa-magit elpa-magit-popup elpa-with-editor
  emacs-goodies-el magit yaml-mode elpa-markdown-mode
  
