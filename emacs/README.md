# Emacs Notes #

Just some notes about installing, configuring, and running Emacs.

## Conflicting Packages On Ubuntu 16.04 ##

For Ubuntu 16.04, some Debian packages were conflicting with the ELPA packages I
normally install. (The system packages seem to block the Emacs packages.) I'm
not sure how they got installed; maybe as suggested packages or dependencies of
the Emacs 24 package. Here's the bad packages:

  * dash-el
  * elpa-git-commit
  * elpa-magit-popup
  * elpa-magit
  * elpa-markdown-mode
  * elpa-with-editor
  * magit
  * yaml-mode

Remove them with:

    sudo apt remove --purge dash-el elpa-git-commit elpa-magit-popup \
         elpa-magit elpa-markdown-mode elpa-with-editor magit yaml-mode


<!-- Local Variables: -->
<!-- fill-column: 80 -->
<!-- End: -->
