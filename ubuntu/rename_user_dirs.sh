#!/bin/bash

# This script takes the annoyingly names default dirs, such as
# `Documents` and `Downloads`, and renames them to be easy to type and
# more consistent, like `docs` and `dls`. For best results, run on a
# new system I guess.

BETTER_DESKTOP_NAME="desktop"
BETTER_DOCUMENTS_NAME="docs"
BETTER_DOWNLOADS_NAME="dls"
BETTER_MUSIC_NAME="music"
BETTER_PICTURES_NAME="pics"
BETTER_PUBLIC_NAME="public"
BETTER_TEMPLATES_NAME="templates"
BETTER_VIDEOS_NAME="vids"

mv -n ~/Desktop ~/${BETTER_DESKTOP_NAME}
mv -n ~/Documents ~/${BETTER_DOCUMENTS_NAME}
mv -n ~/Downloads ~/${BETTER_DOWNLOADS_NAME}
mv -n ~/Music ~/${BETTER_MUSIC_NAME}
mv -n ~/Pictures ~/${BETTER_PICTURES_NAME}
mv -n ~/Public ~/${BETTER_PUBLIC_NAME}
mv -n ~/Templates ~/${BETTER_TEMPLATES_NAME}
mv -n ~/Videos ~/${BETTER_VIDEOS_NAME}

if [ -f ~/.config/user-dirs.dirs ]; then
    sed -e "s;Desktop;${BETTER_DESKTOP_NAME};" \
        -e "s;Documents;${BETTER_DOCUMENTS_NAME};" \
        -e "s;Downloads;${BETTER_DOWNLOADS_NAME};" \
        -e "s;Music;${BETTER_MUSIC_NAME};" \
        -e "s;Pictures;${BETTER_PICTURES_NAME};" \
        -e "s;Public;${BETTER_PUBLIC_NAME};" \
        -e "s;Templates;${BETTER_TEMPLATES_NAME};" \
        -e "s;Videos;${BETTER_VIDEOS_NAME};" \
        -i ~/.config/user-dirs.dirs
fi

if [ -f ~/.config/gtk-3.0/bookmarks ]; then
    sed -e "s;Desktop;${BETTER_DESKTOP_NAME};" \
        -e "s;Documents;${BETTER_DOCUMENTS_NAME};" \
        -e "s;Downloads;${BETTER_DOWNLOADS_NAME};" \
        -e "s;Music;${BETTER_MUSIC_NAME};" \
        -e "s;Pictures;${BETTER_PICTURES_NAME};" \
        -e "s;Public;${BETTER_PUBLIC_NAME};" \
        -e "s;Templates;${BETTER_TEMPLATES_NAME};" \
        -e "s;Videos;${BETTER_VIDEOS_NAME};" \
        -i ~/.config/gtk-3.0/bookmarks
fi

echo "You need to logout and login to finish renaming user directories!"
