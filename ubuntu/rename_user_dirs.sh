#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail
IFS=$'\n\t'

# This script takes the annoyingly names default dirs, such as
# `Documents` and `Downloads`, and renames them to be easy to type and
# more consistent, like `docs` and `dls`. For best results, run on a
# new system I guess.

OLD_DESKTOP_NAME="Desktop"
OLD_DOCUMENTS_NAME="Documents"
OLD_DOWNLOADS_NAME="Downloads"
OLD_MUSIC_NAME="Music"
OLD_PICTURES_NAME="Pictures"
OLD_PUBLIC_NAME="Public"
OLD_TEMPLATES_NAME="Templates"
OLD_VIDEOS_NAME="Videos"

OLD_NAMES=(${OLD_DESKTOP_NAME} ${OLD_DOCUMENTS_NAME} ${OLD_DOWNLOADS_NAME} ${OLD_MUSIC_NAME}
           ${OLD_PICTURES_NAME} ${OLD_PUBLIC_NAME} ${OLD_TEMPLATES_NAME} ${OLD_VIDEOS_NAME})

BETTER_DESKTOP_NAME="desktop"
BETTER_DOCUMENTS_NAME="docs"
BETTER_DOWNLOADS_NAME="dls"
BETTER_MUSIC_NAME="music"
BETTER_PICTURES_NAME="pics"
BETTER_PUBLIC_NAME="public"
BETTER_TEMPLATES_NAME="templates"
BETTER_VIDEOS_NAME="vids"

BETTER_NAMES=(${BETTER_DESKTOP_NAME} ${BETTER_DOCUMENTS_NAME} ${BETTER_DOWNLOADS_NAME}
              ${BETTER_MUSIC_NAME} ${BETTER_PICTURES_NAME} ${BETTER_PUBLIC_NAME}
              ${BETTER_TEMPLATES_NAME} ${BETTER_VIDEOS_NAME})

echo "Moving home directories"
for index in ${!OLD_NAMES[*]}; do
    old_name=${OLD_NAMES[${index}]}
    better_name=${BETTER_NAMES[${index}]}
    [ -d "${HOME}/${old_name}" ] && mv -n "${HOME}/${old_name}" -T "${HOME}/${better_name}"
    # Try to delete empty directories if we couldn't move them
    [ -d "${HOME}/${old_name}" ] && rmdir --ignore-fail-on-non-empty "${HOME}/${old_name}"
done

echo "Updating ~/.config/user-dirs.dirs"
for index in ${!OLD_NAMES[*]}; do
    old_name=${OLD_NAMES[${index}]}
    better_name=${BETTER_NAMES[${index}]}
    sed -e "s;\${HOME}/${old_name};\${HOME}/${better_name};" \
        -i "${HOME}/.config/user-dirs.dirs"
done

echo "Updating ~/.config/gtk-3.0/bookmarks"
for index in ${!OLD_NAMES[*]}; do
    old_name=${OLD_NAMES[${index}]}
    better_name=${BETTER_NAMES[${index}]}
    sed -e "s;${HOME}/${old_name};${HOME}/${better_name};" \
        -i "${HOME}/.config/gtk-3.0/bookmarks"
done

echo "You need to login again for changes to take effect!"
