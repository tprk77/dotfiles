#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail

SCRIPT_DIR="$(dirname "$(readlink -f "${0}")")"

WORDLIST_TXT="${SCRIPT_DIR}/wordlist.txt"
ASPELL_DICT="${HOME}/.aspell.en.pws"

# For Firefox, assume we only care about the first profile
FIREFOX_PROFILE=$(
    find "${HOME}/.mozilla/firefox/" -maxdepth 1 -type d -name "*.default" | head -n 1)
FIREFOX_DICT=$(if [ -n "${FIREFOX_PROFILE}" ]; then echo "${FIREFOX_PROFILE}/persdict.dat"; fi)

# Dump all but the first line of the Aspell dictionary
if [ -f "${ASPELL_DICT}" ]; then
    tail -n +2 "${ASPELL_DICT}" >> "${WORDLIST_TXT}"
fi

# Dump Firefox dictionary
if [ -n "${FIREFOX_DICT}" -a -f "${FIREFOX_DICT}" ]; then
    cat "${FIREFOX_DICT}" >> "${WORDLIST_TXT}"
fi

# Sort and make unique
LC_ALL="C" sort "${WORDLIST_TXT}" -u | tee "${WORDLIST_TXT}" &>/dev/null
