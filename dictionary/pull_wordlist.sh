#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail

SCRIPT_DIR="$(dirname "$(readlink -f "${0}")")"

WORDLIST_TXT="${SCRIPT_DIR}/wordlist.txt"
ASPELL_DICT="~/.aspell.en.pws"
FIREFOX_DICT="~/.mozilla/firefox/*.default/persdict.dat"

# Convert these to actual filenames
ASPELL_DICT=$(eval "ls -1 ${ASPELL_DICT}" | head -n 1)
FIREFOX_DICT=$(eval "ls -1 ${FIREFOX_DICT}" | head -n 1)

# Dump all but the first line of the Aspell dictionary
tail -n +2 "${ASPELL_DICT}" >> "${WORDLIST_TXT}"

# Dump Firefox dictionary
cat "${FIREFOX_DICT}" >> "${WORDLIST_TXT}"

# Sort and make unique
sort "${WORDLIST_TXT}" | uniq | tee "${WORDLIST_TXT}" &>/dev/null
