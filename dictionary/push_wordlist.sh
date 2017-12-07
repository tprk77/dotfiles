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

# Write wordlist to Aspell dictionary
echo "personal_ws-1.1 en 0" > "${ASPELL_DICT}"
cat "${WORDLIST_TXT}" >> "${ASPELL_DICT}"

# Write to Firefox dictionary
cat "${WORDLIST_TXT}" > "${FIREFOX_DICT}"
