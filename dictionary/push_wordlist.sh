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

# Write wordlist to Aspell dictionary
echo "personal_ws-1.1 en 0" > "${ASPELL_DICT}"
cat "${WORDLIST_TXT}" >> "${ASPELL_DICT}"

# Write to Firefox dictionary
if [ -n "${FIREFOX_DICT}" ]; then
    cat "${WORDLIST_TXT}" > "${FIREFOX_DICT}"
fi
