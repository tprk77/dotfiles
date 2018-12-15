#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail
IFS=$'\n\t'

readonly SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"

readonly DECRYPTED_DIR="${SCRIPT_DIR}/secret_files/decrypted"

readonly HOME_SECRET_KEY_FILE="${DECRYPTED_DIR}/id-ed25519-github-home"
readonly HOME_PUBLIC_KEY_FILE="${HOME_SECRET_KEY_FILE}.pub"

readonly WORK_SECRET_KEY_FILE="${DECRYPTED_DIR}/id-ed25519-github-work"
readonly WORK_PUBLIC_KEY_FILE="${WORK_SECRET_KEY_FILE}.pub"

readonly DEST_PUBLIC_KEY_DIR="${SCRIPT_DIR}/public_keys"

old_umask="$(umask)"
umask 077 && mkdir -p "${DECRYPTED_DIR}" && umask "${old_umask}"
(yes || true) | ssh-keygen -t ed25519 -N "" -C "tim@home" -f "${HOME_SECRET_KEY_FILE}"
(yes || true) | ssh-keygen -t ed25519 -N "" -C "tim@work" -f "${WORK_SECRET_KEY_FILE}"

mkdir -p "${DEST_PUBLIC_KEY_DIR}"
mv -t "${DEST_PUBLIC_KEY_DIR}" "${HOME_PUBLIC_KEY_FILE}" "${WORK_PUBLIC_KEY_FILE}"
