#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail
IFS=$'\n\t'

readonly SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"

# Changes for each run
readonly CRYPT_TEMP_DIR="$(mktemp -d)"
trap "encrypt_cleanup" EXIT

readonly DECRYPTED_DIR="${SCRIPT_DIR}/secret_files/decrypted"
readonly ENCRYPTED_DIR="${SCRIPT_DIR}/secret_files/encrypted"

readonly ENCRYPTED_PASSWORD_FILE="${SCRIPT_DIR}/secret_files/password.txt.asc"
readonly DECRYPTED_PASSWORD_FILE="$(mktemp -p "${CRYPT_TEMP_DIR}")"

# Use --no-use-agent with older GPG like 1.4
readonly GPG_VERSION="$(gpg --version | head -n 1 | egrep -o "[0-9]\.[0-9]\.[0-9]{1,2}")"
readonly GPG_MAJOR_VERSION="$(echo "${GPG_VERSION}" | egrep -o "^[0-9]")"
readonly GPG_AGENT_ARG="$([ "${GPG_MAJOR_VERSION}" -lt "2" ] && echo "--no-use-agent")"

function encrypt_cleanup {
    rm -rf "${CRYPT_TEMP_DIR}"
}

function decrypted_password {
    encrypted_password_file="${1}"
    decrypted_password_file="${2}"
    if [ ! -f "${encrypted_password_file}" ]; then
        echo "No password file!" 1>&2
        return 1
    fi
    echo -n "" > "${decrypted_password_file}"
    chmod 600 "${decrypted_password_file}"
    gpg --yes --armor --decrypt --cipher-algo AES256 \
        -o "${decrypted_password_file}" "${encrypted_password_file}"
}

function decrypt_file {
    encrypted_file="${1}"
    decrypted_file="${2}"
    password_file="${3}"
    decrypted_dirname="$(dirname "${decrypted_file}")"
    old_umask="$(umask)"
    umask 077 && mkdir -p "${decrypted_dirname}" && umask "${old_umask}"
    echo -n "" > "${decrypted_file}"
    chmod 600 "${decrypted_file}"
    gpg --batch --yes --quiet --armor ${GPG_AGENT_ARG} \
        --decrypt --cipher-algo AES256 \
        --passphrase-file "${password_file}" \
        -o "${decrypted_file}" "${encrypted_file}"
}

decrypted_password "${ENCRYPTED_PASSWORD_FILE}" "${DECRYPTED_PASSWORD_FILE}"

readonly encrypted_files="$(find "${ENCRYPTED_DIR}" -type f | LC_ALL=C sort)"

for encrypted_file in ${encrypted_files}; do
    partial_path_mask="$(echo "${ENCRYPTED_DIR}/" | sed -e "s;.;.;g")"
    partial_path_file_with_asc="$(echo "${encrypted_file}" | sed -e "s;^${partial_path_mask};;")"
    partial_path_file="$(echo "${partial_path_file_with_asc}" | sed -e "s;\.asc$;;")"
    decrypted_file="${DECRYPTED_DIR}/${partial_path_file}"
    decrypt_file "${encrypted_file}" "${decrypted_file}" "${DECRYPTED_PASSWORD_FILE}"
done

exit 0
