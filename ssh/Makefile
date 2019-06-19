backupdate := $(shell date +%Y-%m-%d_%H-%M-%S)

DECRYPTED_DIR := secret_files/decrypted
PUBLIC_KEY_DIR := public_keys

CONFIG_FILE := $(DECRYPTED_DIR)/config

HOME_PRIVATE_KEY_FILE := $(DECRYPTED_DIR)/id-ed25519-github-home
HOME_PUBLIC_KEY_FILE := $(PUBLIC_KEY_DIR)/id-ed25519-github-home.pub

WORK_PRIVATE_KEY_FILE := $(DECRYPTED_DIR)/id-ed25519-github-work
WORK_PUBLIC_KEY_FILE := $(PUBLIC_KEY_DIR)/id-ed25519-github-work.pub

T77_PRIVATE_KEY_FILE := $(DECRYPTED_DIR)/id-ed25519-t77
T77_PUBLIC_KEY_FILE := $(PUBLIC_KEY_DIR)/id-ed25519-t77.pub

DEST_SSH_DIR := ${HOME}/.ssh
SSH_BACKUP_DIR := $(DEST_SSH_DIR)/backups.$(backupdate)

all:
	@echo ""
	@echo "Remeber to decrypt the files! Run: ./decrypt_files.sh"
	@echo ""
	test -f "$(CONFIG_FILE)"
	test -f "$(HOME_PRIVATE_KEY_FILE)"
	test -f "$(HOME_PUBLIC_KEY_FILE)"
	test -f "$(WORK_PRIVATE_KEY_FILE)"
	test -f "$(WORK_PUBLIC_KEY_FILE)"
	mkdir -p -m 700 "$(DEST_SSH_DIR)"
	mkdir -p -m 700 "$(SSH_BACKUP_DIR)"
	rsync --checksum -b --backup-dir="$(SSH_BACKUP_DIR)" \
	    "$(CONFIG_FILE)" "$(DEST_SSH_DIR)"
	rsync --checksum -b --backup-dir="$(SSH_BACKUP_DIR)" \
	    "$(HOME_PRIVATE_KEY_FILE)" "$(DEST_SSH_DIR)"
	rsync --checksum -b --backup-dir="$(SSH_BACKUP_DIR)" \
	    "$(HOME_PUBLIC_KEY_FILE)" "$(DEST_SSH_DIR)"
	rsync --checksum -b --backup-dir="$(SSH_BACKUP_DIR)" \
	    "$(WORK_PRIVATE_KEY_FILE)" "$(DEST_SSH_DIR)"
	rsync --checksum -b --backup-dir="$(SSH_BACKUP_DIR)" \
	    "$(WORK_PUBLIC_KEY_FILE)" "$(DEST_SSH_DIR)"
	rsync --checksum -b --backup-dir="$(SSH_BACKUP_DIR)" \
	    "$(T77_PRIVATE_KEY_FILE)" "$(DEST_SSH_DIR)"
	rsync --checksum -b --backup-dir="$(SSH_BACKUP_DIR)" \
	    "$(T77_PUBLIC_KEY_FILE)" "$(DEST_SSH_DIR)"
	rmdir --ignore-fail-on-non-empty "$(SSH_BACKUP_DIR)"
