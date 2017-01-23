backupdate := $(shell date +%Y-%m-%d_%H-%M-%S)

all:
	if [ -f /etc/sudoers.d/tperkins-nopasswd ]; then \
	    mv /etc/sudoers.d/tperkins-nopasswd /etc/sudoers.d/tperkins-nopasswd.$(backupdate)~; fi
	cp tperkins-nopasswd /etc/sudoers.d/tperkins-nopasswd
	chmod 0440 /etc/sudoers.d/tperkins-nopasswd
