include /projects/python/python.org/ftp/pub/www.python.org/scripts/make.rules

SUBDIRS = cc-mode python-mode

all: $(SUBDIRS)

local: $(TARGETS)

cc-mode: always
	(cd cc-mode; $(MAKE))

python-mode: always
	(cd python-mode; $(MAKE))

$(TARGETS): links.h

always:
	@echo >/dev/null
