include /projects/python/python.org/ftp/pub/www.python.org/scripts/make.rules
ROOT_OFFSET =	emacs/cc-mode

SUBDIRS = 

LOCAL_INCLUDE = --include /cc-mode-html/* --include /cc-mode-5.26/* --include /cc-mode-manual/* --include "*.ht" --include "*.h"

all: $(SUBDIRS)

local: $(TARGETS)

$(TARGETS): links.h compatlinks.h changelinks.h

always:
	@echo >/dev/null
