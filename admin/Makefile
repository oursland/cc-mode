include /projects/python/python.org/ftp/pub/www.python.org/scripts/make.rules
ROOT_OFFSET =	emacs/cc-mode
HT2HTML = $(SCRIPTDIR)/ht2html/ht2html.py
HTFLAGS = -f -r ../.. -s PDOGenerator

SUBDIRS = 

LOCAL_INCLUDE = --include /cc-mode-html/* --include /cc-mode-5.25/* --include /cc-mode-manual/*

all: $(SUBDIRS)

local: $(TARGETS)

$(TARGETS): links.h compatlinks.h changelinks.h

always:
	@echo >/dev/null
