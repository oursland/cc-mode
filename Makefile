EMACS=emacs
VERSION=5.28

COREFILES=\
 cc-align.el \
 cc-bytecomp.el \
 cc-cmds.el \
 cc-compat.el \
 cc-defs.el \
 cc-engine.el \
 cc-langs.el \
 cc-menus.el \
 cc-mode.el \
 cc-styles.el \
 cc-vars.el \
 cc-mode.texi

EXTRAFILES=\
 ANNOUNCEMENT \
 MANIFEST \
 README \
 NEWS \
 COPYING \
 cc-guess.el \
 cc-lobotomy.el \
 cc-mode-19.el

all: bytecomp

force:

bytecomp:
	$(EMACS) -batch -q -no-site-file -f batch-byte-compile *.el

release: docs dists

distdir:
	@test -d dist || mkdir dist

docs: distdir info html dvi ps
	tar cf - cc-mode.info* | gzip -c > dist/cc-mode.info.tar.gz
	gzip -c cc-mode.html > dist/cc-mode.html.gz
	gzip -c cc-mode.dvi > dist/cc-mode.dvi.gz
	gzip -c cc-mode.ps > dist/cc-mode.ps.gz
	gzip -c cc-mode.rev.ps > dist/cc-mode.rev.ps.gz

info:
	makeinfo cc-mode.texi

html:
	makeinfo --html cc-mode.texi

dvi:
	test -d texi || mkdir texi
	cd texi && texi2dvi ../cc-mode.texi
	mv texi/cc-mode.dvi .

ps: dvi
	dvips -o cc-mode.ps cc-mode.dvi
	dvips -r -o cc-mode.rev.ps cc-mode.dvi

dists: xemacsdist emacsdist dist

# The Emacs maintainers' release.
emacsdist:
	gzip -c ChangeLog > ChangeLog.gz
	$(MAKE) "FILES=ChangeLog.gz $(COREFILES)" \
	        TARGZFILE=cc-mode-$(VERSION).emacs.tar.gz distcommon

# The XEmacs maintainers' release.  They don't care about the ChangeLog.
xemacsdist:
	$(MAKE) "FILES=$(COREFILES)" \
	        TARGZFILE=cc-mode-$(VERSION).xemacs.tar.gz distcommon

# The standalone release.
dist: force
	$(MAKE) "FILES=$(EXTRAFILES) $(COREFILES)" \
	        TARGZFILE=cc-mode-$(VERSION).tar.gz distcommon

# Used internally with FILES and TARGZFILE.
distcommon: distdir
	$(RM) -r cc-mode-$(VERSION)
	mkdir cc-mode-$(VERSION)
	chmod 755 cc-mode-$(VERSION)
	cp $(FILES) cc-mode-$(VERSION)
	chmod 644 cc-mode-$(VERSION)/*
	tar cf - cc-mode-$(VERSION) | gzip -c > dist/$(TARGZFILE)

clean:
	$(RM) *.elc
	$(RM) ChangeLog.gz
	for d in cc-mode-* "texi"; do \
	  if test -d $$d; then $(RM) -r $$d; else :; fi; \
	done

spotless: clean
	$(RM) cc-mode.info*
	$(RM) cc-mode.html cc-mode.dvi cc-mode*.ps
	$(RM) -r dist
