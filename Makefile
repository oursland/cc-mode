EMACS=emacs
VERSION=5.31

COREFILES=\
 NEWS \
 cc-align.el \
 cc-bytecomp.el \
 cc-cmds.el \
 cc-compat.el \
 cc-defs.el \
 cc-engine.el \
 cc-fonts.el \
 cc-langs.el \
 cc-menus.el \
 cc-mode.el \
 cc-styles.el \
 cc-vars.el \
 cc-mode.texi

EXTRAFILES=\
 MANIFEST \
 README \
 COPYING \
 cc-guess.el \
 cc-lobotomy.el \
 cc-fix.el

all: bytecomp

force:

bytecomp:
	$(EMACS) -batch -q -no-site-file -f batch-byte-compile cc-*.el

derived-mode-ex.elc: force
	test -f derived-mode-ex.el || ln -s admin/derived-mode-ex.el .
	EMACSLOADPATH=".${EMACSLOADPATH:+:}${EMACSLOADPATH}" $(EMACS) -batch -q -no-site-file -f batch-byte-compile derived-mode-ex.el

release: docs dists

distdir:
	@test -d dist || mkdir dist

docs: distdir info html dvi ps
	tar cf - cc-mode.info* | gzip -c > dist/cc-mode.info.tar.gz
	cd html && \
	$(RM) -r cc-mode-$(VERSION) && \
	mkdir cc-mode-$(VERSION) && \
	chmod 755 cc-mode-$(VERSION) && \
	cp *.html cc-mode-$(VERSION) && \
	chmod 644 cc-mode-$(VERSION)/* && \
	tar cf - cc-mode-$(VERSION) | gzip -c > ../dist/cc-mode.html.tar.gz
	gzip -c cc-mode.dvi > dist/cc-mode.dvi.gz
	gzip -c cc-mode.ps > dist/cc-mode.ps.gz
	gzip -c cc-mode.rev.ps > dist/cc-mode.rev.ps.gz

info:
	makeinfo cc-mode.texi

html:
	makeinfo --html -o html cc-mode.texi

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

test:
	$(EMACS) -q -batch -no-site-file -l tests/000tests.el -f do-all-tests

clean:
	$(RM) *.elc
	$(RM) ChangeLog.gz
	for d in cc-mode-* "texi" html/cc-mode-*; do \
	  if test -d $$d; then $(RM) -r $$d; else :; fi; \
	done

spotless: clean
	$(RM) cc-mode.info*
	$(RM) -r html
	$(RM) cc-mode.dvi cc-mode*.ps
	$(RM) -r dist

rcs2log:
	@rcs2log -u "mast	Martin Stjernholm	bug-cc-mode@gnu.org" \
		-u "acmacm	Alan Mackenzie	bug-cc-mode@gnu.org" \
		-l 74 cc-*.el
