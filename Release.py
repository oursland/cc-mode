#! /usr/bin/env python

"""Manage releases of CC Mode.

Usage: %(program)s [-b] [-t|-T] [-p] [-d] [-h] revnum

Where:

    --bump
    -b
            set version number of versioned files to minor revision number

    --tag 
    -t
            tag all release files with minor revision number

    --TAG
    -T
            like --tag, but relocates any existing tag.  See
	    `cvs tag -F'.  Only one of --tag or --TAG can be given on the
	    command line.

    --package
    -p
            create the distribution packages

    --docs
    -d      - create the documentation packages

    --help
    -h      - this help message

    revnum is required and is the minor revision number of this release
    (e.g. for release 5.20, revnum would be `20').

"""

import sys
import os
import string
import re
import getopt

program = sys.argv[0]

def usage(status):
    print __doc__ % globals()
    sys.exit(status)


RELEASE = None
RELEASE_NAME = ''

# for bumping
VERSIONED_FILES = [
    # file, prefix -- not grouped! trailing space is significant
    ('ANNOUNCEMENT', 'CC Mode Version '),
    ('MANIFEST', 'Manifest for CC Mode '),
    ('README', 'README for CC Mode '),
    ('cc-mode.el', r'\(defconst c-version "'),
    ('cc-mode.texi', r'(@center @titlefont\{CC Mode|version) '),
    ]

# list of files to go to X/Emacs maintainers
FILES = [
    'cc-align.el',
    'cc-cmds.el',
    'cc-compat.el',
    'cc-defs.el',
    'cc-engine.el',
    'cc-langs.el',
    'cc-menus.el',
    'cc-mode.el',
    'cc-styles.el',
    'cc-vars.el',
    'cc-mode.texi',
    ]

FATRELEASE_FILES = [
    # should not contain ChangeLog.  That's packaged separately due to its
    # size.  ChangeLog differences go to RMS, but those must be handcrafted.
    'cc-guess.el',
    'cc-lobotomy.el',
    'cc-mode-19.el',
    'cc-make.el',
    'ANNOUNCEMENT',
    'MANIFEST',
    'README',
    'NEWS',
    ]

ALL_FILES = FILES + FATRELEASE_FILES



def tag_release(revnum, retag):
    # first verify that the ChangeLog is up-to-date
    fp = open("ChangeLog")
    cre = re.compile('^[ \t]*[*] Release 5.(?P<rev>[0-9]{2})')
    while 1:
	line = fp.readline()
	if line == '':
	    print '*****WARNING*****'
	    print 'Could not find a Release tag in the ChangeLog!'
	    sys.exit(1)
	mo = cre.match(line)
	if mo:
	    docorev = mo.group('rev')
	    if docorev <> revnum:
		print '*****WARNING*****'
		print 'ChangeLog has not been updated... exiting!'
		sys.exit(1)
	    break
    fp.close()
    relname = '"Release_5_' + revnum + '"'
    cvscmd = 'cvs tag'
    option = ''
    if retag:
	option = '-F'
    os.system('%s %s %s' % (cvscmd, option, relname))



def pkg_release(revnum):
    dir = 'cc-mode-5.' + revnum
    os.mkdir(dir)
    # rwxrwsr-x
    os.chmod(dir, 02775)
    # first, make the X/Emacs maintainers' release
    for f in FILES:
	os.system('cp %s %s' % (f, dir))
	# force permissions, though umask ought to cover this
	os.chmod(os.path.join(dir, f), 0664)
    # rmslb == RMS + SLB :-)
    os.system('tar cvf - %s | gzip -c > Distrib/%s.rmslb.tar.gz' % (dir, dir))
    # now make the general distribuition release
    for f in FATRELEASE_FILES:
	os.system('cp %s %s' % (f, dir))
	# force permissions, though umask ought to cover this
	os.chmod(os.path.join(dir, f), 0664)
    os.system('tar cvf - %s | gzip -c > Distrib/%s.tar.gz' % (dir, dir))
    # Now do the ChangeLog
    os.system('cp ChangeLog ' + dir)
    os.system('cd %s ; gzip -c ChangeLog > ../Distrib/ChangeLog.gz' % dir)
    # clean up temporary directory
    for f in ALL_FILES + ['ChangeLog']:
	os.unlink(os.path.join(dir, f))
    os.rmdir(dir)



def make_docs():
    dir = 'TeX'
    os.mkdir(dir)
    for f in ['texinfo.tex', 'cc-mode.texi', 'texi2html.py']:
	os.system('cp %s %s' % (f, dir))
	# force permissions, though umask ought to cover this
	os.chmod(os.path.join(dir, f), 0664)

    # now create the files
    os.chdir(dir)
    try:
	#
	# build the DVI file from which much else is derived.
	#
	os.system('texi2dvi cc-mode.texi')
	os.system('gzip -c cc-mode.dvi > ../Distrib/cc-mode.dvi.gz')
	#
	# build the PS files, both forward and reverse
	#
	os.system('dvips -o cc-mode.ps cc-mode.dvi')
	os.system('dvips -r -o cc-mode.rev.ps cc-mode.dvi')
	os.system('gzip -c cc-mode.ps > ../Distrib/cc-mode.ps.gz')
	os.system('gzip -c cc-mode.rev.ps > ../Distrib/cc-mode.rev.ps.gz')
	#
	# make the info files
	#
	os.system('makeinfo cc-mode.texi')
	os.system('tar cf - cc-mode.info* | '
		  'gzip -c > ../Distrib/cc-mode.info.tar.gz')
	
	#
	# make the html files
	#
	os.mkdir('cc-mode-html')
	os.system('makeinfo -E tmpfile cc-mode.texi')
	os.system('python texi2html.py tmpfile cc-mode-html')
	os.system('tar cf - cc-mode-html | '
		  'gzip -c > ../Distrib/cc-mode-html.tar.gz')
    finally:
	#
	# cleanup
	#
	os.chdir('..')
	os.system('rm -rf %s' % dir)


def bump_release(revnum):
    compiled = {}
    for f, prefix in VERSIONED_FILES:
        print '%s:' % f,
	cre = re.compile('^(?P<prefix>' +
			 prefix +
			 ')5.(?P<rev>[0-9]{2})(?P<suffix>.*)$')
	print 'checking...',
	fp = open(f, 'r')
	while 1:
	    line = fp.readline()
	    if not line:
                if not compiled.has_key(f):
                    print 'no matching version line.'
                break
	    mo = cre.match(line)
	    if mo:
		if int(mo.group('rev')) <> int(revnum) - 1:
		    continue
		else:
                    compiled[f] = cre
	fp.close()
    # now bump them
    for f, cre in compiled.items():
	cre = compiled[f]
	print 'bumping.',
	fp_in = open(f, 'r')
	fp_out = open(f + '.new', 'w')
        matched = 0
	while 1:
	    line = fp_in.readline()
	    if not line:
		break
            mo = cre.match(line)
            if mo:
                prefix, suffix = mo.group('prefix', 'suffix')
                line = '%s5.%s%s\n' % (prefix, revnum, suffix)
                matched = matched + 1
	    fp_out.write(line)
	fp_in.close()
	fp_out.close()
	os.rename(f + '.new', f)
        print matched, 'lines changed.'


def main():
    try:
	opts, args = getopt.getopt(
	    sys.argv[1:],
	    'btTpdh',
	    ['bump', 'tag', 'TAG', 'package', 'docs', 'help'])
    except getopt.error, msg:
	print msg
	usage(1)

    # required minor rev number
    if len(args) <> 1:
	print 'Minor revision number is required'
	usage(1)
    revnum = args[0]

    # default options
    tag = 0
    retag = 0
    package = 0
    docs = 0
    help = 0
    bump = 0

    for opt, arg in opts:
	if opt in ('-h', '--help'):
	    help = 1
	elif opt in ('-b', '--bump'):
	    bump = 1
	elif opt in ('-t', '--tag'):
	    tag = 1
	elif opt in ('-T', '--TAG'):
	    tag = 1
	    retag = 1
	elif opt in ('-p', '--package', '-P', '--PACKAGE'):
	    package = 1
	elif opt in ('-d', '--docs'):
	    docs = 1

    if help:
	usage(0)

    # very important!!!
    os.umask(002)
    if tag:
	tag_release(revnum, retag)

    if package:
	pkg_release(revnum)

    if docs:
	make_docs()

    if bump:
	bump_release(revnum)


if __name__ == '__main__':
    main()
