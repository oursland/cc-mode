#! /usr/bin/env python

"""Bump version number and tag for next release.

Usage: %(program)s [-b] [-t] [-h]

Where:

    --bump
    -b      - bump to the next minor rev number

    --tag
    -t      - tag all releaseable files with new version number

    --package
    -p      - create the distribution packages

    --docs
    -d      - create the documentation packages

    --all
    -       - do all of the above

    --help
    -h      - this help message

"""

import sys
import os
import string
import regex
import getopt

program = sys.argv[0]

def usage(status):
    print __doc__ % globals()
    sys.exit(status)


RELEASE = None
RELEASE_NAME = ''

version_cre = regex.compile(';*[ \t]+Version:[ \t]+\(5.[0-9]+\)')
version_format = ';; Version:    %s\n'

extra_cre = regex.compile('(defconst c-version "\(5.[0-9]+\)"')
extra_format = '(defconst c-version "%s"\n'

FILES = [
    ('cc-align.el',    version_cre, version_format),
    ('cc-auto.el',     version_cre, version_format),
    ('cc-cmds.el',     version_cre, version_format),
    ('cc-compat.el',   version_cre, version_format),
    ('cc-engine.el',   version_cre, version_format),
    ('cc-guess.el',    version_cre, version_format),
    ('cc-langs.el',    version_cre, version_format),
    ('cc-lobotomy.el', version_cre, version_format),
    ('cc-menus.el',    version_cre, version_format),
    ('cc-mode.el',     version_cre, version_format),
    ('cc-styles.el',   version_cre, version_format),
    ('cc-vars.el',     version_cre, version_format),
    ('ANNOUNCEMENT',
     regex.compile('CC Mode Version \(5.[0-9]+\)'),
     'CC Mode Version %s\n'),
    ('MANIFEST',
     regex.compile('Manifest for CC Mode \(5.[0-9]+\)'),
     'Manifest for CC Mode %s\n'),
    ('README',
     regex.compile('README for CC Mode \(5.[0-9]+\)'),
     'README for CC Mode %s\n'),
    ('Makefile', None, None),
    ('ChangeLog', None, None),
    ('cc-mode.texi',
     regex.compile('@center @titlefont{CC Mode \(5.[0-9]+\)}'),
     '@center @titlefont{CC Mode %s}\n'),
    ]

def tag_release():
    bump = []
    for f, cre, format in FILES:
	if not cre or not format:
	    bump.append((f, cre, format))
	    continue
	print 'checking:', f
	fp = open(f, 'r')
	while 1:
	    line = fp.readline()
	    if not line:
		print 'file has no matching version line:', f
		break
	    if cre.match(line) >= 0:
		version = cre.group(1)
		if version <> RELEASE:
		    bump.append((f, cre, format))
		break
	fp.close()
    # now bump them
    for f, cre, format in bump:
	print 'bumping:', f
	os.system('co -l ' + f)
	if cre and format:
	    fp_in = open(f, 'r')
	    fp_out = open(f + '.new', 'w')
	    matched = None
	    while 1:
		line = fp_in.readline()
		if not line:
		    break
		# TBD: hackery since cc-mode.el is special
		if f == 'cc-mode.el' and extra_cre.match(line) >= 0:
		    fp_out.write(extra_format % RELEASE)
		    matched = 1
		elif matched or cre.match(line) < 0:
		    fp_out.write(line)
		else:
		    fp_out.write(format % RELEASE)
		    if f <> 'cc-mode.el':
			matched = 1
	    fp_in.close()
	    fp_out.close()
	    os.rename(f + '.new', f)
	os.system('ci -f -m"#Bumping to release revision %s" %s' %
		  (RELEASE, f))
	os.system('co -kv -u ' + f)
	os.system('rcs -n%s: %s' % (RELEASE_NAME, f))


def get_release():
    global RELEASE, RELEASE_NAME
    fp = open('VERSION', 'r')
    version = string.strip(fp.read())
    fp.close()
    RELEASE = version
    RELEASE_NAME = 'Release_' + string.translate(RELEASE,
						 string.maketrans('.', '_'))
    print 'This RELEASE     :', RELEASE
    print 'This RELEASE_NAME:', RELEASE_NAME


def new_release():
    [major_rev, minor_rev] = string.split(RELEASE, '.')
    fp = open('VERSION', 'w')
    next_rev = string.atoi(minor_rev) + 1
    if next_rev < 100:
	format = '%s.%02d\n'
    else:
	format = '%s.%03d\n'
    fp.write(format % (major_rev, next_rev))
    fp.close()


def pkg_release():
    dir = 'cc-mode-' + RELEASE
    os.mkdir(dir)
    for f, cre, format in FILES:
	if f == 'ChangeLog':
	    # package this up separately
	    continue
	os.system('cp %s %s' % (f, dir))
    os.system('tar cvf - %s | gzip -c > %s.tar.gz' % (dir, dir))
    os.system('cp ChangeLog ' + dir)
    os.system('cd %s ; gzip -c ChangeLog > ../ChangeLog.gz' % dir)
    for f, cre, format in FILES:
	os.unlink(os.path.join(dir, f))
    os.rmdir(dir)
    


def make_docs():
    curdir = os.getcwd()
    dir = os.path.join('..', 'Docs')
    try:
	os.chdir('TeX')
	#
	# build the DVI file from which much else is derived.  Don't
	# compress this yet
	#
	os.system('texi2dvi cc-mode.texi')
	os.system('cp cc-mode.dvi ' + dir)
	#
	# build the PS files, both forward and reverse, compress them later
	#
	os.system('dvips -o %s cc-mode.dvi' %
		  os.path.join(dir, 'cc-mode.ps'))
	os.system('dvips -r -o %s cc-mode.dvi' %
		  os.path.join(dir, 'cc-mode.rev.ps'))
	#
	# make the info files
	#
	os.system('makeinfo cc-mode.texi')
	os.system('tar cvf - cc-mode.info* | gzip -c > %s' %
		  os.path.join(dir, 'cc-mode.info.tar.gz'))
	#
	# make the html files
	#
	os.system('makeinfo -E tmpfile cc-mode.texi')
	os.system('python texi2html.py tmpfile cc-mode.html')
	os.unlink('tmpfile')
	os.system('tar cvf - cc-mode.html | gzip -c > %s' %
		  os.path.join(dir, 'cc-mode.html.tar.gz'))
	#
	# now go back and compress those that weren't compressed yet
	#
	os.chdir(os.path.join(curdir, 'Docs'))
	for f in ('cc-mode.ps', 'cc-mode.rev.ps', 'cc-mode.dvi'):
	    os.system('gzip -f ' + f)
    finally:
	os.chdir(curdir)

def main():
    try:
	opts, args = getopt.getopt(
	    sys.argv[1:], 'abtpdh',
	    ['all', 'bump', 'tag', 'package', 'docs', 'help'])
    except getopt.error, msg:
	print msg
	usage(1)

    if args:
	usage(1)

    bump = None
    tag = None
    package = None
    docs = None
    help = None

    for opt, arg in opts:
	if opt in ('-a', '--all'):
	    bump = 1
	    tag = 1
	    package = 1
	    docs = 1
	if opt in ('-b', '--bump'):
	    bump = 1
	elif opt in ('-t', '--tag'):
	    tag = 1
	elif opt in ('-p', '--package'):
	    package = 1
	elif opt in ('-d', '--docs'):
	    docs = 1
	elif opt in ('-h', '--help'):
	    help = 1

    if help:
	usage(0)

    os.umask(002)
    get_release()
    if bump:
	new_release()

    if tag:
	tag_release()

    if package:
	pkg_release()

    if docs:
	make_docs()

if __name__ == '__main__':
    main()
