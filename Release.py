#! /usr/bin/env python

"""Bump version number and tag for next release.
"""

import os
import string
import regex


RELEASE = '5.03'
RELEASE_NAME = 'Release_' + string.translate(RELEASE,
					     string.maketrans('.', '_'))
						       
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
    ('cc-mode.texi',
     regex.compile('@center @titlefont{CC Mode \(5.[0-9]+\)}'),
     '@center @titlefont{CC Mode %s}\n'),
    ]

def bump_els():
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
	os.system('ci -f -m"Bumping to release revision %s" %s' % (RELEASE, f))
	os.system('co -kv -u ' + f)
	os.system('rcs -n%s: %s' % (RELEASE_NAME, f))

if __name__ == '__main__':
    bump_els()
