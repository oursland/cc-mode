#! /bin/perl
#
# Perl script to support version release of Supercite
# $Id: Release.py,v 1.1 1993-09-30 00:57:12 warsaw Exp $
#
# Usage:
# -r  -- release version by bumping up this file's internal version number
# -n  -- add symbolic name of release to head of branch
# -N  -- force symbolic of release to head of branch
# -d  -- create the RCS diff (i.e. patch) file
# -d- -- overwrite the generated patch file if one already exists
# -c  -- generate new ChangeLog file
# -c- -- overwrite new ChangeLog file if one already exists
# -t  -- generate a new tar file
# -t- -- overwrite new tar file if one already exists
# -h  -- help message

# Variables
@Lisp = (regi, 'sc-oloads', 'sc-unsupp', supercite);
@Files = (README, 'supercite.texi', grep(($_ .= ".el") && 1, @Lisp));

# Usage
sub usage {
	print "Usage: $0: [-r] [-n] [-N] [-d[-]] [-c[-]] [-t[-]]\n";
	print "-r  -- release new symbolic version\n";
	print "-n  -- add symbolic name of release to head of branch\n";
	print "-N  -- force symbolic of release to head of branch\n";
	print "-d  -- create the RCS diff (i.e. patch) file\n";
	print "-d- -- overwrite the generated patch file if one already exists\n";
	print "-c  -- generate new ChangeLog file\n";
	print "-c- -- overwrite new ChangeLog file if one already exists\n";
	print "-t  -- generate new tar file\n";
	print "-t- -- overwrite new tar file if one already exists\n";
	print "-h  -- help message\n";
}


# scan argv
while ($_ = $ARGV[0], /^-/) {
	shift;
	/^-r$/  && ++$Release && next;
	/^-n$/  && ++$AddSymName && next;
	/^-N$/  && ++$AddSymName && ++$ForceSymName && next;
	/^-d-$/ && ++$OverwriteDiff && ++$GenerateDiff && next;
	/^-d$/  && ++$GenerateDiff && next;
	/^-c-$/ && ++$OverwriteCL && ++$ChangeLog && next;
	/^-c$/  && ++$ChangeLog && next;
	/^-t-$/ && ++$GenerateTar && ++$OverwriteTar && next;
	/^-t$/  && ++$GenerateTar && next;
	/^-h(elp)?/ && ++$Help && next;

	# bad switch
	print "$0: illegal switch $_\n";
	++$ArgvError;
}

&usage() if $ArgvError || $Help;
exit(0) if $Help;
exit(1) if $ArgvError;


# First bump up the symbolic revision numbers
chop($OldVersion = <DATA>);
print "Old Supercite version: $OldVersion\n";

($Name, $MajorRev, $MinorRev) = split('_', $OldVersion);
$NewMinorRev++;
$NewVersion = join('_', ($Name, $MajorRev, $NewMinorRev));
print "New Supercite version: $NewVersion\n";

# Now modify ourselves for next time
if ($Release) {
	@Path = split('/', $0);
	$ThisFile = $Path[$#Path];
	`co -l $ThisFile`;

	open(THISFILE,$ThisFile) || die "Couldn't open $ThisFile for reading!\n";
	@MyLines = <THISFILE>;
	close(THISFILE);

	open(THISFILE, ">$ThisFile")
		|| die "Couldn't open $ThisFile for writing!\n";

	foreach $Line (@MyLines) {
		chop $Line;
		if ($Line eq $OldVersion) {
			print THISFILE "$NewVersion\n";
		}
		else {
			print THISFILE "$Line\n";
		}
	}
	close(THISFILE);
	`ci -u -m\"Bumping to next release level\" $ThisFile`;
}


# Now do the RCS massaging on each file
if ($GenerateDiff) {
	$DiffFile = "sc-diff-$MajorRev.$MinorRev-$MajorRev.$NewMinorRev";
	if (-e $DiffFile) {
		die "Diff file exists: $DiffFile\n" if !$OverwriteDiff;
		print "Overwriting existing Diff file: $DiffFile\n";
	}
	else {
		print "Diff'ing to file: $DiffFile\n";
	}
	open(DIFFOUT, ">" . $DiffFile)
		|| die "Cannot open $DiffFile for output.\n";
}


foreach $File (@Files) {
	if ($AddSymName) {
		$Switch = ($ForceSymName ? "-N" : "-n");
		`rcs ${Switch}${NewVersion}: $File`;
	}
	if ($GenerateDiff) {
		`co -u -kv $File`;
		open(DIFF, "rcsdiff -kv -r${OldVersion} -c2 $File | ")
			|| die "Couldn't get an RCS diff on file: $File\n";

		while ($_ = <DIFF>) {
			print DIFFOUT $_;
		}
		close(DIFF);
	}		
}

if ($GenerateDiff) {
	close(DIFFOUT);
}


# Now generate ChangeLog file
if ($ChangeLog) {
	$CLFile = "ChangeLog";
	if (-e $CLFile) {
		die "ChangeLog file exists.\n" if !$OverwriteCL;
		print "Overwriting existing ChangeLog file.\n";
	}

	$Files = join(' ', @Files);
	open(RCSLOG, "rcs2log -r ${OldVersion}:${NewVersion} $Files |")
		|| die "Could not open rcs2log pipe.\n";

	open(CHANGELOG, ">" . $CLFile) || die "Couldn't open ChangeLog file.\n";
	while (<RCSLOG>) {
		print CHANGELOG $_;
	}
}

# Now generate tar file
if ($GenerateTar) {
	$TarFile = "sc${MajorRev}.${MinorRev}.tar";
	$GZTarFile = "${TarFile}.gz";
	if (-e $TarFile) {
		die "tar file exists: $TarFile\n" if !$OverwriteTar;
		print "Overwriting tar file: $TarFile\n";
		unlink $TarFile;
	}
	if (-e $GZTarFile) {
		die "gzip'd tar file exists: $GZTarFile\n" if !$OverwriteTar;
		print "Overwriting gzip'd tar file: $GZTarFile\n";
		unlink $GZTarFile;
	}
	$Reporter = "reporter.el";
	unlink $Reporter;
	`cp ../reporter/$Reporter .`;

	$Files = join(' ', @Files, 'reporter.el');
	`tar cvf $TarFile $Files`;
	`gzip $TarFile`;
}


__END__
Supercite_3_1
### Local Variables: ;
### mode: perl ;
### End: ;
