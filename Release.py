#! /bin/perl
#
# Perl script to support version release of Supercite
# $Id: Release.py,v 1.2 1993-09-30 01:50:28 warsaw Exp $
#
# Usage:
# -p  -- non-beta release (i.e. public)
# -n  -- add symbolic name of release to head of branch
# -N  -- force symbolic of release to head of branch
# -d  -- create the RCS diff (i.e. patch) file
# -d- -- overwrite the generated patch file if one already exists
# -t  -- generate a new tar file
# -t- -- overwrite new tar file if one already exists
# -h  -- help message

# Variables, default to beta release
@Lisp = ('c++-mode');
@Files = (grep(($_ .= ".el") && 1, @Lisp));
$Release = "Beta";

# Usage
sub usage {
	print "Usage: $0: [-p] [-n] [-N] [-d[-]] [-t[-]]\n";
	print "-p  -- non Beta (i.e. Public release\n";
	print "-n  -- add symbolic name of release to head of branch\n";
	print "-N  -- force symbolic of release to head of branch\n";
	print "-d  -- create the RCS diff (i.e. patch) file\n";
	print "-d- -- overwrite the generated patch file if one already exists\n";
	print "-t  -- generate new tar file\n";
	print "-t- -- overwrite new tar file if one already exists\n";
	print "-h  -- help message\n";
}


# scan argv
while ($_ = $ARGV[0], /^-/) {
	shift;
	/^-p$/  && ($Release = "Public", next);
	/^-r$/  && (++$Release, next);
	/^-n$/  && (++$AddSymName, next);
	/^-N$/  && (++$AddSymName, ++$ForceSymName, next);
	/^-d-$/ && (++$OverwriteDiff, ++$GenerateDiff, next);
	/^-d$/  && (++$GenerateDiff, next);
	/^-t-$/ && (++$GenerateTar, ++$OverwriteTar, next);
	/^-t$/  && (++$GenerateTar, next);
	/^-h(elp)?/ && (++$Help, next);

	# bad switch
	print "$0: illegal switch $_\n";
	++$ArgvError;
}

&usage() if $ArgvError || $Help;
exit(0) if $Help;
exit(1) if $ArgvError;

# Calculate the new version number
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
$Month = ("Jan", "Feb", "Mar", "Apr", "May", "Jun",
		  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")[$mon];
$Date = "${mday}_${Month}_${year}";
$NewVersion = "${Release}_${Date}";
print "New Supercite version: $NewVersion\n";

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
### Local Variables: ;
### mode: perl ;
### End: ;
