#! /home/bwarsaw/bin/perl
#
# Perl script to support public releases of cc-mode
# $Id: Release.py,v 1.4 1997-04-23 02:56:35 bwarsaw Exp $
#

# parse command line options
$Symname = 1;

while ($_ = $ARGV[0], /^-/) {
    shift;
    /^-nosymname$/ && ($Symname=0, next);
    /^-(h(elp)?)|\?$/ && ($HelpP++, next);

    # error
    $ErrorP++;
    print STDERR "Illegal switch: $_\n";
}
if ($HelpP || $ErrorP) {
    print STDERR "Usage: $0",
    "\n\t-nosymname : do not assign a symbolic name to this branch.",
    "\n\t-help      : print this message.",
    "\n";

    exit ($ErrorP ? $ErrorP : 0);
}


# constants
@Lisp = ("cc-compat", "cc-mode", "cc-mode-18");
@Text = ("ChangeLog", "DUMPING", "MANIFEST", "README");
@Files = (grep(($_ .= ".el") && 1, @Lisp), @Text);
$Main = "cc-mode.el";
$Tarfile = "cc-mode-XXX.tar";
$Release = "X--------------------->Full";
$Compress = "gzip";
$Tmpdir = "./tmp$$";


# Calculate the new version number
$_ = (grep(/^head:/, `rlog -h $Main`))[0];
die "Could not extract branch head from $Main.\n"
    if ( ! $_ || ! /^head:\s*([0-9.]+)/);

$Revision = $1;
$Tarfile =~ s/XXX/$Revision/;
print "Using revision number $Revision and tarfile: $Tarfile\n";


# Make sure files we're going to tar up are not RCS locked
@Locks = `rlog -L -R -l @Files`;
die "The following distribution files are still locked: @Locks\n"
    if @Locks;


if ($Symname) {
    # set the symbol name for the branch heads of all the files.
    ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);

    $Month = ("Jan", "Feb", "Mar", "Apr", "May", "Jun",
	      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")[$mon];

    $Monthday = ($mday < 10 ? "0" : "") . "$mday";

    $SymbolicRevname = "${Release}_$Monthday-$Month-$year";
    print "Assigning symbolic revision name: $SymbolicRevname\n";
    system("rcs -n\"$SymbolicRevname\": @Files")
	&& die "Could not assign symbolic revision name. $!\n";
}
else {
    print "Skipping assignment of symbolic revision name.\n";
}


# Now check out all files, unlocked, with keywords removed.  Create a
# temporary directory and move the files to that directory.  We do
# this because we're going to hack the permissions before they go into
# the tar file.
system("co -u -kv @Files") && die "co -u -kv @Files failed. $!\n";

mkdir($Tmpdir, 750) || die "Could not make temporary directory: $Tmpdir\n";
system("cp @Files $Tmpdir") && die "Could not copy files to: $Tmpdir\n";
chdir($Tmpdir) || die "Could not change to directory: $Tmpdir\n";
chmod(644, @Files) || die "Could not change modes\n";


# Now tar them up and compress the tar file
print "Tar'ing and ${Compress}'ing...\n";
@Unlinkables = split("\n", `ls ${Tarfile}* | cat`);
#print "@Unlinkables\n";
unlink(@Unlinkables);

system("tar cvf $Tarfile @Files") && die "Couldn't tar: $!\n";
system("$Compress $Tarfile") && die "Couldn't $Compress: $!\n";
system("mv ${Tarfile}* ..");

# clean up temp files
unlink(@Files);
chdir("..") || die "Could not chdir to parent (..)\n";
rmdir($Tmpdir) || die "Could not rmdir: $Tmpdir\n";

print "Done.\n";
