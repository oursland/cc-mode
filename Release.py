#! /depot/gnu/plat/bin/perl
#
# Perl script to support public releases of cc-mode
# $Id: Release.py,v 1.3 1994-09-10 18:38:47 warsaw Exp $
#

# constants
@Lisp = ("cc-compat", "cc-mode", "cc-mode-18");
@Text = ("ChangeLog", "DUMPING", "MANIFEST", "README");
@Files = (grep(($_ .= ".el") && 1, @Lisp), @Text);
$Main = "cc-mode.el";
$Tarfile = "cc-mode-XXX.tar";
$Release = "X--------------------->Full";
$Compress = "gzip";


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


# set the symbol name for the branch heads of all the files.
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);

$Month = ("Jan", "Feb", "Mar", "Apr", "May", "Jun",
	  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")[$mon];

$Monthday = ($mday < 10 ? "0" : "") . "$mday";

$SymbolicRevname = "${Release}_$Monthday-$Month-$year";
print "Assigning symbolic revision name: $SymbolicRevname\n";
system("rcs -n\"$SymbolicRevname\": @Files")
    && die "Could not assign symbolic revision name. $!\n";


# Now check out all files, unlocked, with keywords removed
system("co -u -kv @Files") && die "co -u -kv @Files failed. $!\n";


# Now tar them up and compress the tar file
print "Tar'ing and ${Compress}'ing...\n";
@Unlinkables = split("\n", `ls ${Tarfile}* | cat`);
#print "@Unlinkables\n";
unlink(@Unlinkables);

system("tar cvf $Tarfile @Files") && die "Couldn't tar: $!\n";
system("$Compress $Tarfile") && die "Couldn't $Compress: $!\n";


print "Done.\n";
