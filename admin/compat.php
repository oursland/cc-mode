<!-- -*- html -*- -->
<?php
  $title = "Compatibility Notes";
  include ("header.h");
?>

<p>CC Mode should work out of the box with Emacs &gt;= 19.34, and
XEmacs &gt;= 19.15.

<p>However, the new AWK Mode requires a feature
(<code>syntax-table</code> text properties) which first appeared in
Emacs 20.1 and XEmacs 21.4.  If you are still using an older version
of (X)Emacs, CC Mode will revert to using the older AWK Mode which is
distributed with (X)Emacs (from file <code>awk-mode.elc</code>) for
AWK files, even if you have an autoload form that loads
<code>cc-mode</code> for <code>awk-mode</code> in your
<code>.emacs</code> or <code>init.el</code> file.  Upgrading your
(X)Emacs is strongly recommended if this is the case.  You can check
which AWK Mode you are running by displaying the mode documentation
string with C-h m from an AWK buffer.  The newer mode's doc string
contains <code>"to submit a problem report, enter `C-c C-b'"</code>
near the top of the doc string where the older mode has <code>"This is
much like C mode except ...."</code>.

<p>If you're using Emacs 21.3 or earlier, or XEmacs 21.3 or earlier,
then the file <code>cc-fix.el(c)</code> is needed and will be loaded
automatically.  It corrects for some bugs in those versions, and also
contains compatibility glue for missing functions in older
versions. <code>cc-fix.el(c)</code> is not needed for later versions
(when they become available, in the case of Emacs).

<p>Emacs 19.34 still works with CC Mode, but the version of the <a
href="http://www.dina.kvl.dk/~abraham/custom/">Custom library</a> that
comes with it is not compatible with CC Mode. CC Mode will detect this
when it's byte compiled, and will in that case leave out the Custom
support altogether. So if you later install a newer version of Custom,
you will need to recompile CC Mode. Also note that if you have a newer
Custom library installed locally, you might need to leave out the
<code>-no-site-file</code> argument when you compile CC Mode, so that
Emacs finds the new library. Whether that's necessary or not depends
on how Emacs and the Custom library are installed.

<?php include ("footer.h"); ?>
