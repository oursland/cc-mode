<!-- -*- html -*- -->
<?php
  $title = "Current Release";
  include ("header.h");
?>

<p>The current release is 5.31.3. <a
href="http://prdownloads.sourceforge.net/cc-mode/cc-mode-5.31.3.tar.gz">Download</a>
the source package to upgrade the version that came with your Emacs or
XEmacs dist.  Installation instructions are available in the <a
href="src/README">README</a> file in the tarball and <a
href="installation.php">on-line</a>.  Older releases can be found <a
href="http://prdownloads.sourceforge.net/cc-mode/">here</a>.

<p>[Note: "5.31.4" and "5.31.5" denote the versions of CC Mode contained in
  GNU Emacs 22.1 and 22.2.  These versions have not been released as
  stand-alone versions here.]

<p>You can also browse the <a href="src/">individual files</a>.

<p>The documentation is also available in several forms.  You can
either <a href="html-manual/index.html">browse</a> it on-line or grab
one of the <a href="manual/">pre-formatted</a> documents generated
from the texinfo source.

<p>See the list of <a href="changes-531.php">user visible changes</a>
since 5.30 and earlier versions, and the <a
href="src/ChangeLog">ChangeLog file</a> for details about the bugs
fixed in the patch releases made since the first 5.31 release.

<h3>Notes</h3>

<p>Since version 5.26, CC Mode has supported the use of Filladapt mode to fill
text in comments and string literals.  There are however some <a
href="filladapt.php">issues</a> with this that you should be aware of if you
want to use Filladapt in CC Mode.

<?php include ("footer.h"); ?>
