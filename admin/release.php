<!-- -*- html -*- -->
<?php
  $title = "Current Release";
  include ("header.h");
?>

<p>The current release is 5.30.8. <a
href="http://download.sourceforge.net/cc-mode/cc-mode-5.30.8.tar.gz">Download</a>
the source package to upgrade the version that came with your Emacs or
XEmacs dist.  Installation instructions are available in the <a
href="src/README">README</a> file in the tarball and <a
href="installation.php">on-line</a>.

<p>You can also browse the <a href="src/">individual files</a>.

<p>The documentation is also available in several forms.  You can
either <a href="html-manual/index.html">browse it on-line</a> or grab
one of the several <a href="manual/">pre-formatted</a> documents
generated from the texinfo source.

<p>See the list of <a href="changes-530.php">user visible changes</a>
since 5.29 and earlier versions, and the <a
href="src/ChangeLog">ChangeLog file</a> for details about the fixed
bugs since the first 5.30 release.

<h3>Notes</h3>

<p>Since version 5.26, CC Mode supports using Filladapt mode to fill
text in comments and string literals.  There are however some <a
href="filladapt.php">issues</a> with this that you should be aware of
if you want to use Filladapt in CC Mode.

<?php include ("footer.h"); ?>
