<!-- -*- html -*- -->
<?php
  $title = "Current Release";
  include ("header.h");
?>

<p>It's been a long time (too long) since the last CC Mode release,
and consequently the amount of change is large - this version has
almost twice the code size of the previous one.  Most of the new code
concerns the new advanced font lock support for all the supported
languages, and the AWK support contributed by Alan Mackenzie.
Beginning with this release, there will be unannounced bugfix releases
available with version numbers like 5.30.1, 5.30.2, etc. This is to
improve the responsiveness to bugs.

<p>The current release is available in several forms.  You can
download the entire source distribution, including the manual in
texinfo format as an approximately <a
href="http://download.sourceforge.net/cc-mode/cc-mode-5.30.tar.gz">300KB
gzip'd tarball</a>.  You can browse the <a href="src/">individual
files</a> too.

<p>The documentation is also available in several forms.  You can
either <a href="manual/html/index.html">browse the on-line
documentation</a> or grab one of the several <a
href="manual/">pre-formatted</a> documents generated from the texinfo
source, including PostScript, Emacs Info, and HTML versions</a>.

<p>Since version 5.26, CC Mode supports using Filladapt mode to fill
text in comments and string literals.  There are however some <a
href="filladapt.php">issues</a> with this that you should be aware of
if you want to use Filladapt in CC Mode.

<?php include ("footer.h"); ?>
