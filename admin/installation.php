<!-- -*- html -*- -->
<?php
  $title = "Installation Notes";
  include ("header.h");
?>

<h3>Setting <code>load-path</code></h3>

<p>You need to make sure that this new version of CC Mode is on your
<code>load-path</code>, <em>before</em> any version that is
distributed with your (X)Emacs.  Note that the CC Mode distribution
unpacks into its own subdirectory.  You can use this test to see which
version of CC Mode your (X)Emacs finds first:

<pre>
M-x locate-library RET cc-mode RET</pre>

Make sure this finds the one you expect. If not, you can add this to
your <code>.emacs</code> file:

<pre>
(setq load-path (cons "/dir/of/cc-mode/" load-path))</pre>

The path you use should be an absolute path (starting with a slash).
You cannot use a path beginning with "~" in the <code>load-path</code>
variable.

<h3>Byte Compiling</h3>

<p>It is <em>highly</em> recommended that you byte-compile CC Mode,
for both performance and compatibility reasons.  Running CC Mode
un-bytecompiled is not supported and <strong>may not work</strong> at
all depending on the flavor of (X)Emacs you are using.  You can byte
compile CC Mode by doing the following from your shell:

<pre>
% cd /path/to/cc-mode
% $(EMACS) -batch -no-site-file -q -f batch-byte-compile cc-*.el</pre>

where <code>$(EMACS)</code> is either <code>emacs</code> or
<code>xemacs</code> depending on the flavor you use.  The compilation
will produce a lot of warnings for XEmacs 19.  They can safely be
ignored.

<p>Be sure to see the list of <a href="compat.php">compatibility
issues</a>, for special notes about (X)Emacs versions and package
interactions.  To test that you have things set up correctly, visit a
C file and then type:<br>

<pre>
M-x c-version RET
=> Using CC Mode version 5.XX</pre>

where <code>XX</code> is the correct minor revision number.

<?php include ("footer.h"); ?>
