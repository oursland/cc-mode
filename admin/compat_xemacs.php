<!-- -*- html -*- -->
<?php
  $title = "XEmacs and Emacs 20 Compatibility";
  $menufiles = array ("links.h", "compatlinks.h");
  include ("header.h");
?>

<p>If you are using XEmacs 19.15, 19.16, 20.4, or any XEmacs 21, you
should be able to use this new version right out of the box.  An
earlier version of CC Mode probably comes with your XEmacs, so you
just need to make sure that this new version is found earlier on your
<code>load-path</code> than the version that comes with your XEmacs.
See the <a href="installation.php">installation notes</a> for more
details.

<p>The same goes for Emacs 20 as of 20.2 and beyond.

<p>I have no idea whether CC Mode 5 will work with XEmacs 19.14 or
earlier.  You might try following the directions for <a
href="compat_emacs19.php">Emacs 19.34</a>, although you will
<em>not</em> need the <code>cc-mode-19.el</code> file.  No earlier
XEmacs version is actively supported.

<?php include ("footer.h"); ?>
