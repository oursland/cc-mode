<!-- -*- html -*- -->
<?php
  $title = "Compatibility for XEmacs and Emacs 20 or later";
  $menufiles = array ("links.h", "compatlinks.h");
  include ("header.h");
?>

<p>If you are using Emacs 20.2 or later, or XEmacs 19.15, 19.16, 20.4
or later, you should be able to use this version right out of the box.

<p>An earlier version of CC Mode probably comes with your (X)Emacs, so
you just need to make sure that the new CC Mode version you install is
found earlier on your <code>load-path</code> than the version that
comes with the (X)Emacs distribution. See the <a
href="installation.php">installation notes</a> for more details.

<p>I have no idea whether CC Mode 5 will work with XEmacs 19.14 or
earlier.  You might try following the directions for <a
href="compat_emacs19.php">Emacs 19.34</a>. No earlier XEmacs version
is actively supported.

<?php include ("footer.h"); ?>
