<!-- -*- html -*- -->
<?php
  $title = "Compatibility Notes";
  $menufiles = array ("links.h", "compatlinks.h");
  include ("header.h");
?>

<p>CC Mode should work out of the box with Emacs &gt;= 19.34, and
XEmacs &gt;= 19.15.

<ul>

  <li><a href="compat_xemacs.php">XEmacs and Emacs 20 or later</a>

  <li><a href="compat_emacs19.php">Emacs 19</a>

  <li><a href="compat_emacs18.php">Emacs 18</a>

  <li><a href="compat_delbs_awk.php">Interactions with other Elisp
  packages</a>, e.g. <code>delbackspace.el</code> and
  <code>awk-mode</code>.

</ul>

<?php include ("footer.h"); ?>
