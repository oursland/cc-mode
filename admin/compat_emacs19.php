<!-- -*- html -*- -->
<?php
  $title = "Emacs 19 Compatibility";
  $menufiles = array ("links.h", "compatlinks.h");
  include ("header.h");
?>

<p>CC Mode contains the compatibility glue necessary to run on Emacs
19.34, and it should be loaded automatically if needed. Therefore no
special steps are necessary to get it to work.

<p>The version of the <a
href="http://www.dina.kvl.dk/~abraham/custom/">Custom library</a> that
comes with Emacs 19.34 is not compatible with CC Mode. CC Mode will
detect this when it's byte compiled, and will in that case leave out
the Custom support altogether. So if you later install a newer version
of Custom, you will need to recompile CC Mode. Also note that if you
have a newer Custom library installed locally, you might need to leave
out the <code>-no-site-file</code> argument when you compile CC Mode,
so that Emacs finds the new library. Whether that's necessary or not
depends on how Emacs and the Custom library are installed.

<p>I have no idea whether CC Mode 5 will work with Emacs versions
before 19.34. You might try following the directions above, but no
earlier Emacs versions are supported actively.

<?php include ("footer.h"); ?>
