<!-- -*- html -*- -->
<?php
  $title = "Contributed Stuff";
  include ("header.h");
?>

<h3>Font lock for IDL mode</h3>

<p>Three people have contributed Emacs font-lock definitions for CORBA
IDL code.  These are useful to IDL writers since CC Mode does not
contain any font-lock definitions (for IDL, or any other supported
language), and X/Emacs itself also doesn't contain IDL font-lock
definitions by default.

<p>I'm sorry, I can't tell you which one to use.  Hopefully someday
the three alternatives will be merged and contributed to X/Emacs so
you'll just get them automatically.

<ul>

  <li><a href="idl-font-lock.el">Scott Hassan's</a> original.

  <li><a href="idl-font-lock-2.el">Brian Ewins'</a> alternative.

  <li><a
  href="http://www.cs.utah.edu/~eeide/emacs/idl-font-lock.el.gz">A
  link to Eric Eide's</a> third alternative (gzip'd)

</ul>

<h3>Font lock for Pike mode</h3>

<p>Like IDL mode, neither CC Mode nor X/Emacs does not come with
font-lock definitions for Pike mode.  <a href="pike.el">This
package</a> written by Per Hedbor and others provides that.

<?php include ("footer.h"); ?>
