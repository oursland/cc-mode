<!-- -*- html -*- -->
<?php
  $title = "Emacs 19 Compatibility";
  $menufiles = array ("links.h", "compatlinks.h");
  include ("header.h");
?>

<p>First of all, it's <em>highly</em> recommend that you upgrade to
Emacs 20.3 or XEmacs 20.4 or later, if at all possible.  Both of the
latest releases of either flavor are very stable and much better than
Emacs 19.34.  Windows versions of both flavors should also be
available, if you look around a bit.  It's getting harder for us to
guarantee CC Mode compatibility with Emacs 19.34, so it would be
better if you do upgrade your Emacs.

<p>If you must use CC Mode 5 with Emacs 19.34, you will need to load
the file <code>cc-mode-19.el</code> <em>before</em> you load any of
the other CC Mode files.  CC Mode <em>should</em> do this
automatically.

<p>You will also need to download and install <a
href="http://www.dina.kvl.dk/~abraham/custom/"> Per's Custom
library</a>, and edit the <code>cc-make.el</code> file to include the
path to the Custom library when you byte-compile CC Mode.  You
<em>must</em> also added the path to the Custom to your
<code>load-path</code> variable in your <code>.emacs</code> file for
your run-time Emacs environment.

<p>After you do this, read the <code><a
href="ftp://cc-mode.sourceforge.net/pub/cc-mode/src/README">README</a></code>
file that comes with the CC Mode distribution for more details.

<p>I have no idea whether CC Mode 5 will work with Emacs versions
before 19.34.  You might try following the directions above, but I do
not plan to actively support any earlier Emacs version.

<?php include ("footer.h"); ?>