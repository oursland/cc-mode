<!-- -*- html -*- -->
<?php
  $title = "Accessing the Mercurial Repository";
  include ("header.h");
?>

<h3>Downloading the repository</h3>

<p>To download a snapshot of the CC Mode master repository, just use the
  standard mercurial "clone" command, like this:

  <pre>hg clone http://hg.code.sf.net/p/cc-mode/cc-mode TARGET-DIRECTORY</pre>

  , or alternatively, if you have a SourceForge ID:

  <pre>hg clone ssh://USERNAME@hg.code.sf.net/p/cc-mode/cc-mode TARGET-DIRECTORY</pre>

  .  After cloning the repository, you will be able to pull future revisions
  from it in the normal way.  (Note that the former location of the repository
  is now a read-only fossilised snapshop from June 2013.  Don't use it!)

<p>Being under development, there is no guarantee that this version will work
properly, or even at all.  That said, it usually works well: at least I (Alan)
use it in my daily work, not only when I hack on it.  It might not be entirely
compatible with user settings, but it can get more advanced fixes that are
considered too risky to be allowed into the patch branch.

<p>Alternatively, you can switch to using the patch branch, the one with just bug
fixes in it.  Use this command:

<pre>hg update Branch_5_32</pre>

<p>To browse the project's history and current state online, use
<a href="https://sourceforge.net/p/cc-mode/cc-mode/ci/default/tree/">this</a>.

<p>You are especially welcome to report bugs, opinions and patches
regarding the development version.  However if you've found a bug,
it's a good idea to try an update before you report it, since chances
are that we have found it ourselves already.

<h3>The source tree</h3>

<p>The CC Mode source will be in the root of the checked out tree.
It's the same thing you'll find in a dist tarball (with a couple of
extra files around it), so it can be byte compiled and used straight
away.  You'll find the regression test suite in the <code>tests</code>
directory; take a look in <code>000tests.el</code> to figure out how
to use it.  The <code>admin</code> directory probably isn't very
interesting; it just contains the source for this web site.

<h3>The Mercurial version control system</h3>

<p>The canonical reference work for Mercurial is
<a href="http://hgbook.red-bean.com">Mercurial: the Definitive Guide</a> by
Bryan O'Sullivan.  The SourceForge
<a href="http://sourceforge.net/p/forge/documentation/Mercurial">documentation</a>
might be helpful when setting up your Mercurial host.  If you are new to Mercurial,
you might want to read Joel Spolsky's
excellent <a href="http://hginit.com">tutorial</a>.

<?php include ("footer.h"); ?>
