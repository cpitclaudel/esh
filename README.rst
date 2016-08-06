=====================================
 Emacs Syntax Highlighting for LaTeX
=====================================

This programs processes TeX source files, adding syntax-highlighting macros to
the contents of specially-delimited environments and macros.  For example,
esh2tex transforms this block:

.. code:: latex

   %% ESH: c-mode
   \begin{verbatim}
     int main() { return 0; }
   \end{verbatim}

into something like that:

.. code:: latex

   \begin{ESHBlock}
     \color{8CC4FF}{int} \color{FCE94F}{main}() \{ \color{B4FA70}{return} 0; \}
   \end{ESHBlock}

See the ``example`` folder of the GitHub repository for a full example.  One of
the best parts is that your document can keep pretending to use ``lstlistings``,
``minted``, ``fancyvrb``, ``verbatim`` etc. while using ESH, and thus still be
compilable by people who don't have ESH.


Setup
=====

**Dependencies:** Emacs > 24.2; XeLaTeX (recommended); Cask (optional)

**Setup:** Clone the repository somewhere, and add ``<wherever>/bin`` to your path
(alternatively, just use ``<wherever>/bin/esh2tex`` explicitly).  This program
is tested only on GNU/Linux.

**Sanity check:** Running ``make`` in the ``example`` directory of the Git repo
should produce a (partially) syntax-highlighted ``example.pdf``.


Quickstart
==========

In ``minimal.tex`` put the following::

  \documentclass{minimal}
  %% ESH-preamble-here
  \begin{document}
    %% ESH: c-mode
    \begin{verbatim}
    int main() { return 0; }
    \end{verbatim}
  \end{document}

Process with ``esh2tex minimal.tex > minimal.esh.tex``, and compile with
``pdflatex minimal.esh.tex`` or ``xelatex minimal.esh.tex``. Run ``make`` in the
``example/`` directory of the Git repository for a more advanced example.


Usage
=====

::

  esh2tex [<options>...] [<input>]
  emacs -Q --script esh2tex [<options>...] [<input>]

``<input>`` should be an UTF-8 encoded text file; output goes to standard out.


In ``<input>``, you may indicate source blocks like this::

  %% ESH: <mode>
  \begin{...}
    ...
  \end{...}

``<mode>`` should be the name of an Emacs major mode; the name of the
environment does not matter.


Additionally, ``<input>`` should include a special comment::

  %% ESH-preamble-here

in its preamble, which ``esh2tex`` will replace by appropriate set-up code
(``\usepackage``, definition of ``ESHBlock``, etc.).


You can enable highlighting of specific inline verb-like macros using the
following special comment::

  %% ESH-inline-verb: <mode> <pattern>

For example, the following will highlight each occurrence of ``\verb|...|`` as C
code, and each occurrence of ``\python|...|`` as Python code::

  \def\python{\verb} % To remain compatible with plain LaTeX
  %% ESH-inline: c-mode \verb
  %% ESH-inline: python-mode \python

Options
=======

* ``--usage``

  Show this help.

* ``--persist``

  Leave server running after processing ``<input>``.  Don't forget to
  ``--kill-server`` if you make changes to your ``esh-init.el``!

* ``--kill-server``

  Kill previously-started instances of the ESH server.

* ``--no-cask``

  Normally, when the current directory contains a Cask file and the cask binary
  is in your path, ESH uses ``cask exec emacs`` instead of ``emacs`` to start
  the syntax-highlighting daemon.  With this option, ESH will stick to using
  the plain ``emacs``.

* ``--no-Q``

  Load your full Emacs configuration instead of the ``esh-init.el`` file.  Use
  this option with caution; there are subtle differences between ESH and a
  regular Emacs that can prevent your Emacs configuration from loading properly.
  In general, it's much better to extract just what you need from your
  ``.emacs`` and put it in an ``esh-init.el``, as described below.

* ``--debug-on-error``

  Print stack traces for errors.

Notes
=====

* ``esh2tex`` does not load your personal Emacs configuration (though see the
  ``--no-Q`` option); instead, it looks for a file named ``esh-init.el`` in the
  current directory, one of its parents, or ``~/.emacs.d/``.  You can use that
  file to chose a different theme, load packages; this works great in
  conjunction with the `Cask <https://github.com/cask/cask>`_ package manager.

* Starting a server can be slow if your configuration file is large.  Use
  ``--persist`` to leave a server running after the first run and reuse it on
  subsequent runs.

See https://github.com/cpitclaudel/esh2tex for more information.

Tips and suggestions
====================

All the following tricks, and more, are demonstrated in the
``example/example.tex`` file of the Git repository.

Loading a different theme
-------------------------

To load a different theme, include the following line in your ``esh-init.el``::

  (load-theme '<theme-name> t) ;; tango, dichromacy, leuven, adwaita...

Using prettification
--------------------

Emacs can render operators using unicode symbols, displaying ``→`` instead of
``->``, for example.  This feature is called ``prettify-symbols-mode``.

To enable it in ESH, add the following to your ``esh-init.el``::

  (add-hook '<mode>-hook #'prettify-symbols-mode) ;; lisp-mode, perl-mode...

You'll probably want to use XeLaTeX of LuaLaTeX for this to work well;
otherwise, ``pdfLaTeX`` will be confused by the Unicode symbols, and probably
won't find a font to display them anyway.

You'll also want to redefine the ``\ESHSpecialChar`` command, too (see below).

Defining inline environments
----------------------------

Here are a few examples of inline environments:

.. code:: latex

   \def\cppverb{\verb}
   %% ESH-inline-verb: c++-mode \cppverb

   \def\pythonverb{\lstinline[language=python]}
   %% ESH-inline-verb: python-mode \pythonverb

Adding these lines to your preamble lets you use ``\pythonverb|yield 1|`` or
``\cppverb/*p++ |= *q++/`` in the body of your document.  With plain ``xelatex``
these will be rendered verbatim, and with ``esh2tex`` they will be highlighted.

Installing extra packages
-------------------------

If the languages that you want to highlight are not supported by Emacs out of
the box, use `Cask <https://github.com/cask/cask>`_ to install the corresponding
packages locally.  This is much cleaner and more stable than loading your full
Emacs configuration (in short, ``Cask`` is to Emacs Lisp what ``VirtualEnv`` is
to Python).

The repo's ``example/`` directory uses a Cask file to manage external
dependencies.

Customizing the output
----------------------

All customizations should be done **before** the ``%% ESH-preamble-here`` line.

Changing fonts::

  ;; Use a roman font for code blocks
  \newcommand{\ESHFont}{\textrm}

  ;; Use Ubuntu Mono for inline code
  \newfontfamily{\UbuntuMono}[Mapping=tex-ansi]{Ubuntu Mono}
  \DeclareTextFontCommand{\ESHInlineFont}{\UbuntuMono}

  ;; Use Symbola for special characters:
  \usepackage{fontspec}
  \newfontfamily{\Symbola}{Symbola}
  \DeclareTextFontCommand{\ESHSpecialCharFont}{\Symbola}

Customizing spacing::

  ;; Leave two blank lines before and after each code block
  \newlength{\ESHSkip}
  \setlength{\ESHSkip}{2\baselineskip}

Overriding the ``ESHBlock`` environment::

  \newenvironment{ESHBlock}{%
    \ESHBasicSetup\par\addvspace{\ESHSkip}\ESHFont
  }{%
    \par\addvspace{\ESHSkip}
  }

All these tricks, and more, are demonstrated in the ``example/example.tex``
subfolder of the repository.

Adding ``esh2tex`` comments in ``org-mode``
-------------------------------------------

See `README.org-mode.rst`_.

Fixing font inconsistencies
---------------------------

If you're having font issues, try switching to XeLaTeX or LuaLaTeX.  ESH uses
Emacs' display engine to guess which characters need to use a fallback font;
this will work well if you use the same fonts in your ``esh-init.el``::

  (set-face-attribute 'default nil :family "Ubuntu Mono")
  (dolist (ft (fontset-list))
    (set-fontset-font ft 'unicode (font-spec :name "Ubuntu Mono"))
    (set-fontset-font ft 'unicode (font-spec :name "Symbola") nil 'append))

and in your XeLaTeX document::

  \usepackage{fontspec}
  \newfontfamily{\Symbola}{Symbola}
  \newfontfamily{\UbuntuMono}{Ubuntu Mono}

  \newcommand{\ESHFont}{\UbuntuMono}
  \DeclareTextFontCommand{\ESHInline}{\UbuntuMono}
  \DeclareTextFontCommand{\ESHSpecialChar}{\Symbola}

Using a different version of Emacs
----------------------------------

If the Emacs in your path isn't the right one, you can use the ``EMACS``
environment variable to let ESH know about the right one::

  EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs esh2tex your-file.tex

Debugging
---------

If you run into issues, try getting the example (in the ``example`` folder of
the repository) to work.  If you can't make the example work, please open a
GitHub issue.

For more advanced debugging, you can load the ``esh`` package into Emacs, and
use ``M-x esh2tex-current-buffer`` on your TeX file::

  cask exec emacs -Q -L . -l esh your-file.tex
