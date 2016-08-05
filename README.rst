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

Clone the repository somewhere, and add ``<wherever>/bin`` to your path.  This
program is tested only on GNU/Linux.


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

Process with ``esh2tex minimal.tex > esh2tex.esh.tex``, and compile with
``xelatex esh2tex.esh.tex``. See the ``example/`` directory of the Git
repository for a more advanced example.


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

in its preamble, which esh2tex will replace by appropriate set-up code
(``\usepackage``, definition of ``ESHBlock``, etc.).


You can enable highlighting of specific inline macros using the following
special comment::

  %% ESH-inline: <mode> <pattern mentionning '...'>

For example::

  %% ESH-inline: c-mode \verb|...|
  %% ESH-inline: python-mode \python{...}


Options
=======

* ``--usage``

  Show this help.

* ``--kill-server``

  Kill previously-started instances of the ESH server.

* ``--persist``

  Leave server running after processing ``<input>``.  Don't forget to
  ``--kill-server`` if you make changes to your configuration!

* ``--cask``

  Use ``cask exec emacs`` instead of ``emacs`` to start the
  syntax-highlighting subprocess.

* ``--no-Q``

  Load your full Emacs configuration instead of an ``esh-init.el`` file.  Use
  this option with caution; there are subtle differences between ESH and a
  regular Emacs that can prevent your Emacs configuration from loading properly.

* ``--debug-on-error``

  Print stack traces for errors.

Notes
=====

* esh2tex does not load your personal Emacs configuration (though see the
  ``--no-Q`` option); instead, it looks for a file named esh-init.el in the
  current directory, one of its parents, or ~/.emacs.d/.  You can use that file
  to chose a different theme, load packages (though see also the ``--cask``
  option), etc.

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
   %% ESH-inline: c++-mode \cppverb|...|

   \def\pythonverb{\lstinline[language=python]}
   %% ESH-inline: python-mode \pythonverb|...|

Adding these lines to your preamble let's you use ``\pythonverb|yield 1|`` or
``\cppverb|*p++ = !*q++|`` in the body of your document.  With plain ``xelatex``
these will be rendered verbatim, and with ``esh2tex`` they will be highlighted.

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

Debugging
---------

If you run into issues, try getting the example (in the ``example`` folder of
the repository) to work.  If you can't make the example work, please open a
GitHub issue.

For more advanced debugging, you can load the ``esh`` package into Emacs, and
use ``M-x esh2tex-current-buffer`` on your TeX file.
