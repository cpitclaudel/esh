=============================
 Using esh2tex with Org-mode
=============================

Code blocks
===========

Make sure that ``org-latex-listings`` is nil::

  (setq org-latex-listings nil)

Add the following to the beginning of your org file::

  #+LATEX_HEADER: %% ESH-preamble-here

Then before each Org source block::

  #+LATEX: %% ESH: <mode>

Alternatively, the following snippet will add the headers automatically upon export:

.. code:: emacs-lisp

   (defun ~/org-latex-src-block (fn src-block contents info)
     "Prefix ESH comment to environment in return of `org-latex-src-block'.
   FN is the unadvised `org-latex-src-block' (which see about
   SRC-BLOCK, CONTENTS, and INFO)."
     (let ((retv (funcall fn src-block contents info))
           (lang (org-element-property :language src-block)))
       (if (and (stringp retv) lang)
           (with-temp-buffer
             (insert retv)
             (goto-char (point-min))
             (when (search-forward "\\begin" nil t)
               (goto-char (match-beginning 0))
               (insert "%% ESH: " lang "-mode\n"))
             (buffer-substring-no-properties (point-min) (point-max)))
         retv)))

   (advice-add 'org-latex-src-block :around #'~/org-latex-src-block)

Inline code
===========

For inline code, start with this to ensure that you get ``\verb`` in the output
when you use verbatim or code in the input:

.. code:: lisp

  (with-eval-after-load 'ox-latex
    (dolist (env '(code verbatim))
      (setcdr (assoc env org-latex-text-markup-alist) 'verb)))

The prefix each inline code block like this::

  This is an example of C++ code: (c++) ~int main() { return 0; }~.
  This, on the other hand, is Python: (python) ~def main(): return 0~.

And add the following lines to your Org header::

  #+LATEX_HEADER: %% ESH-inline-verb: c++-mode (c++) \verb
  #+LATEX_HEADER: %% ESH-inline-verb: python-mode (python) \verb


Preamble
========

Get rid of inputenc, fontenc, babel, etc:

.. code:: emacs-lisp

   (with-eval-after-load 'org
    (setq org-latex-default-packages-alist
          '(("AUTO" "polyglossia" t)
            ("" "fontspec" t)
            ("" "fixltx2e" nil)
            ("" "graphicx" t)
            ("" "grffile" t)
            ("" "longtable" nil)
            ("" "wrapfig" nil)
            ("" "rotating" nil)
            ("normalem" "ulem" t)
            ("" "amsmath" t)
            ("" "textcomp" t)
            ("" "amssymb" t)
            ("" "capt-of" nil)
            ("" "hyperref" nil))))


Full example
============

::

   #+LATEX_HEADER: %% ESH-preamble-here
   #+LATEX_HEADER: %% ESH-inline-verb: c++-mode (c++) \verb
   #+LATEX_HEADER: %% ESH-inline-verb: python-mode (python) \verb

   * Code blocks

     Here's a block of code:

     #+LATEX: %% ESH: c++-mode
     #+BEGIN_SRC c++
     int main() {
       return 0;
     }
     #+END_SRC

   * Inline code

     This is an example of C++ code: (c++) ~int main() { return 0; }~.
     This, on the other hand, is Python: (python) ~def main(): return 0~.

   # Local Variables:
   # org-latex-listings: nil
   # End:
