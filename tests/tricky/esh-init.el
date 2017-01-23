(when esh-server-initializing
  (load-theme 'tango t))

(define-minor-mode tricky-mode
  "Major mode demonstrating tricky highlighting."
  :lighter " trk"
  (esh-add-keywords
    `(("abcdef" (0 '(face ((:slant italic))) append))
      ("efghij" (0 '(face ((:foreground "red"))) append))
      ("ijklmn" (0 '(face ((:weight bold))) append))
      ("bcdefghijkl" (0 '(face ((:box t))) append))
      ("4" (0 '(face ((:height 2.5))))))))
