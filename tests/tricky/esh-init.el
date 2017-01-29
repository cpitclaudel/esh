(when esh-server-initializing
  (load-theme 'tango t))

(define-minor-mode tricky-mode
  "Major mode demonstrating tricky highlighting."
  :lighter " trk"
  (add-to-list 'font-lock-extra-managed-props 'line-height)
  (esh-add-keywords
    `(("abcdef" (0 '(face ((:slant italic))) append))
      ("efghij" (0 '(face ((:foreground "red"))) append))
      ("ijklmn" (0 '(face ((:weight bold))) append))
      ("bcdefghijkl" (0 '(face ((:box t))) append))
      ("4" (0 '(face ((:height 2.5)))))
      ("tall\\(\n\\)" (1 '(face nil line-height 2.0))))))


(define-minor-mode subsup-mode
  "Major mode demonstrating tricky highlighting."
  :lighter " subssup"
  (add-to-list 'font-lock-extra-managed-props 'display)
  (esh-add-keywords
    `(("x\\(.*\\)" (1 '(face nil display (raise 1))))
      ("y\\(.*\\)" (1 '(face nil display (raise -1)))))))
