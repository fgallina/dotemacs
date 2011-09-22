(add-to-list 'load-path "~/.emacs.d/lisp/color-theme/remember")

(org-remember-insinuate)

(defun make-remember-frame ()
  "turn the current frame into a small popup frame for remember mode;
this is meant to be called with emacsclient -c -e '(make-remember-frame)'.

you might also want to set (setq
org-agenda-skip-unavailable-files t) so these warnings won't
annoy the little remember-frame"
  (let ((org-agenda-skip-unavailable-files t))
    (modify-frame-parameters nil
                             '( (name . "*Remember*") ;; must be same as in mode-hook below
                                (width .  80)
                                (height . 20)
                                (vertical-scroll-bars . nil)
                                (menu-bar-lines . nil)
                                (tool-bar-lines . nil)))
    (org-remember)
    (when (fboundp 'x-focus-frame) (x-focus-frame nil)) ;; X only....
    (delete-other-windows)))

;; when we're in such a remember-frame, close it when done.
(add-hook 'org-remember-mode-hook
  (lambda()
    (define-key org-remember-mode-map (kbd "C-c C-c")
      '(lambda()(interactive)
         (let ((remember-frame-p
                 (string= (frame-parameter nil 'name) "*Remember*")))
           (when remember-frame-p (make-frame-invisible))  ;; hide quickly
           (org-remember-finalize)
           (when remember-frame-p (delete-frame)))))))

(setq org-remember-templates
      '(("ServerAccess" ?a "* %^{Project}\n  %?"
         "~/Org/remember/access.org" "Server Access")
        ("RegionSnippet" ?r "* %T %^{Description}\n  %i"
         "~/Org/remember/code.org" "Interesting")
        ("ClipboardSnippet" ?s "* %T %^{Description}\n  %x"
         "~/Org/remember/code.org" "Interesting")
        ("Note" ?n "* %T %^{Description}\n  %?"
         "~/Org/remember/notes.org" "Note")
        ("Clipboard" ?c "* %T %^{Description}\n  %x"
         "~/Org/remember/clipboard.org" "Interesting")))
