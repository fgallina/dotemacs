(setq yas-snippet-dirs '("~/.emacs.d/el-get/yasnippet/snippets" "~/.emacs.d/snippets")
      yas-prompt-functions '(yas-ido-prompt yas-no-prompt))

(yas-global-mode 1)

(defun yas-not-activate ()
  (memq major-mode '(term-mode)))

(set-default 'yas-dont-activate (cons #'yas-not-activate yas-dont-activate))
