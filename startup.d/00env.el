(let ((bindirs (list
                (expand-file-name "~/.emacs.d/bin")
                (expand-file-name "~/bin"))))
  (dolist (dir bindirs)
    (setenv "PATH" (concat dir path-separator (getenv "PATH")))
    (add-to-list 'exec-path dir)))
