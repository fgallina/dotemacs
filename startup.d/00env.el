(let ((bindir (expand-file-name "~/.emacs.d/bin")))
  (setenv "PATH" (concat bindir path-separator (getenv "PATH")))
  (add-to-list 'exec-path bindir))
