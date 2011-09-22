;; Extracted from http://www.emacswiki.org/emacs/dired-extension.el
(defun dired-gnome-open-file ()
  "Opens the current file in a Dired buffer."
  (interactive)
  (gnome-open-file (dired-get-file-for-visit)))

(defun gnome-open-file (filename)
  "gnome-opens the specified file."
  (interactive "fFile to open: ")
  (let ((process-connection-type nil))
    (start-process "" nil "/usr/bin/gnome-open" filename)))

(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key "E" 'dired-gnome-open-file)))
