;; Originally taken from
;; http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html.
;; Minor cleanups applied.
(defun open-in-external-app ()
  "Open the current file or dired marked files in external app.
Works in Microsoft Windows, Mac OS X, Linux."
  (interactive)
  (let ((myFileList
         (cond
          ((memq major-mode '(dired-mode sr-mode))
           (dired-get-marked-files))
          (t (list (buffer-file-name))))))
    (and
     (or (<= (length myFileList) 5)
         (y-or-n-p "Open more than 5 files?"))
     (cond
      ((string-equal system-type "gnu/linux")
       (mapc (lambda (fPath)
               (let ((process-connection-type nil))
                 (start-process "" nil
                                (or (executable-find "mimeo")
                                    (executable-find "xdg-open"))
                                fPath)))
             myFileList))
      ((string-equal system-type "windows-nt")
       (mapc (lambda (fPath)
               (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)))
             myFileList))
      ((string-equal system-type "darwin")
       (mapc (lambda (fPath)
               (let ((process-connection-type nil))
                 (start-process "" nil "open" fPath))) myFileList))))))

(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key "E" 'open-in-external-app)))
