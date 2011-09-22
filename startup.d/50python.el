(setenv "PYMACS_PYTHON" "python2")

(add-to-list 'load-path "~/Workspace/python-mode")
(require 'pymacs)
(require 'python)

(setq python-shell-interpreter "python2")

;; (setq
;;  python-shell-interpreter "ipython"
;;  python-shell-interpreter-args ""
;;  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;  python-shell-completion-setup-code ""
;;  python-shell-completion-string-code "';'.join(__IP.complete('''%s'''))\n")

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

(pymacs-load "ropemacs" "rope-")
(setq ropemacs-codeassist-maxfixes 5
      ropemacs-guess-project t
      ropemacs-enable-autoimport t
      ropemacs-completing-read-function 'completing-read)

(defun strip-whitespace (string)
  "Return STRING stripped of all whitespace."
  (while (string-match "[\r\n\t ]+" string)
    (setq string (replace-match "" t t string)))
  string)

(defun try-complete-ropemacs (old)
  (save-excursion
    (unless old
      (he-init-string (he-dabbrev-beg) (point))
      (when (not (equal he-search-string ""))
        (setq he-expand-list
              (sort (all-completions
                     he-search-string
                     (delete-duplicates
                      (mapcar
                       (lambda (completion)
                         (concat he-search-string
                                 (strip-whitespace
                                  (nth 0 (split-string completion ":")))))
                       (ignore-errors
                         (rope-completions)))
                      :test 'string=))
                    'string-lessp))))
    (while (and he-expand-list
                (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if (null he-expand-list)
        (progn (if old (he-reset-string)) ())
      (progn
        (he-substitute-string (car he-expand-list))
        (setq he-tried-table (cons (car he-expand-list)
                                   (cdr he-tried-table)))
        t))))

(add-hook 'python-mode-hook
	  (lambda ()
            (set (make-local-variable 'hippie-expand-try-functions-list)
                  '(yas/hippie-try-expand
                    try-complete-file-name
                    try-complete-ropemacs))))

(setq pdb-path '/usr/lib/python2.7/pdb.py
      gud-pdb-command-name (symbol-name pdb-path))

(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path (file-name-nondirectory
				      buffer-file-name)))))
