(require 'flymake)
(require 'python)

(setq flymake-gui-warnings-enabled nil)
(setq flymake-log-level -1)

(defun flymake-goto-error-and-message (&optional direction)
  (ignore-errors
    (let ((direction (or direction 1)))
      (if (>= direction 0)
          (flymake-goto-next-error)
        (flymake-goto-prev-error))
      (message "%s"
               (flymake-ler-text
                (caar (flymake-find-err-info
                       flymake-err-info
                       (flymake-current-line-no))))))))

(defun flymake-goto-next-error-and-message ()
  (interactive)
  (flymake-goto-error-and-message 1))

(defun flymake-goto-prev-error-and-message ()
  (interactive)
  (flymake-goto-error-and-message -1))

(defvar flymake-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-p" 'flymake-goto-prev-error-and-message)
    (define-key map "\M-n" 'flymake-goto-next-error-and-message)
    map)
  "Keymap for flymake minor mode.")

(add-to-list 'minor-mode-map-alist `(flymake-mode . ,flymake-minor-mode-map) t)

(defun flymake-python-init ()
  (let* ((process-environment (python-shell-calculate-process-environment))
         (exec-path (python-shell-calculate-exec-path))
         (checker (executable-find "python-check")))
    (when checker
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list checker (list local-file))))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-python-init))

(defadvice flymake-start-syntax-check-process
  (around python-flymake-start-syntax-check-process (cmd args dir))
  "`flymake-start-syntax-check-process' with virtualenv support."
  (if (eq major-mode 'python-mode)
      (let* ((process-environment (python-shell-calculate-process-environment))
             (exec-path (python-shell-calculate-exec-path)))
        ad-do-it)
    ad-do-it))
(ad-activate 'flymake-start-syntax-check-process)

(defun flymake-find-file-hook-noerror ()
  (ignore-errors (flymake-find-file-hook)))

(add-hook 'find-file-hook 'flymake-find-file-hook-noerror)
