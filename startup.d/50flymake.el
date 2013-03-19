(require 'flymake)

(setq flymake-gui-warnings-enabled nil)
(setq flymake-log-level -1)

(defun flymake-goto-error-and-message (&optional direction)
  "Goto next/prev error and message it.
Argument DIRECTION is an integer.  When >= 0 direction is
forward, else backwards."
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

(defun flymake-goto-next-error-and-message (&optional num)
  "Goto next error message.
Argument NUM is the number of error moves."
  (interactive "p")
  (flymake-goto-error-and-message (or num 1)))

(defun flymake-goto-prev-error-and-message (&optional num)
  "Goto prev error message.
Argument NUM is the number of error moves."
  (interactive "p")
  (flymake-goto-error-and-message (- (or num 1))))

(defvar flymake-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-p" 'flymake-goto-prev-error-and-message)
    (define-key map "\M-n" 'flymake-goto-next-error-and-message)
    map)
  "Keymap for flymake minor mode.")

(add-to-list 'minor-mode-map-alist `(flymake-mode . ,flymake-minor-mode-map) t)
(add-hook 'find-file-hook 'flymake-find-file-hook)
