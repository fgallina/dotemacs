
;;;###autoload
(defun mc/unmark-next-like-this (arg)
  "Deselect next part of the buffer matching the currently active region."
  (interactive)
  (mc/mark-next-like-this -1))

;;;###autoload
(defun mc/unmark-previous-like-this (arg)
  "Deselect prev part of the buffer matching the currently active region."
  (interactive)
  (mc/mark-previous-like-this -1))
