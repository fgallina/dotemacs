(require 'emms-setup)
(emms-standard)
(emms-default-players)


;;; From http://www.gnu.org/software/emms/configs/lb-emms.el
;; play what i mean
(defun emms-pwim (truc &optional rien)
  "Plays the TRUC specified, whatever it is. The function tries to
guess the type of TRUC, between playlist, directory containing
playable tracks, and files. If the directory does not contain
playable tracks, but some sub-directories, it will play the
tree."
  (interactive
   (find-file-read-args "Play what ? " t))
  (cond
   ((file-directory-p truc)
    (emms-play-directory-tree truc))
   ((file-exists-p truc)
    (emms-play-file truc))))

;; add what i mean
(defun emms-awim (truc &optional rien)
  "Adds the TRUC specified, whatever it is. The function tries to
guess the type of TRUC, between playlist, directory containing
playable tracks, and files. If the directory does not contain
playable tracks, but some sub-directories, it will add the
tree."
  (interactive
   (find-file-read-args "Add what ? " t))
  (cond
   ((file-directory-p truc)
    (emms-add-directory-tree truc))
   ((file-exists-p truc)
    (emms-add-file truc))))

(define-key emms-playlist-mode-map "A" 'emms-awim)
(define-key emms-playlist-mode-map "P" 'emms-pwim)

(global-set-key (kbd "C-x m") 'emms)
