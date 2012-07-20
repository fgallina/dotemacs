(require 'emms-setup)
(emms-devel)
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

;; Libtag support
(require 'emms-info-libtag)
(setq emms-info-functions '(emms-info-libtag))

;; Stolen and adapted from TWB
(defun my-emms-info-track-description (track)
  "Return a description of the current track."
  (if (and (emms-track-get track 'info-artist)
           (emms-track-get track 'info-title))
      (let ((pmin (emms-track-get track 'info-playing-time-min))
            (psec (emms-track-get track 'info-playing-time-sec))
            (ptot (emms-track-get track 'info-playing-time))
            (art  (emms-track-get track 'info-artist))
            (tit  (emms-track-get track 'info-title)))
        (cond ((and pmin psec) (format "%s - %s [%02d:%02d]" art tit pmin psec))
              (ptot (format  "%s - %s [%02d:%02d]" art tit (/ ptot 60) (% ptot 60)))
              (t (emms-track-simple-description track))))
    (let ((name (emms-track-name (emms-playlist-current-selected-track))))
      (when name
        (file-name-nondirectory name)))))

(setq emms-track-description-function 'my-emms-info-track-description)

(setq
 emms-info-asynchronously t
 emms-info-functions '(emms-info-libtag)
 later-do-interval 0.0001
 emms-mode-line-format " %s "
 emms-show-format "NP: %s")


(define-key emms-playlist-mode-map "A" 'emms-awim)
(define-key emms-playlist-mode-map "P" 'emms-pwim)

(global-set-key (kbd "C-x m") 'emms)
