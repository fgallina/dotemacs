(require 'emms-setup)
(emms-devel)
(emms-default-players)


;;; From http://www.gnu.org/software/emms/configs/lb-emms.el
;; play what i mean
(defun my-emms-pwim (truc &optional rien)
  "Plays the TRUC specified, whatever it is. The function tries to
guess the type of TRUC, between playlist, directory containing
playable tracks, and files. If the directory does not contain
playable tracks, but some sub-directories, it will play the
tree."
  (interactive
   (find-file-read-args "Play what? " t))
  (cond
   ((file-directory-p truc)
    (emms-play-directory-tree truc))
   ((file-exists-p truc)
    (emms-play-file truc))))

;; add what i mean
(defun my-emms-awim (truc &optional rien)
  "Adds the TRUC specified, whatever it is. The function tries to
guess the type of TRUC, between playlist, directory containing
playable tracks, and files. If the directory does not contain
playable tracks, but some sub-directories, it will add the
tree."
  (interactive
   (find-file-read-args "Add what? " t))
  (cond
   ((file-directory-p truc)
    (emms-add-directory-tree truc))
   ((file-exists-p truc)
    (emms-add-file truc))))

(defun my-emms ()
  "Same as `emms' but use `my-emms-pwim' at startup."
  (interactive)
  (if (or (null emms-playlist-buffer)
          (not (buffer-live-p emms-playlist-buffer)))
      (call-interactively 'my-emms-pwim))
  (emms-playlist-mode-go))

;; Libtag support
(require 'emms-info-libtag)

;; Stolen and adapted from TWB
(defun my-emms-info-track-description (track)
  "Return a description of the current track."
  (if (not (eq (emms-track-type track) 'file))
      (emms-track-simple-description track)
    (let* ((ptot (emms-track-get track 'info-playing-time))
           (pmin (or
                  (emms-track-get track 'info-playing-time-min)
                  (and ptot (/ ptot 60)) 0))
           (psec (or
                  (emms-track-get track 'info-playing-time-sec)
                  (and ptot (% ptot 60)) 0))
           (artist (or (emms-track-get track 'info-artist) "unknown"))
           (title (or (emms-track-get track 'info-title)
                      (file-name-nondirectory (emms-track-get track 'name)))))
      (format "%s - %s [%02d:%02d]" artist title pmin psec))))

(setq emms-track-description-function 'my-emms-info-track-description)

(setq
 emms-info-asynchronously t
 emms-info-functions '(emms-info-libtag)
 later-do-interval 0.0001
 emms-mode-line-format " %s "
 emms-show-format "NP: %s"
 emms-info-libtag-program-name "~/.emacs.d/bin/emms-print-metadata")

(emms-cache -1)
(emms-mode-line 1)
(emms-playing-time -1)

(define-key emms-playlist-mode-map "A" 'my-emms-awim)
(define-key emms-playlist-mode-map "P" 'my-emms-pwim)
(define-key emms-playlist-mode-map "S" 'emms-streams)
(define-key emms-stream-mode-map "q" 'emms)
(define-key emms-stream-mode-map "Q" 'emms)
(global-set-key (kbd "C-x m") 'my-emms)
