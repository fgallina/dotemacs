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
  "Same as `emms' but allow creating an empty playlist."
  (interactive)
  (and (or (null emms-playlist-buffer)
           (not (buffer-live-p emms-playlist-buffer)))
       (setq emms-playlist-buffer (emms-playlist-new)))
  (emms-playlist-mode-go))

;; Libtag support
(require 'emms-info-libtag)

(defun my-emms-default-track-set (track name value)
  "Set TRACK attribute NAME to VALUE if not already set."
  (let ((current-value (emms-track-get track name)))
    (and (or (not current-value)
             (and (eq name 'info-title)
                  (string-match
                   "^[tT]rack[[:space:]]+[[:digit:]]\\{1,2\\}"
                   current-value)))
         (emms-track-set track name value))))

;; Euristic filename metadata by fgallina
(defun my-emms-info-filename-heuristic (track)
  "Guess file metadata for TRACK.
This function assumes TRACK's filename is in any of the formats:
  + <artist> - <title>
  + <tracknumber> - <title>
  + <tracknumber> - <artist> - <title>
  + <artist> - <tracknumber> - <title>
  + <tracknumber> - <artist> - <album> - <title>
  + <artist> - <tracknumber> - <album> - <title>
  + <artist> - <album> - <tracknumber> - <title>"
  (when (eq 'file (emms-track-type track))
    (let* ((filename
            (replace-regexp-in-string
             (rx (and word-boundary
                      (group (repeat 1 3 digit))
                      (and (? space) (or ?. ?-))))
             "\\1 -"
             (file-name-sans-extension
              (file-name-nondirectory (emms-track-name track)))))
           (splits (mapcar #'(lambda (str)
                               (replace-regexp-in-string
                                (rx (or (and line-start (+ space))
                                        (and (+ space) line-end)))
                                "" str))
                           (split-string filename (rx (? space) ?- space))))
           (number-pos (position-if
                        #'(lambda (str)
                            (and (< (length str) 4)
                                 (> (string-to-number str) 0))) splits))
           (num-splits (length splits)))
      (and number-pos
           (my-emms-default-track-set track 'info-tracknumber
                           (nth number-pos splits)))
      (and (> (length splits) 0)
           (my-emms-default-track-set track 'info-title (car (last splits))))
      (cond ((=  num-splits 0) nil)     ;; Nothing to do here.
            ((not number-pos)
             ;; assume <artist> - <title>
             (my-emms-default-track-set track 'info-artist (nth 0 splits))
             (and (> num-splits 2)
                  (my-emms-default-track-set track 'info-album (nth 1 splits))))
            ((=  num-splits 3)
             (if (= number-pos 0)
                 ;; assume <tracknumber> - <artist> - <title>
                 (my-emms-default-track-set track 'info-artist (nth 1 splits))
               ;; assume <artist> - <tracknumber> - <title>
               (my-emms-default-track-set track 'info-artist (nth 0 splits))))
            ((>  num-splits 3)
             (if (= number-pos 0)
                 ;; assume <tracknumber> - <artist> - <album> - <title>
                 (my-emms-default-track-set track 'info-artist (nth 1 splits))
                 (my-emms-default-track-set track 'info-album (nth 2 splits))
               (if (= number-pos 1)
                   ;; assume <artist> - <tracknumber> - <album> - <title>
                   (progn
                     (my-emms-default-track-set
                      track 'info-artist (nth 0 splits))
                     (my-emms-default-track-set
                      track 'info-album (nth 2 splits)))
                 ;; assume <artist> - <album> - <tracknumber> - <title>
                 (my-emms-default-track-set
                  track 'info-artist (nth 0 splits))
                 (my-emms-default-track-set
                  track 'info-album (nth 1 splits)))))))))

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
           (tracknumber (string-to-number
                         (or (emms-track-get track 'info-tracknumber) "0")))
           (artist (or (emms-track-get track 'info-artist) "unknown"))
           (title (or (emms-track-get track 'info-title)
                      (file-name-nondirectory (emms-track-get track 'name)))))
      (format "%02d. %s - %s [%02d:%02d]" tracknumber artist title pmin psec))))

(setq emms-track-description-function 'my-emms-info-track-description)

(setq
 emms-info-asynchronously t
 emms-info-functions '(emms-info-libtag my-emms-info-filename-heuristic)
 later-do-interval 0.0001
 emms-mode-line-format " %s "
 emms-show-format "NP: %s"
 emms-info-libtag-program-name "emms-print-metadata"
 emms-repeat-playlist t)

(emms-cache 1)
(emms-mode-line -1)
(emms-playing-time-enable-display)

(define-key emms-playlist-mode-map "A" 'my-emms-awim)
(define-key emms-playlist-mode-map "P" 'my-emms-pwim)
(define-key emms-playlist-mode-map "S" 'emms-streams)
(define-key emms-playlist-mode-map "h" 'describe-mode)
(define-key emms-playlist-mode-map "H" 'describe-mode)
(define-key emms-playlist-mode-map "?" 'emms-shuffle)
(define-key emms-playlist-mode-map "g" 'emms-cache-sync)
(define-key emms-playlist-mode-map " " 'emms-playlist-mode-play-smart)
(define-key emms-stream-mode-map "q" 'emms)
(define-key emms-stream-mode-map "Q" 'emms)
(global-set-key (kbd "C-x m") 'my-emms)
