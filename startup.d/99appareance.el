(when (and (boundp 'custom-safe-themes)
           (load-theme 'deeper-blue t))
  ;; Fix deeper blue theme terminal colors.
  (face-spec-set 'default
                 '((((class color)
                     (min-colors 4096))
                    (:background "#181a26" :foreground "gray80"))
                   (((class color) (min-colors 89))
                    (:background "#1c1c1c" :foreground "#e1e1e0"))))
  (mapc (lambda (face)
          (face-spec-set face
                         '((((class color)
                             (min-colors 4096))
                            (:foreground "gray50"))
                           (((class color) (min-colors 89))
                            (:foreground "#808080")))))
        '(font-lock-comment-delimiter-face font-lock-comment-face)))

(setq-default
 frame-title-format
 '(:eval
   (format "%s@%s:%s"
           (or (file-remote-p default-directory 'user) user-login-name)
           (or (file-remote-p default-directory 'host) system-name)
           (or (buffer-file-name)
               (and (member (current-buffer) (emms-playlist-buffer-list))
                    (emms-playlist-current-selected-track)
                    (emms-track-description
                     (emms-playlist-current-selected-track)))
               (buffer-name)))))
