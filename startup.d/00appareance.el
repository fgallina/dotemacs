(when (boundp 'custom-safe-themes)
  (load-theme 'deeper-blue t))

(setq-default
 frame-title-format
 '(:eval
   (format "Emacs - %s@%s:%s"
           (or (file-remote-p default-directory 'user) user-login-name)
           (or (file-remote-p default-directory 'host) system-name)
           (or (buffer-file-name)
               (and (fboundp 'emms-playlist-buffer-list)
                    (member (current-buffer) (emms-playlist-buffer-list))
                    (emms-playlist-current-selected-track)
                    (emms-track-description
                     (emms-playlist-current-selected-track)))
               (buffer-name)))))
