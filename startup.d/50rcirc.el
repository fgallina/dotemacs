(eval-after-load 'rcirc
  '(progn
     ;(require 'rcirc-notify)

     (defun rcirc-mark-all-as-read ()
       (interactive)
       (progn (setq rcirc-activity nil)
              (rcirc-update-activity-string)))

     (defun-rcirc-command op (input)
       "Op myself on the current channel."
       (interactive "s")
       (rcirc-send-message process "chanserv"
			   (concat "op " target)))
     (defun-rcirc-command deop (input)
       "Deop myself on the current channel."
       (interactive "s")
       (rcirc-send-message process "chanserv"
			   (concat "deop " target)))
     (defun-rcirc-command mute (input)
       "Mute nick"
       (interactive "s")
       (rcirc-send-string process (format "MODE %s +q %s!*@*"
					  target input)))
     (defun-rcirc-command unmute (input)
       "Unmute nick"
       (interactive "s")
       (rcirc-send-string process (format "MODE %s -q %s!*@*"
					  target input)))
     (defun-rcirc-command ban (input)
       "Ban nick"
       (interactive "s")
       (rcirc-send-string process (format "MODE %s +b %s!*@*"
					  target input)))
     (defun-rcirc-command unban (input)
       "Unban nick"
       (interactive "s")
       (rcirc-send-string process (format "MODE %s -b %s!*@*"
					  target input)))
     (defun-rcirc-command kickban (input)
       "Kickban nick"
       (interactive "s")
       (rcirc-send-string process (format "MODE %s +b %s!*@*"
					  target input))
       (rcirc-send-string process (format "KICK %s %s kickban!"
					  target input)))))

(setq rcirc-log-flag t)
(setq rcirc-server-alist
      '(("irc.freenode.net"
         :channels ("#rcirc" "#emacs" "#lugro" "#lugro-mesh"
                    "#python" "#pyar" "#django-es" "#django"
                    "#django-dev" "#archlinux" "#the_it_crowd"
                    "#machinalis""#anue" "#org-mode" "#python.el"
                    "#jquery" "#logn" "#logn-ar"))))
(setq rcirc-time-format "%Y-%m-%d %H:%M ")
(rcirc-track-minor-mode 1)
