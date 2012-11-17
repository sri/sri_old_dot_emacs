;;; home.el -- initialization for home
;;; Initializations for home.

(require 'dired)

(defvar user-homedir-ignore-files
  '(".ICEauthority" ".XSM-Default" ".Xauthority" ".Xdefaults"
    ".autosaves" ".dialdir" ".emacs.d" ".esd_auth" ".gnome*"
    ".gqviewrc" ".gtk-black-bg" ".gtkrc" ".inputrc" ".mwmrc"
    ".newsrc.eld" ".sawmill" ".sawmillrc-old" ".xsm*")
  "Filename to ignore in user's home directory.
Filenames may contain regular expressions.")

(defun user-homedir ()
  "Omits files listed in `user-homedir-ignore-files' and display
users home directory."
  (interactive)
  (let ((dired-omit-files
         (concat dired-omit-files "\\|"
                 (mapconcat 'identity user-homedir-ignore-files
                            "\\|"))))
    (dired "~")
    ;; bind `g' to call user-homedir again
    ))

(global-set-key (kbd "C-j ~") 'user-homedir)

(setq *rfc-location-prefix*
      "/usr/doc/doc-rfc/all-included-rfcs/")


(define-keys global-map
  "ESC [ C-a"  previous-line
  "ESC [ C-b"  next-line
  "ESC [ C-c"  forward-char
  "ESC [ C-d"  backward-char)

(define-keys dired-mode-map
  "ESC [ C-a" dired-previous-line
  "ESC [ C-b" dired-next-line
  "ESC [ C-c" dired-advertised-find-file
  "ESC [ C-d" dired-up-directory)

(defun src ()
  (interactive)
  (dired "/usr/local/src/"))


;;(load "~/code/smsnotice/smsnotice/smsnotice-emacs.el")
