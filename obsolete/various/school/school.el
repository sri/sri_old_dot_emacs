;;; school.el -- initialization for school
;;; Initializations for school.

(setq user-mail-address "")

;;; Not used in Message/Gnus.  See `message-default-mail-headers' below.
(setq mail-archive-file-name
      "~/Mail/sentmail-archive/asu-imap-sentmail")

;; (let ((date (format-time-string "%Y-%m-%d"))
;;       (int-month
;;        (string-to-number (format-time-string "%m"))))
;;   (format "~/Mail/sentmail.%s"
;;           date))

(load-maybe-byte-compiled
 (expand-file-name "gnus-init" dot-emacs-home))
(load-maybe-byte-compiled
 (expand-file-name "fetchmail" "~/files/mail"))



(setq *rfc-location-prefix*
      "http://www.ietf.org/rfc/")

(require 'lynx)

(defun browse-url (url &rest args)
  (when args
    (error "local version of BROWSE-URL doesn't support arguments"))
  (lynx-fetch-url url))

(let ((file (expand-file-name "~/school/school.el")))
  (when (file-exists-p file)
    (cload file)))

;;; KDE screws up the configuration in CET383 class
;;; This doesn't work
;;;(let ((value (getenv "DISPLAY")))
;;;  (when (and value
;;;             (save-match-data (string-match "general.*\\.0" value)))
;;;    (set-background-color "black")
;;;    (set-foreground-color "white")))
