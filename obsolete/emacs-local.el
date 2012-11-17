;;; Get and store Emacs local files.
;;; This is convinent to transfer between one system to another.

(defun change-inhibit-startup-echo-area-message-username ()
  (let ((filename (expand-file-name "default.el" dot-emacs-home)))
    (with-temp-buffer
      (insert-file-contents-literally filename)
      (save-match-data
        (if (re-search-forward
             "inhibit-startup-echo-area-message[\n\t ]*\"\\(.*\\)\")"
             nil t)
            (let ((match (match-string 1)))
              (unless (string= match (user-login-name))
                (replace-match (user-login-name) t nil nil 1)
                (write-region
                 (point-min) (point-max) filename nil 'no-message)
                ;; indicate that we have modified default.el
                t))
          (message "Unable to find `inhibit-startup-echo-area-message' in %s"
                   filename)
          nil)))))

(defvar emacs-local-archive
  (expand-file-name "~/emacs-local.tar.gz"))

(defvar ignore-local-files '(".elc$"))

(defun create-emacs-local-archive ()
  )
