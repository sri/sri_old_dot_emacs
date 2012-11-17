;;; build-emacs.el -- custom build GNU Emacs.

(require 'ut-time)                      ;Local

(defvar emacs-source-directory "~/programs/emacs-21.3/"
  "The source tree for GNU Emacs.")

(defvar quit-char-replacement "'G'"
  "Replacement for quit_char in src/keyboard.c.
The original line that sets the quit_char in src/keyboard.c is:
  quit_char = Ctl ('g');
The value of this variable replaces everything after `= ' till `;'.")

(defun change-quit-char ()
  "Replaces the quit_char in src/keyboard.c.
See documentation for the variable QUIT-CHAR-REPLACEMENT."
  (let ((fname (expand-file-name "src/keyboard.c" emacs-source-directory))
        (regexp (format "^  quit_char = \\(Ctl ('g')\\|%s\\);$"
                        ;; don't complain if we have already done this before
                        quit-char-replacement)))
    (with-temp-buffer
      (insert-file-contents fname)
      (save-match-data
        (re-search-forward regexp)
        (let ((match (buffer-substring-no-properties (match-beginning 1)
                                                     (match-end 1))))
          (unless (string= match quit-char-replacement)
            (replace-match quit-char-replacement t nil nil 1)
            (write-region 1 (point-max) fname nil 'no-message)))))))

(defvar emacs-configure-options nil)
(defvar emacs-make-options nil)

(defvar emacs-build-start-time nil)

;;; With a prefix-arg, use previous CONFIGURE and MAKE options.

(defun build-emacs (arg)
  (interactive "P")
  (setq emacs-build-start-time (get-universal-time))
  (with-current-buffer (get-buffer-create "*build-emacs*")
    (erase-buffer)
    (cd emacs-source-directory)
    (unless arg
      (setq emacs-configure-options
            (read-string "Configure options: " system-configuration-options))
      (setq emacs-make-options (read-string "Make options: ")))
    (when quit-char-replacement
      (change-quit-char))
    ;; programs to run in the order listed
    (let* ((programs (list "make distclean"
                           (format "./configure %s" emacs-configure-options)
                           (format "make %s" emacs-make-options)))
           (command (mapconcat 'identity programs " && "))
           (process (start-process-shell-command "*build-emacs*"
                      "*build-emacs*" command)))
      (set-process-sentinel process
        (lambda (process event)
          (when (zerop (process-exit-status process))
            (with-current-buffer (process-buffer process)
              (let* ((secs (- (get-universal-time) emacs-build-start-time))
                     (mins (/ secs 60.0)))
                (insert
                 "\n\n"
                 (format "** Done building emacs in %.2f seconds (%.2f minutes)"
                         secs mins)
                 "\n")))))))))

(provide 'build-emacs)

;;; build-emacs.el ends here
