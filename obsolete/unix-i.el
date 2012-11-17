;;; unix.el -- an interface to unix program
;;;            and experiments using X from within GNU Emacs


;;; Utilities:

(defun unix-buffer (x)
  (let ((name (format "*unix %s*" (prin1-to-string x))))
    (get-buffer-create name)))

;;; Elements are of the form (BUFFER-NAME CONF MARKER).
;;; It might probably be easier if this variable was buffer-local...
(defvar unix-previous-configurations nil)

(defun unix-save-current-configuration ()
  (push (list (current-buffer) (current-window-configuration) (point-marker))
        unix-previous-configurations))

(defun unix-restore-previous-configuration ()
  (let ((buffer (current-buffer))
        (tail unix-previous-configurations))
    (if (eq (caar tail) buffer)
        (cdr (pop unix-previous-configurations))
      (let ((last tail) conf)
        (setq tail (cdr tail))
        (while (and tail (not conf))
          (if (not (eq (caar tail) buffer))
              (setq last tail tail (cdr tail))
            (setq conf (cdar tail))
            (setcdr last (cdr tail))))
        conf))))

;;; If this is non-NIL, then when reusing buffers, tell us that we
;;; don't need to make a new keymap.

(defvar unix-buffer-keymap-initialized-p nil)
(make-variable-buffer-local
 'unix-buffer-keymap-initialized-p)



;;; Commands:

(defun unix-ps ()
  (interactive)
  (with-current-buffer (unix-buffer "ps")
    (unix-save-current-configuration)
    (let (buffer-read-only)
      (erase-buffer)
      (call-process "ps" nil t nil
                    (if (eq system-type 'gnu/linux) "--user" "-u")
                    (user-login-name))
      (goto-char (point-min))
      (save-excursion
        ;; to be extra save remove this emacs' pid from the
        ;; list shown there
        (when (re-search-forward (format "%s" (emacs-pid)) nil t)
          (delete-region (progn (beginning-of-line) (point))
                         (progn (end-of-line) (forward-char 1) (point)))))
      (overlay-put (make-overlay (point) (progn (end-of-line) (point)))
                   'face '(:foreground "white" :weight semi-bold))
      (forward-char)
      (unless unix-buffer-keymap-initialized-p
        (let ((map (make-sparse-keymap)))
          (define-keys map  ; local defn
            "C-m"     unix-kill-pid
            [mouse-1] unix-kill-pid
            "p"       previous-line
            "n"       next-line
            "q"       unix-restore-previous-configuration
            [mouse-3] unix-restore-previous-configuration)
          (use-local-map map)
          (setq unix-buffer-keymap-initialized-p t)
          (save-excursion
            (while (not (eobp))
              (let ((start (point))
                    (end (progn (end-of-line) (point))))
                (put-text-property start end 'keymap map)
                (put-text-property start end 'mouse-face 'highlight)
                (forward-char 1))))))
      (set-buffer-modified-p nil)
  (pop-to-buffer (unix-i-buffer 'ps) t t)
  (let ((window-min-height
         (with-current-buffer (unix-i-buffer 'ps)
           (count-lines (point-min) (point-max)))))
    (shrink-window-if-larger-than-buffer)))))

(defun unix-kill-pid (&optional event)
  (interactive)
  (save-excursion
    (destructuring-bind (pid tty time cmd)
        (let ((start (progn (beginning-of-line) (point)))
              (end (progn (end-of-line) (point))))
          (split-string (buffer-substring-no-properties start end)))
      (when (and (not (zerop (string-to-number pid))) ;we have a valid pid
                 (let ((prompt
                        (format "Kill program `%s' with pid `%s'?" cmd pid)))
                   (if window-system
                       (x-popup-dialog (selected-frame)
                                       `(,prompt ("yes". t) ("no")))
                     (y-or-n-p prompt))))
        (let ((status (call-process "kill" nil nil nil "-9" pid)))
          (if (not (zerop status))
              (message "Kill returned non-zero status")
            (message "Killed process")
            (let (buffer-read-only)
              (delete-region (point-at-bol) (point-at-eol))
              (ignore-errors (delete-char 1)))))))))


;;; Failed (?) experiments:

;;
;;(defvar shell-lisp-interface-char ?:)
;;(defvar *shell-commands-interface*
  ;; (COMMAND-NAME . FUNCTION-NAME)
;;  '(("ls" . unix-ps)))


;;(defvar shell-colon-map (make-keymap))
;;(define-key shell-mode-map ":" shell-colon-map)


;; (dolist (alist *shell-commands-interface*)
;;   (let ((current-map shell-mode-map))
;;     (let* ((string (car alist))
;;            (length (length string)))
;;       (do ((i 0 (+ i 1)))
;;           ((= i (- length 2))
;;            ;;
;;            )
;;         ;;
;;         )
           
    
    
;;     (dolist (c (butlast (string-to-list (car alist))))
;;       (let ((name (string c)))
;;         (unless (find-key name );;change me

;;         (unless (find-key name );;change me
;;           (define-prefix-command name))
;;         (setq current-map <the-map-newly-formed>)

;;     (if (find-key (string (last (car alist))))
;;         (error)
;;       (define-key current-map (string (last (car alist)))
;;         (cdr alist)))))


;;; Asynchronous find

;;;(defun unix-find () (interactive))

