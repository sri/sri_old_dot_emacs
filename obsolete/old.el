;;; Some functions and utilities that are not used anymore.  This file
;;; should not be loaded into Emacs at all. If the functionality of
;;; anything below is desired, then cut-and-paste that item into a file
;;; that is loaded upon startup.


;; Simple search & replace commands
(defun search-n-replace (old new &optional beg end)
  "Replace all occurences of OLD with NEW in current buffer.
When BEG and END are given, then the form the boundary."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (or beg (point-min)))
      (while (re-search-forward old end t)
	(replace-match new)))))

(defun dos-to-unix ()
  "Converts newlines from dos to unix format.
To see ^M's, do `find-file-literally';  also see
`comint-strip-ctrl-m' and `shell-strip-ctrl-m."
  (interactive)
  (search-n-replace "\r" ""))

(defun unix-to-dos ()
  "Convert current buffer from unix to dos format."
  (interactive)
  (search-n-replace "\n" "\r\n"))
		 
(defun sans-trailing-whitespace (beg end)
  "Sans whitespaces at the end of lines."
  (interactive "r")
  (search-n-replace "[ \t\r]+$" "" beg end))

;; Useful shell commands
(defun %ps (arg)
  "List my all processes in current login session."
  (interactive "P")
  (shell-command (concat "ps -u " (user-login-name) (if arg " &"))
		 nil nil))

(defun %count-char-in-buffer (&optional buffer)
  "Displays the number of characters in current buffer.
Does not count blanks, tabs, or newlines."
  (interactive "bCount characters in buffer: ")
  (let ((buf (or buffer (current-buffer)))
	(count 0))
    (save-excursion
      (save-restriction
	(set-buffer buf)
	(widen)
	(let ((max (point-max))) ; after widening
	  (goto-char (point-min))
	  (while (< (point) max)
	    (if (not (looking-at "[ \t\n]"))
		(setq count (1+ count)))
	    (forward-char 1)))))
    (message "%d characters in %s" count buf)))

(defun %display-region-endpoints (beg end)
  (interactive "r")
  (let ((ans (list beg end)))
    (if current-prefix-arg
	(message "%S" ans)
      ans)))

;; Make this accept any buffer...
(defun %buffer-narrowed-p ()
  "Return t iff current-buffer is narrowed."
  (interactive)
  (or (/= (point-min) 1)
      (/= (point-max)
	  (save-restriction
	    (widen)
	    (save-excursion (point-max))))))

(defmacro %define-keys (mode-map &rest keys)
  "Define keys in MODE-MAP.
KEYS is a alist, where the car is either a string or symbol,
and the cdr is a function.
Todo:
  -  Need to make it so that each a key if _redefined_ the old value is
saved somewhere."
  `(progn
     ,@(mapcar
	(lambda (key)
	  `(define-key ,mode-map ,(if (stringp (car key))
				      (read-kbd-macro (car key))
				    (car key))
	     ',(cdr key)))
	keys)))

;; Todo: delete files that are older that 30 days (?).
(defun %clean-up-autosaves ()
  "Delete un-necessary files in `~/.autosaves/'."
  (interactive)
  (let ((files (%directory "~/.autosaves" :absolute t)))
    (mapc #'delete-file files)
    (message "Autosaves deleted")))

(defun %switch-to-scratch-buffer ()
  "Switch to the `*scratch*'.
One is created if `*scratch*' does not already exist and
its put in `lisp-interaction-mode'"
  (interactive)
  (switch-to-buffer "*scratch*")
       ;; not needed anymore:  see `default-major-mode' in
       ;; startup.el

       ;; (not (eq major-mode 'lisp-interaction-mode))
       ;; in case it was accidently deleted...
       ;;(lisp-interaction-mode)
  )

(defun redefine-key (prefix &optional key function)
  "Redefine a KEY quickly.
With prefix argument redefine key locally."
   (interactive "P")
   (let ((key (or key
		  (read-key-sequence "Key to rebind: " nil))))
     ;; How to translate from say [7] to its string equavilent ""?)
     ;; (where-is-internal 'keyboard-quit global-map t)
     (if (and (stringp key) (string= key ""))
	 (message "Not redefining any key")
       (let ((which (if prefix #'local-set-key #'global-set-key))
	     (function (or function
			   (read-command "Function: "))))
	 (funcall which key function)
	 (message "`%s' is now bound to %s, %s" function key
		  (if prefix "locally" "globally"))))))

(defun %make-cute-header (beg fill header)
  (interactive "*sBeginning: \nsFill: \nsHeader: ")
  (let ((line (make-string (- 70 (length header) 6) (string-to-char fill))))
    (insert (format "%s%s {%s}\n" beg line (upcase header)))))

(defun %collapse-path (pathname &optional dir-name-only)
  "`Collapses' a PATHNAME, which is a string.
The first character of each directory replaces that directory,
unless that directory with a dot, in which case, the first two
letters are to included (the dot and the first letter of the directory).

Examples:
    \"~/programs/Emacs/share/emacs/\"      =>  \"~/p/E/s/e/\"
    \"/afs/asu.edu/user/s/t/h/sthaiyar/\"  =>  \"/a/a/u/s/t/h/s/\"

If INTERACTIVE is non-nil, then the resultant string is shown in
the minibuffer, via `message'. If DIR-NAME-ONLY is non-nil, then
everything after the last `/' is cast out."

  (let* ((abs (if (= (aref pathname 0) ?/) "/" ""))
	 (path (if dir-name-only
		   (substring pathname 0 (string-match "[^/]*$" pathname))
		 pathname))
	 (new-path (mapconcat
		    (lambda (string)
		      ;; If its a dot-directory, then include first two chars.
		      (if (char-equal (aref string 0) ?.)
			  (substring string 0 2)
			(substring string 0 1)))
		    (split-string path "/")
		    "/")))
    (concat abs new-path "/")))



(defun mode-line-buffer-id ()
  (let ((max-length 32)
	(name buffer-file-truename))
    (cond
     ((null name)
      "Emacs: %12b")
     ((<= (length name) max-length)
      name)
     (t
      (let ((short (concat (collapse-path name t) (buffer-name))))
	(if (> (length short) max-length)
	    ;; When all else fails, just use buffer name.
	    (buffer-name)
	  short))))))

;; RAMM mode -- read-archived-mail-mode
(defun %ramm-next (&optional arg)
    "View the next message in read-archived-mail-mode."
    (interactive "P")
    (let ((beg (point-min)))
      (save-excursion
	(widen)
	;; We don't go to END, because the very first time
	;; when we start to read, the buffer is NOT narrowed.
	(goto-char beg)
	(forward-line 1) ; Skip past current "Subject: " line.
	(let ((new-beg (progn (re-search-forward "^Subject: ")
			      (beginning-of-line)
			      (point))))
	  (narrow-to-region new-beg
			    (progn (goto-char new-beg)
				   (re-search-forward "^From ")
				   (previous-line 1)
				   (point)))))))

(defun %ramm-action (&optional arg)
    "Scroll down current message or view next one."
    (interactive "P")
    ;; From view.el -- test to see if eob is visible
    (if (not (pos-visible-in-window-p (point-max)))
	(scroll-up arg)
      (%ramm-next arg)))

;; read-archived-mail-mode:
(define-derived-mode %ramm  view-mode "Read Archived Mail"
  "Read an archived mailing list."
  (setq buffer-read-only t)
  (%ramm-next) ; Start it off

  (eval-when-compile
    (defvar ramm-map))

  (%define-keys ramm-map
    ("SPC" . %ramm-action)
    ("n" . %ramm-next)))
