;;; lynx.el -- a (simple) interface to the Lynx web browser

(require 'cl)
(require 'rx)
(require 'ring)
(require 'thingatpt)

;;; Can be a string or function accepting a single argument, the URL.
;;; Used in function LYNX-FETCH-URL.

(defvar lynx-buffer-name
  #'(lambda (url)
      (let ((regexp (rx (and string-start (or "http://" "ftp://" "file:")
                             (submatch (+ anything)) string-end))))
        (if (string-match regexp url)
            (substring url (match-beginning 1))
          url))))


;;; Links in current buffer.  Unless lynx start with the option
;;; "-nolist", when it dumps an HTML file, it also dumps a list
;;; of links:  it numbers the links starting from 1 and after the
;;; webpage, under "References" lists the links in this format:
;;; "  1. <link>"
                   
(defvar lynx-current-buffer-links nil)
(make-variable-buffer-local
 'lynx-current-buffer-links)

(defvar lynx-hands-off-links nil)
(make-variable-buffer-local
 'lynx-hands-off-links)

(defvar lynx-show-url-when-over-link t)
(make-variable-buffer-local
 'lynx-show-url-when-over-link)

;;(run-with-idle-timer 2.0 t 'lynx-show-current-link)

;;; Overlays are used to mark links on the page.

;;; LYNX-CURRENT-BUFFER-LINKS contains all the links.
;;; Returns nothing useful

(defface lynx-current-link-face
  '((t :background "black" :foreground "red"))
  "")

(defface lynx-links-face
  '((t :background "black" :foreground "blue"))
  "Face for links.")

(defface lynx-links-mouse-face
  '((t :foreground "red" :background "black"))
  "Mouse face for links.")

(defvar lynx-links-overlay-properties
  '((face lynx-links-face)
    (mouse-face lynx-links-mouse-face)))
;    (display ,) (help-echo) (invisible )

;;; Lynx when executed without the `-nolist' option, outputs
;;; the url of the links at the end.  `^References$' marks
;;; the beginning of the links.  They are listed like:
;;; "  1. <link>"
;;; The links themselves are represented by `[N]' in the text,
;;; N represents the number of the link.
;;;
;;; LYNX-PARSE-LINKS returns the links in a list.
;;; LYNX-MAKE-LINKS makes them look nice.

(defun lynx-parse-links ()
  (goto-char (point-max))
  (when (search-backward "References" nil t)
    (let ((start (point))
          (links ())
          (regexp (rx (and line-start (0+ blank) (1+ digit) "." blank))))
      (forward-line 2)
      (while (re-search-forward regexp nil t)
        (push (buffer-substring (point) (point-at-eol))
              links))
      (delete-region start (point-max))
      (nreverse links))))

(defun lynx-make-links (&optional disable-sanity-check)
  (save-excursion
    (let ((links (lynx-parse-links))
          (n 1))
      (goto-char (point-min))
      (while (re-search-forward (rx (and "[" (submatch (1+ digit)) "]")) nil t)
        ;; sanity check
        (unless (or disable-sanity-check
                    (= n (string-to-number (match-string 1))))
          (error "Inconsistency detected in link numbering: %d %s"
                 n (match-data 1)))
        (let ((overlay (make-overlay (match-beginning 0) (match-end 0)))
              (link (pop links)))
          (overlay-put overlay 'link link)
          (overlay-put overlay 'number n)
          (dolist (x lynx-links-overlay-properties)
            (apply 'overlay-put overlay x)))
        (setq n (1+ n))))))



(defun lynx-fetch-url-other-window ()
  (split-window-vertically)
  (lynx-fetch-url))

(defun lynx-clone-buffer ()
  (interactive)
  (clone-buffer nil t))

(defun lynx-show-current-link ()
  (interactive)
  (let ((overlays (overlays-at (point))))
    (cond ((null overlays)
           )
          ((null (cdr overlays))
           (message "(%d) %s"
                    (overlay-get (car overlays) 'number)
                    (overlay-get (car overlays) 'link)))
          (t
           nil))))

(defalias 'lynx-show-url-at-point
  'lynx-show-current-link)
      
;;; Move point down/up relative to the current link.

(defun lynx-next-link (arg)
  (interactive "p")
  (let ((point (point)))
    (dotimes (i arg)
      ;; Move out of all the overlays we are in.
      (dolist (x (overlays-at point))
        (setq point (max point (overlay-end x))))
      (setq point (next-overlay-change point)))
    (if (not (interactive-p))
        point
      (goto-char (point))
      ;; current-link overlay here
      )))

(defun lynx-previous-link (arg)
  (interactive "p")
  (let ((point (point)))
    (dotimes (i arg)
      ;; Move out of all the overlays we are in.
      (dolist (x (overlays-at point))
        (setq point (min point (overlay-start x))))
      (setq point (previous-overlay-change point)))
    ;; The above might move to where an overlay ends.
    ;; So we need to move to the beginning of that.
    (if (interactive-p) (goto-char point) point)))

(defun lynx-absolute-link (n)
  (interactive "p")
  (goto-char (save-excursion (goto-char (point-min)) (lynx-next-link n))))

;;; Key bindings.
(defvar lynx-mode-map
  (let ((map (make-sparse-keymap)))
    (mapcar (lambda (x)
	      (let ((key
                     (if (stringp (car x)) (read-kbd-macro (car x)) (car x))))
		(define-key map key (cdr x))))
	    '(("0"    . digit-argument)
	      ("1"    . digit-argument)
	      ("2"    . digit-argument)
	      ("3"    . digit-argument)
	      ("4"    . digit-argument)
	      ("5"    . digit-argument)
	      ("6"    . digit-argument)
	      ("7"    . digit-argument)
	      ("8"    . digit-argument)
	      ("9"    . digit-argument)
	      ("-"    . digit-argument)
              ("o"    . lynx-)
	      ("g"    . lynx-fetch-url)
              ("s"    . isearch-forward)
              ("r"    . isearch-backward)
	      ("SPC"  . scroll-up)
	      ("<"    . beginning-of-buffer)
	      (">"    . end-of-buffer)
              ("a"    . lynx-absolute-link)
	      ("p"    . lynx-previous-link)
	      ("n"    . lynx-next-link)
	      ("TAB"  . lynx-next-link)
	      ("RET"  . lynx-follow-url)
	      ([left] . lynx-backward-history)
              ([right] . lynx-forward-history)
	      ("b"    . lynx-backward-history)
              ("f"    . lynx-forward-history)              
	      ("k"    . kill-this-buffer)))
    map)
  "Keymap for `lynx-mode'.")

(define-derived-mode lynx-mode fundamental-mode "Lynx"
  "A simple interface to the Lynx browser."
  (setq major-mode 'lynx))

(defvar lynx-current-url nil)
(make-variable-buffer-local
 'lynx-current-url)

(defun lynx-show-current-url ()
  (interactive)
  (message "Buffer's url: %s" lynx-current-url))

(defun lynx-follow-url ()
  (interactive)
  (let ((url (lynx-url-at-point)))
    (cond (url (lynx-fetch-url url))
          ((find ?# url)
           ;; `#news'-like items needs to be handled more carefully
           (message "Cannot handle `#' links yet"))
          (t (message "No URL to follow")))))

;;; What happens when PROCESS is done before its SENTINEL
;;; is set?

(defun lynx-exec (url name &optional lynx-args sentinel)
  (let ((process (apply 'start-process name (buffer-name)
                        "lynx" "-dump" (append lynx-args (list url)))))
    (when sentinel
      (set-process-sentinel process sentinel))
    process))

(defun lynx-fetch-url (&optional url args)
  (interactive
   (list (completing-read "Url: " (mapcar 'list lynx-url-history)
                          nil nil nil 'lynx-url-history)
         (and current-prefix-arg
               (split-string (read-string "Lynx arguments: ")))))
  (when url
    (let ((buffer
           (if (functionp lynx-buffer-name) (funcall lynx-buffer-name url)
             lynx-buffer-name)))
      (with-current-buffer (get-buffer-create buffer)
        (setq lynx-current-url url
              buffer-read-only nil)
        (erase-buffer)
        (lynx-mode)
        (lynx-exec url (generate-new-buffer-name
                        (concat (buffer-name) "-process"))
                   args 'lynx-url-fetch-sentinel)))))

(defun lynx-url-fetch-sentinel (process event)
  (if (not (zerop (process-exit-status process)))
      (message "Unable to retrieve %s"
               (with-current-buffer (process-buffer process)
                 lynx-current-url))
    (when (eq (process-status process) 'exit)
      (with-current-buffer (process-buffer process)
        (unless lynx-hands-off-links
          (lynx-make-links))
        (let ((name (buffer-name)))
          (goto-char (point-min))
          (dolist (x lynx-after-display-alist)
            ;; Don't save any excursions.  This is a feature.
            (when (string-match (car x) name)
              (goto-char (point-min))
              (funcall (cadr x))))
          (let ((pop-up-windows t))
            (pop-to-buffer name t)))))))

;;; lynx.el ends here



;;; From the currently modified lynx.el


(defun lynx-exec (url &optional save-as)
  (let ((args
         (append lynx-default-arguments
                 (if lynx-connect-timeout-secs
                     (list (format "-connect_timeout=%s"
                                   lynx-connect-timeout-secs))
                   nil)
                 (list url))))
    (if (not save-as)
        (zerop (apply #'call-process "lynx" nil t nil args))
      (push "lynx" args)
      (let* ((cmd (format "%s > %s"
                          (mapconcat 'identity args " ")
                          (or save-as (file-name-nondirectory url))))
             (proc (start-process "lynx-exec" (current-buffer)
                                  shell-file-name shell-command-switch
                                  cmd)))
        (message "Downloading `%s'..." save-as)
        (push (cons proc save-as)
              lynx-process-state)
        (set-process-sentinel proc
          (lambda (proc string)
            (unless (eq (process-status proc) 'run)
              (let (save-as)
                (setq lynx-process-state
                      (delete-if (lambda (x)
                                   (when (eq (car x) proc)
                                     (setq save-as (cdr x))
                                     t))
                                 lynx-process-state))
                ;; how to get value of save-as here?
                (if (zerop (process-exit-status proc))
                    (message "Downloading `%s'...done" save-as)
                  (message "Unable to download and save `%s': %s"
                           save-as string))))))))))

