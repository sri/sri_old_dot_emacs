(require 'gnus)
(require 'gnus-start)
(require 'mail-source)
(require 'imap)
(require 'nntp)

(setq nntp-authinfo-file
      (expand-file-name ".authinfo" dot-emacs-home))

;;; (save-mail-in-other-buffer "~/files/mail/t")
(defun save-mail-in-other-buffer (dir)
  (let ((count 0))
    (save-window-excursion
      (save-excursion
        (with-current-buffer (progn (other-window 1) (buffer-name))
          (goto-char (point-min))
          (while (not (eobp))
            (beginning-of-line)
            (let ((file (expand-file-name (format "%d" count) dir)))
              (gnus-summary-scroll-up 1)
              (gnus-summary-toggle-header 1)
              (with-current-buffer "*Article*"
                (write-region (point-min) (point-max)
                  file nil 'no-message nil t))
              (incf count)
              (forward-line 1))))))))

;;; Original in `gnus-art.el'; uses `lynx' to convert HTML to text.
(defun article-wash-html ()
  "Format an html article."
  (interactive)
  (with-current-buffer gnus-article-buffer ;gnus-original-article-buffer
    (save-window-excursion
      (article-goto-body)
      (save-restriction
        (narrow-to-region (point) (point-max))
        (let ((original (buffer-string))
              (buffer-read-only nil))
          (condition-case error
              (let ((text (html-convert-region (point) (point-max))))
                (delete-region (point) (point-max))
                (insert text))
            (error
             (insert original)
             (message "Unable to convert region to HTML: %s" error))))))))

;;; Hack:  W3-REGION is called by ARTICLE-WASH-HTML in gnus-art.el
;;; to convert a HTML buffer into readable format.
(when (locate-library "w3")
  (error "w3 is in `load-path'; no need to redefine W3-REGION in gnus-init"))

(defun w3-region (start end)
  (interactive "r")
  (let ((temp (make-temp-file "/tmp/"))
        (body (buffer-substring-no-properties start end)))
    ;; We want the current buffer as a string, thus the form
    ;; (b-s-n-p ...) cannot be inside WITH-TEMP-FILE macro.
    (with-temp-file temp (insert body))
    (delete-region start end)
    (call-process "lynx" nil t t
                  "-nolist" "-force_html" "-dump" temp)
    (delete-file temp)))

;;; Normally a word seperator expands the subject, however
;;; we bind `C-n' and `M->' to do the same thing
;(let ((default-directory dot-emacs-home))
;  (cload "address-book"))

;; (setq mail-header-separator "")

(defun gnus-message-redefine ()
  ;(defun message-unique-id ()
  ;  (format "%.0f" (get-internal-real-time)))
  (setf message-citation-line-function
        (lambda ()
          (when message-reply-headers
            (insert "* "
                    (mail-header-from message-reply-headers)
                    ":\n"))))
  (setf message-yank-prefix "| ")
  (setf (cdr (assoc 'References message-header-format-alist)) nil)
  (setq message-header-format-alist
    (delete* 'Distribution message-header-format-alist :key #'car))

  (setq message-indentation-spaces 2)
  (setq message-default-mail-headers
        (concat "Fcc: " mail-archive-file-name)))

;;; Whatever form is below, it won't be byte-compile; hence, we
;;; put most of it in a function that /can/ be byte-compiled.
(eval-after-load "message"
  '(gnus-message-redefine))

(add-hook 'message-setup-hook 'mail-abbrevs-setup)
(add-hook 'message-setup-hook
	  (lambda ()
	    (substitute-key-definition
	     'next-line 'mail-abbrev-next-line
	     message-mode-map global-map)
	    (substitute-key-definition
	     'end-of-buffer 'mail-abbrev-end-of-buffer
	     message-mode-map global-map)))

(setq read-mail-command 'gnus)
(setq mail-user-agent 'gnus-user-agent)


(setq gnus-visual '(article-highlight))
(setq gnus-interactive-exit nil)

(setq gnus-inhibit-startup-message t)

;;; TODO: put preloading stuff in misc.el and others
;; required by gnus in its init-file here...and modify
;; the var below (it must also go in misc.el)
;; (setq gnus-init-file ...)


;;; (apropos-variable "-directory$")
;; (let ((home (expand-file-name "~/.gnu-emacs/gnus+news/")))
;;   (setq gnus-home-directory home
;; 	gnus-dribble-directory home
;; 	gnus-init-file (concat home ".gnus.el")
;; 	mail-source-directory (concat home "Mail/")
;; 	mail-source-crash-box (concat mail-source-directory
;; 				      ".emacs-mail-crash-box")
;; 	message-directory (concat home "Mail/")
;; 	message-auto-save-directory (concat message-directory
;; 					    "/drafts")

;; 	gnus-startup-file (concat home ".newsrc")
;; 	gnus-article-save-directory (concat home "News/")
;; 	gnus-cache-directory (concat gnus-article-save-directory
;; 				     "cache/")
;; 	gnus-directory (concat home "News/")
;; 	gnus-kill-files-directory gnus-directory))	




;;; The number of microseconds since 1900-01-01 00:00:00.000000 UTC.

(defun gnus-time-stamp-signature ()
  (setf (point) (point-max))
  (unless (= (preceding-char) ?\n)
    (insert ?\n))
  (insert (format "\n#<sri %.f>\n" (get-internal-real-time))))

(defun gnus-mail-signature ()
  (setf (point) (point-max))
  (unless (= (preceding-char) ?\n)
    (insert ?\n))
  (insert "--")
;;   (let ((sig (gnus-random-signature)))
;;    (when (> 4 (length sig))
;;       (error "Length of signature is more than four lines"))
;;    (insert sig))
  )

;; (setq message-signature #'gnus-time-stamp-signature)
;; (add-hook 'message-send-hook  'gnus-mail-signature)

(defun gnus-delete-zombie-processes ()
  "Sometimes when a non-character is input, when Gnus asks
for a password, it crashes.  Running this command gets rid
of that."
  (interactive)
  (let ((zombies
         (remove-if-not
            (lambda (p)
              (string-match "^\\(imap\\|nntpd\\).*" (process-name p)))
            (process-list))))
    (if (null zombies)
        (message "No Gnus related process to delete")
      (let ((count (length zombies)))
        (mapc #'delete-process zombies)
        (message "Deleted %d Gnus related process%s"
                 count
                 (if (= count 1) "" "es"))))))
      

;(setq nnimap-authinfo-file
;      (expand-file-name "files/.authinfo" dot-emacs-home))

(setq gnus-treat-display-smileys nil)
(setq gnus-treat-emphasize nil)

;; Used to be 'ask-server: see `New Groups' in manual supposed to make
;; startup faster
(setq gnus-check-new-newsgroups nil)

(setq gnus-save-killed-list nil)
(setq gnus-large-newsgroup 3000)
;(setq gnus-kill-file-name "KILLS")
(setq gnus-file-save-name 'gnus-plain-save-name)


;;; Gnus debugging
(when nil
  (setq gnus-verbose 10)
  (setq nnimap-debug "*nnimap-debug*")
  (setq imap-log "*imap-log*")
  (setq imap-debug "*imap-debug*")
  )


(add-hook 'gnus-select-group-hook
	  (lambda ()
	    ;; SPC doesn't select any articles
	    ;; (setq gnus-auto-select-first nil)
	    ))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;;(setq gnus-topic-line-format "%i (%(%{%n%}%))%v\n")
(setq gnus-topic-line-format "%i %(%{%n%}%)%v\n")


(setq nnimap-split-crosspost t)

(defun delete-imap-mail (&optional n)
  "Delete mail."
  (interactive "P")
  (if (string-match "nnimap" (buffer-name))
      (let ((gnus-novice-user nil))
	(gnus-summary-delete-article n))
    (message "Not reading mail")))


(add-hook 'gnus-summary-mode-hook
	  (lambda ()
	    (define-key gnus-summary-mode-map
	      (kbd "C-c C-k") 'delete-imap-mail)
	    ;; ! doesn't move point to next line.
	    (local-set-key "!" 'gnus-summary-tick-article)))

(defun match-friends+family-headers (rule)
  "Try and match headers of mail agaings REGEXP.
EMAIL-ADDERSSES is an alist -- much like the ones defined in
`adderss-book.el'.  From the gnus manual: if this is a function, then
it will be called with the headers in a buffer"
  (let ((addr +friends/family-email-addresses+)
	(found nil))
    (while (and addr (not found))
      ;; Ignore any leading items
      (let ((regexp (concat "^From:.*" (cdar addr))))
	(if (re-search-forward regexp nil t)
 	    (setq found t)
	  (setq addr (cdr addr)))))
    found))

;;; Gnus initializations

;;; *-select-method(s) -- tell gnus where to store the incoming
;;; messages

(setq gnus-select-method '(nntp "news.gmane.org"))
; '(nnml ""))

;(let ((new (expand-file-name "~/Mail/new/new"))
;      (fetch-via (expand-file-name "fem" "~/lib/mail")))
;  (setq mail-sources
;        `((file :path ,new
;                :prescript ,fetch-via
;                :suffix ""
;                :postscript "rm -f ~/Mail/new/*"))))

(setq gnus-secondary-select-methods
      '(
        ;;(nnml (nnml-directory "~/Mail") (nnmail-use-long-file-names t))
        ;; "lib/emacs/"
        ;(nntp "news")

        ;(nntp "news.west.earthlink.net")
        

        (nnimap
         ""
  	 (nnimap-address "")
  	 (nnimap-list-pattern ("INBOX" "INBOX.*"))
  	 (nnimap-authenticator login)
  	 (nnimap-stream network)
  	 (nnimap-expunge-on-close always)

         (nnimap-split-inbox
          ())

	 ;; Should actually quote the `.', otherwise it matches
	 ;; a single character, but regexp matching should work
	 ;; anyways.
         (nnimap-split-rule
           ())
        
          )))

(setq nnmail-split-methods
      '())


;; 
;; ;; Notes:

;; ;; Type `A A' to get a list of all groups; the nnimap groups should be
;; ;; in there; type `u' or `U' to subscribe them.  See Article Washing
;; ;; for some useful utilities


(defadvice imap-interactive-login (before my-imap-interactive-login)
  "A quicker way to login to the IMAP server."
  ;; imap-username is parsed probably from .autoinfo
  ;; or maybe from my username
  (make-variable-buffer-local 'imap-password)
  (make-variable-buffer-local 'imap-username)
  (setq imap-username "")
  ;(setq imap-password (string ))
  )
(ad-activate 'imap-interactive-login)

;; ;;; Eval this to get the above:
; (defun %insert-the-passwd ()
;   (interactive)
;   (let ((x
;          (cons 'string
;                (loop for ch across (read-passwd "")
;                      collect ch))))
;     (insert (format "%S" x))))


;;(defun gnus ()
;;  (interactive)
;;  (message "Doesn't work yet: in gnus-init.el"))


;;; Spam mail handling.

(defvar spam-mail-directory nil)
(defvar spam-mail-headers-filename nil)

(defun make-spam-mail-headers-file ()
  (unless spam-mail-directory
    (error "Spam mail directory not set"))
  (let ((default-directory spam-mail-directory)
        (tempfile (make-temp-file "/tmp/")))
    (with-temp-buffer
      (let ((buffer-undo-list t))
        (dolist (file (directory "." :dot-files t))
          (insert-file-contents file nil nil nil t)
          (let* ((headers ())
                 (from)
                 (subject))
            (write-region (buffer-substring) nil file t)))))))
