(defun insert-user-home (x)
  (interactive "FPath: ")
  (insert (expand-file-name x)))  




(require 'cl)

;;; (experiment 2003-10-07T03:48:16)
;;; Add hooks in a easier way.  Also, Scsh-like `with-cwd'.

(defmacro define-hook (name &rest body)
  (let ((string (if (stringp name) name (symbol-name name))))
;    (unless (string-match "-hook$" string)
;      (warn "Do"
   `(add-hook ',(intern (format "%S-hook" name)) (lambda () ,@body))))

(defmacro with-cwd (directory &rest body)
  `(let ((default-directory ,directory))
     ,@body))

(defun insert-user-home (filename)
  "Inserts FILENAME as an absolute directory in current buffer.
This is useful when editing scripts, or in shell-mode."
  (interactive "FEnter filename: ")
  (insert (expand-file-name filename)))

;;; Is READ-KEY-SEQUENCE the Right function to use here?
;;; Because it reads all keys, including Control-G, which
;;; is usually bound to `keyboard-quit'.
(defun bind-command (&optional arg)
  "Bind a commonly, but temporarily used command to a key.
With prefix argument binds key locally."
   (interactive "P")
   (let ((key (read-key-sequence "Key to bind: " nil)))
     ;; How to translate from say [7] to its string equavilent ""?)
     ;; (where-is-internal 'keyboard-quit global-map t)
     (if (and (stringp key) (string= key ""))
         (message "Not binding any command.")
       (multiple-value-bind (function where)
           (if arg
               (values #'local-set-key "locally")
             (values #'global-set-key "globally"))
         (let ((command (read-command "Command: ")))
           (funcall function key command)
           (message "`%s' is now bound to %s, %s"
                    key command where))))))


;;; Reading RFC's
(defvar *rfc-location-prefix*
  ;; The first hit on google on 2002-12-27.
  "ftp://ftp.rfc-editor.org/in-notes/"
  "Prefix location of the RFC's.  The string \"rfcNNNN.txt\" (NNNN's
prefixed with zero, if necessary) will be appended to this variable to get
the desired RFC (this value of this variable should end with a forward
slash).  If this variable is NIL, then the user is queried for the
url/filename.  This can name a file on the web or the local file system.")

(defun rfc-view (n &optional arg)
  "View an RFC.
Also, see the documentation for the variable RFC-VIEW-URL."
  (interactive "nRFC: \nP")
  (unless (and (not arg) *rfc-location-prefix*)
    (setq *rfc-location-prefix*
          (read-from-minibuffer "RFC location prefix: ")))
  (let ((rfc (format "%srfc%d.txt" *rfc-location-prefix*
                     n)))
    (if (string-match (rx (and string-start (or "ftp://" "http://")))
                      rfc)
        (browse-url rfc)
      (find-file rfc))))
    


;;; TODO: generalize this, -LOCAL-PATH and -LOAD-PATH;
;;; both of them use -IN-PATH.

(defun check-for-duplicate-in-load-path (&optional error)
  (let ((files ()))
    (dolist (path local-load-path files)
      (let ((found-p
             (some (lambda (name)
                     (locate-library name nil original-load-path))
                   ;; Make sure LOCATE-LIBRARY searches for
                   ;; both `.el' and `.elc'.
                   (mapcar 'file-name-sans-extension
                           (directory path :match "\\.el$")))))
        (when found-p
          (if error
              (error "Duplicate files:  ~A/~A ~A"
                     path (file-name-nondirectory found-p) found-p)
            (push (list (file-name-nondirectory found-p)
                        path
                        (file-name-directory found-p))
                  files)))))))

(defvar *ignore-duplicates-in-path*
  '("." ".." "README" "COPYING" "subdirs.el")
  "List of filenames to be ignored by CHECK-FOR-DUPLICATES-IN-PATH.
May contain regular expressions.  Also, see the documentation of that
function.")

  "Check for duplicate names in PATH.
If PATH is NIL, then the value of LOAD-PATH is used.
ERRORP non-NIL, signals an error if duplicates are found."

(defun check-for-duplicates-names-in-path (&optional path)
  "Check for duplicate filenames in a list of directories.
The optional argument, PATH specifies a list of directory names.  It
defaults to the value of LOAD-PATH.  The variable
`*IGNORE-DUPLICATES-IN-PATH*' contains a list of filenames that will be
ignored.  See the documentation of that variable.  If this function is called
interactively, then duplicate files, if any, are output in buffer
 \"*File Duplicates*\", which pops up.  Otherwise, a list of lists is
returned where the first element contains the duplicate filename and
the rest of the elements are directories where the duplicate filename
was found."
  (interactive)
  (let ((table (make-hash-table :test #'equal))
        (duplicates ()))
    (dolist (dir (or path load-path))
      (dolist (file (directory-files dir nil nil t))
        (unless (member* file *ignore-duplicates-in-path*
                         :test (lambda (file elt)
                                 (save-match-data
                                   (string-match elt file))))
          (push (concat dir "/" file)
                (gethash file table ())))))
    (maphash (lambda (key value)
               (when (cdr value)
                 (push (cons key value) duplicates)))
             table)
    (if (not (interactive-p))
        duplicates
      (if (null duplicates)
          (message "No duplicate names found")
        (with-current-buffer (get-buffer-create "*Duplicates*")
          (setf buffer-read-only nil)
          (setf (point) (point-min))
          (erase-buffer)
          (dolist (elt duplicates)
            (let ((length (length (first elt)))
                  (absolute-names (rest elt)))
              (insert (first elt) ": " (first absolute-names) "\n")
              (dolist (x (rest absolute-names))
                (move-to-column (+ length 2) t)
                (insert x "\n"))))
          (setf (point) (point-min))
          (view-mode 1)
          (pop-to-buffer (current-buffer) t t))))))

(defun check-for-duplicates-files-in-path (&optional path)
  "Check for duplicate file contents in PATH.
This function uses MD5 checksums (specification: RFC 1321) to compare the
files.  Also, see documentation of CHECK-FOR-DUPLICATES-NAMES-IN-PATH."
  (interactive)
  (let ((table (make-hash-table :test #'equal))
        (files ())
        (duplicates ()))
    (dolist (dir (or path load-path))
      (dolist (file (directory-files dir nil nil t))
        (let ((truename (concat dir "/" file)))
          (unless (or (member* file *ignore-duplicates-in-path*
                               :test (lambda (file elt)
                                       (save-match-data
                                         (string-match elt file))))
                      (file-directory-p truename))
            (push truename files)))))
    (with-temp-buffer
      (apply #'call-process "md5sum" nil t nil files)
      (setf (point) (point-min))
      (while (not (eobp))
        (let ((md5sum
               (buffer-substring (point) (progn (forward-word 1) (point))))
              (file
               (buffer-substring (progn (skip-chars-forward " \t")
                                        (point))
                                 (progn (end-of-line 1) (point)))))
          (push file (gethash md5sum table ()))
          (forward-char 1))))
    (maphash (lambda (key value)
               (when (cdr value)
                 (push (cons key value) duplicates)))
             table)
    (if (not (interactive-p))
        duplicates
      (if (null duplicates)
          (message "No duplicate files found")
        (with-current-buffer (get-buffer-create "*Duplicates*")
          (setf buffer-read-only nil)
          (setf (point) (point-min))
          (erase-buffer)
          (dolist (elt duplicates)
            (let ((length (length (first elt)))
                  (absolute-names (rest elt)))
              (insert (first elt) ": " (first absolute-names) "\n")
              (dolist (x (rest absolute-names))
                (move-to-column (+ length 2) t)
                (insert x "\n"))))
          (setf (point) (point-min))
          (view-mode 1)
          (pop-to-buffer (current-buffer) t t))))))



;; (defun collapse-pathname (pathname &optional include-filename chars
;; (if (not (find directory-sep-char pathname))
;;       pathname
;;     (save-match-data
;;       (let ((components '())
;;             (directory-sep-string (string directory-sep-char))
;;             (userhome (expand-file-name "~")))
;;         (when (and (not dont-substitute-tilde-for-userhome)
;;                    (>= (length pathname) (length userhome))
;;                    (string= userhome (subseq pathname 0 (length userhome))))
;;           (dolist (x (split-string userhome directory-sep-string))
;;             (push x components))
;;           (setq pathname (subseq pathname (length pathname))))
;;         (setq components
;;               (nreconc components
;;                        (split-string pathname directory-sep-string)))
;;         (when (null chars)
;;           (setq chars 1))
;;         (let ((name
;;                (mapconcat (lambda (x)
;;                             (if (>= (length x) chars) (subseq x 0 chars) x))
;;                           components
;;                           directory-sep-string)))
;;           (if (eq (aref pathname 0) directory-sep-char)
;;               (setq name (concat "/"))
          
;;           (if include-filename
;;               (concat name directory-sep-string
;;                       (file-name-nondirectory pathname))
;;             name))))))                                   dont-substitute-tilde-for-userhome)
  

;; (collapse-pathname "/home/sri/")
          
          

;;; Use DIRECTORY-SEP-CHAR here:

(defun collapse-path (path &optional include-file)
  (let ((sep directory-sep-char))
    (if (not (find sep path))
        path
      (save-match-data
        (when (string-match (format "^%s" (expand-file-name "~"))
                            path)
          (setq path (replace-match "~/" t t path)))
        (let ((directories (split-string (file-name-directory path)
                                         (string sep))))
          (setq directories (delete "" directories))
          (let ((shortened
                 (mapconcat (lambda (dir) (string (aref dir 0)))
                            directories (string sep))))
            (if (file-name-absolute-p path)
                ;; not portable
                (setq shortened (concat "/" shortened)))
            (if include-file
                (concat shortened (file-name-nondirectory path))
              shortened)))))))

(defun ask-require (feature)
  (interactive "SRequire feature: ")
  (if (featurep feature)
      (message "Feature `%s' is already present" feature)
    (condition-case nil
        (progn
          (require feature)
          (message "Feature `%s' is now loaded" feature))
      (file-error
       (message "Unable to locate feature `%s' in load-path"
                feature)))))


;;; Converting HTML to text.

(defun html-convert-buffer (&optional buffer insert)
  "Convert the current buffer to text.
If INSERT is non-NIL, delete the contents of BUFFER, without
removing any restrictions, and insert the replacement."
  (interactive "bBuffer: \nP")
  (with-current-buffer (or buffer (current-buffer))
    (let ((text (html-convert-region (point-min) (point-max))))
      (if (not insert)
          text
        (delete-region (point-min) (point-max))
        (insert text)
        (goto-char (point-min))))))

(defun html-convert-region (start end)
  "Converts a region of HTML in current buffer and returns a string.
If the optional argument, INSERT-P is specified, then replaces
the HTML with the text version."
  (interactive "r")
  (save-excursion
    (let ((tmp (make-temp-file "html-convert")))
      (write-region (buffer-substring-no-properties start end)
                    nil tmp nil 'no-message)
      (with-temp-buffer
        (call-process "lynx" nil t nil "-dump" "-nolist" "-force_html" tmp)
        (delete-file tmp)
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun html-view-buffer-other-window (&optional buffer)
  (interactive "bHTML buffer: ")
  (let ((name
         (format "<%s>" (if (bufferp buffer) (buffer-name buffer) buffer))))
    (with-current-buffer (get-buffer-create (generate-new-buffer-name name))
      (insert-buffer-substring buffer)
      (html-convert-buffer nil 'insert)
      (set-buffer-modified-p nil)
      (view-mode)
      (pop-to-buffer (current-buffer) nil 'no-record))))



;; does this work interactive with cl lambda list?
(defun watch-web (arg)
  (interactive "P")
  (with-current-buffer (get-buffer-create "Watcher")
    (when (or (not (get-buffer-process (current-buffer)))
              (and (y-or-n-p
                    "A watcher process is already running. Continue? ")
                   (progn
                     (kill-process (get-buffer-process (current-buffer)))
                     t)))
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((process
             (apply 'start-process "watcher" "Watcher"
               (expand-file-name "~/files/watcher/watcher.py")
               ;;
               ;; -f      -- Force all
               ;; [digit] -- revert back than many versions
               ;;
               (if arg
                   (if (minusp arg) '("-f") (list arg))
                 '()))))
        (set-process-coding-system process 'unix)
        (set-process-sentinel process
          (lambda (process event)
            (with-current-buffer (process-buffer process)
              (let ((map (make-sparse-keymap)))
                (set-keymap-parent map view-mode-map)
                (define-key map "n" 'forward-page)
                (define-key map " " 'scroll-up)
                (use-local-map map)
                (set-buffer-modified-p nil)
                (setq buffer-read-only t))
              (goto-char (point-min))
              (if window-system
                  (make-frame)
                (pop-to-buffer (current-buffer))))))))))


;;; Each element remembers the group of files that were changed and
;;; their undo list.
(defvar replace-in-files-undo-list '())

(defun replace-in-files (matches files &optional not-literal)
  (save-excursion
    (with-temp-buffer
      (let ((file-undo-list '()))
        (dolist (file files)
          (setq buffer-undo-list nil)
          (insert-file-contents file)
          (dolist (x matches)
            (while (search-forward (car x) nil t)
              (replace-match (cdr x) t (not not-literal))))
          (write-region (point-min) (point-max) file nil 'no-message)
          (push (cons file buffer-undo-list)
                file-undo-list)
          (delete-region (point-min) (point-max)))
        (push file-undo-list
              replace-in-files-undo-list)))))

(defun replace-in-files-undo (&optional files-undo-list)
  )
  

(defun run-time-statistics ()
  (make-list 4 0))


;;; A little phonebook

(defvar phone-number-file "~/files/phone/ph")
(defvar phone-number-list nil)

(defun phone (&optional arg)
  (interactive "P")
  ;; set the phone numbers from a file if we haven't set it before or
  ;; the user explicitly requests it
  (when (or (not phone-number-list) arg)
    (with-temp-buffer
      (insert-file-contents phone-number-file)
      (setq phone-number-list
            (read (current-buffer)))))
  ;; display ISearch style
  (let ((cursor-in-echo-area t)
        ;; restore this after finishing up
        (winconf (cons (current-window-configuration) (point-marker)))
        ;; start displaying matches after this many characters
        (match-length 2)
        (prompt "Phone: ")
        (pbuf (get-buffer-create " *phone*"))
        (start 7)
        (done nil))
    (with-current-buffer pbuf
      (setq cursor-type nil)
      (erase-buffer)
      (pop-to-buffer (current-buffer))
      (shrink-window-if-larger-than-buffer))
    (while (not done)
      (let* ((char (read-char prompt))
             (string (char-to-string char)))
        (cond ((or (= char 8) (= char 127)) ; C-h and Backspace
               (if (> (length prompt) start)
                   (setq prompt
                     (substring prompt 0 (1- (length prompt))))
                 (beep)))
              ((string-match "[[:alpha:]]" string)
               (setq prompt (concat prompt string)))
              (t (setq done t)))
        
        ;; when we have seen MATCH-LENGTH chars, start displaying results
        (when (and (not done) (>= (- (length prompt) start) match-length))
          (with-current-buffer pbuf
            (erase-buffer)
            (let ((name (substring prompt start))
                  found)
              (save-excursion
                (dolist (x phone-number-list)
                  (when (string-match name (car x))
                    (setq found t)
                    (insert (car x))
                    ;; if this number here isn't larger that
                    ;; the largest name in phone-number-list, then
                    ;; watch out...
                    (move-to-column 25 t)
                    (dolist (elt (cdr x))
                      (insert (format "%s " elt)))
                    (insert "\n")))
                (unless found
                  (beep))))
            ;;(let ((window-min-height 1))
              (shrink-window-if-larger-than-buffer)))));)
    (set-window-configuration (car winconf))
    (goto-char (cdr winconf))))


;;; Frequently used directories and files.  Counts how many time do I visit
;;; a directory or file.  The most frequently visited ones should be on
;;; a fast-key or a register.

(defvar frequent-visits-table (make-hash-table :test #'equal))

(defun frequent-visits-save (&optional name)
  (unless name
    ;; right now, instead of using the code below
    ;; we redefine the function DIRED.  see below.
    (cond ((eq major-mode 'dired-mode)
           (let ((dir dired-directory))
             (setq name (if (consp dir) (car dir) dir))))
          (buffer-file-name
           (setq name buffer-file-name))
          (t
           (message "frequent-visits-save-name: unknown mode, ignoring"))))
  (when name
    (incf (gethash (expand-file-name name) frequent-visits-table 0))))

(add-hook 'find-file-hooks 'frequent-visits-save)

(defadvice dired (before frequent-visits-dired-save first
                         (dirname &optional switches) act comp)
  (frequent-visits-save (expand-file-name dirname)))
  

;;(add-hook 'kill-emacs-hook





;;; Same thing as above but analyzing buffers instead.
;;; But this should be a little bit more elobrate because
;;; we need do find out things like:
;;;  - which buffer were we in and to which buffer did we
;;;    switch to
;;;  - how are the buffers related
;;;  - ...

(defvar buffers-windows-list nil)




;;; Movies and their showtimes:

;;; theatre-info:
;;; (theatre-name address . movie-info-list)
;;;
;;; movie info:
;;; (movie-name rating length-in-minutes showtimes)


(defface movie-theatre-name-face
  '((t :weight ultra-bold :underline t
       :background "black" :foreground "blue")) "")
(defface movie-theatre-name-mouse-over
  '((t :background "black" :foreground "blue")) "")
(defface movie-movie-name-face
  '((t :background "black" :foreground "red")) "")
(defface movie-movie-name-mouse-over
  '((t :background "black" :foreground "blue")) "")
(defface movie-remaining-showtimes-face
  '((t :background "black" :foreground "yellow")) "")

;;; (parse-function url . theatres)

(defvar movie-listings-alist
  '((harkins-parse
     "http://harkinstheatres.moviefone.com/showtimes/closesttheaters.adp?_action=setLocation&csz=85281&submit.x=0&submit.y=0&submit=GO%21"
     "Harkins Centerpoint" "Harkins Arizona Mills")))

;; (defvar movie-review-url "http://us.imdb.com/Tsearch?%s")

(defun movie-current-listings ()
  (interactive)
  (let ((result '())
        (width (window-width))
        (current-time
         (multiple-value-bind (sec min hour)
             (decode-universal-time (get-universal-time))
           (list hour min))))
    (dolist (x movie-listings-alist)
      (destructuring-bind (parse-fn url &rest theatres) x
        (with-temp-buffer
          (erase-buffer)
          (let ((proc (call-process  "lynx" nil t nil "-dump" "-nolist" url)))
            (save-excursion
              (goto-char (point-min))
              (push (funcall parse-fn theatres)
                    result))))))
    (setq result (nreverse result))
    (with-current-buffer (get-buffer-create "*Movie Showtimes*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (dolist (x result)
        (dolist (y x)
          ;;
          ;; theatre name and address
          (insert (car y) ":")
          (let ((ov (make-overlay (point-at-bol)
                      (progn (move-to-column  (1- width) t) (point)))))
            (overlay-put ov 'face 'movie-theatre-name-face)
            (overlay-put ov 'help-echo (cadr y))
            (overlay-put ov 'mouse-face 'movie-theatre-name-mouse-over))
          (insert "\n")
          ;;
          ;; movies in this theatre
          ;; (movie-name rating length-in-minutes showtimes)
          (let ((movies (sort* (caddr y) #'string-lessp :key #'car)))
            (dolist (m movies)
              (let ((mins (string-to-int (caddr m))))
                (if mins
                    (multiple-value-bind (hours mins*) (floor* mins 60)
                      (if (> hours 0)
                          (setq mins
                            (format "%s hour%s %s minute%s"
                                    hours (if (= hours 1) "" "s") mins*
                                    (if (= mins* 1) "" "s")))
                        (setq mins "%s minutes" mins)))
                  (setq mins (caddr m)))
                (insert "  " (car m))
                (let ((ov (make-overlay (point-at-bol)
                            (progn (move-to-column (1- width) t) (point)))))
                  (overlay-put ov 'face 'movie-movie-name-face)
                  (overlay-put ov 'mouse-face 'movie-movie-name-mouse-over)
                  (overlay-put ov 'help-echo
                               (format "rating: %s | length: %s"
                                       (cadr m) mins)))
                  ;;; (insert "\n" "    " (cadr m) " | " mins "\n")
                (insert "\n")
                (let ((cols 60)
                      (showtimes (cadddr m)))
                  (insert "      ")
                  (while showtimes
                    (if (> (+ (current-column) (length (car showtimes)))
                           cols)
                        (insert "\n      ")
                      (let* ((showtime (pop showtimes))
                             (military-time
                              (movie-convert-to-military-time showtime))
                             (start (point))
                             end)
                        (insert showtime)
                        (setq end (point))
                        (insert "  ")
                        ;; if current-time is less that the showtime,
                        ;; highlight the showtime
                        (unless (null military-time)
                          (when (or (< (car current-time) (car military-time))
                                  (and (= (car current-time)
                                          (car military-time))
                                       (<= (cadr current-time)
                                           (cadr military-time))))
                            (let ((ov (make-overlay start end)))
                              (overlay-put
                               ov 'face 'movie-remaining-showtimes-face)))))))
                  (insert "\n\n")))))
          (insert "\n\f\n")))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq cursor-type nil)
      (view-mode 1)
      (setq truncate-lines t)
      (pop-to-buffer (current-buffer))
      ;; there should be a better way:
      ;(message "Remaining showtimes are colored in %s"
      ;  (cadr (member :foreground
      ;          (caadr (symbol-plist 'movie-remaining-showtimes-face)))))
      (message "Current time is %s:%s%s"
               (if (= (car current-time) 12)
                   12
                 (mod (car current-time) 12))
               (cadr current-time)
               (if (>= (car current-time) 12) "pm" "am"))
      )))

;;; TIME is of the format HH:MM{am|pm}
;;; hack:  12:00am gets converted to 24:00
(defun movie-convert-to-military-time (time)
  (let (hour min)
    (when (string-match "\\([0-9]+\\):\\([0-9]+\\)" time)
      (setq hour (mod (string-to-int (match-string 1 time)) 12))
      (setq min (string-to-int (match-string 2 time)))
      (cond ((string-match "pm$" time) (incf hour 12))
            ((zerop hour) (setq hour 24)))
      (list hour min))))


;;; return T if T1 is less that T2.  they are of the format HH:MM{am|pm}
;;; error cases: "12:00am" "11:00am" => NIL
;; (defun movie-compare-times (t1 t2)
;;   (unless (and (= (length t1) 7) (= (length t2) 7))
;;     (error "times aren't of the format HH:MM{am|pm}" t1 t2))
;;   (cond ((and (string-match "am$" t1) (string-match "pm$" t2)) t)
;;         ((and (string-match "pm$" t1) (string-match "am$" t2)) nil)
;;         (t
;;          (let ((h1 (string-to-int t1)) (h2 (string-to-int t2)))
;;            (if (not (= h1 h2))
;;                (< h1 h2)
;;              (let ((m1 (string-to-int (substring t1 3)))
;;                    (m2 (string-to-int (substring t2 3))))
;;                (<= m1 m2)))))))


(defun harkins-parse (theatres)
  (let ((list '())
        (ending-re "^[[:blank:]]+[[:digit:]]+\\.$"))

    ;; get the theatres we are interested in
    (dolist (theatre theatres)
      (goto-char (point-min))
      (unless (re-search-forward (concat "^[\t ]*" theatre) nil t)
        (error "Couldn't find theatre `%s'" theatre))
      (let ((start (progn (beginning-of-line) (point))))
        (forward-line 1)
        (unless (re-search-forward ending-re nil t)
          (goto-char (point-max)))
        (beginning-of-line)
        (push (buffer-substring start (point))
              list)))
    (setq list (nreverse list))

    ;; now parse them
    (let ((result '())
          (showtime-re "[0-9:]*\\(am\\|pm\\)")
          (movie-name-re "^ *\\(.*\\) *(\\(.*\\), \\([0-9]+\\) min\\.)$"))
      (with-temp-buffer
        (while list
          (erase-buffer)
          (insert (pop list))
          (goto-char (point-min))
          (let (theatre addr (movies '()))
            (skip-chars-forward " \t")
            (setq theatre (buffer-substring (point) (point-at-eol)))
            (forward-line 1)
            (skip-chars-forward " \t")
            (let ((start (point)))
              (end-of-line)
              (search-backward " (map)" (point-at-bol) t)
              (setq addr (buffer-substring start (point))))
            (forward-line 1)
            (while (re-search-forward movie-name-re nil t)
              (let ((movie (match-string 1))
                    (rating (match-string 2))
                    (length (match-string 3))
                    (showtimes '()))
                (forward-line 1)
                (while (and (not (looking-at movie-name-re)) (not (eobp)))
                  (while (re-search-forward showtime-re (point-at-eol) t)
                    (let ((showtime (match-string 0)))
                      ;; ewww..
                      (while (not (= (length showtime) 7))
                        (setq showtime (concat "0" showtime)))
                      (push showtime showtimes)))
                  (forward-line 1))
                (push (list movie rating length (nreverse showtimes))
                      movies)))
            (push (list theatre addr (nreverse movies))
                  result))))
      (nreverse result))))




(defun slide-show (directory &optional timeout)
  (interactive "DDirectory: \nP")
  (setq timeout
        (if (null timeout) 3 (prefix-numeric-value timeout)))
  (unless (file-directory-p directory)
    (setq directory (file-name-directory directory)))
  (let ((regexp (rx (or "jpg" "gif" "ppm" "pnm"))))
    (dolist (name (directory-files directory t regexp t))
      (let ((p (start-process "slide-show" nil "xloadimage" name)))
        (sleep-for timeout)
        (kill-process p)))))


(defvar %check-mail-point nil)
(defvar %check-mail-count 0)
(defvar %check-mail-new-count 0)

(defun %check-mail ()
  (interactive)
  (let ((path (expand-file-name "icheck" "~/lib/bin")))
    (with-current-buffer (get-buffer-create "*IMAP Mail*")
      (incf %check-mail-count)
      (goto-char (setq %check-mail-point (point-max)))
      (unless (bobp)
        (insert-char ?- 78)
        (insert "\n\f\n"))
      (print-readable-date t)
      (insert "\n")
      (let ((process (start-process "icheck" (current-buffer) path)))
        (set-process-sentinel process
          (lambda (process event)
            (with-current-buffer (process-buffer process)
              (if (not (zerop (process-exit-status process)))
                  (with-temp-message "No new mail"
                    (sit-for 1.0))
                (incf %check-mail-new-count)
                (goto-char %check-mail-point)
                (pop-to-buffer (current-buffer))))))))))


(defun %firebird (arg)
  (interactive "P")
  (with-current-buffer (get-buffer-create "*Mozilla-Firebird*")
    (let ((process (get-buffer-process (current-buffer))))
      (cond (arg
             (if process
                 (kill-process process)
               (with-temp-message
                   "Can't find a Mozilla-Firebird process to kill"
                 (sit-for 1.0))))
            (process
             (with-temp-message
                 "Mozilla-Firebird is already running"
               (sit-for 1.0)))
            (t
             (let ((name (buffer-name (current-buffer))))
               (start-process name name
                 (expand-file-name "MozillaFirebird"
                   "/usr/local/src/mozilla-firebird/MozillaFirebird/"))))))))


;;; Downloading files:

(require 'ffap)

(defvar wget-all-processes '())

(defun download (url &optional where)
  (interactive
     (list (or (ffap-url-at-point) (read-string "URL: "))
           (expand-file-name (read-file-name "Location: " nil ""))))
  (when (file-directory-p where)
    (cd where)
    (setq where nil))
  (with-current-buffer
      ;; New buffers for each download.
      (get-buffer-create (generate-new-buffer " *wget download*"))
    (let ((proc
           (apply #'start-process "wget" (current-buffer) "wget" url
                  (if where (list "-O" where) '()))))
      (push (list proc url (get-universal-time))
            wget-all-processes)
      (set-process-sentinel proc
        (lambda (proc event)
          (with-current-buffer (process-buffer proc)
            (let ((list (assoc proc wget-all-processes)))
              (setq wget-all-processes
                    (delete proc wget-all-processes))
              (if (zerop (process-exit-status proc))
                  (let ((elapsed (- (get-universal-time) (third list))))
                    (message "Downloaded `%s' (%f, %f) to %s"
                             (second list)
                             elapsed
                             (/ elapsed 60.0)
                             ;; get saved location from wget's output
                             (save-excursion
                               (goto-char (point-min))
                               (forward-line 1)
                               (skip-chars-forward " =>")
                               (buffer-substring (point)
                                                 (point-at-eol)))))
                (message "Couldn't download `%s': %s"
                         (second list)
                         ;; EVENT ends with a newline
                         (subseq event 0 (- (length event) 1))))
              (pop-to-buffer (current-buffer)))))))))
