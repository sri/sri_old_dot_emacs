;;; Miscellaneous customizations

;(defvar *commands-count* 0)

;(add-hook 'pre-command-hook
;  (lambda ()
;    (incf *commands-count*)))


(iswitchb-mode 1)
(setq iswitchb-prompt-newbuffer nil)

;(setq enable-local-eval t)
(setq safe-local-variable-values
      '((Log . code\.log) (Package . Extensions)))

;;; used to be in x-win:
(show-paren-mode t)
;;; set background the same as background of window
(let ((bgcolor (cdr (assoc 'background-color default-frame-alist))))
  (custom-set-faces
   `(show-paren-match-face
     ((t :background ,bgcolor :foreground "Red" :bold t))
     t)
   `(show-paren-mismatch-face ((t :background ,bgcolor)) t)))

;;; From gnu.emacs.sources post on 1998/05/02.
(add-hook 'minibuffer-setup-hook
          (lambda () (set-buffer-modified-p nil))
          'append)

(fset 'lisp-complete-symbol             ; also used in emacs-lisp-mode
      'PC-lisp-complete-symbol)

(defadvice abort-recursive-edit (before minibuffer-history
                                        act comp)
  (when (and (eq (selected-window) (active-minibuffer-window))
             (buffer-modified-p) 
             (not (null minibuffer-history-variable))
             (boundp minibuffer-history-variable))
    (let ((value (symbol-value minibuffer-history-variable))
          (string (field-string))) ;emacs 21
      (set minibuffer-history-variable
           (cond ((not (listp value))   (list string))
                 ((member string value) (cons string (delete string value)))
                 (t                     (cons string value)))))))

;;; Register
(define-keys ctl-x-map
  "r v"  view-register
  "r p"  point-to-register)

(mapc
 (lambda (x)
   (let ((val
          (if (symbolp (cdr x)) (symbol-value (cdr x)) (cdr x))))
     (set-register (car x)
                   `(file . ,(expand-file-name val)))))
 '((?~ . "~")
   (?h . dot-emacs-home)))



;;; Bookmarks
(require 'bookmark)

;;; shouldn't be here
(setq bookmark-default-file
      (expand-file-name "bookmarks" dot-emacs-home))

(add-hook 'bookmark-load-hook
	  (lambda ()
	    (setq bookmark-save-flag 1)))

;(autoload 'diary "diary")
;(autoload nil "calendar")

;;; Diary setup
(add-hook 'diary-hook
  (lambda ()
    (setq diary-file (expand-file-name "diary" dot-emacs-home))))

;; (add-hook 'diary-display-hook 'fancy-diary-display)
;; (diary)


;;; Comint
(add-hook 'comint-load-hook
  (lambda ()
    (setq comint-scroll-to-bottom-on-input t)))
;; comint-scroll-to-bottom-on-output
;; comint-scroll-show-maximum-output


(add-hook 'comint-output-filter-functions ;don't echo passwords
	  'comint-watch-for-password-prompt)


;;; Shell
;;; - Fix for ESC ? -> comint-dynamic-list-filename-completions
(require 'shell)

(define-key shell-mode-map (kbd "ESC ?") ; what it is, globally
  'help-command)

(define-key shell-mode-map (kbd "C-c ESC c")
  'comint-dynamic-list-filename-completions)

(defun shell-other-window-switch-to-window-directory (&optional arg)
  (interactive "P")
  (let ((windir default-directory))
    (when (= (count-windows) 1)
      (let ((split-window-keep-point t))
        (split-window-vertically)))
    (shell)
    (when (get-buffer-process (current-buffer))
      (goto-char (point-max))
      (let ((def (expand-file-name default-directory)))
        (unless (or arg (string= (expand-file-name windir) def))
          (insert "pushd " windir)
          (comint-send-input))))))



;;; Web
(require 'browse-url)

(setq browse-url-netscape-program
      "/usr/local/src/mozilla-firebird/MozillaFirebird/MozillaFirebird")
;;      "gnome-help-browser")
(setq browse-url-browser-function 'browse-url-gnome-moz)


;;; Language support
(add-hook 'lisp-interaction-mode-hook   ; *scratch* buffer
          (lambda ()
            (set (make-variable-buffer-local
                  'next-line-add-newlines)
                 t)))

;(substitute-key-definition
; 'newline 'move-past-close-and-reindent lisp-interaction-mode-map)
;(substitute-key-definition
                           


;; (mapc
;;  (lambda (map)
;;    (substitute-key-definition
;;     'newline 'move-past-close-and-reindent
;;     (symbol-value map)))
;;  '(lisp-interaction-mode-map
;;    lisp-mode-map scheme-mode-map
;;    emacs-lisp-mode-map
;;    inferior-scheme-mode-map
;;    inferior-lisp-mode-map))

;;; Isn't this nicer than the below two:

;(define-hook java-mode-hook
;  (c-set-style "gnu"))
;
;(define-hook scheme-mode-hook
;  (local-unset-key (kbd "C-j"))
;  (define-key scheme-mode-map
;    (kbd "C-m") 'newline-and-indent)
;  (put 'receive 'scheme-indent-function 2))


(add-hook 'java-mode-hook
          (lambda () (c-set-style "gnu")))

;(autoload  "cmuscheme")
(require 'cmuscheme)

(setq scheme-program-name
      (concat "scsh -l "
              (expand-file-name "~/lib/scsh/utilities.scm")))

(push '("scsh" . scheme-mode) interpreter-mode-alist)

(add-hook 'scheme-mode-hook
  (lambda ()
    (local-unset-key (kbd "C-j"))
    (define-key scheme-mode-map (kbd "C-m") 'newline-and-indent)
    (put 'receive 'scheme-indent-function 2)))

(push '("\\.mms" . asm-mode) auto-mode-alist) ; Knuth's MMIX


;;; Ruby:

;; (autoload 'ruby-mode "ruby-mode" "Ruby editing mode" t)
;; (autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
;; (autoload 'inf-ruby-keys "inf-ruby"
;;   "Set local key defs for inf-ruby in ruby-mode")

;; (push '("\\.rb$" . ruby-mode) auto-mode-alist)
;; (push '("ruby" . ruby-mode) interpreter-mode-alist)
;; ;(require 'ruby-electric)

;; (push '(?/ . ?/) ruby-electric-matching-delimeter-alist)
 
;; (add-hook 'ruby-mode-hook
;;   (lambda ()
;;     (local-unset-key (kbd "C-j"))
;;     (define-key ruby-mode-map (kbd "C-m")
;;       'ruby-reindent-then-newline-and-indent)
;;     ;; inf-ruby
;;     (inf-ruby-keys)
;;     (ruby-electric-mode)
;;     (setq ruby-program-name "irb1.8 --inf-ruby-mode"))
;;   )


;;; Python:

(autoload 'python-mode "python-mode" "Python editing mode" t)
(push '("\\.py$" . python-mode) auto-mode-alist)
(push '("python" . python-mode) interpreter-mode-alist)

(defvar my-py-shell-init "\
import os
def mystartupthings():
    startupdir = '/home/sri/code/Py/startup/'
    for name in os.listdir(startupdir):
        if name.startswith('.#'): continue
        fullpath = os.path.join(startupdir, name)
        fp = open(fullpath)
        contents = fp.read()
        fp.close()
        yield fullpath, contents
startuperrors = []
for filename, src in mystartupthings():
    try:
        exec src
    except Exception, e:
         startuperrors.append('Failed loading %s:\\n%s' % (filename, str(e)))
if startuperrors:
    print '** Startup errors:'
    spaces = ' ' * 4
    all = '\\n'.join(startuperrors)
    all = '\\n'.join(spaces+line for line in all.split('\\n'))
    print all
print 'Done'
")

;;;Not Used
(defun my-py-toplevel-constructs ()
  (let ((path (expand-file-name "~/code/Py/startup/toplevel_constructs.py"))
        (list '()))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (while (not (eobp))
        (if (or (looking-at "^ *#") (looking-at "\n"))
            (forward-line 1)
          (let ((start (point)))
            (while (and (not (eobp))
                        (not (looking-at "\n"))
                        (not (looking-at "^ *#")))
              (forward-line 1))
            (let ((item (buffer-substring-no-properties
                         start
                         (if (bolp) (1- (point)) (point)))))
              (setq list (cons (concat item "\n\n")
                               list)))))))
    (nreverse list)))

(defun my-py-shell (&optional arg)
  (interactive "P")
  (let* ((buf (get-buffer "*Python*"))
         (proc (and buf (get-buffer-process buf)))
         (status (and proc (process-status proc))))
    (py-shell arg)
    (unless (and buf proc status)
      (with-temp-buffer
        (insert my-py-shell-init)
        (py-execute-buffer)))))
    
    
;;(add-hook 'python-mode-hook
;;          (lambda () (local-set-key (kbd "C-c !") 'my-py-shell)))

;;; read python library files:
(defvar python-library-directory nil)
(defvar python-library-files nil)

(defun set-python-library-directory-and-files (&optional force)
  (interactive "P")
  (unless (and python-library-directory python-library-files (not force))
    (let ((python (executable-find "python")))
      (unless python
        (error "Unable to find `python' in path"))
      (with-temp-buffer
        (call-process python nil t nil "-c"
          "import sys; print sys.prefix + '/lib/python' + sys.version[:3]")
        (goto-char (point-min))
        (let ((dir (buffer-substring (point) (progn (end-of-line) (point)))))
          (unless (file-directory-p dir)
            (error "`%s' returned %s for sys.prefix which doesn't exist"
                   python (buffer-string)))
          (setq python-library-directory (concat dir "/"))
          (setq python-library-files '())
          (let ((dirs (list dir)))
            (while dirs
              (dolist (x (directory-files (pop dirs) t nil t))
                (unless (string-match
                         "\\(test\\|\\.py[co]\\|\\.gif\\|\\.\\|\\.\\.\\)$"
                         x)
                  (if (file-directory-p x)
                      (push x dirs)
                    (push
                     (cons
                      (file-name-sans-extension (file-name-nondirectory x))
                      x)
                     python-library-files)))))))))
    ;; user will most likely want to read files from the
    ;; toplevel dir
    (setq python-library-files
          (nreverse python-library-files))))

;;; TODO: do something different when arg is either zero, negative,
;;; or positive.  Positive could mean query when there is more than
;;; one file (since it easier to invoke a positive arg than a negative
;;; one).  Negative could mean find all those files into emacs.

(defun read-python-library-file (name &optional arg)
  (interactive "sFilename: \nP")
  (let ((files '()))
    (catch 'done
      (dolist (x python-library-files)
        (when (string-match name (car x))
          (push (cdr x) files)
          (when arg ;;(and arg (not (minusp arg)))
            (throw 'done t)))))
    ;; the order in which we found them
    (setq files (nreverse files))
    (let ((names (mapcar (lambda (x)
                           ;; P-L-D *will* be a prefix of X, otw
                           ;; it should be wrapped around in an `if' expr
                           (string-match python-library-directory x)
                           (substring x (match-end 0) (length x)))
                         files)))
      (if (null files)
          (message "Couldn't find python library file `%s'" name)
        (let ((frame-buffer-list (frame-parameter nil 'buffer-list))
              (buffers
               (mapcar (lambda (file) (find-file-noselect file t)) files)))
          (pop-to-buffer (car buffers))
          ;; these *must* be buffers (buffer objects) not names!
          ;; this maintains the order in which the files were found.
          (modify-frame-parameters nil
            `((buffer-list ,@(nconc buffers frame-buffer-list)))))
        (if (null (cdr names))
            (message "Found `%s'" (car names))
          ;; TODO: this should really be output to a temporary
          ;; buffer with prefixes (in the name of the files) displayed
          ;; in a different color, than the rest of the name of the file
          (message "Found %d files: %s"
                   (length names) (mapconcat 'identity names " ")))))))



;;; Info

(add-hook 'Info-mode-hook
  (lambda ()
    (define-key Info-mode-map
      "." 'Info-next-reference)
    (define-key Info-mode-map
      ";" 'Info-prev-reference)
    (define-key Info-mode-map
      "z" 'Info-follow-nearest-node)))
      

;;; Lisp

(push '("\\.cl$" . lisp-mode) auto-mode-alist)
(push '("lisp" . lisp-mode) interpreter-mode-alist)

;(cload
; (expand-file-name "lisp/lisp-init" dot-emacs-home))

(add-hook 'lisp-mode-hook
	  (lambda ()
	    (set (make-local-variable 'lisp-indent-function)
		 'common-lisp-indent-function)))




;;;
(defvar capitalize-words nil)

(defun capitalize-sentences-in-region (&optional beg end buffer arg)
  "Capatilize sentences in a buffer.
Capitalized form means that the first word of each sentence.
Upcases words in variable CAPITALIZE-WORDS."
  (interactive "r\nbBuffer: \nP")
  (unless beg
    (setq beg (point-min)))
  (unless end
    (setq end (point-max)))
  (with-current-buffer (or buffer (current-buffer))
    (setf (point) beg)
    (while (< (point) end)
      (capitalize-word 1)
      (forward-sentence))
    (dolist (word capitalize-words)
      (setf (point) beg)
      (while (re-search-forward (car word) end t)
        (replace-match (cdr word) nil nil)))))

(require 'sendmail)
(require 'mail-utils)

(defun capitalize-mail-buffer (&optional arg)
  (interactive "P")
  (save-excursion
    (save-restriction
      (capitalize-sentences-in-region
       (search-forward mail-header-separator nil t)))))

; (add-hook 'mail-send-hook 'capitalize-mail-buffer)
