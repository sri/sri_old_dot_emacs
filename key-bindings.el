(defmacro define-keys (keymap &rest keys) ;derived from gnus-define-keys
  `(define-keys-1 (quote ,keymap) (quote ,keys)))

(defun define-keys-1 (keymap keys)
  (cond ((= (logand (length keys) 1) 1) ;odd
	 (error "Length of keys must be even %S" keys))
	((null keymap)
	 (error "Can't set keys in a null keymap"))
	((symbolp keymap)
	 (setq keymap (symbol-value keymap)))
	((keymapp keymap))
	((listp keymap)
	 ;; example:  (ctl-x-ctl-b-map "C-b" ctl-x-map)
	 (set (car keymap) nil)
	 (define-prefix-command (car keymap))
	 (define-key (symbol-value (nth 2 keymap))
	   (read-kbd-macro (cadr keymap)) (car keymap))
	 (setq keymap (symbol-value (car keymap)))))
  (let (key command)
    (while keys
      (setq key (car keys)
            command (cadr keys)
            keys (cddr keys))
      (cond ((symbolp key)
	     (setq key (symbol-value key)))
	    ((stringp key)
	     (setq key (read-kbd-macro key))))
      (when (and (symbolp command)
		 (boundp command)
		 (keymapp (symbol-value command)))
	(setq command (symbol-value command)))
      (define-key keymap key command))))

(defun insert-user-home (x)
  (interactive "FPath: ")
  (insert (expand-file-name x)))

;; ctrl <-> shift
(defun keyboard-translate-swap (a b)
  (keyboard-translate a b)
  (keyboard-translate b a))

(defvar previous-keyboard-translate-table nil)

(defun normalize-keyboard ()
  (interactive)
  (setq previous-keyboard-translate-table keyboard-translate-table
	keyboard-translate-table nil))

(defun revert-keyboard ()
  (interactive)
  (setq keyboard-translate-table previous-keyboard-translate-table))

;;; Emacs Manual, "Translating Input Events"
(setq keyboard-translate-table
      (make-char-table 'keyboard-translate-table nil))

;;; Swap Control-<letter> and uppercase letters.
(let ((ctrl 1) (upcase-beg (1- ?A)) (letters 26))
  (while (<= ctrl letters)
    (keyboard-translate-swap ctrl (+ upcase-beg ctrl))
    (setq ctrl (1+ ctrl))))

(define-key function-key-map
  (read-kbd-macro "C-h") (read-kbd-macro "DEL" t))

(define-key global-map (kbd "C-x ?") 'help-command)

(setq help-char ??)
(setq three-step-help t)

(define-keys help-map
  "C-a"  apropos
  "r"    read-library-file)

(defun read-library-file (name)
  (interactive "Memacs lib file: ")
  (let ((file (locate-library (if (string-match "\\.el$" name)
                                  name
                                (concat name ".el")))))
    (if file
        (view-file file)
      (message "can't locate \"%s\"" name))))


(define-keys global-map
  "C-o"         other-window
  "C-t"         lisp-complete-symbol
  "\""          my-insert-double-quotes
  "C-h"         delete-backward-char
  [find]        beginning-of-buffer   ; <home> in vt100
  [select]      end-of-buffer         ; <end>  in vt100
  [home]        beginning-of-buffer
  [end]         end-of-buffer
  [f6]          my-correct-case
  [f7]          my-buffer-shell
  ;; [f8]          
  [f10]         my-paste
  "ESC [ C-a"   previous-line
  "ESC [ C-b"   next-line
  "ESC [ C-c"   forward-char
  "ESC [ C-d"   backward-char
  "C-z"         dabbrev-expand)

(defun my-buffer-shell ()
  (interactive)
  (if (not (eq major-mode 'shell-mode))
      (let ((name (concat (buffer-name (current-buffer))
                          "-*tmp-shell*")))
        (shell name))))

(defun my-insert-double-quotes (&optional arg)
  (interactive "P")
  (insert "\"")
  (save-excursion (if arg (forward-word arg))
                  (insert "\"")))

(defvar my-correct-case-last-arg nil)

(defun my-correct-case (arg)
  (interactive "p")
  (let ((fn (cond ((eq last-command 'my-correct-case)
                   (setq arg my-correct-case-last-arg)
                   'capitalize-word)
                  (t (setq my-correct-case-last-arg arg)
                     'upcase-word))))
    (save-excursion
      (funcall fn (- arg)))))
    
(defun my-paste ()
  (interactive)
  (if (not (file-exists-p "~/x"))
      (message "~/x doesn't exist")
    (insert-file-contents "~/x")
    (message "Pasted contents of ~/x")))

(defun narrow-to-here (here)
  (interactive "d")
  (narrow-to-region (point-min) here))

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

;; for set-mark-command below, so that
;; after c-u c-x spc, c-x space will move
;; back in mark-list
(setq set-mark-command-repeat-pop t)

;;; ctl-x-map
(define-keys ctl-x-map
  "C-w"  kill-ring-save      ; was ESC-w
  "/"    help-command
  "n r"  narrow-to-region
  "n h"  narrow-to-here

  "r a" list-registers
  "d"       dired
  "C-a"  pop-global-mark
  "SPC"  set-mark-command ; was, c-spc, but s-space doesn't work

  "j"  jump-to-register
  ;; kill-this-buffer in menu-bar.el, menu-bar is never used.
  "k"    kill-current-buffer

  ;"p"    paren-close-defun
  "t"    transpose-chars
  "*"    apply-macro-to-region-lines
  "l"    my-count-lines
  
  )

(defun my-count-lines ()
  "count lines in buffer"
  (interactive)
  (message (format "Total lines: %d; current line: %d"
                   (count-lines (point-min)
                                (point-max))
                   (line-number-at-pos))))
                   
                   


;;; this key is quite important.  not only is it easy to type by the former
;;; command bound to this key, `newline-and-indent', fits nicely to RETURN
;;; key.  also, certain commands wouldn't work under vt100 terminals, like
;;; esc-c-backspace, `backward-kill-sexp'.  ok, this used to bind c-j as a
;;; prefix key, but felt very awkward to use for lisp based commands...
(defvar ctl-l-map (make-keymap)
  "Keymap for C-l; for Lisp operations.")

(require 'cmuscheme) ; temp hack, for below


(add-hook 'cmuscheme-load-hook
          (lambda ()
            (define-key scheme-mode-map
              (kbd "RET") 'newline-and-indent)))

(define-key emacs-lisp-mode-map
  (kbd "RET") 'newline-and-indent)

(add-hook 'lisp-mode-hook
          (lambda ()
            (define-key lisp-mode-map
              (kbd "RET") 'newline-and-indent)))

(define-keys (ctl-l-map "C-l" global-map)
  "C-a"  beginning-of-defun
  "C-b"  backward-sexp
  "C-d"  down-list
  "C-e"  end-of-defun
  "C-f"  forward-sexp
  "C-h"    backward-kill-sexp
  "C-j"  end-of-line-and-reindent+paren
  "C-k"  kill-sexp
  "C-n"  forward-list
  "C-q"  indent-region
  ";"    indent-new-comment-line
  "c"    comment-dwim ;region
  "h"    mark-defun
  "j"    move-past-close-and-reindent
  "l"    eval-last-sexp
  "m"    mark-sexp
  "p"    backward-list
  ;; "q"    indent-sexp
  "s"    mark-sexp
  "t"    transpose-sexps
  "u"    backward-up-list
  "x"    eval-defun
  ")"    paren-close-defun
  )

;; ctl-x-4-map
(defun switch-to-scratch-buffer-other-window ()
  ;; See comment for `switch-to-scratch-buffer'.
  (interactive)
  (switch-to-buffer-other-window "*scratch*"))

(define-keys ctl-x-4-map
  "h"    html-view-buffer-other-window
  "s"    switch-to-scratch-buffer-other-window)


(define-keys ctl-x-map
  "a a"  abbrev-mode
  "a e"  edit-abbrevs)

(defun my-date ()
  (interactive)
  (let (server phx india)

    (setq server (format-time-string "%l:%M%p"))
    (if (= (aref server 0) ? )
        (setq server (substring server 1)))

    (set-time-zone-rule t)

    (let ((tmp (seconds-to-time (- (float-time)
                                   (* 7 3600)))))
      (setq phx (format-time-string "%l:%M%p" tmp))
      (if (= (aref phx 0) ? )
          (setq phx (substring phx 1))))

    (let ((tmp (seconds-to-time (+ (float-time)
                                   (* 5 3600)
                                   1800))))
      (setq india (format-time-string "%l:%M%p (%a)" tmp))
      (if (= (aref india 0) ? )
          (setq india (substring india 1))))

    (set-time-zone-rule nil)

    (let ((str (concat "server: %s          "
                       "            az: %s  "
                       "                    india: %s")))
      (message (downcase (format str
                                 server
                                 phx
                                 india))))))

;; c-c <letter>
(define-keys mode-specific-map
  "b"  nil ;transpose-buffers
  "c"  calendar
  "d"  my-date
  "l"  goto-line
  "m"  count-matches
  "o"  occur
  "r"  replace-regexp
  "s"  replace-string
  "t"  dabbrev-expand  ; similar to c-t, *** see c-j / & c-j '
  )

(define-key ctl-x-map "f"
  'find-file) ; Was set-fill-column; now, C-x C-f F

(define-key minibuffer-local-completion-map (kbd "C-c a")
  (lambda ()
    (interactive)
    (beginning-of-line)
    (when (looking-at "~/")
      (delete-minibuffer-contents)
      (insert "~/"))))


(defvar ctl-x-ctl-f-map (make-keymap)
  "Keymap for C-x C-f; for file operations.")

(define-key ctl-x-map (kbd "C-f") ctl-x-ctl-f-map)

(define-keys (ctl-x-ctl-f-map "C-f" ctl-x-map)
  "C-a"  update-file-autoloads
  "C-b"  byte-compile-file
  "C-f"  set-fill-column     ; was c-x f, whic is now find-file
  "C-l"  find-file-literally
  "C-n"  next-file
  "C-p"  find-file-at-point
  "C-r"  find-file-read-only
  "C-v"  find-alternate-file
  "C-w"  write-file
  "C-x"  hexl-find-file
  "A"    add-name-to-file
  "L"    load-library
  "P"    ediff-patch-files
  "R"    read-abbrev-file
  "a"    append-to-file
  "c"    copy-file
  "d"    delete-file
  "e"    ediff-files
  "f"    find-file
  "i"    insert-file
  "l"    load-file
  "n"    set-visited-file-name
  "p"    prepend-to-buffer ;; what about this???
  "r"    recover-current-file
  "v"    view-file
  "w"    write-abbrev-file
  )


(defun view-current-buffer ()
  (interactive)
  (if view-mode
      (view-mode -1)
    (view-buffer (current-buffer))))

(defun switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defvar ctl-x-ctl-b-map (make-keymap)
  "Keymap for C-x C-b; for buffer operations.")
(define-key ctl-x-map (kbd "C-b") ctl-x-ctl-b-map)

;; Keymap for C-x C-b; for buffer operations.
(define-keys (ctl-x-ctl-b-map "C-b" ctl-x-map)
  "C-b"  ibuffer ; buffer-menu
  "C-c"  erase-buffer  ; envision `clear-buffer'
  "C-d"  buffer-disable-undo
  "C-e"  buffer-enable-undo
  "C-p"  print-buffer
  "C-s"  ispell-buffer
  "C-r"  revert-buffer
  "a"    append-to-buffer
  "b"    bury-buffer
  "c"    copy-to-buffer
  "e"    eval-current-buffer
  "i"    insert-buffer
  "m"    mark-whole-buffer
  "n"    rename-buffer  ; envision `buffer-new-name'
  "p"    prepend-to-buffer
  "r"    eval-region
  "s"    switch-to-scratch-buffer
  "u"    rename-uniquely
  "v"    view-current-buffer
  "x"    hexlify-buffer
  )

;; Dired operations -- should these be done only
;; in dired mode?

;; doesn't work in minibuffer
(defvar ctl-j-map (make-keymap)
  "Keymap for C-j; replaces main portions of ESC map.")

(require 'view)

(define-key view-mode-map (kbd "C-j") ctl-j-map)


;; bind key for eval-last-expression and fix for esc-: map
;; also hook up ilisp
(define-key lisp-interaction-mode-map (kbd "C-j") ctl-j-map)

(define-keys (ctl-j-map "C-j" global-map)
  "C-j"  dired-jump
  "C-l"  reposition-window
  "C-o"  split-line
  "C-f"  lynx-fetch ;show-face-at-point
  "C-c"  exit-recursive-edit
  "C-r"  isearch-backward-regexp
  "C-s"  isearch-forward-regexp
  "C-m"  mark-paragraph
  "C-q"  query-replace-regexp
  "C-v"  scroll-other-window  ; very easy to type

  ;; do c-u 10 c-u 0 to insert 10 zeros
  "0"    digit-argument
  "1"    digit-argument
  "2"    digit-argument
  "3"    digit-argument
  "4"    digit-argument
  "5"    digit-argument
  "6"    digit-argument
  "7"    digit-argument
  "8"    digit-argument
  "9"    digit-argument

  "-"    negative-argument
  "="    count-lines-region
  "|"    shell-command-on-region
  ";"    eval-expression
  ","    tags-loop-continue
  "."    find-tag
  "<"    beginning-of-buffer
  ">"    end-of-buffer

  "{"    backward-paragraph
  "}"    forward-paragraph
  "\\"   delete-horizontal-space
  "'"    dabbrev-completion
  "/"    dabbrev-expand
  "!"    shell-command
  "%"    query-replace
  "SPC"  just-one-space
  "~"    not-modified
  "^"    delete-indentation
  "a"    backward-sentence
  "b"    backward-word
  "d"    kill-word
  "e"    forward-sentence
  "f"    forward-word
  "h"    backward-kill-word
  "i"    tab-to-tab-stop
  "k"    kill-sentence
  "l"    recenter
  "m"    back-to-indentation
  "n"    forward-paragraph
  "o"    open-line
  "q"    query-replace
  "r"    repeat-complex-command ; was c-x :
  "s"    shell
  "v"    scroll-down
  "x"    execute-extended-command
  "y"    yank-pop
  "z"    suspend-emacs
  )


(require 'dired)
(require 'dired-x)

(define-key ctl-x-4-map (kbd "C-j")
  'dired-jump-other-window)

(defvar ctl-x-ctl-d-map (make-keymap)
  "Keymap for C-x C-d; for directory operations.")

(define-key ctl-x-map (kbd "C-d")
  ctl-x-ctl-d-map)

(define-keys (ctl-x-ctl-d-map "C-d" ctl-x-map)
  "C-c"  byte-recompile-directory
  "C-a"  update-autoloads-from-directories
  "c"    cd
  "d"    delete-directory
  "l"    list-directory
  "p"    pwd
  "m"    make-directory)

(define-keys dired-mode-map
  "C-c C-o"  dired-display-file
  "C-o"      other-window
  "f"        nil ; flagging & marking files
  "f a"      dired-flag-auto-save-files
  "f b"      dired-flag-backup-files
  "f c"      dired-clean-directory
  "f d"      dired-mark-directories
  "f f"      dired-mark-files-containing-regexp
  "f g"      dired-flag-garbage-files
  "f r"      dired-flag-files-regexp
  "f s"      dired-mark-symlinks
  "f x"      dired-mark-executables
  [left]     dired-up-directory
  [right]    dired-advertised-find-file
  "b"        dired-beginning-of-buffer
  "e"        dired-end-of-buffer
  "SPC"      dired-next-dirline
  "C-c C-a"  beginning-of-line
  "a"        dired-do-search
  "ESC [ C-a" dired-previous-line
  "ESC [ C-b" dired-next-line
  "ESC [ C-c" dired-advertised-find-file
  "ESC [ C-d" dired-up-directory)

(defvar dired-listing-switches "-lFDh")

(add-hook 'dired-mode-hook
          (lambda ()
            (setq dired-dwim-target t)
            (setq dired-listing-switches "-lFDh")
            (setq dired-recursive-deletes t)
            (setq dired-no-confirm '(byte-compile load))
            (setq dired-omit-extensions
                  (remove ".pdf" dired-omit-extensions))
            (dired-omit-mode 1)
            (setq dired-omit-size-limit nil)))


(defun dired-beginning-of-buffer ()
  (interactive)
  (goto-char (point-min))
  (dired-initial-position (buffer-name)))

(defun dired-end-of-buffer ()
  (interactive)
  (goto-char (point-max))
  (dired-previous-line 1))
