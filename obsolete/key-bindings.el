;;; key-bindings.el --- Local key bindings

;;; New keybindings based on the most comfortable keys and
;;; the most frequently used ones.  Also, since I work under
;;; VT100 terminals care must be taken when binding keys.
;;;
;;; Radical idea:  Bind uppercase letters to commands!
;;; Rationale:     Since I hack code most of the time as opposed to
;;;                editing text, I rarely use uppercase letters.
;;;                Commands *must* be provided to upcase and downcase
;;;                words and sentences quickly.  This works under
;;;                VT100.
;;;
;;; make undo like this: c-x u, and a char iterates through
;;; undo list, maybe showing certain previous ones, also
;;; should c-x c-f become c-x f. and fix keys for backwords-kill-
;;; <word> sentence, and make captalize, uppercase commands reverse.
;;; fix key bindings in view-mode
;;;
;;; a replacement for set-mark
;;;
;;; problems:  copying from one prefix to another, say from esc,
;;; to c-j:  esc-c-\ works under esc prefix, and so does esc-|.
;;; however, under c-j, both the keys have the same value.
;;; c-j doesn't work under minibuffer
;;;
;;; create a script that bytecompiles files as soon as you exit emacs
;;; writing errors to an output file

;;; Prefix keys bound to Control and their description:
;;;
;;; C-x   -- for commonly used keys
;;; C-j   -- for commonly used keys
;;; C-z   --
;;; C-l   -- lisp bindings, common programming tasks
;;; C-o   --
;;;
;;; Unused keys and their replacements:
;;; Done:
;;;
;;; C-t      -- transpose-chars     -- C-x t
;;; C-x C-v  -- find-alternate-file -- C-x C-f C-v
;;; C-x i    -- insert-file         -- C-x C-f i
;;;          -- insert-parentheses
;;; C-x d
;;; C-x C-d
;;; C-x f    -- set-fill-column     -- C-x C-f F
;;;          -- find-file
;;;
;;; C-x C-r  -- find-file-read-only -- C-x C-f C-r
;;; C-x C-w  -- write-file          -- C-x C-f C-w
;;; C-x C-j
;;; c-x c-n  -- set-goal-column
;;; c-x s    -- save-some-buffers
;;; C-x z -- repeat
;;; c-v   -- make it press a key and keep going down via
;;;          one letter, any other letters stops going down
;;;          some other letter makes it go up

(defun insert-user-home (x)
  (interactive "FPath: ")
  (insert (expand-file-name x)))  


;;; Derived from gnus-util.el's `gnus-define-keys'
(defmacro define-keys (keymap &rest keys)
  `(define-keys-1 (quote ,keymap) (quote ,keys)))

(defun define-keys-1 (keymap keys)
  (cond ((oddp (length keys))
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
	 (define-key (symbol-value (caddr keymap))
	   (read-kbd-macro (cadr keymap)) (car keymap))
	 (setq keymap (symbol-value (car keymap)))))
  (let (key command)
    (while keys
      (setq key (pop keys) command (pop keys))
      (cond ((symbolp key)
	     (setq key (symbol-value key)))
	    ((stringp key)
	     (setq key (read-kbd-macro key))))
      ;; command could be a keymap
      (when (and (symbolp command)
		 (boundp command)
		 (keymapp (symbol-value command)))
	(setq command (symbol-value command)))
      (define-key keymap key command))))

;;; control becomes shift and vice-versa. when this happens certain
;;; things are broken: c-space sets mark, however shift-space doesn't
;;; anymore.  for esc, use alt (press this with the thumb, which
;;; doesn't do *anything* except hit the damn space bar) plus the key.
;;; expriment: 04:29PM Friday November 23 2001.  hopefully, things
;;; will be much easier this way.
;;; to do: change esc-c-f to esc-F.  also need to be careful about
;;; esc-ctrl-<non-letter> like esc-ctrl-. and esc-ctrl-backspace. how to
;;; fix these.  also since i usually type a lowercase word and want to
;;; upcase or capatilize it afterwards, make those commands default to the
;;; previous word.

;;; Another idea:  make keyboard-translate-table buffer local so that
;;; in certain modes, like gnus, we can operate normally (or maybe
;;; change it during an entry and exit hook.

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

;;; The below marks some experiments...

;;; tab key is unaffected, so "I" emulating is wasting
;;; valuable real-estate.  "t" transpose-chars -> C-x t
;;;(let ((chars-to-rebind "IOT"))
;;;  (mapc (lambda (char)
;;;	  (keyboard-translate char nil))
;;;	(string-to-list chars-to-rebind)))
 
;;; ctrl-z is the same as esc, since i *never* use ctrl-z under x
;;; (define-key global-map (kbd "C-z") esc-map)

;;; (keyboard-translate-swap ? ?
)

;;; ;; Move () to [] to {} to (), in that order.
;;; (let ((list (map 'list 'identity "[(]){[}]({)}")))
;;;   (while list
;;;     (keyboard-translate (car list) (cadr list))
;;;     (setq list (cddr list))))

;;; (keyboard-translate-swap ?= ?+)
;;; (keyboard-translate-swap ?' ?\")

;;; why doesn't esc-ctl-x work when esc is defined like this?
;;; (define-keys global-map
;;;   "("  insert-parentheses
;;;   ")"  esc-map)

;;; ESC used to be C-[, which was quiet comfortable.  however, that key
;;; combination produces the character `[' due to the above bindings.
;;; experiment:  03:44AM Wednesday November 14 2001,  tab and enter
;;; note:  most modes bind ret & tab to something special.  so for
;;; any mode that does that we have to redefine it.  what about
;;; substitute-key-definition?
;;; (defun ret+tab-to-esc-prefix (mode)
;;;   (let* ((name (concat (symbol-name mode) "-map"))
;;; 	 (map (intern-soft name)))
;;;     (unless map
;;;       (error "Mode `%S' doesn't contain `%s'" mode name))
;;;     (define-keys mode
;;;       "RET"  esc-map
;;;       "TAB"  esc-map)))

;;; (defvar ret+tab-to-esc-prefix-modes
;;;   '(lisp-mode inferior-lisp-mode
;;; 	      scheme-mode inferior-scheme-mode
;;; 	      emacs-lisp-mode lisp-interaction-mode
;;; 	      py-mode py-shell
;;; 	      sml-mode inferior-sml-mode
;;; 	      text-mode)
;;;   "")
;;;(mapc 'ret+tab-to-esc-prefix
;;;      ret+tab-to-esc-prefix-modes)


;;; Help keys

;;; (define-key global-map "\M-h" 'help-for-help)
(define-key function-key-map
  (read-kbd-macro "C-h") (read-kbd-macro "DEL" t))

(define-key global-map (kbd "C-x ?") 'help-command)

(setq help-char ??)
(setq three-step-help t)

(define-keys help-map
  "C-a"  apropos
  "r"    read-library-file)

(defun read-library-file (name &optional arg)
  (interactive "MLibrary File: \nP")
  (let ((file
         (locate-library (if (string-match "\\.el$" name)
                             name
                           (concat name ".el")))))
    (if (not file)
	(message "Unable to find `%s' in `load-path'." name)
      (if arg
          (view-file-other-window file)
        (view-file file)))))

;;; Default item should have `t' as its first element.
(defvar run-interpreter-alist
  '((lisp-mode   . cmulisp)
    (scheme-mode . run-scheme)
    (python-mode . py-shell)
    (sml-mode    . run-sml)
    (t           . shell)))

(defun run-interpreter (&optional arg)
  "Quickly run a command intrepreter depending on the `major-mode'."
  (interactive "P")
  (let ((interpreter
         (assoc-default major-mode run-interpreter-alist
                        (lambda (item key)
                          (if (eq item t) t (eq item key))))))
    (unless arg
      (delete-other-windows)
      (split-window-vertically))
    (call-interactively interpreter)))
                        

;;; global-map
(define-keys global-map
  ;; from Olin Shiver's post on ll2.ai.mit.edu mailing list;
  ;; from former LispM hacker; swap () and []
  ;"("     "["
  ;")"     "]"
  ;"["     "("
  ;"]"       ")"
  
  "C-o"    other-window          ; was open-line:  now c-j o
  "C-t"    lisp-complete-symbol  ; was transpose-chars: now c-x t
  ;;"TAB"    insert-parentheses
  
  "\""     insert-delimiter-pair ; hit c-q " to enter just one
  "C-h"    delete-backward-char
  [find]   beginning-of-buffer   ; <home> in vt100
  [select] end-of-buffer         ; <end>  in vt100
  [home]   beginning-of-buffer
  [end]    end-of-buffer
  [f6]     gnus
  [27 f6]  gnus-other-frame      ; ESC F6
  [f7]     run-interpreter

;;; a learning experience: i prefer to have c-i (which really is shift-i)
;;; bound to insert-parentheses. but i am used to c-i (which really is TAB)
;;; being bound to several distinct keys.  therefore, i make `I' (capital
;;; i) to insert parentheses: bad idea! keys like c-x c-i don't work.
;;;  "O"      other-window
;;;  "T"      lisp-complete-symbol  ; was: transpose-chars -> c-x t
;;;  "I"      insert-parentheses

  )

(defun matching-parenthesis ()
  "Show matching parenthesis.
`blink-matching-open' on the previous parenthesis.  If `char-before'
 is one of `}', ']', '>', then it is matched."
  (interactive)
  (when (memq (char-before) '(?\} ?\] ?\> ?\)))
    (blink-matching-open)))

(defun narrow-to-here (here)
  (interactive "d")
  (narrow-to-region (point-min) here))

(defun kill-current-buffer ()
  "Kill the current buffer.
Queries, only if, the buffer has a file and buffer isn't saved."
  (interactive)
  (kill-buffer (current-buffer)))

;; for set-mark-command below, so that
;; after c-u c-x spc, c-x space will move
;; back in mark-list
(setq set-mark-command-repeat-pop t)

;;; ctl-x-map
(define-keys ctl-x-map
  "C-w"  kill-ring-save      ; was ESC-w
  "n r"  narrow-to-region
  "n h"  narrow-to-here

  "r a" list-registers
  
  "C-a"  pop-global-mark
  "SPC"  set-mark-command ; was, c-spc, but s-space doesn't work

  ;; kill-this-buffer in menu-bar.el, menu-bar is never used.
  "k"    kill-current-buffer

  "p"    paren-close-defun
  "t"    transpose-chars
  "*"    apply-macro-to-region-lines
  )

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

;(define-key ctl-l-map "z"
;  (if window-system
;      'iconify-or-deiconify-frame
;    'suspend-emacs))


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
  "c"    comment-region
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
  "\""   insert-delimiter-pair
  "`"    insert-delimiter-pair
  "'"    insert-delimiter-pair
  "<"    insert-delimiter-pair
  )

;; ctl-x-4-map
(defun switch-to-scratch-buffer-other-window ()
  ;; See comment for `switch-to-scratch-buffer'.
  (interactive)
  (switch-to-buffer-other-window "*scratch*"))

(define-keys ctl-x-4-map
  "h"    html-view-buffer-other-window
  "s"    switch-to-scratch-buffer-other-window)

;; ctl-x-5-map
;; <nothing here yet>


;; ctl-x-a: abbrevs
(define-keys ctl-x-map
  "a a"  abbrev-mode
  "a e"  edit-abbrevs   ; Was: expand-abbrev, use C-x a '
  )


;(require 'ut-time)

(defun print-readable-date (&optional arg)
  ""
  (interactive "P")
  (let ((universal-time (get-universal-time)))
    (destructuring-bind (ss mm hh d m y dow dst zone)
        (decode-universal-time universal-time)
      (let ((string (format "%04d-%02d-%02dT%02d:%02d:%02d"
                            y m d hh mm ss)))
        (if arg
            (insert string)
          (message string))))))


;; make simpler
(defun transpose-buffers (&optional arg)
  "Rotate buffers up or down in current frame, ARG times."
  (interactive "p")
  (let* ((selected-buffer (current-buffer))
	 (sw (selected-window))
	 (window-list (window-list nil 1))
	 (sw-on-top (append (memq sw window-list)
			    (reverse
			     (cdr (memq sw (reverse window-list))))))

	 ;; `top' & `bottom' get the correct order
	 (top (if (< arg 0)
		  (nthcdr (- arg) sw-on-top)
		(nthcdr (- (length sw-on-top) arg) sw-on-top)))

	 (bottom (if (< arg 0)
		     (reverse (nthcdr (+ (length sw-on-top) arg)
				      (reverse sw-on-top)))
		   (reverse (nthcdr arg (reverse sw-on-top)))))

	 ;; windows are now shifted arg times. get the associated buffers
	 (buffers (mapcar (lambda (window)
			    (window-buffer window))
			  (append top bottom))))
    (dolist (buffer buffers)
      (switch-to-buffer buffer)
      (other-window 1))))

;; Parenthesis insertion on [Fri Oct 13 16:30:21 MST 2000].
;; Into two function: [07:40PM Friday December 29 2000]
(defun paren-close-defun ()
  "Insert parenthesis to close the current function.
If a function does not exist in the vicinity, then closes the current
list."
  (interactive)
  (let ((open (count-parens (save-excursion
			      (save-restriction
				(widen)
				(beginning-of-defun)
				(point)))
			    (point)
			    t)))
    (when (plusp open)
      (insert (make-string open (string-to-char ")")))
      (blink-matching-open)
      (end-of-line))))

(defun count-parens (beg end &optional difference)
  "Count the number of open and closed parenthesis from BEG to END.
BEG and END are points in a buffer.  If the optional NON-INTERACTIVE
argument is non-nil, then open and closed parenthesis are returned as
a list (in that order).  Otherwise, a message containing the open and
closed parenthesis is displayed."
  (interactive "r")
  (let ((open 0)
	(closed 0))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char beg)
	(while (< (point) end)
	  (cond ((looking-at "(")
		 (setq open (1+ open))
		 (forward-char 1))
		((looking-at ")")
		 (setq closed (1+ closed))
		 (forward-char 1))
		;; lisp char, either ?\( or ?\)
		((looking-at "\?\\\\[()]")
		 (forward-char 3))
		;; string
		((looking-at "[^;\\]?\"")
		 ;; move past the first one
		 (forward-char 2)
		 ;; awful hack: handle "" (empty strings)
		 ;; correctly
		 (if (looking-at "\"")
		     (forward-char 1)
		   (re-search-forward "[^\\]\"")))
		;; comments; since we have seen a string, and have
		;; gone to the end of it, we dont need to check for
		;; situations like -> ";
		((looking-at ";")
		 (end-of-line))
		(t (forward-char 1))))))
    (cond ((interactive-p)
	   (message "Open parenthesis: %d; Closed: %d" open closed))
	  (difference
	   (- open closed))
	  (t
	   (list open closed)))))

;;; mode-specific-map == ctl-c-map
;;; Experiment: (01:58AM Sunday November 04 2001) since I haven't used too
;;; much of C-c <letter>, most of them are going to be Lisp specific.
;;; Actually, it would be better if they were mode specific...  Experiment:
;;; (08:00PM Friday November 30 2001) bind most ESC-C-<letter> (Lisp keys)
;;; to C-c-<letter>.
(define-keys mode-specific-map
  "b"  transpose-buffers
  "c"  calendar
  "d"  print-readable-date
  "l"  goto-line
  "m"  count-matches
  "o"  occur
  "r"  replace-regexp
  "s"  replace-string
  "t"  dabbrev-expand  ; similar to c-t, *** see c-j / & c-j '
  )

(define-key ctl-x-map "f" 'find-file) ; Was set-fill-column; now, C-x C-f F

;;; When invoking FIND-FILE, C-c a moves point to after `~/', if its there.
;;; Otherwise, point moves to beginning of line.

(define-key minibuffer-local-completion-map (kbd "C-c a")
  (lambda ()
    (interactive)
    (beginning-of-line)
    (when (looking-at "~/")
      (delete-minibuffer-contents)
      (insert "~/"))))


;; (defadvice find-file (around file-file-customizations act comp)
;;   "Various customizations added on to find-file."
;;   (let* ((tilde-key (kbd "C-c a")) ; F-F uses M-L-C-M not M-L-M
;;          (map minibuffer-local-completion-map)
;;          (old-key (lookup-key map tilde-key)))
  
;;     (unwind-protect
;;         (progn
;;           (define-key map tilde-key
;;             (lambda ()
;;               (interactive)
;;               (beginning-of-line)
;;               (when (looking-at "~/")
;;                 (delete-minibuffer-contents)
;;                 (insert "~/"))))
;;           ad-do-it)
;;       (define-key map tilde-key old-key))))
    

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
  "Put current buffer in view mode.
Turns off view-mode, when already active."
  (interactive)
  (if view-mode
      (view-mode -1)
    (view-buffer (current-buffer))))

;; Buffer operations
(defun switch-to-scratch-buffer ()
  (interactive)
  ;; Variable `default-major-mode' in `upstart.el'
  ;; automatically puts *scratch* into lisp-interaction-mode.
  (switch-to-buffer "*scratch*"))

(defvar ctl-x-ctl-b-map (make-keymap)
  "Keymap for C-x C-b; for buffer operations.")
(define-key ctl-x-map (kbd "C-b") ctl-x-ctl-b-map)

;; Keymap for C-x C-b; for buffer operations.
(define-keys (ctl-x-ctl-b-map "C-b" ctl-x-map)
  "C-b"  buffer-menu
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

(cload (expand-file-name "lynx.el" dot-emacs-home))


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

  ;; 2003-06-17T22:22:58:  previously c-j 0 to c-j 9
  ;; used to be digit-argument.  however, we can use
  ;; c-u to get to them.  so they are changed here.
  ;; pop quiz:  how to insert 19 zero's.
  ;; answer: c-u 19 c-u 0
  
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
  
  ;"$"    report-typing-measures
  ;"#"    reset-typing-measures
  "-"    negative-argument
  "="    count-lines-region
  "|"    shell-command-on-region
  ";"    eval-expression
  ","    tags-loop-continue
  "."    find-tag
  "<"    beginning-of-buffer
  ">"    end-of-buffer

  ;;"["    backward-page
  ;;"]"    forward-page

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
  "o"    open-line
  "q"    query-replace
  "r"    repeat-complex-command ; was c-x :
  "s"    shell
  "v"    scroll-down
  "x"    execute-extended-command
  "y"    yank-pop
  )

(defun show-face-at-point ()
  (interactive)
  (message "%S" (get-text-property (point) 'face)))

;;; Bind these to keys that are easily pressed.
(macrolet ((make-reverse-command (name)
             (let* ((command  (concat name "-word"))
                    (reversed (concat name "-previous-word"))
                    (doc-string
                     (format "Same as `%s', except negates prefix argument."
                             command)))
	       `(defun ,(intern reversed) (&optional arg)
                  ,doc-string
		  (interactive "p")
		  (,(intern command) (- arg))))))

  (make-reverse-command "capitalize")
  (make-reverse-command "upcase")
  (make-reverse-command "downcase"))

;; "c"    capitalize-previous-word
;; "u"    upcase-previous-word

(defun my-paste ()
  (interactive)
  (if (not (file-exists-p "~/x"))
      (message "~/x doesn't exist")
    (insert-file-contents "~/x")
    (message "Pasted contents of ~/x")))

;;; Typing

(define-key global-map
  [f6] 'home-window)
(define-key global-map
  [f10] 'my-paste)
(define-key global-map
  [f7] 'other-window)
(define-key global-map
  [f8] 'other-frame)
(defun home-window ()
  (interactive)
  (select-window (window-at 0 0)))

(define-key global-map
  [f12] 'info)


;;
(define-key ctl-x-map
  (kbd "j") 'jump-to-register)



;;; Dired customizations

(define-key ctl-x-4-map (kbd "C-j")
  'dired-jump-other-window)

(define-key global-map (kbd "C-x d")
  'dired)

(defvar ctl-x-ctl-d-map (make-keymap)
  "Keymap for C-x C-d; for directory operations.")

(define-key ctl-x-map (kbd "C-d") ctl-x-ctl-d-map)

(define-keys (ctl-x-ctl-d-map "C-d" ctl-x-map)
  "C-c"  byte-recompile-directory
  "C-a"  update-autoloads-from-directories
  "c"    cd
  "d"    delete-directory
  "l"    list-directory
  "p"    pwd
  "m"    make-directory)


(add-hook 'dired-load-hook
  (lambda ()
    (require 'dired-x)

    (setq dired-dwim-target t)

  (setq dired-listing-switches "-lFDh") ; b -- can't delete filenames with spaces
  ;; ask for each directory
  (setq dired-recursive-deletes t)
  ;;
  ;; others: '(chgrp chmod chown compress copy
  ;;                 delete hardlink move print
  ;;                 shell symlink uncompress)
  (setq dired-no-confirm '(byte-compile load))
  (setq dired-omit-extensions
        (remove ".pdf" dired-omit-extensions))

  (define-keys dired-mode-map
    ;; was C-o, which is other-window globally
    "C-c C-o"  dired-display-file
    "C-o"      other-window

    ;; `f' is for flagging & marking files;
    ;; setting it to NIL makes it a prefix command
     "f"        nil
     "f a"      dired-flag-auto-save-files
     "f b"      dired-flag-backup-files
     "f c"      dired-clean-directory
     "f d"      dired-mark-directories
     "f f"      dired-mark-files-containing-regexp
     "f g"      dired-flag-garbage-files
     "f r"      dired-flag-files-regexp
     "f s"      dired-mark-symlinks
     "f x"      dired-mark-executables

     [left]   dired-up-directory
     [right]  dired-advertised-find-file

     "b"      dired-beginning-of-buffer  ;was undefined
     "e"      dired-end-of-buffer        ;was dired-find-file 

     ;; was describe-mode, which for me is always
     ;; under C-x ? m
     "h"  dired-view-html-as-text
     
     ;;      "j"    ;was undefined

     "SPC"    dired-next-dirline         ;was dired-next-line

     "C-c C-a"   beginning-of-line

     ;; see below;  now with an arg replace without query
     "Q"  dired-replace-in-files
     
     ;; these two are swapped
     "A"   dired-find-alternate-file
     "a"   dired-do-search
     )

   ))

(add-hook 'dired-mode-hook
	  (lambda ()
	    ;; Buffer local variable -- cannot setq-default
	    ;; otherwise all buffers have "Omit" on modeline.
	    (setq dired-omit-files-p t)
            (setq dired-omit-size-limit nil)))

(defun dired-beginning-of-buffer ()
  "Move to the first file in dired buffer."
  (interactive)
  (goto-char (point-min))
  (dired-initial-position (buffer-name)))

(defun dired-end-of-buffer ()
  "Move to the last file in dired buffer."
  (interactive)
  (goto-char (point-max))
  (dired-previous-line 1))

(mapc (lambda (ignore)
        (push ignore debug-ignored-errors))
      '("^No file on this line$"
        "^Setting current directory: permission denied, .*"
        "^No more subdirectories$"))

(defun dired-replace-in-marked-files (from to)
  (interactive
   "sReplace in marked files (regexp): \nsReplace by: \n")
  (replace-in-files (list (cons from to))
                    (dired-get-marked-files)))

(defun dired-replace-in-files (arg)
  (interactive "P")
  (call-interactively
   (if arg 'dired-replace-in-marked-files 'dired-do-query-replace-regexp)))

;;; TODO:  combine below with HTML functions in functions.el

(defun dired-view-html-as-text (&optional arg)
  ;; Interface is like DIRED-DO-DELETE.  With a prefix arg
  ;; or if no files are marked, use the file that is next to
  ;; the point.  Otherwise, use files that are marked.
  "View marked files or current file as HTML."
  (interactive "P")
  (let ((files (dired-get-marked-files nil arg)))
    (dolist (file files)
      (let* ((name (concat "LYNX:" (file-name-nondirectory file)))
             (buffer (get-buffer-create name)))
        (with-current-buffer buffer
          (when (= 1 (point-max))
            (setq buffer-read-only nil)
            (erase-buffer))
          (buffer-disable-undo))
        (let ((proc (start-process name buffer "lynx" "-dump" file)))
          (set-process-sentinel proc
            'dired-view-html-as-text-process-sentinel)
          ;; If PROC finishes before we set its sentinel,
          ;; we need to explicit do it ourselves.
          (unless (eq (process-status proc) 'run)
            (dired-view-html-as-text-process-sentinel proc)))))))

(defun dired-view-html-as-text-process-sentinel (proc &optional event)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (view-mode)
    (pop-to-buffer (process-buffer proc))))
