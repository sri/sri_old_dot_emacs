(if (not window-system)
    (setq ring-bell-function (lambda ()))) ;term.c

(setq make-backup-files nil)
(setq disabled-command-hook nil)

(defvar my-major-mode-alist
  '((python-mode     "py")
    (emacs-lisp-mode "el")
    (lisp-mode       "lisp" "cl")
    (scheme-mode     "scm")))

;; for when opening a new buffer (not file!)
(setq default-major-mode
      (lambda ()
        (if (eq major-mode 'fundamental-mode)
            (let ((mode 'text-mode)
                  (ext (file-name-extension (buffer-name)))
                  (alist my-major-mode-alist))
              (while alist
                (if (not (member ext (cdar alist)))
                    (setq alist (cdr alist))
                  (setq mode (caar alist))
                  (setq alist nil)))
              (funcall mode)))))

(with-current-buffer (get-buffer-create "*scratch*")
  (text-mode)
  (set (make-variable-buffer-local
        'next-line-add-newlines)
       t))
                 
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'html-mode-hook 'turn-off-auto-fill)

(fset 'yes-or-no-p 'y-or-n-p)
(fset 'ange-ftp-message 'message)

(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(line-number-mode -1)
(column-number-mode -1)
(blink-cursor-mode -1)
(auto-compression-mode t)
(partial-completion-mode t)

;; xxx
(delete '(-3 . "%p") default-mode-line-format)

(iswitchb-mode 1)
(setq iswitchb-prompt-newbuffer nil)

(setq enable-local-eval t)

(show-paren-mode t)

;; From gnu.emacs.sources post on 1998/05/02.
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


(require 'register)

(define-keys ctl-x-map
  "r v"  view-register
  "r p"  point-to-register)


(set-register ?~ (cons 'file "~"))
(set-register ?h (cons 'file my-dot-emacs-home))
(set-register ?p (cons 'file "~/code/Py"))
(set-register ?s (cons 'file "~/public_html/src"))
(set-register ?b (cons 'file "~/bin"))


;; don't echo passwords
(add-hook 'comint-output-filter-functions
	  'comint-watch-for-password-prompt)


(require 'shell)

(define-key shell-mode-map (kbd "ESC ?") ; what it is, globally
  'help-command)

(define-key shell-mode-map (kbd "C-c ESC c")
  'comint-dynamic-list-filename-completions)

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map "\C-m"
              'newline-and-indent)
            (outline-minor-mode 1)))


(add-hook 'Info-mode-hook
  (lambda ()
    (define-key Info-mode-map
      "." 'Info-next-reference)
    (define-key Info-mode-map
      ";" 'Info-prev-reference)
    (define-key Info-mode-map
      "z" 'Info-follow-nearest-node)))

(transient-mark-mode 1)


(autoload 'ruby-mode "ruby-mode" "Ruby editing mode" t)
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

(push '("\\.rb$" . ruby-mode) auto-mode-alist)
(push '("ruby" . ruby-mode) interpreter-mode-alist)

(require 'ruby-electric)
(push '(?/ . ?/) ruby-electric-matching-delimeter-alist)
 
(add-hook 'ruby-mode-hook
  (lambda ()
    (local-unset-key (kbd "C-j"))
    (define-key ruby-mode-map (kbd "C-m")
      'ruby-reindent-then-newline-and-indent)
    ;; inf-ruby
    (inf-ruby-keys)
    (ruby-electric-mode)
    (setq ruby-program-name "irb1.8 --inf-ruby-mode"))
  )
