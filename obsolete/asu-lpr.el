;;; asu-lpr.el -- Emacs interface to print files at ASU.

;; Author: Sriram Thaiyar
;; Created: Fri Mar 30 01:50:29 MST 2001
;; Keywords: unix, printing, lpr

;; This file is not part of GNU Emacs, but distributed under the same
;; conditions as GNU Emacs, and is useless without GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Sends a file to a printer; tab completion of all printers at
;; Arizona State University.  This is much simpler (for me, at least)
;; that the library file `lpr.el'.

;; To Do:
;;   - Ability to recogonize different file formats and print them
;;     out. Before this figure out how to print a `.dvi' file;
;;   - Printing buffers (is this really necessary)?


;;; Code:

(require 'dired)

;; Defined below; is here to shut up the byte-compiler.
(defvar asu-list-of-printers '()
  "The complete list of printers at ASU.
To get this list do, run the command, `npstat -p \"\"' on the
command line. Also, see the man pages for npstat, npdel, npshow.")

;; "P" used to be `dired-do-print'; see dired-aux.el.
(define-key dired-mode-map "P" 'asu-print-in-dired)

(defun asu-print-in-dired (&optional arg)
  "Print files in dired to an printer at ASU."
  (interactive "P")
  (asu-print-files (dired-get-marked-files t arg)))

(defun asu-print-file (file)
  "Print a single file to a printer at ASU."
  (interactive "fPrint file: ")
  (asu-print-files file))

(defun asu-print-files (files)
  "Print files from the general server.
FILES is a string or a list of strings.  `expand-file-name' is applied
to each file in FILES.  FILES are not checked if they exist or not;
they are simply passed to the lpr process."
  (interactive "sFiles to print: ")
  (let ((files (mapconcat 'expand-file-name
			  (if (stringp files)
			      (split-string files)
			    files)
			  " "))
	(printer (completing-read
		  "Select printer: "
		  (mapcar (lambda (p)
			    ;; Without the leading "Q"
			    (list (substring (car p) 1)))
			  asu-list-of-printers)
		  nil t))
	(lpr-output-buffer (get-buffer-create "*Lpr Output*")))
    (save-excursion
      (with-current-buffer lpr-output-buffer
	(goto-char (point-max))
	(insert "Printing file(s) " files ":\n")
	(call-process "lpr" nil lpr-output-buffer
		      nil (concat "-P" printer " " files))
	(insert "Done\n\n")))
    (message (concat "Pick up printout at " printer))))
    

(setq asu-list-of-printers
  '()
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NOTE:  The below function is obsolete, ever since, Xterms were
;; removed from the ASU campus.... so "localhost" cannot be used.
;;
;; How to get the value of `asu-print-to-localhost-file' in the doc
;; string?  Make this more clear & make this variable easily customizable.
;; (defcustom asu-print-to-localhost-file
;;   (expand-file-name "~/.hpux/files-to-print")
;;   "File to insert the files to printed.
;; This is because the files cannot be directly printed.  They need to
;; be lpr-ed from the localhost.  Then we can have a shell-script, like
;;
;;     #! /bin/sh
;;     lpr `asu-print-to-localhost-file'
;;
;; Thus we need to invoke the shell-script from the localhost to print
;; these files.")
;;
;; (defun asu-print-files (files)
;;   "Print file either from the general server or localhost.
;; FILES is either a string or a list of strings.
;; `expand-file-name' expands the file(s) in FILES to the
;; default directory, if they don't start with a directory name."
;;   (interactive "fFile to print: ")
;;   (let ((files (mapconcat #'expand-file-name
;; 			  (if (stringp files)
;; 			      (list file)
;; 			    files)
;; 			  " "))
;; 	(print-from (completing-read "Where to print from: "
;; 				     '(("general") ("localhost")) nil t)))
;;
;;     ;; Print from localhost
;;     (if (string= print-from "localhost")
;; 	(prog1
;; 	    (save-excursion
;; 	      (find-file asu-print-to-localhost-file)
;; 	      (erase-buffer)
;; 	      (insert files)
;; 	      (save-buffer)
;; 	      (kill-buffer (current-buffer)))
;; 	  (message "Click on `print-file' in Mwm menu to print the file"))
;;
;;       ;; Print from general
;;       (let ((printer (completing-read "Select printer: "
;; 				      (mapcar  ;without the leading `Q'
;; 				       (lambda (item)
;; 					 (list (substring (car item) 1)))
;; 				       asu-list-of-printers)
;; 				      nil t))
;; 	    (lpr-output (get-buffer-create "*lpr output*")))
;; 	(save-excursion
;; 	  (pop-to-buffer lpr-output)	  
;; 	  (goto-char (point-max))    ; Don't delete previous messages
;; 	  (insert "Printing file " files ":\n")
;; 	  (call-process "lpr" nil lpr-output nil
;; 			(concat "-P " printer " " files))
;; 	  (insert "Done\n\n"))
;; 	(message (concat "Pick up printout at " printer))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
