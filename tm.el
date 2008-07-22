;;; tm.el --- functions for editing Test Manager files using forth-mode

;; Copyright (C) 1997, 2000 W. Martin Borgert <debacle@debian.org>

;; Author:     1997, 2000 W. Martin Borgert <debacle@debian.org>
;; Maintainer: W. Martin Borgert <debacle@debian.org>
;; Version:    $Id: tm.el,v 1.11 2000/07/30 12:20:59 debacle Exp $
;; Keywords:   Test Manager, K1297, protocol test

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains functions for programming Test Managers, an
;; enhancement of the Forth language as used in the Tektronix K1297
;; protocol tester.  These functions don't do much, just a bit of
;; colouring, some simple menus, and other not so important stuff.  It
;; uses forth-mode as provided by gforth.

(require 'font-lock)			; uses font-lock,
(require 'imenu)			; and imenu
(if (not (functionp 'forth-mode))	; is forth.el loaded?
    (load-library "forth"))		; similar to gforth.el

(defconst tm-positives
  (concat forth-positives
	  "state{ state_init{ action{ "))
(defconst tm-negatives
  (concat forth-negatives
	  "}state }state_init }action "))

(defvar tm-font-lock-keywords nil
  "Expressions to highlight in Forth mode.")
(setq tm-font-lock-keywords
      (eval-when-compile
	(list
	 (list "\\<\\(\\\\[ \t].*$\\)"	; one line comments
	       '(1 font-lock-comment-face nil t))
	 ;; new words
	 (list "^[ \t\n]*\\(\\:\\|CREATE\\)[ \t]+\\([^ \n\t]+\\)"
	       '(2 font-lock-function-name-face nil t))
	 ;; state definitions
	 (list "^\\([^ \t\n]+\\)[ \t]+\\(STATE{\\|STATE_INIT{\\)"
	       '(1 font-lock-function-name-face nil t))
	 ;; structuring keywords
	 (list
	  (concat
	   "\\<"
	   (regexp-opt
	    '("}ACTION" "ACTION{" "BEGIN" "CREATE" "DO" "DOER" "DOES>"
	      "ENDCASE" "CASE" "DOCASE" "ENDOF" "ELSE" "IF" "LOOP" "OF"
	      "REPEAT" "THEN" "UNTIL" "WHILE" "STATE_INIT{" "}STATE_INIT"
	      "STATE{" "}STATE") t) "\\>")
	  '(1 font-lock-keyword-face nil t))
	 ;; conditional compiling
	 (list
	  (concat
	   "\\<"
	   (regexp-opt
	    '("[IF]" "[ELSE]" "[THEN]"	; ANS Forth
	      "#IF" "#IFDEF" "#IFNOTDEF" "#ELSE" "#ENDIF") ; obsolete MForth
	    t) "\\>")
	  '(1 font-lock-reference-face nil t))
	 ;; string output, this highlighting doesn't work
	 (list "\\(\\.( [^)]*)\\)"
	       '(1 font-lock-string-face nil t))
	 ;; constants and variables
	 (list "\\<\\(CONSTANT\\|VARIABLE\\)[ \t]+\\([^ \n\t]+\\)"
	       '(2 font-lock-variable-name-face nil t))
	 ;; basic stack operations
	 (list
	  (concat
	   "\\<"
	   (regexp-opt
	    '("!"  "2DROP" "2DUP" "2OVER" "2SWAP" "3DROP" "3DUP"
	      ">R" "@" "C!"  "C@" "DROP" "DUP" "L!"  "L@" "NIP"
	      "OVER" "PICK" "R" "R>" "R@" "ROT" "SWAP" "W!"
	      "W@") t) "\\>")
	  '(1 font-lock-type-face nil t)))))
      
(defun tm-functions ()
  "
Functions for editing Test Manager code using forth-mode."
  (interactive)
  (setq imenu-generic-expression
	tm-imenu-generic-expression)
  (set (make-local-variable 'forth-positives)
       tm-positives)
  (set (make-local-variable 'forth-negatives)
       tm-negatives)
  (set (make-local-variable 'require-final-newline)
       t)
  (set (make-local-variable 'comment-start)
       "( ")
  (set (make-local-variable 'comment-end)
       " )")
  (set (make-local-variable 'comment-multi-line)
       t)
  (set (make-local-variable 'font-lock-defaults)
       '(tm-font-lock-keywords nil t nil nil))
  (set (make-local-variable 'comment-column)
       32)
  (set (make-local-variable 'comment-start-skip)
       "[\\(][ \n\t]+")
  (imenu-add-menubar-index))

(defun tm-beginning-of-word (&optional arg)
  "Bound to \\[tm-beginning-of-word]

Move backward to the beginning of a word definition.  With argument,
do it that many times.  Negative arg -N means move forward to Nth
following beginning of word definition.  Returns t unless search stops
due to beginning or end of buffer."

  (interactive "p")
  (and arg (< arg 0) (not (eobp)) (forward-char 1))
  (and
   (re-search-backward
    "\\(\\(^[ \t]*\\(:\\|CREATE\\)\\)\\|STATE{\\|STATE_INIT{\\)\\>"
    nil 'move (or arg 1))
   (progn (beginning-of-line) (recenter 0) t)))

(defun tm-end-of-word (&optional arg)
  "Bound to \\[tm-end-of-word]

Move forward to the end of a word definition.  With argument, do it
that many times.  Negative arg -N means move backward to Nth following
end of word definition.  Returns t unless search stops due to
beginning or end of buffer."

  (interactive "p")
  (and arg (> arg 0) (not (eobp)) (forward-char 1))
  (and
   (re-search-forward
    "\\(\\(^[ \t]*;\\)\\|}STATE\\|}STATE_INIT\\)\\>"
    nil 'move (or arg 1))
   (progn (beginning-of-line) (recenter (- (window-height) 2)) t)))

(defun tm-insert-repeat-loop ()
  "Insert a dummy begin/while/repeat loop at point."
  (interactive)
  (forth-indent-command)
  (insert "begin\n")
  (forth-indent-command)
  (insert "( condition )\n")
  (forth-indent-command)
  (insert "while\n")
  (forth-indent-command)
  (forth-indent-command)
  (insert "( action )\n")
  (forth-indent-command)
  (insert "repeat")
  (forth-indent-command)
  (insert "\n"))

(defun tm-insert-until-loop ()
  "Insert a dummy begin/until loop at point."
  (interactive)
  (forth-indent-command)
  (insert "begin\n")
  (forth-indent-command)
  (insert "( action, condition )\n")
  (forth-indent-command)
  (insert "until")
  (forth-indent-command)
  (insert "\n"))

(defun tm-insert-do-loop ()
  "Insert a dummy do/loop loop at point."
  (interactive)
  (forth-indent-command)
  (insert "( limit index ) do\n")
  (forth-indent-command)
  (insert "( i . j . )\n")
  (insert "loop")
  (forth-indent-command)
  (insert "\n"))

(defun tm-insert-case-construct ()
  "Insert an empty case/endcase construct at point."
  (interactive)
  (if (> (current-column) 0)
      (insert "\n"))
  (forth-indent-command)
  (insert "case\n")
  (forth-indent-command)
  (insert "1 of (  ) endof\n")
  (forth-indent-command)
  (insert "(  )\n")
  (forth-indent-command)
  (insert "endcase")
  (forth-indent-command)
  (insert "\n"))

(defvar tm-imenu-generic-expression nil
  "Imenu generic expression for Test Manager files.  See `imenu-generic-expression'.")
(setq tm-imenu-generic-expression
      '(("Constants"
	 "\\<constant\\>[ \t]+\\(\\sw+\\)" 1)
	("Variables"
	 "\\<variable\\>[ \t]+\\(\\sw+\\)" 1)
	("States"
	 "\\(\\sw+\\)[ \t]+\\<state{\\>" 1)
	("Words"
	 "\\<\\(:\\|create\\|make\\)\\>[ \t]+\\(\\sw+\\)" 2)))

(defun tm-dummy-id ()
  "Insert version control dummy.  Works for CVS, RCS, SCCS, and Continuus."
  (interactive)
  (beginning-of-line)
  (insert "\\ @(#) $Id: %full_filespec: ")
  (insert (if (buffer-file-name)
	      (file-name-nondirectory (buffer-file-name))
	    (buffer-name)))
  (insert " % $\n"))

(defun tm-adjust-comment ()
  "Adjust the the comment in current line."
  (interactive)
  (if (or (looking-at "(")
	  (looking-at "\\\\"))
      (let ((col comment-column))
	(while (< (current-column) col)
	  (insert ?\ ))
	(while (> (current-column) col)
	  (delete-backward-char 1)))))

;;; GNU Emacs Menu

(define-key forth-mode-map [menu-bar tm]
  (cons "TM" (make-sparse-keymap "TM")))
(define-key forth-mode-map [menu-bar tm stack-comment]
  '("Insert stack comment"    . tm-stack-comment))
(define-key forth-mode-map [menu-bar tm line]
  '("Insert line"             . tm-line))
(define-key forth-mode-map [menu-bar tm beautify-comment]
  '("Beautify comment"        . tm-beautify-comment))
(define-key forth-mode-map [menu-bar tm comment]
  '("Insert comment"          . indent-for-comment))
(define-key forth-mode-map [menu-bar tm beginning-of-word]
  '("Goto beginning of word"  . tm-beginning-of-word))
(define-key forth-mode-map [menu-bar tm end-of-word]
  '("Goto end of word"        . tm-end-of-word))
(define-key forth-mode-map [menu-bar tm adjust-comment]
  '("Adjust comment"          . tm-adjust-comment))
(define-key forth-mode-map [menu-bar tm dummy-id]
  '("Insert dummy id"         . tm-dummy-id))
(define-key forth-mode-map [menu-bar tm indent-command]
  '("Indent line"             . tm-indent-command))
(define-key forth-mode-map [menu-bar tm insert-repeat-loop]
  '("Insert a REPEAT loop"    . tm-insert-repeat-loop))
(define-key forth-mode-map [menu-bar tm insert-until-loop]
  '("Insert a UNTIL loop"     . tm-insert-until-loop))
(define-key forth-mode-map [menu-bar tm insert-do-loop]
  '("Insert a DO loop"        . tm-insert-do-loop))
(define-key forth-mode-map [menu-bar tm insert-case-construct]
  '("Insert a CASE construct" . tm-insert-case-construct))
(define-key forth-mode-map [menu-bar tm export-stripped]
  '("Export stripped file"    . tm-export-stripped))

(defun tm-export-stripped ()
  "Save file w/o any spaces, empty lines, or comments.
Useful for files > 64kByte to transfer to the good old K1197."
  (interactive)
  (let ((export-buffer (get-buffer-create
			(format "*%s-export*" (buffer-name))))
	(export-file-name tm-export-file-name))
    (save-excursion
      (switch-to-buffer export-buffer)
      (erase-buffer))
    (copy-region-as-kill (point-min) (point-max))
    (switch-to-buffer export-buffer)
    (yank)
    (goto-char (point-min))		; delete new-style comments
    (while (re-search-forward "[ \t]+\\[ \t]+.*$" nil t)
      (replace-match "" nil nil))
    (goto-char (point-min))		; delete comments at end of line
    (while (re-search-forward "[ \t]+([ \t][^)]*)[ \t]*$" nil t)
      (replace-match "" nil nil))
    (goto-char (point-min))		; delete comments at begin of line
    (while (re-search-forward "^[ \t]*([ \t][^)]*)" nil t)
      (replace-match "" nil nil))
    (goto-char (point-min))		; delete spaces at begin of line
    (while (re-search-forward "^[ \t]+" nil t)
      (replace-match "" nil nil))
    (goto-char (point-min))		; delete spaces at end of line
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match " " nil nil))	; leave one space, needed sometimes
    (goto-char (point-min))		; delete empty lines
    (flush-lines "^[ \t]*$")
    (write-file export-file-name t)))

(defun tm-header ()
  "Insert a comment line with date etc."
  (interactive)
  (beginning-of-line)
  (insert (format "( %s %s : "
		  (format-time-string "%Y-%m-%d" (current-time))
		  (or grammalogue
		      (getenv "GRAMMALOGUE")
		      (substring
		       (or (user-full-name)
			   (getenv "USER")
			   (getenv "LOGNAME")) 0 2))))
  (while (< (current-column) (- fill-column 1))
    (insert ?\ ))
  (insert ")") 
  (insert ?\n)
  (forward-line -1)
  (forward-char 16))

(defun tm-align-pics ()
  "Align PICS files made by the Tektronix K1297 TTCN compiler."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\(\\[\\|/\\) +" nil t)
    (replace-match "\\1" nil nil))
  (goto-char (point-min))
  (while (re-search-forward " +\\(/\\|\\]\\|\\.\\|,\\)" nil t)
    (replace-match "\\1"))
  (goto-char (point-min))
  (while (< (point) (point-max))
    (if (looking-at "V\.C\" (  *)\"  *VCR *$")
	(progn (kill-line)
	       (kill-line)
	       (forward-line -1))
      (if (looking-at "V\.C\" (")
	  (progn (re-search-forward
		  "V\.C\" ( *\\(.*\\))\" VCR[ \t]*$" nil t)
		 (replace-match "V\.C\" ( \\1)\" VCR ")
		 (backward-char 7)
		 (just-one-space)
		 (while (< (current-column) (- fill-column 7))
		   (insert ?\ )))))
    (while (looking-at "")
      (delete-char 1))
    (delete-blank-lines)
    (forward-line)))

(defun tm-beautify-comment ()
  "Align comment at begin and end of line."
  (interactive)
  (beginning-of-line)
  (while (looking-at "[ \t]*([ \t]")
    (if (looking-at
	 "[ \t]*([ \t]+[ \t-]*\\(-\*- tm -\*-\\)*[ \t-]*)[ \t]*$")
	(let ((char (char-after (+ (point) 2))))
	  (kill-line)
	  (tm-line char)
	  (forward-line)
	  (delete-char 1))
      (progn (re-search-forward "[ \t]*([ \t]" nil t)
	     (replace-match "( " nil nil)
	     (re-search-forward "[ \t]*)[ \t]*" nil t)
	     (replace-match ")")
	     (backward-char)
	     (while (< (current-column) (- fill-column 1))
	       (insert ?\ ))
	     (forward-line)))))

(defun tm-current-line ()
  "Return the vertical position of point."
  (+ (count-lines (window-start) (point))
     (if (= (current-column) 0) 1 0)
     -1))

(defun tm-line (&optional char)
  "Insert a comment line.  Optional char specifies line character.
Default is \"-\"."
  (interactive "P")
  (if (null char) (setq char ?-))
  (if (char-equal char ?\t) (setq char ?\ ))
  (beginning-of-line)
  (insert "( ")
  (if (= (tm-current-line) 0)
      (let ((count 0))
	(while (< count (- fill-column 18))
	  (insert char)
	  (setq count (1+ count)))
	(insert " -*- tm -*-"))
    (let ((count 0))
      (while (< count (- fill-column 4))
	(insert char)
	(setq count (1+ count)))))
  (insert " )")
  (insert ?\n)
  (forward-line -1))

(defun tm-stack-comment ()
  "Insert a stack comment for a Forth word."
  (interactive)
  (end-of-line)
  (while (< (current-column) comment-column)
    (insert ?\t))
  (insert "( -- )")
  (backward-char 4))

;;; tm.el ends here
