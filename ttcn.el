;;; ttcn.el --- a major mode for editing TTCN.MP files

;; Copyright (C) 1997, 2000 W. Martin Borgert <debacle@debian.org>

;; Author:     1997, 2000 W. Martin Borgert <debacle@debian.org>
;; Maintainer: W. Martin Borgert <debacle@debian.org>
;; Created:    1997
;; Version:    $Id: ttcn.el,v 1.1.1.1 2000/04/02 10:56:53 debacle Exp $
;; Keywords:   TTCN, languages, ASN.1

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

(require 'easymenu)			; uses easymenu,
(require 'font-lock)			; font-lock,
(require 'imenu)			; and imenu

(defvar ttcn-mode-abbrev-table nil
  "Abbrev table used while in TTCN mode.")
(define-abbrev-table 'ttcn-mode-abbrev-table ())

(defvar ttcn-mode-syntax-table nil
  "Syntax table used while in TTCN mode.")

(if ttcn-mode-syntax-table
    ()
  (setq ttcn-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?$  "w"  ttcn-mode-syntax-table)
  (modify-syntax-entry ?(  "()" ttcn-mode-syntax-table)
  (modify-syntax-entry ?)  ")(" ttcn-mode-syntax-table)
  (modify-syntax-entry ?*  ". 23"  ttcn-mode-syntax-table)
  (modify-syntax-entry ?+  "."  ttcn-mode-syntax-table)
  (modify-syntax-entry ?-  ". 1234b"  ttcn-mode-syntax-table)
  (modify-syntax-entry ?/  ". 14"  ttcn-mode-syntax-table)
  (modify-syntax-entry ?:  "."  ttcn-mode-syntax-table)
  (modify-syntax-entry ?<  "(>"  ttcn-mode-syntax-table)
  (modify-syntax-entry ?=  "."  ttcn-mode-syntax-table)
  (modify-syntax-entry ?>  ")<"  ttcn-mode-syntax-table)
  (modify-syntax-entry ?[  "(]" ttcn-mode-syntax-table)
  (modify-syntax-entry ?\' "\""  ttcn-mode-syntax-table)
  (modify-syntax-entry ?\^m "> b" ttcn-mode-syntax-table)
  (modify-syntax-entry ?\_ "w"  ttcn-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" ttcn-mode-syntax-table)
  (modify-syntax-entry ?]  ")[" ttcn-mode-syntax-table)
  (modify-syntax-entry ?{  "(}" ttcn-mode-syntax-table)
  (modify-syntax-entry ?}  "){" ttcn-mode-syntax-table)
   )

(defvar ttcn-mode-map ()
  "Keymap used in TTCN mode.")

; (setq ttcn-mode-map nil)

(if ttcn-mode-map
    ()
  (setq ttcn-mode-map (make-sparse-keymap))
  (define-key ttcn-mode-map "\M-."    'find-tag-other-window)
  (define-key ttcn-mode-map "\M-a"    'ttcn-beginning-of-tabular)
  (define-key ttcn-mode-map "\M-e"    'ttcn-end-of-tabular)
  (define-key ttcn-mode-map "\C-c>"   'ttcn-behaviour-line-incr)
  (define-key ttcn-mode-map "\C-c<"   'ttcn-behaviour-line-decr)
  (define-key ttcn-mode-map [\M-down] 'ttcn-forward-line)
  (define-key ttcn-mode-map [\M-up]   'ttcn-backward-line)
  )

(define-key ttcn-mode-map [menu-bar ttcn]
  (cons "TTCN" (make-sparse-keymap "TTCN")))
(define-key ttcn-mode-map [menu-bar ttcn beginning-of-tabular]
  '("Goto beginning of tabular" . ttcn-beginning-of-tabular))
(define-key ttcn-mode-map [menu-bar ttcn end-of-tabular]
  '("Goto end of tabular" . ttcn-end-of-tabular))
(define-key ttcn-mode-map [menu-bar ttcn visit-tags-table]
  '("Visit TAGS table" . ttcn-visit-tags-table))
(define-key ttcn-mode-map [menu-bar ttcn remove-tabular]
  '("Remove tabular" . ttcn-remove-tabular))
(define-key ttcn-mode-map [menu-bar ttcn behaviour-line-incr]
  '("-->" . ttcn-behaviour-line-incr))
(define-key ttcn-mode-map [menu-bar ttcn behaviour-line-decr]
  '("<--" . ttcn-behaviour-line-decr))

(defvar ttcn-imenu-generic-expression nil
  "Imenu generic expression for TTCN.MP mode.  See `imenu-generic-expression'.")
(setq ttcn-imenu-generic-expression
      '(("Constants"
	 "\\$TS_ConstId[ \t]+\\(\\sw+\\)" 1)
 	("Variables"
	 "\\$T[CS]_VarId[ \t]+\\(\\sw+\\)" 1)
	("Timers"
	 "\\$TimerId[ \t]+\\(\\sw+\\)" 1)
	("Constraints"
	 "\\$ConsId[ \t]+\\(\\sw+\\)" 1)
	("Simple Types"
	 "\\$SimpleTypeId[ \t]+\\(\\sw+\\)" 1)
	("Defaults"
	 "\\$DefaultId[ \t]+\\(\\sw+\\)" 1)
	("Test Steps"
	 "\\$TestStepId[ \t]+\\(\\sw+\\)" 1)
	("Test Cases"
	 "\\$TestCaseId[ \t]+\\(\\sw+\\)" 1)
	("Parts"
	 "\\$\\([A-Z][A-Za-z]+\\)Part" 1)))

(defun ttcn-visit-tags-table ()
  "Visit TAGS table in current directory."
  (interactive)
  (visit-tags-table
   (file-name-directory
    (buffer-file-name)))) ; 'local))

(defun ttcn-fix4tags ()
  "Replace some newlines by spaces."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward
	  "^[ \n\t]*\\(\$[A-Za-z_]+\\)[ \n\t]+\\([^\$]\\)" nil t)
    (replace-match "\\1 \\2" nil nil))
; delete some spaces
  (goto-char (point-min))
  (while (re-search-forward
	  "[ \n\t]*\\(\\*/\\)[ \n\t]*\\()?\\)[ \n\t]+" nil t)
    (replace-match " \\1\\2\n" nil nil)))

(defun ttcn-beginning-of-tabular (&optional arg)
  "Move backward to the beginning of a tabular.
With argument, do it that many times.  Negative arg -N
means move forward to Nth following beginning of tabular.
Returns t unless search stops due to beginning or end of buffer.
A tabular starts with \"$Begin_\"."
  (interactive "p")
  (and arg (< arg 0) (not (eobp)) (forward-char 1))
  (and
   (re-search-backward "^[ \t]*\\$Begin_" nil 'move (or arg 1))
   (progn (beginning-of-line) t)))

;(defun ttcn-end-of-this-tabular ()
;  "Move forward to the end of a tabular.
;Returns t unless search stops due to beginning or end of buffer.
;A tabular ends with \"$End_\"."
;  (interactive)

(defun ttcn-end-of-tabular (&optional arg)
  "Move forward to the end of a tabular.
With argument, do it that many times.  Negative arg -N
means move backward to Nth following end of tabular.
Returns t unless search stops due to beginning or end of buffer.
A tabular ends with \"$End_\"."
  (interactive "p")
  (ttcn-beginning-of-tabular (- arg))
  (and (not (eobp)) (forward-char 1))
  (and
   (re-search-backward "^[ \t]*\\$Begin_\\([A-Xa-y1_&]+\\>\\)" nil 'move)
   (re-search-forward (concat "^[ \t]*\\$End_" (match-string 1)) nil 'move)
   (progn (beginning-of-line) t))
  (forward-line))

(defun ttcn-forward-line (&optional arg)
  "Move forward to next tabular line."
  (interactive "p")
  (re-search-forward "^[ \t]*\\$End_" nil 'move (or arg 1))
  (forward-line))

(defun ttcn-backward-line (&optional arg)
  "Move backward to previous tabular line."
  (interactive "p")
  (re-search-backward "^[ \t]*\\$End_" nil 'move (or arg 1)))

(defun ttcn-beginning-of-behaviour-line ()
  "Move backward to the beginning of a behaviour line.
Returns t unless search stops due to beginning or end of buffer.
A tabular starts with \"$BehaviourLine\"."
  (interactive)
  (and
   (re-search-backward "^[ \t]*\\$BehaviourLine" nil 'move 1)
   (progn (beginning-of-line) t)))

(defun ttcn-behaviour-line-incr (&optional arg)
  "Increase indentation level of behaviour line at point.
If optional argument arg is negative, decrease it."
  (interactive "p")
  (if (looking-at "\\$BehaviourLine")
      ()
    (ttcn-beginning-of-behaviour-line))
  (re-search-forward "^[ \t]*\\$Line[ \t]*\\[\\([0-9]+\\)\\]" nil 'move)
  (replace-match
   (number-to-string (max (+ (string-to-number (match-string 1)) arg) 0))
   t nil nil 1))

(defun ttcn-behaviour-line-decr ()
  "Decrease indentation level of behaviour line at point."
  (interactive)
  (ttcn-behaviour-line-incr -1))

(defun ttcn-remove-tabular ()
  "Remove TTCN tabular at point."
  (interactive)
  (ttcn-beginning-of-tabular) 
  (let ((beg (point)))
    (forward-line 1)
    (ttcn-end-of-tabular 1)
    (delete-region beg (point))))

; faces for the verdict
(defvar ttcn-verdict-pass-face 'ttcn-verdict-pass-face
  "Face for preliminary and final pass verdicts.")
(make-face ttcn-verdict-pass-face)
(set-face-foreground ttcn-verdict-pass-face "white")
(set-face-background ttcn-verdict-pass-face "blue")

(defvar ttcn-verdict-inconc-face 'ttcn-verdict-inconc-face
  "Face for preliminary and final inconc verdicts.")
(make-face ttcn-verdict-inconc-face)
(set-face-foreground ttcn-verdict-inconc-face "black")
(set-face-background ttcn-verdict-inconc-face "yellow")

(defvar ttcn-verdict-fail-face 'ttcn-verdict-fail-face
  "Face for preliminary and final fail verdicts.")
(make-face ttcn-verdict-fail-face)
(set-face-foreground ttcn-verdict-fail-face "white")
(set-face-background ttcn-verdict-fail-face "red")

(defvar ttcn-font-lock-keywords nil
  "Expressions to highlight in TTCN mode.")
(setq ttcn-font-lock-keywords
  (eval-when-compile
    (list
     ;; test cases, steps, defaults etc.
     (list (concat
	    "\\$\\(Default\\|Test\\(Case\\|Step\\)Id\\|Header\\)\\>"
	    "[ \t]+\\(\\sw+\\)?")
	   '(1 font-lock-keyword-face)
	   '(3 font-lock-function-name-face nil t))
     
     ;; $-keywords
     (list "^[ \t]*\\(\\$[A-V][A-Xa-y1_&]+\\)\\>"
	   '(1 font-lock-keyword-face nil t))
     ;; other TTCN keywords
     (list
      (concat
       "\\<"
       (regexp-opt
	'("?DONE" "?OTHERWISE" "?TIMEOUT" "ACTIVATE" "AND" "BITSTRING"
	  "BIT_TO_INT" "BOOLEAN" "BY" "CANCEL" "CREATE" "FALSE" "GOTO"
	  "GeneralString" "GraphicString" "HEXSTRING" "HEX_TO_INT"
	  "IA5String" "IF" "IF_PRESENT" "INFINITY" "INTEGER"
	  "INT_TO_BIT" "INT_TO_HEX" "IS_CHOSEN" "IS_PRESENT" "IUT"
	  "LENGTH_OF" "LT" "MOD" "NOT" "NUMBER_OF_ELEMENTS"
	  "NumericString" "OCTETSTRING" "OMIT" "OR" "PDU"
	  "PERMUTATION" "PrintableString" "R" "READTIMER" "REPEAT"
	  "REPLACE" "START" "SUBSET" "SUPERSET" "TO" "TRUE"
	  "TeletexString" "UNTIL" "UT" "VideotexString"
	  "VisibleString" "fail" "inconc" "min" "ms" "none" "ns"
	  "pass" "ps" "s" "us") t) "\\>")
      '(1 font-lock-builtin-face))
     ;; verdicts
     (list "^[ \t]*\\$VerdictId[ \t]+\\((?P\\(ASS\\)?)?\\)"
	   '(1 ttcn-verdict-pass-face nil t))
     (list "^[ \t]*\\$VerdictId[ \t]+\\((?I\\(NCONC\\)?)?\\)"
	   '(1 ttcn-verdict-inconc-face nil t))
     (list "^[ \t]*\\$VerdictId[ \t]+\\((?F\\(AIL\\)?)?\\)"
	   '(1 ttcn-verdict-fail-face nil t))
     ;; misc. identifier
     (list "^[ \t]*\\$[A-V][A-Xa-y1_&]+Id\\>[ \t]+\\([A-Za-z_][A-Za-z0-9_]+\\)"
	   '(1 font-lock-variable-name-face keep t))
; test step calls
;    ("^[ \t]*\\$Line[ \t]*\\[[0-9]+\\][ \t]+\\(\\+[ \t]*[A-Za-z_][A-Za-z0-9_]+\\)"
;     (1 font-lock-variable-name-face nil t))
;     )
     ;; ASN.1 keywords
     (list
      (concat
       "\\<"
       (regexp-opt
	'("ABSENT" "ABSTRACT-SYNTAX" "ALL" "APPLICATION" "AUTOMATIC" "BEGIN"
	  "BIT" "BMPSTRING" "BOOLEAN" "BY" "CHARACTER" "CHOICE" "CLASS"
	  "COMPONENT" "COMPONENTS" "CONSTRAINED" "DEFAULT" "DEFINITIONS"
	  "EMBEDDED" "END" "ENUMERATED" "EXCEPT" "EXPLICIT" "EXPORTS"
	  "EXTERNAL"
	  "FALSE" "FROM" "GeneralizedTime" "GeneralString" "IA5String"
	  "IDENTIFIER" "IMPLICIT" "IMPORTS" "INCLUDES" "INSTANCE" "INTEGER"
	  "INTERSECTION" "ISO646String" "MAX" "MIN" "MINUS-INFINITY" "NULL"
	  "NumericString" "OBJECT" "ObjectDescriptor" "OCTET" "OF" "OPTIONAL"
	  "PDV" "PLUS-INFINITY" "PRESENT" "PrintableString" "PRIVATE" "REAL"
	  "SEQUENCE" "SET" "SIZE" "STRING" "SYNTAX" "T61String" "TAGS"
	  "TeletexString" "TRUE" "TYPE-IDENTIFIER" "UNION" "UNIQUE"
	  "UNIVERSAL" "UniversalString" "UTCTime" "VideotexString"
	  "VisibleString" "WITH") t) "\\>")
      '(1 font-lock-reference-face)))))

;;;###autoload
(defun ttcn-mode ()
  "Major mode for editing TTCN code.
Not much, but highlighting."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'ttcn-mode)
  (setq mode-name "TTCN")
  (use-local-map ttcn-mode-map)
  (make-local-variable 'tags-file-name)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  (make-local-variable 'comment-end)
  (setq comment-end " */")
  (setq comment-multi-line t)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "-- \\|/\\*")
  (make-local-variable 'comment-column)
  (setq comment-column 9)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (set-syntax-table ttcn-mode-syntax-table)
  (setq blink-matching-paren t)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(ttcn-font-lock-keywords
	  nil nil ((?_ . "w"))
	  ttcn-beginning-of-tabular))
  (setq imenu-generic-expression ttcn-imenu-generic-expression
	imenu-case-fold-search nil)
  (imenu-add-menubar-index)
  (run-hooks 'ttcn-mode-hook))

;;; ttcn.el ends here

