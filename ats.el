;;; ats2-mode.el --- Major mode to edit ATS2 source code

;; Copyright (C) 2007  Stefan Monnier
;; updated and modified by Matthew Danish <mrd@debian.org> 2008-2013

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Todo:
;; - font-lock
;; - imenu
;; - outline
;; - indentation

;;; Code:

(require 'cl)
(require 'compile)
(require 'smie)

(when (not (boundp 'xemacsp))
  (setq xemacsp (boundp 'xemacs-logo)))

(defvar ats-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; (*..*) for nested comments.
    (modify-syntax-entry ?\( "()1" st)
    (modify-syntax-entry ?\) ")(4" st)
    (modify-syntax-entry ?*  ". 23n" st)
    ;; Not sure how to do // for single-line comments.
    ;; The current setting means that (/ and /* start a comment as well :-(
    (modify-syntax-entry ?/  ". 12b" st)
    (modify-syntax-entry ?\n ">  b" st)
    ;; Strings.
    (modify-syntax-entry ?\" "\"" st)
    ;; Same problem as in Ada: ' starts a char-literal but can appear within
    ;; an identifier.  So we can either default it to "string" syntax and
    ;; let font-lock-syntactic-keywords correct its uses in symbols, or
    ;; the reverse.  We chose the reverse, which fails more gracefully.
    ;; Oh, and ' is also overloaded for '( '{ and '[  :-(
    (modify-syntax-entry ?\' "_ p" st)
    ;;
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    ;; Skip over @/# when going backward-sexp over @[...], #[...],
    ;; #ident and $ident.
    (modify-syntax-entry ?\@ ". p" st)
    (modify-syntax-entry ?\# ". p" st)
    (modify-syntax-entry ?\$ ". p" st)
    ;; Same thing for macro&meta programming.
    (modify-syntax-entry ?\` ". p" st)
    (modify-syntax-entry ?\, ". p" st)
    ;; Just a guess for now.
    (modify-syntax-entry ?\\ "\\" st)
    ;; Handle trailing +/-/* in keywords.
    ;; (modify-syntax-entry ?+ "w" st)
    ;; (modify-syntax-entry ?- "w" st)
    ;; (modify-syntax-entry ?* "_" st)
    ;; Symbolic identifiers are kind of like in SML, which is poorly
    ;; supported by Emacs.  Worse: there are 2 kinds, one where "!$#?" are
    ;; allowed and one where "<>" are allowed instead.  Hongwei, what's that
    ;; all about?
    (modify-syntax-entry ?% "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    ;; (modify-syntax-entry ?. "." st)
    ;; (modify-syntax-entry ?/ "." st)  ; Already covered above for comments.
    (modify-syntax-entry ?: "." st)
    (modify-syntax-entry ?= "." st)
    ;; (modify-syntax-entry ?@ "." st)  ; Already defined above.
    (modify-syntax-entry ?~ "." st)
    ;; (modify-syntax-entry ?` "." st)  ; Already defined above.
    (modify-syntax-entry ?^ "." st)
    (modify-syntax-entry ?| "." st)
    ;; (modify-syntax-entry ?* "." st)  ; Already covered above for comments.
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?! "." st)
    ;; (modify-syntax-entry ?$ "." st)  ; Already defined above.
    ;; (modify-syntax-entry ?# "." st)  ; Already defined above.
    (modify-syntax-entry ?? "." st)
    ;; Real punctuation?
    (modify-syntax-entry ?:  "." st)
    (modify-syntax-entry ?\; "." st)
    st))

(defvar ats-mode-font-lock-syntax-table
  (let ((st (copy-syntax-table ats-mode-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    st))

;; Font-lock.

(defface ats-font-lock-static-face
  '(
    (default :inherit font-lock-type-face)
    ;; (t (:foreground "SkyBlue" :weight normal))
    )
  "Face used for static-related parts of code."
  :group 'ats-font-lock-faces)
(defvar ats-font-lock-static-face 'ats-font-lock-static-face)

(defface ats-font-lock-metric-face
  `(
    (default :inherit font-lock-constant-face)
    ;; (t (:foreground ,(cdr (assoc "zenburn-orange" zenburn-colors-alist)) :weight bold))
    )
  "Face used for termination metrics."
  :group 'ats-font-lock-faces)
(defvar ats-font-lock-metric-face 'ats-font-lock-metric-face)

(defface ats-font-lock-keyword-face
  '(
    (default :inherit font-lock-keyword-face)
    ;; (t (:foreground "Cyan" :weight normal))
    )
  "Face used for keywords."
  :group 'ats-font-lock-faces)
(defvar ats-font-lock-keyword-face 'ats-font-lock-keyword-face)

(defface ats-font-lock-c-face
  `(
    (default :inherit font-lock-comment-face)
    ;; (t (:foreground ,(cdr (assoc "zenburn-magenta" zenburn-colors-alist)) :weight normal))
    )
  "Face used for C code."
  :group 'ats-font-lock-faces)
(defvar ats-font-lock-c-face 'ats-font-lock-c-face)

(defun ats-context-free-search (regexp &optional limit)
  "Use inside a parenthesized expression to find a regexp at the same level."
  (let ((nest-lvl 0) foundp)
    (while (and (not (eobp))
                (or (null limit) (not (> (point) limit)))
                (not (minusp nest-lvl))
                (not (setq foundp
                           (and (zerop nest-lvl)
                                (looking-at regexp)))))
      (cond ((looking-at "(\\|\\[\\|{")
             (incf nest-lvl))
            ((looking-at ")\\|\\]\\|}")
             (decf nest-lvl)))
      (forward-char 1))
    foundp))

(defun ats-font-lock-mark-block ()
  (let ((lines 64))                     ; bit of a hack
    (set-mark (save-excursion (forward-line lines) (point)))
    (forward-line (- lines))))

(defun ats-font-lock-c-code-search (&optional limit)
  (interactive)
  ;; Font-lock mode works on regions that may not be large enough to
  ;; find both {% and %}.  Really, they should be treated like
  ;; comments and put into the syntax table.  Then the syntactic pass
  ;; would take care of C code.  However, there is only room for 2
  ;; kinds of comments in the table, and those are taken.  So the
  ;; keyword pass can try to get them.  But keyword pass doesn't
  ;; handle multiline keywords very well (because of region cutoff).
  ;; We can ignore the limit while searching, but coloration will not
  ;; happen outside the region anyway.  So it's going to be a little
  ;; screwy no matter what.  Not sure what to do about it.
  (setq limit nil)
  (let (begin end)
    (when (re-search-forward "%{" limit t)
      (setq begin (match-beginning 0))
      (when (re-search-forward "%}" limit t)
        (setq end (match-end 0))
        (when (and begin end)
          (store-match-data (list begin end))
          (point))))))

(defun ats-font-lock-static-search (&optional limit)
  (interactive)
  (when (null limit) (setq limit (point-max)))
  (let (foundp begin end (key-begin 0) (key-end 0) pt)
    (flet ((store ()
             (store-match-data (list begin end key-begin key-end))))
      ;; attempt to find some statics to highlight and store the
      ;; points beginning and ending the region to highlight.  needs
      ;; to be a loop in order to handle cases like ( foo : type )
      ;; where initially it considers ( .. | .. ) but finds no '|'
      ;; char so it must then go inside and look for sub-regions like
      ;; ": type".
      ;;
      ;; Each branch of the cond must be sure to make progress, the
      ;; point must advance, or else infinite-loop bugs may arise.
      (while (and (not foundp) (< (point) limit))
        (setq key-begin 0 key-end 0)
        (cond
         ((re-search-forward "(\\|:[^=]\\|{\\|[^[:space:].:-]<" limit t)
          (setq pt (setq begin (match-beginning 0)))
          (when pt (goto-char pt))
          (cond
           ;; handle { ... }
           ((looking-at "{")
            (forward-char 1)
            (cond
             ((save-excursion
                (forward-word -1)
                (looking-at "where"))
              ;; except when preceeded by "where" keyword
              (setq pt nil))
             ((re-search-forward "}" limit t)
              (setq end (match-end 0))
              (store)
              (setq pt end)
              (setq foundp t))
             (t
              (setq pt nil))))
           ;; handle ( ... | ... )
           ((looking-at "(")
            (forward-char 1)
            (incf begin)
            (cond
             ((null (ats-context-free-search "|\\|)" limit))
              (setq pt nil))
             ((looking-at "|")
              (setq end (match-end 0))
              (store)
              (setq foundp t))
             ((looking-at ")")
              (setq pt nil)
              ;; no | found so scan for other things inside ( )
              (goto-char (1+ begin)))))
           ;; handle ... : ...
           ((looking-at ":[^=]")
            (forward-char 1)
            (let ((nest-lvl 0) finishedp)
              ;; emacs22 only:
              ;;(ats-context-free-search ")\\|\\_<=\\_>\\|," limit)
              (ats-context-free-search ")\\|[^=]=[^=]\\|,\\|\n\\|\\]" limit)
              (setq begin (1+ begin)
                    end (point)
                    key-begin (1- begin)
                    key-end begin)
              (store)
              (setq foundp t)))
           ((looking-at "[^[:space:].:-]<")
            (forward-char 2)
            (incf begin)
            (cond
             ((re-search-forward ">" limit t)
              (setq end (match-end 0))
              (store)
              (setq pt end)
              (setq foundp t))
             (t
              (setq pt nil))))
           (t
            (setq pt nil)
            (forward-char 1)
            (setq foundp t))))
         (t
          (setq foundp t)
          (setq pt nil)))))
    pt))

(defvar ats-word-keywords
  '("abstype" "abst0ype" "absprop" "absview" "absvtype" "absviewtype" "absvt0ype" "absviewt0ype"
    "and" "as" "assume" "begin" "break" "continue" "classdec" "datasort"
    "datatype" "dataprop" "dataview" "datavtype" "dataviewtype" "do" "dynload" "else"
    "end" "exception" "extern" "extype" "extval" "fn" "fnx" "fun"
    "prfn" "prfun" "praxi" "castfn" "if" "in" "infix" "infixl"
    "infixr" "prefix" "postfix" "implmnt" "implement" "primplmnt" "primplement" "lam"
    "llam" "fix" "let" "local" "macdef" "macrodef" "nonfix" "overload"
    "of" "op" "rec" "scase" "sif" "sortdef" "sta" "stacst"
    "stadef" "stavar" "staload" "symelim" "symintr" "then" "try" "tkindef"
    "type" "typedef" "propdef" "viewdef" "vtypedef" "viewtypedef" "val" "prval"
    "var" "prvar" "when" "where" "for" "while" "with" "withtype"
    "withprop" "withview" "withvtype" "withviewtype"))

(defun wrap-word-keyword (w)
  (concat "\\<" w "\\>"))

(defvar ats-special-keywords
  '("$arrpsz" "$arrptrsize" "$delay" "$ldelay" "$effmask" "$effmask_ntm" "$effmask_exn" "$effmask_ref"
    "$effmask_wrt" "$effmask_all" "$extern" "$extkind" "$extype" "$extype_struct" "$extval" "$lst"
    "$lst_t" "$lst_vt" "$list" "$list_t" "$list_vt" "$rec" "$rec_t" "$rec_vt"
    "$record" "$record_t" "$record_vt" "$tup" "$tup_t" "$tup_vt" "$tuple" "$tuple_t"
    "$tuple_vt" "$raise" "$showtype" "$myfilename" "$mylocation" "$myfunction" "#assert" "#define"
    "#elif" "#elifdef" "#elifndef" "#else" "#endif" "#error" "#if" "#ifdef"
    "#ifndef" "#pragma" "#print" "#then" "#undef" "#include" "#staload" "#dynload" "#require"))

(defconst ats-=-starter-syms
  `("|"
    "val"
    "var"
    "prval"
    "prvar"
    "fn"
    "fnx"
    "fun"
    "prfn"
    "prfun"
    "praxi"
    "implmnt"
    "primplement"
    "primplmnt"
    "and"
    "datatype"
    "dataprop"
    "dataview"
    "datavtype"
    "dataviewtype"
    "type"
    "typedef"
    "abstype"
    "abst0ype"
    "absprop"
    "absview"
    "absvtype"
    "absviewtype"
    "absvt0ype"
    "absviewt0ype"
    "sortdef"
    "staload"
    ))

(defun wrap-special-keyword (w)
  (concat "\\" w "\\>"))

(defvar ats-keywords
  (append (list "\\<\\(s\\)?case\\(\\+\\|\\*\\|-\\)?\\>")
          (mapcar 'wrap-word-keyword ats-word-keywords)
          (mapcar 'wrap-special-keyword ats-special-keywords)))

(defvar ats-font-lock-keywords
  (append
   '((ats-font-lock-c-code-search (0 'ats-font-lock-c-face t))
     ;; ("%{[[:print:][:cntrl:]]*%}" (0 'ats-font-lock-c-face))

     ;;     ("[^%]\\({[^|}]*|?[^}]*}\\)" (1 'ats-font-lock-static-face))
     ;;     ("[^']\\(\\[[^]|]*|?[^]]*\\]\\)" (1 'ats-font-lock-static-face))
     ("\\.<[^>]*>\\." (0 'ats-font-lock-metric-face))
     (ats-font-lock-static-search
      (0 'ats-font-lock-static-face)
      (1 'ats-font-lock-keyword-face)))

   (list (list (mapconcat 'identity ats-keywords "\\|")
               '(0 'ats-font-lock-keyword-face)))))

(defvar ats-font-lock-syntactic-keywords
  '(("(\\(/\\)" (1 ". 1b"))             ; (/ does not start a comment.
    ("/\\(*\\)" (1 ". 3"))              ; /* does not start a comment.
    ("\\(/\\)///" (0 "< nb"))           ; Start a comment with no end.
    ;; Recognize char-literals.
    ("[^[:alnum:]]\\('\\)\\(?:[^\\]\\|\\\\.[[:xdigit:]]*\\)\\('\\)"
     (1 "\"'") (2 "\"'"))
    ))

;;; Indentation

(defvar ats-indent-separator-outdent 2)

(defcustom ats-indent-args ats-indent-separator-outdent
  "Indentation of args placed on a separate line."
  :type 'integer)

(defconst ats-smie-grammar
  ;; We have several problem areas where ATS's syntax can't be handled by an
  ;; operator precedence grammar:
  ;;
  ;; "= A before B" is "= A) before B" if this is the
  ;;   `boolean-=' but it is "= (A before B)" if it's the `definitional-='.
  ;;   We can work around the problem by tweaking the lexer to return two
  ;;   different tokens for the two different kinds of `='.
  ;; "of A | B" in a "case" we want "of (A | B, but in a `datatype'
  ;;   we want "of A) | B".
  ;; "= A | B" can be "= A ) | B" if the = is from a "fun" definition,
  ;;   but it is "= (A | B" if it is a `datatype' definition (of course, if
  ;;   the previous token introducing the = is `and', deciding whether
  ;;   it's a datatype or a function requires looking even further back).
  ;; "functor foo (...) where type a = b = ..." the first `=' looks very much
  ;;   like a `definitional-=' even tho it's just an equality constraint.
  ;;   Currently I don't even try to handle `where' at all.
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((exp ("if" exp "then" exp "else" exp)
	    ("case+" exp "of" branches)
	    ("case-" exp "of" branches)
	    ("case" exp "of" branches)
      ("let" decls "in" cmds "end")
      (sexp)
      ("begin" cmds "end")
      ("try" exp "with" branches)
      ("lam" sexp "=>" exp))
       ;; "simple exp"s are the ones that can appear to the left of `handle'.
       (sexp (sexp ":" type) ("(" exps ")")
             (".<" exp ">.")
             (sexp "orelse" sexp)
             (sexp "andalso" sexp))
       (cmds (cmds ";" cmds) (exp))
       (exps (exps "," exps) (exp))     ; (exps ";" exps)
       (branches (sexp "=>" exp) (branches "|" branches))
       ;; Operator precedence grammars handle separators much better then
       ;; starters/terminators, so let's pretend that let/fun are separators.
       (decls (sexp "d=" exp)
              (sexp "d=" databranches)
              (funbranches "|" funbranches)
              (sexp "=of" type)         ;After "exception".
              ;; FIXME: Just like PROCEDURE in Pascal and Modula-2, this
              ;; interacts poorly with the other constructs since I
              ;; can't make "local" a separator like fun/val/type/...
              ("local" decls "in" decls "end")
              ;; (decls "type" decls)
              ;; (decls "and" decls)
              (decls "withtype" decls)
              ;; (decls "infix" decls)
	      ;; (decls "infixl" decls)
              ;; (decls "infixr" decls)
              ;; (decls "nonfix" decls)
              ;; (decls "abstype" decls)
              ;; (decls "datatype" decls)
              ;; (decls "include" decls)
              ;; (decls "sharing" decls)
              ;; (decls "exception" decls)
              ;; (decls "fun" decls)
              ;; (decls "val" decls)
	      ;; (decls "implement" decls)
	      (decls "exception" decls)
	      (decls "nonfix" decls)
	      (decls "suffix" decls)
	      (decls "prefix" decls)
	      (decls "infixl" decls)
	      (decls "infixr" decls)
	      (decls "infix" decls)
	      (decls "absviewt0ype" decls)
	      (decls "absvt0ype" decls)
	      (decls "absviewtype" decls)
	      (decls "absvtype" decls)
	      (decls "absview" decls)
	      (decls "absprop" decls)
	      (decls "abst0ype" decls)
	      (decls "abstype" decls)
	      (decls "dataviewtype" decls)
	      (decls "datavtype" decls)
	      (decls "dataview" decls)
	      (decls "dataprop" decls)
	      (decls "datatype" decls)
	      (decls "type" decls)
	      (decls "tkindef" decls)
	      (decls "viewtypedef" decls)
	      (decls "vtypedef" decls)
	      (decls "viewdef" decls)
	      (decls "propdef" decls)
	      (decls "typedef" decls)
	      (decls "praxi" decls)
	      (decls "prfun" decls)
	      (decls "prfn" decls)
	      (decls "fun" decls)
	      (decls "fnx" decls)
	      (decls "fn" decls)
	      (decls "prvar" decls)
	      (decls "prval" decls)
	      (decls "var" decls)
	      (decls "val" decls)
	      (decls "assume" decls)
	      (decls "primplmnt" decls)
	      (decls "primplement" decls)
	      (decls "extern" decls)
	      (decls "implmnt" decls)
	      (decls "implement" decls)
	      (decls "sortdef" decls)
	      (decls "staload" decls)
	      )
       (type (type "->" type)
             (type "*" type))
       (funbranches (sexp "d=" exp))
       (databranches (sexp "=of" type) (databranches "d|" databranches))
       ;; Module language.
       ;; (mexp ("functor" marg "d=" mexp)
       ;;       ("structure" marg "d=" mexp)
       ;;       ("signature" marg "d=" mexp))
       (marg (marg ":" type) (marg ":>" type))
       (toplevel (decls) (exp) (toplevel ";" toplevel)))
     ;; '(("local" . opener))
     ;; '((nonassoc "else") (right "handle"))
     '((nonassoc "of") (assoc "with" "|"))     ; "case a of b => case c of d => e | f"
     ;; '((nonassoc "handle") (assoc "|")) ; Idem for "handle".
     '((assoc "->") (assoc "*"))
     '(;; (assoc "implement" "val" "fun" "type" "datatype" "abstype" "open" "infix" "infixr" "infixl"
       ;;        "nonfix" "functor" "signature" "structure" "exception"
       ;;        "include" "sharing" "local")
       (assoc
        "staload"
        "sortdef"
        "implement"
        "implmnt"
        "extern"
        "primplement"
        "primplmnt"
        "assume"
        "val"
        "var"
        "prval"
        "prvar"
        "fn"
        "fnx"
        "fun"
        "prfn"
        "prfun"
        "praxi"
        "typedef"
        "propdef"
        "viewdef"
        "vtypedef"
        "viewtypedef"
        "tkindef"
        "type"
        "datatype"
        "dataprop"
        "dataview"
        "datavtype"
        "dataviewtype"
        "abstype"
        "abst0ype"
        "absprop"
        "absview"
        "absvtype"
        "absviewtype"
        "absvt0ype"
        "absviewt0ype"
        "infix"
        "infixr"
        "infixl"
        "prefix"
        "suffix"
        "nonfix"
        "exception")
       (assoc "withtype")
       (assoc "and"))
     '((assoc "orelse") (assoc "andalso") (nonassoc ":"))
     '((assoc ";")) '((assoc ",")) '((assoc "d|")))

    (smie-precs->prec2
     '((nonassoc "andalso")                       ;To anchor the prec-table.
       (assoc "before")                           ;0
       (assoc ":=" "o")                           ;3
       (nonassoc ">" ">=" "<>" "<" "<=" "=")      ;4
       (assoc "::" "@")                           ;5
       (assoc "+" "-" "^")                        ;6
       (assoc "/" "*" "quot" "rem" "div" "mod")   ;7
       (nonassoc " -dummy- ")))                   ;Bogus anchor at the end.
    )))

(defun ats--rightalign-and-p ()
  (when ats-rightalign-and
    ;; Only right-align the "and" if the intervening code is more deeply
    ;; indented, to avoid things like:
    ;; datatype foo
    ;;   = Foo of int
    ;;      and bar = Bar of string
    (save-excursion
      (let ((max (line-end-position 0))
            (_data (smie-backward-sexp "and"))
            (startcol (save-excursion
                        (forward-comment (- (point)))
                        (current-column)))
            (mincol (current-column)))
        (save-excursion
          (search-forward "=" max t)
          (forward-line 1)
          (if (< (point) max) (setq max (point))))
        (while (and (<= (point) max) (not (eobp)))
          (skip-chars-forward " \t")
          (setq mincol (current-column))
          (forward-line 1))
        (>= mincol startcol)))))

(defun ats-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) ats-indent-separator-outdent)
    (`(:elem . args)  ats-indent-args)
    (`(:elem . empty-line-token) t)
    (`(:list-intro . "fun") t)
    (`(:list-intro . "lam") t)
    (`(:list-intro . "llam") t)
    (`(:list-intro . ";") t)
    (`(:list-intro . nil) t)
    (`(:close-all . "}") 0)
    (`(:close-all . ,_) t)
    (`(:after . "end") 0)
    (`(:after . "begin") 2)
    (`(:after . "=>") (if (smie-rule-hanging-p) 0 2))
    (`(:after . "in") (if (smie-rule-parent-p "local") 0))
    (`(:after . "of") 3)
    (`(:after . ,(or `"(" `"{" `"[")) (if (not (smie-rule-hanging-p)) 2))
    (`(:after . "else") (if (smie-rule-hanging-p) 0)) ;; (:next "if" 0)
    (`(:after . ,(or `"|" `"d|" `";" `",")) (smie-rule-separator kind))
    (`(:after . "d=")
     (if (and (smie-rule-parent-p "prval" "val" "var")
              (smie-rule-next-p "fn"
                                "fun"
                                "fnx"
                                "prfn"
                                "prfun"
                                "implement"
                                "implmnt")) -3))
    (`(:before . "=>") (if (smie-rule-parent-p "fn") 3 ))
    (`(:before . "of") 1)
    ;; FIXME: pcase in Emacs<24.4 bumps into a bug if we do this:
    ;; (`(:before . ,(and `"|" (guard (smie-rule-prev-p "of")))) 1)
    (`(:before . "|") (if (smie-rule-prev-p "of") 1 (smie-rule-separator kind)))
    (`(:before . ,(or `"|" `"d|" `";" `",")) (smie-rule-separator kind))
    ;; Treat purely syntactic block-constructs as being part of their parent,
    ;; when the opening statement is hanging.
    (`(:before . ,(or `"let")) (if (or
                                    (smie-rule-parent-p "let"
                                                        "if"
                                                        "implement"
                                                        "fun"
                                                        "fn"
                                                        "fnx"
                                                        "val"
                                                        "assume"
                                                        "var"
                                                        "prval"
                                                        "prvar"
                                                        "implement"
                                                        "implmnt"
                                                        "primplement"
                                                        "primplmnt")
                                    (smie-rule-hanging-p))
                                   (if (smie-rule-parent-p "if")
                                       `(column . ,(+ 2 (cdr (smie-rule-parent))))
                                     (smie-rule-parent)
                                       )
                                 2))
    (`(:before . ,(or `"(" `"[" `"{")) ; "struct"? "sig"?
     (if (smie-rule-hanging-p) (smie-rule-parent)
       (if (smie-rule-prev-p "=of") 4 (smie-rule-parent))))
    ;; Treat if ... else if ... as a single long syntactic construct.
    ;; Similarly, treat fn a => fn b => ... as a single construct.
    (`(:before . ,(or `"if"))
     (if
         (and (not (smie-rule-bolp))
              (smie-rule-prev-p (if (equal token "if") "else" "=>") "d=")
              (smie-rule-parent))
         t (if (smie-rule-parent-p "fun" "fn" "fnx" "prfun" "prfn")
               `(column . ,(+ 2 (cdr (smie-rule-parent))))
             (if (smie-rule-parent-p "let")
                 nil (smie-rule-parent)))))
    (`(:before . "and")
     ;; FIXME: maybe "and" (c|sh)ould be handled as an smie-separator.
     (cond
      ((smie-rule-parent-p "datatype"
                           "dataprop"
                           "dataview"
                           "datavtype"
                           "dataviewtype"
                           "withtype")
       (if (ats--rightalign-and-p) 5 0))
      ((smie-rule-parent-p
        "fun"
        "fn"
        "fnx"
        "val"
        "assume"
        "var"
        "prval"
        "prvar"
        "implement"
        "implmnt"
        "primplement"
        "primplmnt"
        "praxi"
        )
       2)))
    (`(:before . "withtype") 0)
    (`(:before . "d=")
     (cond
      ((smie-rule-parent-p
        "datatype"
        "dataprop"
        "dataview"
        "datavtype"
        "dataviewtype") (if (smie-rule-bolp) 0 2))))
    ;; ((smie-rule-parent-p "structure" "signature" "functor") 0)))
    ;; Indent an expression starting with "local" as if it were starting
    ;; with "fun".
    (`(:before . "local") (smie-indent-keyword "fun"))
    ;; FIXME: type/val/fun/... are separators but "local" is not, even though
    ;; it appears in the same list.  Try to fix up the problem by hand.
    ;; ((or (equal token "local")
    ;;      (equal (cdr (assoc token smie-grammar))
    ;;             (cdr (assoc "fun" smie-grammar))))
    ;;  (let ((parent (save-excursion (smie-backward-sexp))))
    ;;    (when (or (and (equal (nth 2 parent) "local")
    ;;                   (null (car parent)))
    ;;              (progn
    ;;                (setq parent (save-excursion (smie-backward-sexp "fun")))
    ;;                (eq (car parent) (nth 1 (assoc "fun" smie-grammar)))))
    ;;      (goto-char (nth 1 parent))
    ;;      (cons 'column (smie-indent-virtual)))))
    ))

(defun ats-smie-definitional-equal-p ()
  "Figure out which kind of \"=\" this is.
Assumes point is right before the = sign."
  ;; The idea is to look backward for the first occurrence of a token that
  ;; requires a definitional "=" and then see if there's such a definitional
  ;; equal between that token and ourselves (in which case we're not
  ;; a definitional = ourselves).
  ;; The "search for =" is naive and will match "=>" and "<=", but it turns
  ;; out to be OK in practice because such tokens very rarely (if ever) appear
  ;; between the =-starter and the corresponding definitional equal.
  ;; One known problem case is code like:
  ;; "functor foo (structure s : S) where type t = s.t ="
  ;; where the "type t = s.t" is mistaken for a type definition.
  (save-excursion
    (let ((res (smie-backward-sexp "=")))
      (member (nth 2 res) `(":" ,@ats-=-starter-syms)))))

(defun ats-smie-non-nested-of-p ()
  ;; FIXME: Maybe datatype-|-p makes this nested-of business unnecessary.
  "Figure out which kind of \"of\" this is.
Assumes point is right before the \"of\" symbol."
  (save-excursion
    ;; (let ((case-fold-search nil))
    ;;   (and (re-search-backward (concat "\\(" ats-non-nested-of-starter-re
    ;;                                    "\\)\\|\\_<case\\_>")
    ;;                            nil t)
    ;;        (match-beginning 1)))

    ;; (and (stringp (ats-smie-backward-token-1))
    ;;      (let ((tok (ats-smie-backward-token-1)))
    ;;        (if (equal tok "=")
    ;;            (equal "d=" (ats-smie-forward-token))
    ;; 	     (member tok '("|" "exception")))))

    (beginning-of-line)
    (if (member (ats-smie-forward-token-1) '("|" "exception"))
    	t
      (let ((tok (ats-smie-backward-token-1)))
    	(if (equal tok "=")
    	    (equal "d=" (ats-smie-forward-token))
    	  (member tok '("|" "exception")))))

)
    )

(defun ats-smie-datatype-|-p ()
  "Figure out which kind of \"|\" this is.
Assumes point is right before the | symbol."
  (save-excursion
    (forward-char 1)                    ;Skip the |.
    (let ((after-type-def
           ;; '("|" "of" "in" "datatype" "and" "exception" "abstype" "infix"
           ;;   "infixr" "nonfix" "local" "val" "fun" "fn" "fnx" "prfun" "prfn" "praxi")
	   (list
	    "|" "of" "in" "datatype"
	    "dataprop" "dataview" "datavtype" "dataviewtype" "and" "assume"
	    "exception" "abstype" "abst0ype" "absprop" "absview"
	    "absvtype" "absviewtype" "absvt0ype" "absviewt0ype" "infix"
	    "infixr" "infixl" "prefix" "suffix" "nonfix" "local" "val"
	    "var" "prval" "prvar" "fun" "fn" "fnx" "prfun" "prfn" "praxi")
	   ))
      (or (member (ats-smie-forward-token-1) after-type-def) ;Skip the tag.
          (member (ats-smie-forward-token-1) after-type-def)
          (condition-case nil
              (search-forward "of" (save-excursion (end-of-line) (point)))
            (error nil))
          ))))

(defun ats-smie-forward-token-1 ()
  (forward-comment (point-max))
  (buffer-substring-no-properties
   (point)
   (progn
     (or (/= 0 (skip-syntax-forward "'w_"))
         (skip-syntax-forward ".'"))
     (point))))

(defun ats-smie-forward-token ()
  (let ((sym (ats-smie-forward-token-1)))
    (cond
     ((equal "op" sym)
      (concat "op " (ats-smie-forward-token-1)))
     ((member sym '("|" "of" "="))
      ;; The important lexer for indentation's performance is the backward
      ;; lexer, so for the forward lexer we delegate to the backward one.
      (save-excursion (ats-smie-backward-token)))
     (t sym))))

(defun ats-smie-backward-token-1 ()
  (forward-comment (- (point)))
  (buffer-substring-no-properties
   (point)
   (progn
     (or (/= 0 (skip-syntax-backward ".'"))
	 (skip-syntax-backward "'w_"))
     (point))))

(defun ats-smie-backward-token ()
  (let ((sym (ats-smie-backward-token-1)))
    (unless (zerop (length sym))
      ;; FIXME: what should we do if `sym' = "op" ?
      (let ((point (point)))
	(if (equal "op" (ats-smie-backward-token-1))
	    (concat "op " sym)
	  (goto-char point)
	  (cond
	   ((string= sym "=") (if (ats-smie-definitional-equal-p) "d=" "="))
	   ((string= sym "of") (if (ats-smie-non-nested-of-p) "=of" "of"))
           ((string= sym "|") (if (ats-smie-datatype-|-p) "d|" "|"))
	   (t sym)))))))

;;; mode definitions

(define-derived-mode c/ats-mode c-mode "C/ATS"
  "Major mode to edit C code embedded in ATS code."
  (unless (local-variable-p 'compile-command)
    (set (make-local-variable 'compile-command)
         (let ((file buffer-file-name))
           (format "patscc -tcats %s" file)))
    (put 'compile-command 'permanent-local t))
  (setq indent-line-function 'c/ats-mode-indent-line))

(defun c/ats-mode-indent-line (&optional arg)
  (let (c-start c-end)
    (save-excursion
      (if (re-search-backward "%{[^$]?" 0 t)
          (setq c-start (match-end 0))
        (setq c-start 0)))
    (save-excursion
      (if (re-search-forward "%}" (point-max) t)
          (setq c-end (match-beginning 0))
        (setq c-start (point-max))))
    (save-restriction
      ;; restrict view of file to only the C code for the benefit of
      ;; the cc-mode indentation engine.
      (narrow-to-region c-start c-end)
      (c-indent-line arg))))

;;;###autoload
(define-derived-mode ats-mode fundamental-mode "ATS2"
  "Major mode to edit ATS2 source code."
  (set (make-local-variable 'font-lock-defaults)
       '(ats-font-lock-keywords nil nil ((?_ . "w") (?= . "_") ) nil
         (font-lock-syntactic-keywords . ats-font-lock-syntactic-keywords)
         (font-lock-mark-block-function . ats-font-lock-mark-block)))
  (setq-local comment-start "(*")
  (setq-local comment-continue  " *")
  (setq-local comment-end "*)")
  ;; (setq indent-line-function 'tab-to-tab-stop)
  (smie-setup ats-smie-grammar #'ats-smie-rules
              :backward-token #'ats-smie-backward-token
              :forward-token #'ats-smie-forward-token)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (setq tab-stop-list (loop for x from 2 upto 120 by 2 collect x))
  (setq indent-tabs-mode nil)
  (unless (local-variable-p 'compile-command)
    (set (make-local-variable 'compile-command)
         (let ((file buffer-file-name))
           (format "patscc -tcats %s" file)))
    (put 'compile-command 'permanent-local t))
  (local-set-key (kbd "C-c C-c") 'compile)
  (cond
   ;; Emacs 21
   ((and (< emacs-major-version 22)
         (not xemacsp))
    (pushnew '("\\(syntax error: \\)?\\([^\n:]*\\): \\[?[0-9]*(line=\\([0-9]*\\), offs=\\([0-9]*\\))\\]?" 2 3 4)
             compilation-error-regexp-alist))
   ;; Emacs 22+ has an improved compilation mode
   ((and (>= emacs-major-version 22)
         (not xemacsp))
    (pushnew '(ats "\\(syntax error: \\)?\\([^\n:]*\\): \\[?[0-9]*(line=\\([0-9]*\\), offs=\\([0-9]*\\))\\]?\\(?: -- [0-9]*(line=\\([0-9]*\\), offs=\\([0-9]*\\))\\)?" 2 (3 . 5) (4 . 6))
             compilation-error-regexp-alist-alist)
    (pushnew 'ats compilation-error-regexp-alist))
   ;; XEmacs has something different, to be contrary
   (xemacsp
    (pushnew '(ats ("\\(syntax error: \\)?\\([^\n:]*\\): \\[?[0-9]*(line=\\([0-9]*\\), offs=\\([0-9]*\\))\\]?" 2 3 4))
             compilation-error-regexp-alist-alist)
    (unless (eql 'all compilation-error-regexp-systems-list)
      (pushnew 'ats compilation-error-regexp-systems-list))
    (compilation-build-compilation-error-regexp-alist)
    (message "WARNING! XEMACS IS DEAD AND DEPRECATED."))))


(defun newline-and-indent-relative ()
  (interactive)
  (newline)
  (indent-to-column (save-excursion
                      (forward-line -1)
                      (back-to-indentation)
                      (current-column))))

;;;autoload
(add-to-list 'auto-mode-alist '("\\.\\(s\\|d\\|h\\)ats\\'" . ats-mode))

(provide 'ats-mode)
;;; end of [ats2-mode.el]
