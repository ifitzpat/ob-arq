;;; ob-arq.el --- org-babel functions for evaluating sparql queries on org source blocks

;; Copyright (C) Dr. Ian FitzPatrick

;; Author: Dr. Ian FitzPatrick
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; call arq on org source block by name
;;

;;; Requirements:


;;; Code:
(require 'ob)
;(require 'ob-ref)
;(require 'ob-comint)
;(require 'ob-eval)
(require 'sparql-mode)

;; possibly require modes required for your language
(define-derived-mode arq-mode sparql-mode "arq-build"
  "Major mode for building docker containers from org-babel."
  )


;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:arq '((:source . nil)(:format "CSV")))

;; This function expands the body of a source code block by doing
;; things like prepending argument definitions to the body, it should
;; be called by the `org-babel-execute:arq' function below.
(defun org-babel-expand-body:arq (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  body ; TODO translate params to yaml variables
)

;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;
;; The most common first step in this function is the expansion of the
;; PARAMS argument using `org-babel-process-params'.
;;
;; Please feel free to not implement options which aren't appropriate
;; for your language (e.g. not all languages support interactive
;; "session" evaluation).  Also you are free to define any new header
;; arguments which you feel may be useful -- all header arguments
;; specified by the user will be available in the PARAMS variable.
(defun org-babel-execute:arq (body params)
  "Execute a block of arq code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((vars (org-babel--get-vars params))
	 (source (if (assoc :source params) (cdr (assoc :source params)) nil))
	 (format (if (assoc :format params) (cdr (assoc :format params)) "csv"))
	 )

    (message (concat "executing arq on source block named " source))
    (org-babel-eval-arq source body format)
    )
  ;; when forming a shell command, or a fragment of code in some
  ;; other language, please preprocess any file names involved with
  ;; the function `org-babel-process-file-name'. (See the way that
  ;; function is used in the language files)
  )

(defun ob-arq-src-content (name)
       (save-excursion
         (org-babel-goto-named-src-block name)
         (org-element-property :value (org-element-at-point))))


(defun org-babel-eval-arq (source body format)
  "Run CMD on BODY.
If CMD succeeds then return its results, otherwise display
STDERR with `org-babel-eval-error-notify'."
  (let ((err-buff (get-buffer-create " *Org-Babel Error*"))
	(query-file (org-babel-temp-file "ob-arq-query-" ".sparql"))
	(source-file (org-babel-temp-file "ob-arq-source-" ".ttl"))
	(data (ob-arq-src-content source))
	(output-file (org-babel-temp-file "ob-arq-out-"))
	exit-code)
    (with-temp-file query-file (insert body))
    (with-temp-file source-file (insert data))
    (with-current-buffer err-buff (erase-buffer))
	(org-babel-script-escape (string-replace "\r" "" (shell-command-to-string (concat "arq --query " query-file " --data " source-file " --results=" format)) ) )
	  ))


(provide 'ob-arq)
;;; ob-arq.el ends here
