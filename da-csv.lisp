;;;; da-csv.lisp

(in-package #:da-csv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 1. Define dynamic variables that control delimited file operation
;;;    and defaults for functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *delimiter-char* #\,
  "The delimiting character")
(defparameter *quote-char* #\"
  "The character which designates the quoting character parsing delimited files")
(defparameter *newline-char* #\Newline
  "The character that defines a new line when parsing csv files")

