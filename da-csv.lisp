;;;; da-csv.lisp

(in-package #:da-csv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 1. Define dynamic variables that control delimited file operation
;;;    and defaults for functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *delimiter* #\,
  "The delimiting character")
(defvar *quote-char* #\"
  "The character which designates the quoting character parsing delimited files")
(defvar *newline-char* #\Newline
  "The character that defines a new line when parsing csv files")
(defvar *read-buffer-size* 1048576
  "The size of the buffer arrays used by da-csv (in bytes) to slurp/buffer delimited files")

(defclass delimited-object ()
  ((delimiter
    :initarg :delimiter
    :initform *delimiter*
    :type 'character
    :accessor delimiter
    :documentation "The character that separates items in the delimited file")
   (quote-char
    :initarg :quote-char
    :initform *quote-char*
    :type 'character
    :accessor quote-char
    :documentation "A character that is typically used in csv-style files to designate that what follows is a literal string/value")
   (newline-char
    :initarg :newline-char
    :initform *newline-char*
    :type 'character
    :accessor newline-char
    :documentation "A character that defines a new line when parsing delimited files")
   (read-buffer-size
    :initarg :read-buffer-size
    :initform *read-buffer-size*
    :type 'fixnum
    :accessor read-buffer-size
    :documentation "Dimensions of the buffer used to slurp chunks of the file for processing")
   (read-buffer
    :initarg :read-buffer
    :initform (make-string *read-buffer-size*)
    :type 'string
    :accessor read-buffer
    :documentation "The buffer used to recieve slurped chunks of the delimited file for processing")
   (position-in-buffer
    :initarg :position-in-buffer
    :initform 0
    :type 'fixnum
    :accessor position-in-buffer
    :documentation "The buffer used to recieve slurped chunks of the delimited file for processing")
   (buffer-end
    :initarg :buffer-end
    :initform *read-buffer-size*
    :type 'fixnum
    :accessor buffer-end
    :documentation "If the buffer is not completely filled, this variable holds the first non-valid buffer index")
   (file-path
    :initarg :file-path
    :initform nil
    :accessor file-path
    :documentation "The path to the delimited file on disk")
   (file-size
    :initarg :file-size
    :initform 0
    :type 'fixnum
    :accessor file-size
    :documentation "The size of the delimited file")
   (delimiter-stream
    :initarg :delimiter-stream
    :initform nil
    :accessor delimiter-stream
    :documentation "A holding slot for any stream that may be needed from the delimited file")
   (position-in-file
    :initarg :position-in-file
    :initform 0
    :type 'fixnum
    :documentation "The concept of the position of the stream within the delimted file")
   (num-rows
    :initarg :rows
    :initform nil
    :accessor num-rows
    :documentation "If the number of rows/records in a csv is known/established, this slot holds that number")
   (num-cols
    :initarg :buffer-end
    :initform nil
    :accessor num-cols
    :documentation "If the number of columns in a csv is known/established, this slot holds that number")
   (first-row-variables-p
    :initarg :first-row-variables-p
    :initform nil
    :accessor first-row-variables-p
    :documentation "A predicate that determines whether the first row in the delimited file represents variable names")
   (first-row-variables
    :initarg :first-row-variables
    :initform (make-array 0 :adjustable t :fill-pointer 0)
    :accessor first-row-variables
    :documentation "If we need a specific place to hold a row of variable names, this slot will be that place.  Otherwise it is an empty, yet extendable array.")
   (confirmed-rectangular
    :initarg :confirmed-rectangular
    :initform nil
    :accessor confirmed-rectangular
    :documentation "If the delimited file is believed to be rectangular in nature, that is to say, the same number of rows and columns, we can set this slot to T to indicate that fact.")
   (confirmed-irregular
    :initarg :confirmed-irregular
    :initform nil
    :accessor confirmed-irregular
    :documentation "If the delimited file is believed to be irregular in nature, that is to say, that each row does not contain a similar number of variables, we can set this slot to T to indicate that fact.")))

(defun make-parse-state ()
  "Creates an array representing the state of a csv parsing operation
   Values are binary, and Indexes are equivalent to
   0: in field
   1: in row
   2: saw-quote
   4: file-position"
  
  (make-array 4 :element-type 'fixnum))

;;twiddle a array element between the two binary values of 1 and 0
(defun aref-twiddle (state-array index)
  (setf (aref array index)
	(if (= (aref array index) 0)
	    1
	    0)))

(defun field-twiddle (state-array)
  (aref-twiddle state-array 0))

(defun row-twiddle (state-array)
  (aref-twiddle state-array 1))

(defun saw-quote-twiddle (state-array)
  (aref-twiddle state-array 2))

(defun incf-file-pos (state-array)
  (incf (aref state-array 3)))

(defun in-field? (state-array)
 (if  (= (aref state-array 0) 1)  
      T
      NIL))

(defun in-row? (state-array)
  (if (= (aref state-array 1) 1)
      T
      NIL))

(defun saw-quote? (state-array)
  (if (= (aref state-array 2) 1)
      T
      NIL))

;;Set an array element to 0 

(defmacro aref-0 (array index)
  `(setf (aref ,array ,index) 0))
    
(defun view-char-update-state (state-array char &optional
					(separator *delimiter*)
					(quote-char *quote-char*)
					(newline-char *newline-char*))

  ;; If we are in a state where we're in nothing and have seen nothing
  
  (if (and (null (in-field? state-array))
	   (null (in-row? state-array))
	   (null (saw-quote? state-array)))
  
            ;; Not in anything, seen nothing - see quote
      (cond ((eql char quote-char)
	     ())
	    

	    ;; Not in anything, seen nothing - see newline-char	     
	    ;; I don't think that one should happen, but lets take care of it anyway.
	    ((eql char newline-char)
	     ())

	    ;; Not in anything, seen nothing - see separator
	    ((eql char separator)
	     ())

	    ;; Not in anything, seen nothing - see non-special-character
	    (t
	     ())))

  ;; If we are in a state where we're in a field
  ;; (and must implicitly therefore also be in a row)
  ;; and have not seen a quote

  (if (and (in-field? state-array)
	   (null (saw-quote? state-array)))
      (cond (()())))
  
  ;; If we are in a state where we're in a field
  ;; and we have seen a quote

  (if (and (in-field? state-array)
	   (saw-quote? state-array))
      (cond (()())))

  ;; If we are in a state where we're not in a field, but we are in a row
  ;; and we haven't seen a quote.

  (if (and (null (in-field? state-array))
	   (in-row? state-array)
	   (null (saw-quote? state-array)))
      (cond (()())))

  
  ;; If we are in a state where we're not in a field, but we are in a row
  ;; and we have seen a quote.

  (if (and (null (in-field? state-array))
	   (in-row? state-array)
	   (saw-quote? state-array))
      (cond (()())))

		     
  ;;We've just seen a character, so we should increment the state
  ;;of where we are in a delimited file/
  (incf (aref state-array 3))
  

					
  )

