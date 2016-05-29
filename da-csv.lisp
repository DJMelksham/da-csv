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

;;; The csv finite state machine is the base level parser of csv files.
;;; It doesn't do anything except take in a character and transition
;;; states. The decision of what to do when states transition is
;;; left to a higher level function.

;;; Believe it or not, there are 6 possible parsing states
;;; the csv parser can be in. They are enumerated 0 through 5.
;;;
;;; They are:
;;;
;;; 0: Beginning of row state: beginning of a file, or the state entered after
;;;    seeing a newline at the end of the row.
;;; 1: Basic character: saw a vanilla character and we weren't in a quote.
;;;    Expected course of action if we're monitoring this is to accept that
;;;    character as valid output for a field.
;;; 2: Between fields: seen in the case of seeing a significant delimiter
;;;    character.  Don't want to output it, but its a signal that a field has ended.
;;; 3: Quote at beginning of field: What the name says.  We can transition to state
;;;    3 only from state 0 or 2.
;;; 4: Non-quote character seen while in quote field.  Very similar behaviour to state 1
;;;    except even more liberal - expected course of action is to accept that character
;;;    as a valid character for a field.
;;; 5: Seeing a quote within a quote field:  This means either that we're at the end
;;;    of the field, or that the next character will be a quote.
;;;
;;; The possible transitions between the states are labelled N, D, Q, and E.
;;;
;;;    N - See newline character
;;;    D - See delimiter character
;;;    Q - See quote character
;;;    E - Everything else
;;;
;;; The mapping of states and transitions is thus as follows:
;;; If a transition is not included for a state, it is because
;;; this is an undefined/erroneous transition for that particular
;;; state if we are dealing with a parsable csv file.
;;;
;;; 0: N -> 0, E -> 1, D -> 2, Q -> 3, 
;;; 1: N -> 0, E -> 1, D -> 2
;;; 2: N -> 0, E -> 1, D -> 2, Q -> 3
;;; 3: N -> 4, E -> 4, D -> 4, Q -> 5
;;; 4: N -> 4, E -> 4, D -> 4, Q -> 5
;;; 5: N -> 0,         D -> 2, Q -> 4
;;;

(declaim inline csv-fs-machine)

(defun csv-fs-machine (state seen delimiter newline quote)
  "CSV Finite State Machine - reads chars and returns transitioned states"
  (declare (character seen delimiter newline quote)
	   (fixnum state)
	   (optimize (speed 3)))
  
  (cond ((eql state 1)
	 (cond ((char= seen delimiter) 2)
	       ((char= seen newline) 0)
	       (t 1)))
	((eql state 2)
	 (cond ((char= seen delimiter) 2)
	       ((char= seen newline) 0)
	       ((char= seen quote) 3)
	       (t 1)))
	((eql state 0)
	 (cond ((char= seen delimiter) 2)
	       ((char= seen newline) 0)
	       ((char= seen quote) 3)
	       (t 1)))
	((eql state 3)
	 (cond ((char= seen quote) 5)
	       (t 4)))
	((eql state 4)
	 (cond ((char= seen quote) 5)
	       (t 4)))
	;;If we get to here, we must be in state 5, so don't have to check for it.
	(t
	 (cond ((char= seen quote) 4)
	       ((char= seen delimiter) 2)
	       (t 0))))) ; must be a newline, because other characters would be invalid.

