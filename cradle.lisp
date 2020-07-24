(in-package #:lbac)

(defun get-char (&optional (s *standard-input*)) 
  "Read and return a single char form *standard-input*
   When end of file is reached return nil."
  (read-char s nil))

(defun print-error (string)
  (format t "~%Error: ~a.~%" string))

(defun abort* (string)
  "Report an error and enter debugger."
  (print-error string)
  (break))

(defun expected (string)
  "Report what was expected."
  (abort* (format nil "~a expected." string)))

(defun match (look ch) 
  "Match an expected input character"
  (if (char= look ch) 
    (get-char)
    (expected (format nil "'~a'" ch))))

(defun is-alpha (ch)
  "Recognize an alpha character."
  (alpha-char-p ch))

(defun is-digit (ch) 
  "Recognize a decimal digit."
  (digit-char-p ch))

(defun get-name (look) 
  "Get a name." 
  (cond ((or (null look) (not (is-alpha look))) (expected "name"))
        (t (values look (get-char)))))

(defun get-num (look) 
  "Get a number." 
  (cond ((or (null look) (not (is-digit look))) (expected "integer"))
        (t (values look (get-char)))))

(defun emit (string)
  "Output a string with a tab." 
  (format t "~a~a" #\tab string))

(defun emitLn (string) 
  "Out a string with a tab and a newline." 
  (format t "~a~a~%" #\tab string))

(defvar lparen #\()
(defvar rparen #\))

(defun factor (look) 
  "Parse and translate a factor." 
  (cond ((char= lparen look) 
         (match (expression (match look lparen)) rparen))
        (t (multiple-value-bind (num l) (get-num look)
             (emitLn (format nil "MOVE #~a, D0" num))
             l))))

(defun multiply (look) 
  "Recognize and translate a multiply." 
  (let ((look (factor (match look #\*))))
    (emitLn "MULS (SP)+, D0")
    look))

(defun divide (look) 
  "Recognize and translate a divide." 
  (let ((look (factor (match look #\/))))
    (emitLn "MOVE (SP)+, D1")
    (emitLn "DIVS D1, D0")
    look))

(defun mul-op-p (look) 
  "Returns true if look is a mul-op." 
  (and (not (null look))
       (or (char= look #\*) (char= look #\/))))

(defun mul-op (look) 
  (emitLn "MOVE D0, -(SP)") 
  (cond ((char= look #\*) (multiply look))
        ((char= look #\/) (divide look))
        (t (expected "mulop"))))

(defun mul-op* (look)
  (if (mul-op-p look)
    (mul-op* (mul-op look))
    look))

(defun term (look)
  "Parse and translate a math term." 
  (mul-op* (factor look)))

(defun add (look) 
  "Recognize and trnalate an Add." 
  (let ((look (term (match look #\+))))
    (emitLn "ADD (SP)+, D0") 
    look))

(defun subtract (look) 
  "Recognize and translate a subtract." 
  (let ((look (term (match look #\-))))
    (emitLn "SUB (SP) D0") 
    (emitLn "NEG D0")
    look))

(defun add-op-p (look) 
  "Returns true if char is an addition operator." 
  (and (not (null look))
       (or (char= look #\+) 
           (char= look #\-))))

(defun add-op (look) 
  (emitLn "MOVE D0, -(SP)")
  (cond ((char= look #\+) (add look))
        ((char= look #\-) (subtract look))
        (t (expected "addop"))))

(defun add-op* (look) 
  (if (add-op-p look) 
    (add-op* (add-op look))
    look))

(defun expression (look) 
  "Recognize and translate an expression."  
  (cond ((add-op-p look)
         (emitLn "CLR D0") 
         (add-op* look))
        (t (add-op* (term look)))))

(defun init ()
  "Initialize."
  (get-char))

(defun main (&optional (input nil))
  "Main program" 
  (with-input-from-string (*standard-input* (or input (read-line)))
    (let ((look (init)))
      (expression look))))

;; Notes: 
;; make-string-input-stream 
;; (make-string-input-stream "1 + 1") ; returns a stream with the contents "1 + 1"

;; with-input-from-string
;; (with-input-from-stream (*standard-input* "1 + 1")
;;   ...) 
;; *standard-input* will have the values of  "1 + 1" 
;; *standard-input* is being set with with-input-from-string in the init function 
;; so that every function that reads from standard-input in that context is reading 
;; the line that was typed. 
