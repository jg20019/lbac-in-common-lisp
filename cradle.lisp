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
  (cond ((not (is-alpha look)) (expected "name"))
        (t (values look (get-char)))))

(defun get-num (look) 
  "Get a number." 
  (cond ((not (is-digit look)) (expected "integer"))
        (t (values look (get-char)))))

(defun emit (string)
  "Output a string with a tab." 
  (format t "~a~a" #\tab string))

(defun emitLn (string) 
  "Out a string with a tab and a newline." 
  (format t "~a~a~%" #\tab string))

(defun init ()
  "Initialize."
  (with-input-from-string (*standard-input* (read-line))
    (get-char)))

(defun main () 
  "Main program" 
  (init))

(main)

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
