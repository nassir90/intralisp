(defpackage :intralisp
  (:use :cl)
  (:export :intralisp-parse-stream))

(in-package :intralisp)

(defvar start-statement "[%")
(defvar end-statement "%]")
(defvar start-literal "[@")
(defvar end-literal "@]")

(defvar loaded-files nil)

(defun print-literal (stream literal)
  (unless (= 0 (length literal))
    (format T "~s~%" `(format ,(intern stream) "~a" ,literal))))

(defun parse-statement (stream rest &key (accumulated-length 0) (accumulated-string "") (terminator ")"))
  (cond ((equal start-literal (subseq rest 0 (length start-literal)))
         (format t "~a~%" accumulated-string)
         (multiple-value-bind (start-delimiter-length implicit-predicate literal-terminator stream)
             (cond ((equal (aref rest (length start-literal)) #\^) (values (+ 1 (length start-literal)) "(WITH-OUTPUT-TO-STRING (OUTPUT)" ")" "OUTPUT"))
                   (t (values (length start-literal) "" "" "T")))
           (format t implicit-predicate)
           (let* ((literal-length (+ start-delimiter-length (length end-literal) (parse-literal stream (subseq rest start-delimiter-length)))))
             (format t literal-terminator)
             (parse-statement
              stream
              (subseq rest literal-length)
              :terminator terminator
              :accumulated-length (+ accumulated-length literal-length (length accumulated-string))))))
        ((equal end-statement (subseq rest 0 (length end-statement)))
         (format t "~a~a~%" accumulated-string terminator)
         (+ accumulated-length (length accumulated-string)))
        (t
         (parse-statement
          stream
          (subseq rest 1)
          :terminator terminator
          :accumulated-length accumulated-length
          :accumulated-string (concatenate 'string accumulated-string (subseq rest 0 1))))))

(defun parse-literal (stream rest &key (accumulated-length 0) (accumulated-string ""))
  (cond ((and (<= (1+ (length start-literal)) (length rest))
              (equal start-statement (subseq rest 0 (length start-statement)))
              (equal #\! (aref rest (length start-statement))))
         (print-literal stream accumulated-string)
         (let* ((after-first-quote (1+ (position #\" rest)))
                (second-quote (position #\" rest :start after-first-quote))
                (end-of-section (+ (length end-statement) (search end-statement rest)))
                (file-to-load (truename (subseq rest after-first-quote second-quote))))
           (unless (member file-to-load loaded-files :test #'equal)
             (with-open-file (stream file-to-load)
               (parse-stream stream))
             (push file-to-load loaded-files)
             (format t "(in-package :cl-user)~%"))
           (parse-literal
            stream
            (subseq rest end-of-section)
            :accumulated-length (+ accumulated-length end-of-section))))
        ((and (<= (length start-literal) (length rest))
             (equal start-statement (subseq rest 0 (length start-statement))))
         (print-literal stream accumulated-string)
         (multiple-value-bind (start-delimiter-length
                               implicit-predicate
                               terminator)
             (cond ((equal "=~" (subseq rest (length start-statement) (+ 2 (length start-statement))))
                    (values 4 (concatenate 'string "(FORMAT " stream " ") ")"))
                   ((equal #\~ (aref rest (length start-statement)))
                    (values (+ 1 (length start-statement)) (concatenate 'string "(FORMAT " stream " \"~a\" (") "))"))
                   ((equal #\= (aref rest (length start-statement)))
                    (values (+ 1 (length start-statement)) (concatenate 'string "(FORMAT " stream  " \"~a\" ") ")"))
                   (t (values (length start-statement) "(" ")")))
           (format t "~a" implicit-predicate)
           (let ((statement-length (+ start-delimiter-length
                                      (parse-statement stream
                                                       (subseq rest start-delimiter-length)
                                                       :terminator terminator)
                                      (length end-statement))))
             (parse-literal
              stream
              (subseq rest statement-length)
              :accumulated-length (+ accumulated-length statement-length (length accumulated-string))))))
        ((or (= 0 (length rest))
             (and (<= (length end-literal) (length rest))
                  (equal end-literal (subseq rest 0 (length end-literal)))))
         (print-literal stream accumulated-string)
         (+ accumulated-length (length accumulated-string)))
        (t
         (parse-literal
          stream
          (subseq rest 1)
          :accumulated-length accumulated-length
          :accumulated-string (concatenate 'string accumulated-string (subseq rest 0 1))))))

(defun parse-stream (stream)
  (parse-literal
   "T"
   (reduce (lambda (a b)
             (concatenate 'string a b))
           (loop
             with buffer = (make-string 4096)
             for bytes-read = (read-sequence
                               buffer
                               stream)
             while (not (= 0 bytes-read))
             collecting (subseq buffer 0 bytes-read)))))

(parse-stream *standard-input*)
