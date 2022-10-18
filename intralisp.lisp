(defvar start-statement "[%")
(defvar end-statement "%]")
(defvar start-literal "[@")
(defvar end-literal "@]")

(defvar loaded-files nil)

(defun try-fill-buffer (buffer)
  (let* ((bytes-read (read-sequence buffer *standard-input*)))
    (subseq buffer 0 bytes-read)))

(defun intralisp-print-literal (literal)
  (unless (= 0 (length literal))
    (format t "~s~%" `(format t "~a" ,literal))))

(defun intralisp-parse-statement (rest &key (accumulated-length 0) (accumulated-string "") (terminator ")"))
  (cond ((equal start-literal (subseq rest 0 (length start-literal)))
         (format t "~a~%" accumulated-string)
         (let ((literal-length (+ 4 (intralisp-parse-literal (subseq rest (length start-literal))))))
           (intralisp-parse-statement
            (subseq rest literal-length)
            :terminator terminator
            :accumulated-length (+ accumulated-length literal-length (length accumulated-string)))))
        ((equal end-statement (subseq rest 0 (length end-statement)))
         (format t "~a~a~%" accumulated-string terminator)
         (+ accumulated-length (length accumulated-string)))
        (t
         (intralisp-parse-statement
          (subseq rest 1)
          :terminator terminator
          :accumulated-length accumulated-length
          :accumulated-string (concatenate 'string accumulated-string (subseq rest 0 1))))))

(defun intralisp-parse-literal (rest &key (accumulated-length 0) (accumulated-string "") )
  (cond ((and (<= (1+ (length start-literal)) (length rest))
              (equal start-statement (subseq rest 0 (length start-statement)))
              (equal #\! (aref rest (length start-statement))))
         (intralisp-print-literal accumulated-string)
         (let* ((after-first-quote (1+ (position #\" rest)))
                (second-quote (position #\" rest :start after-first-quote))
                (end-of-section (+ (length end-statement) (search end-statement rest)))
                (file-to-load (truename (subseq rest after-first-quote second-quote))))
           (unless (member file-to-load loaded-files :test #'equal)
             (with-open-file (stream file-to-load)
               (intralisp-parse-stream stream))
             (push file-to-load loaded-files)
             (format t "(in-package :cl-user)~%"))
           (intralisp-parse-literal
            (subseq rest end-of-section)
            :accumulated-length (+ accumulated-length end-of-section))))
        ((and (<= (length start-literal) (length rest))
             (equal start-statement (subseq rest 0 (length start-statement))))
         (intralisp-print-literal accumulated-string)
         (multiple-value-bind (start-delimiter-length
                               implicit-predicate
                               terminator)
             (cond ((equal "=~" (subseq rest (length start-statement) (+ 2 (length start-statement)))) (values 4 "(FORMAT T " ")"))
                   ((equal #\~ (aref rest (length start-statement))) (values (+ 1 (length start-statement)) "(FORMAT T \"~a\" (" "))"))
                   ((equal #\= (aref rest (length start-statement))) (values (+ 1 (length start-statement)) "(FORMAT T \"~a\" " ")"))
                   (t (values (length start-statement) "(" ")")))
           (format t "~a" implicit-predicate)
           (let ((statement-length (+ start-delimiter-length
                                      (intralisp-parse-statement (subseq rest start-delimiter-length)
                                                                 :terminator terminator)
                                      (length end-statement))))
             (intralisp-parse-literal
              (subseq rest statement-length)
              :accumulated-length (+ accumulated-length statement-length (length accumulated-string))))))
        ((or (= 0 (length rest))
             (and (<= (length end-literal) (length rest))
                  (equal end-literal (subseq rest 0 (length end-literal)))))
         (intralisp-print-literal accumulated-string)
         (+ accumulated-length (length accumulated-string)))
        (t
         (intralisp-parse-literal
          (subseq rest 1)
          :accumulated-length accumulated-length
          :accumulated-string (concatenate 'string accumulated-string (subseq rest 0 1))))))

(defun intralisp-parse-stream (stream)
  (intralisp-parse-literal
   (reduce (lambda (a b)
             (concatenate 'string a b))
           (loop
             with buffer = (make-string 4096)
             for bytes-read = (read-sequence
                               buffer
                               stream)
             while (not (= 0 bytes-read))
             collecting (subseq buffer 0 bytes-read)))))

(intralisp-parse-stream *standard-input*)
