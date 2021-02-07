; Function to check whether number is prime or not
(defun prime (n) 
  (defvar flag) 
  (setf flag 1) 
  (loop for i from 1 to (floor (sqrt n)) 
    do  (if (not (= i 1) ) 
          (if (eq (rem n i) 0) 
           (setf flag 0) 
          ) 
        ) 
  ) 
  (if (eq flag 1) (return-from prime t) (return-from prime nil)))

; Function to check whether number is semiprime or not
(defun semip (n)
    (setf k 1)
    (setf cnt 0)
    (loop for i from 2 to (+ (isqrt n) 1)
        do (loop while(= (rem n i) 0) do
              (progn
                (setf n (/ n i))
                (setf cnt (+ cnt 1))
              )
        )
    )
    (if(> n 1)
        (setf cnt (+ cnt 1))
    )
    (eq cnt 2)
) 

; Function to read boundries from file
(defun primecrawler (name)
  (setq str1(with-open-file (stream name)
    (let ((fl_cnt (make-string (file-length stream))))
      (read-sequence fl_cnt stream)
      fl_cnt)))
  (setq bound1 (car (parse-and-get str1)))
  (setq bound2 (cadr (parse-and-get str1)))
)

; Function to parse string and get number
(defun parse-and-get (str)
  (with-input-from-string (s str)
    (loop
      :for n := (read s nil)
      :while n
      :collect n)))

; Function to find prime and semiprime numbers between given range and write them to the output file
(defun find-numbers-and-write()
  (with-open-file 
   (stream "primedistribution.txt" ; Output file
       :direction :output    
       :if-exists :overwrite ; Overwrite if exists
       :if-does-not-exist :create) ; Create file if doesn't exists
  (primecrawler "boundries.txt") ; Get boundries
  (loop for n from bound1 to (+ bound2 1) ; Check number and write to file if it is prime or semiprime
    do(if(prime n)
          (format stream "~A~%" (concatenate 'string (write-to-string n) " is Prime"))
    )
    do(if (semip n)
          (format stream "~A~%" (concatenate 'string (write-to-string n) " is Semi-prime"))
    )
  )))

(find-numbers-and-write)