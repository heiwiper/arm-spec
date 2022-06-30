(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cxml-klacks :silent t)
  (ql:quickload :xmls :silent t))

(defpackage :arm-questions
  (:use :common-lisp))

(in-package :arm-questions)

(defun skip-element (source &optional (depth 1))
  (loop
    for key = (klacks:peek source)
    do (if key
           (case key
             (:start-element
              (incf depth)
              (klacks:consume source))
             (:end-element
              (decf depth)
              (klacks:consume source)
              (when (= 0 depth)
                (klacks:consume source)
                (return-from skip-element)))
             (otherwise
              (klacks:consume source)))
           (error "unexpected eof"))))

(defun count-nodes (source)
  (loop
    for key = (klacks:find-element source "node")
    while key
    do (klacks:consume source)
    sum 1))

(defun count-toplevel-nodes (source)
  (loop
    for key = (klacks:find-element source "node")
    while key
    do (klacks:consume source)
       (skip-element source)
    sum 1))

(defun get-name (source)
  (or (klacks:get-attribute source "groupname")
      (klacks:get-attribute source "iclass")))

(defun display-toplevel-count (source)
  (format t "  node ~a has " (get-name source))
  (klacks:consume source)
  (destructuring-bind (count names) (loop
                                      with count = 0
                                      and depth = 1
                                      and names = nil
                                      for key = (klacks:peek source)
                                      do (if key
                                             (case key
                                               (:start-element
                                                (when (equal "node" (klacks:current-qname source))
                                                  (push (get-name source) names)
                                                  (incf depth)
                                                  (incf count))
                                                (klacks:consume source))
                                               (:end-element
                                                (when (equal "node" (klacks:current-qname source))
                                                  (decf depth))
                                                (klacks:consume source)
                                                (when (= 0 depth)
                                                  (return (list count names))))
                                               (otherwise
                                                (klacks:consume source)))
                                             (error "unexpected eof")))
    (format t "~d sub-nodes~%" count)
    (unless (endp names)
      (cond
        ((> (length names) 5)
         (format t "    ~{ ~a~} ...~%" (subseq names 0 5)))
        (t
         (format t "    ~{ ~a~}~%" names))))))

(defun display-toplevel-counts (source)
  (loop
    for key = (klacks:find-element source "node")
    while key
    do (display-toplevel-count source)))

(defun display-how-many-levels (source)
  (loop
    with max = 0
    and depth = 0
    for key = (klacks:peek source)
    do
       (if key
           (progn
             (case key
               (:start-element
                (when (equal "node" (klacks:current-qname source))
                  (incf depth)
                  (setf max (max depth max))))
               (:end-element
                (when (equal "node" (klacks:current-qname source))
                  (decf depth))))
             (klacks:consume source))
           (return max))))

(defun display-node-names (source)
  (klacks:consume source)
  (loop
    with depth = 1
    and parents = nil
    for key = (klacks:peek source)
    while key
    do
       (case key
         (:start-element
          (when (equal "node" (klacks:current-qname source))
            (format t "  ~{~a~^ -> ~}~%" (reverse (cons (get-name source) parents)))
            (incf depth)
            (push (get-name source) parents))
          (klacks:consume source))
         (:end-element
          (when (equal "node" (klacks:current-qname source))
            (decf depth)
            (pop parents))
          (klacks:consume source)
          (when (= 0 depth)
            (return)))
         (otherwise
          (klacks:consume source)))))

(defun goto-node (source name)
  (loop for key = (klacks:find-element source "node")
        while key
        do (if (equal name (get-name source))
               (return)
               (klacks:consume source))))

(defun goto-instructiontable (source name)
  (loop for key = (klacks:find-element source "instructiontable")
        while key
        do (if (equal name (get-name source))
               (return)
               (klacks:consume source))))

(defun find-element-within (source lname within)
  (loop
    (multiple-value-bind (key current-uri current-lname current-qname)
        (klacks:peek source)
      (cond
        ((null key)
         (return nil))
        ((and (eq key :start-element)
              (equal lname current-lname))
         (return
           (values key current-uri current-lname current-qname)))
        ((and (eq key :end-element)
              (equal within current-lname))
         (return nil)))
      (klacks:consume source))))

(defun display-instructiontable (source name)
  (goto-instructiontable source name)
  (klacks:find-element source "tbody")
  (klacks:consume source)
  (flet ((display-row ()
           (format t "  ")
           (loop
             (if (find-element-within source "td" "tr")
                 (let ((data (nth-value 1 (klacks:peek-next source))))
                   (if data
                       (format t "~a " data)
                       (format t "_ ")))
                 (return nil)))))
    (loop
      (if (find-element-within source "tr" "tbody")
          (progn
            (let ((encname (klacks:get-attribute source "encname")))
              (klacks:consume source)
              (display-row)
              (format t "(~a)~%" encname)))
          (return nil)))))

(defvar *encoding-index-path* (probe-file "~/.cache/encodingindex.xml"))

(klacks:with-open-source (source (cxml:make-source *encoding-index-path*))
  (format t "How many nodes are there? ~d~%" (count-nodes source)))

(terpri)

(klacks:with-open-source (source (cxml:make-source *encoding-index-path*))
  (format t "How many levels are there? ~d~%" (display-how-many-levels source)))

(terpri)

(klacks:with-open-source (source (cxml:make-source *encoding-index-path*))
  (format t "How many top-level nodes are there? ~d~%" (count-toplevel-nodes source)))

(terpri)

(klacks:with-open-source (source (cxml:make-source *encoding-index-path*))
  (format t "For every top-level node, what is its name and how many sub-nodes does it have?~%")
  (display-toplevel-counts source))

(terpri)

(klacks:with-open-source (source (cxml:make-source *encoding-index-path*))
  (format t "What is every node name and depth?~%")
  (display-node-names source))

(terpri)

(klacks:with-open-source (source (cxml:make-source *encoding-index-path*))
  (format t "What does the instruction table for \"movewide\" look like?~%")
  (display-instructiontable source "movewide"))
