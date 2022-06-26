(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cxml-klacks)
  (ql:quickload :xmls))

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

(defvar *encoding-index-path* (probe-file "~/.cache/encodingindex.xml"))

(klacks:with-open-source (source (cxml:make-source *encoding-index-path*))
  (format t "How many nodes are there? ~d~%" (count-nodes source)))

(klacks:with-open-source (source (cxml:make-source *encoding-index-path*))
  (format t "How many top-level nodes are there? ~d~%" (count-toplevel-nodes source)))

(klacks:with-open-source (source (cxml:make-source *encoding-index-path*))
  (format t "For every top-level node, what is its name and how many sub-nodes does it have?~%")
  (display-toplevel-counts source))
