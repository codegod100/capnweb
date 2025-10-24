;;;; basic-example.lisp --- simple ParenScript program using the capnweb wrapper

(in-package #:capnweb.examples)

(defparameter *default-output* #p"basic-example.js"
  "Default pathname used when emitting the basic example module.")

(defun write-basic-example-file (&key (output *default-output*)
                                      (module-name capnweb:*default-module-name*)
                                      (binding 'capnweb)
                                      (if-exists :supersede))
  "Write the ParenScript-generated demonstration module to disk and return its pathname."
  (let* ((pathname (%coerce-output-path output))
         (content (capnweb:generate-basic-example-js
                   :module-name module-name
                   :binding binding)))
    (%write-output-file pathname content if-exists)))
