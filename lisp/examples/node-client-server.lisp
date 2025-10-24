;;;; node-client-server.lisp --- generate Node client/server samples for capnweb

(in-package #:capnweb.examples)

(defparameter *default-client-output* #p"node-client.mjs"
  "Default pathname for the generated Node client module.")

(defparameter *default-server-output* #p"node-server.mjs"
  "Default pathname for the generated Node server module.")

(defun write-node-client-file (&key (output *default-client-output*)
                                    (module-name capnweb:*default-module-name*)
                                    (binding 'capnweb)
                                    (if-exists :supersede))
  "Emit a Node-friendly capnweb client module and return its pathname."
  (let* ((pathname (%coerce-output-path output))
         (content (capnweb:generate-node-client-js
                   :module-name module-name
                   :binding binding)))
    (%write-output-file pathname content if-exists)))

(defun write-node-server-file (&key (output *default-server-output*)
                                    (module-name capnweb:*default-module-name*)
                                    (binding 'capnweb)
                                    (if-exists :supersede))
  "Emit a Node-friendly capnweb server module and return its pathname."
  (let* ((pathname (%coerce-output-path output))
         (content (capnweb:generate-node-server-js
                   :module-name module-name
                   :binding binding)))
    (%write-output-file pathname content if-exists)))

(defun write-node-client-and-server-files (&key (client-output *default-client-output*)
                                                (server-output *default-server-output*)
                                                (module-name capnweb:*default-module-name*)
                                                (binding 'capnweb)
                                                (if-exists :supersede))
  "Generate both client and server modules, returning their pathnames as two values."
  (values (write-node-server-file :output server-output
                                  :module-name module-name
                                  :binding binding
                                  :if-exists if-exists)
          (write-node-client-file :output client-output
                                  :module-name module-name
                                  :binding binding
                                  :if-exists if-exists)))
