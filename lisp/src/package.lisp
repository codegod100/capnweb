;;;; package.lisp --- package definitions for the capnweb Lisp wrapper

(defpackage #:capnweb.parenscript
  (:use #:cl #:parenscript)
  (:export #:new-websocket-session
           #:new-http-batch-session
           #:new-messageport-session
           #:new-rpc-session
           #:rpc-target
           #:rpc-stub
           #:rpc-main
           #:rpc-session-stats
           #:rpc-session-drain
           #:serialize
           #:deserialize))

(defpackage #:capnweb
  (:use #:cl)
  (:import-from #:parenscript #:ps)
  (:export #:*default-module-name*
           #:generate-basic-example-js
           #:generate-node-client-js
           #:generate-node-server-js))

(defpackage #:capnweb.examples
  (:use #:cl)
  (:import-from #:capnweb
                #:generate-basic-example-js
                #:generate-node-client-js
                #:generate-node-server-js
                #:*default-module-name*)
  (:export #:write-basic-example-file
           #:write-node-client-file
           #:write-node-server-file
           #:write-node-client-and-server-files))
