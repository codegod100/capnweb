;;;; capnweb.asd --- Common Lisp / ParenScript bridge for the capnweb client

(asdf:defsystem "capnweb"
  :description "ParenScript helpers that target the capnweb JavaScript RPC client."
  :author "Cloudflare"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:parenscript)
  :serial t
  :components ((:file "src/package")
               (:file "src/parenscript-wrapper")
               (:file "src/common")))

(asdf:defsystem "capnweb/examples"
  :description "Example ParenScript programs that exercise the capnweb wrapper."
  :author "Cloudflare"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("capnweb")
  :serial t
  :components ((:file "examples/utils")
               (:file "examples/basic-example")
               (:file "examples/node-client-server")))
