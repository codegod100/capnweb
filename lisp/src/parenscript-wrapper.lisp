;;;; parenscript-wrapper.lisp --- ParenScript macros for the capnweb API

(in-package #:capnweb.parenscript)

(defparameter *capnweb-binding*
  'capnweb
  "Symbol that ParenScript macros treat as the JavaScript binding for the capnweb module.")

(defmacro %binding ()
  *capnweb-binding*)

(ps:defpsmacro new-websocket-session (websocket &rest args)
  "Expand to a call to capnweb.newWebSocketRpcSession."
  `(ps:chain ,(%binding) (newWebSocketRpcSession ,websocket ,@args)))

(ps:defpsmacro new-http-batch-session (url-or-request &rest args)
  "Expand to a call to capnweb.newHttpBatchRpcSession."
  `(ps:chain ,(%binding) (newHttpBatchRpcSession ,url-or-request ,@args)))

(ps:defpsmacro new-messageport-session (port &rest args)
  "Expand to a call to capnweb.newMessagePortRpcSession."
  `(ps:chain ,(%binding) (newMessagePortRpcSession ,port ,@args)))

(ps:defpsmacro new-rpc-session (transport &rest args)
  "Expand to `new capnweb.RpcSession(...)`."
  `(ps:new (ps:@ ,(%binding) RpcSession) ,transport ,@args))

(ps:defpsmacro rpc-target (&rest args)
  "Expand to `new capnweb.RpcTarget(...)`."
  `(ps:new (ps:@ ,(%binding) RpcTarget) ,@args))

(ps:defpsmacro rpc-stub (value)
  "Expand to `new capnweb.RpcStub(value)`."
  `(ps:new (ps:@ ,(%binding) RpcStub) ,value))

(ps:defpsmacro rpc-main (session)
  "Expand to `session.getRemoteMain()`."
  `(ps:chain ,session (getRemoteMain)))

(ps:defpsmacro rpc-session-stats (session)
  "Expand to `session.getStats()`."
  `(ps:chain ,session (getStats)))

(ps:defpsmacro rpc-session-drain (session)
  "Expand to `session.drain()`."
  `(ps:chain ,session (drain)))

(ps:defpsmacro serialize (value)
  "Expand to capnweb.serialize(value)."
  `(ps:chain ,(%binding) (serialize ,value)))

(ps:defpsmacro deserialize (value)
  "Expand to capnweb.deserialize(value)."
  `(ps:chain ,(%binding) (deserialize ,value)))
