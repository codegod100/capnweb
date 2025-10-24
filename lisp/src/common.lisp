;;;; common.lisp --- Common Lisp helpers for capnweb ParenScript generation

(in-package #:capnweb)

(defparameter *default-module-name* "capnweb"
  "Default npm module specifier used by the ParenScript helpers when emitting imports.")

(defun %ensure-binding-symbol (binding)
  "Return a ParenScript-visible symbol for the capnweb binding and its printable name."
  (etypecase binding
    (symbol (values binding (string-downcase (symbol-name binding))))
    (string (let* ((upper (string-upcase binding))
                   (symbol (make-symbol upper)))
              (values symbol (string-downcase binding))))))

(defun %split-lines (text)
  "Split TEXT into lines without retaining trailing newlines."
  (with-input-from-string (stream text)
    (loop for line = (read-line stream nil nil)
          while line collect line)))

(defun %join-lines (lines)
  "Join a list of LINES with newlines."
  (with-output-to-string (out)
    (loop for line in lines
          for index from 0 do
            (when (> index 0)
              (write-char #\Newline out))
            (write-string line out))))

(defun %indent-block (text &key (spaces 2))
  "Indent TEXT by SPACES spaces per line, trimming trailing blank lines first."
  (let* ((trimmed (string-right-trim '(#\Space #\Tab #\Newline #\Return) text))
         (indent (make-string spaces :initial-element #\Space)))
    (if (string= trimmed "")
        ""
        (with-output-to-string (out)
          (loop for line in (%split-lines trimmed)
                for index from 0 do
                  (when (> index 0)
                    (write-char #\Newline out))
                  (write-string indent out)
                  (write-string line out))))))

(defun generate-basic-example-js (&key (module-name *default-module-name*)
                                       (binding 'capnweb))
  "Return an ES module string that demonstrates the ParenScript wrapper."
  (multiple-value-bind (binding-symbol binding-name)
      (%ensure-binding-symbol binding)
    (let ((capnweb.parenscript:*capnweb-binding* binding-symbol)
          (parenscript:*ps-print-pretty* t))
      (format nil "import * as ~a from \"~a\";~%~%~a~%"
              binding-name module-name
              (ps '(export async function runBasicDemo (connect-url)
                      (let* ((session (await (capnweb.parenscript:new-websocket-session connect-url)))
                             (main (capnweb.parenscript:rpc-main session))
                             (pong (await (ps:chain main (ping))))
                             (stats (capnweb.parenscript:rpc-session-stats session)))
                        (ps:chain console (log "capnweb stats" stats))
                        (await (capnweb.parenscript:rpc-session-drain session))
                        (return pong))))))))

(defun generate-node-server-js (&key (module-name *default-module-name*)
                                     (binding 'capnweb))
  "Produce an ES module Node server that exposes a capnweb RPC endpoint."
  (multiple-value-bind (binding-symbol binding-name)
      (%ensure-binding-symbol binding)
    (let ((capnweb.parenscript:*capnweb-binding* binding-symbol)
          (parenscript:*ps-print-pretty* t))
      (let* ((api-body (ps '(let ((target (capnweb.parenscript:rpc-target)))
                              (setf (ps:chain target ping)
                                    (lambda (message)
                                      (let ((reply (if message message "pong from server")))
                                        (ps:chain console (log "server ping ->" reply))
                                        (return reply))))
                              (return target))))
             (server-lines (%join-lines
                             (list
                              "const opts = options || {};"
                              "const api = opts.api || makeApi();"
                              "const endpointPath = typeof opts.path === \"string\" ? opts.path : \"/rpc\";"
                              "let portValue = opts.port;"
                              "if (portValue === undefined || portValue === null) {"
                              "  portValue = process.env.PORT;"
                              "}"
                              "if (portValue === undefined || portValue === null) {"
                              "  portValue = 3000;"
                              "}"
                              "let port = typeof portValue === \"number\" ? portValue : Number(portValue);"
                              "if (!Number.isFinite(port)) {"
                              "  port = 3000;"
                              "}"
                              "const server = http.createServer((req, res) => {"
                              "  if (req.method !== \"POST\" || req.url !== endpointPath) {"
                              "    res.writeHead(404, { \"content-type\": \"text/plain\" });"
                              "    res.end(\"Not Found\");"
                              "    return;"
                              "  }"
                              (format nil "  ~a.nodeHttpBatchRpcResponse(req, res, api).catch((err) => {"
                                      binding-name)
                              "    res.writeHead(500, { \"content-type\": \"text/plain\" });"
                              "    const message = err && err.stack ? String(err.stack) : String(err);"
                              "    res.end(message);"
            "  });"
            "});"
            "server.listen(port, () => {"
            "  console.log(`capnweb RPC server listening on http://localhost:${port}${endpointPath}`);"
            "});"
            "return server;")))
             (api-block (%indent-block api-body))
       (server-block (%indent-block server-lines))
       (main-guard (format nil
         (concatenate 'string
                "const THIS_MODULE = fileURLToPath(import.meta.url);~%"
                "if (process.argv[1] && THIS_MODULE === process.argv[1]) {~%"
                "  startServer();~%"
                "}~%"))))
  (format nil
    "import http from \"node:http\";~%import { fileURLToPath } from \"node:url\";~%import * as ~a from \"~a\";~%~%export function makeApi() {~%~a~%}~%~%export function startServer(options = {}) {~%~a~%}~%~a"
                binding-name module-name
                api-block
    server-block
    main-guard)))))

(defun generate-node-client-js (&key (module-name *default-module-name*)
                                     (binding 'capnweb))
  "Produce an ES module client that consumes a capnweb HTTP batch endpoint."
  (multiple-value-bind (binding-symbol binding-name)
      (%ensure-binding-symbol binding)
    (let ((capnweb.parenscript:*capnweb-binding* binding-symbol)
          (parenscript:*ps-print-pretty* t))
      (let* ((stub-expr (ps '(capnweb.parenscript:new-http-batch-session endpoint)))
        (call-expr (ps '(ps:chain remote (ping message))))
             (main-guard (format nil
                                 (concatenate 'string
                                              "const THIS_MODULE = fileURLToPath(import.meta.url);~%"
                                              "if (process.argv[1] && THIS_MODULE === process.argv[1]) {~%"
                                              "  const message = process.argv.length > 2 ? process.argv[2] : \"ping\";~%"
                                              "  runClient({ message }).catch((err) => {~%"
                "    console.error(err);~%"
                "    process.exitCode = 1;~%"
                "  });~%"
                "}~%"))))
        (format nil
      "import { fileURLToPath } from \"node:url\";~%import * as ~a from \"~a\";~%~%const DEFAULT_ENDPOINT = \"http://localhost:3000/rpc\";~%~%export async function runClient({ endpoint = DEFAULT_ENDPOINT, message = \"ping\" } = {}) {~%  const remote = ~a;~%  const pong = await ~a;~%  console.log(\"capnweb client received:\", pong);~%  return pong;~%}~%~%export function createStub(endpoint = DEFAULT_ENDPOINT) {~%  return ~a;~%}~%~a"
                binding-name module-name
                stub-expr
                call-expr
      stub-expr
      main-guard)))))
