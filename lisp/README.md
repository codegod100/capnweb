# capnweb Lisp Wrapper

This directory hosts an experimental Common Lisp layer for generating capnweb client code with [ParenScript](https://github.com/vsedach/Parenscript). The goal is to let you write remote procedure call logic in Lisp syntax while targeting the JavaScript capnweb runtime exported from this package.

## Prerequisites
- A Common Lisp implementation (tested with SBCL) and ASDF.
- Quicklisp with the ParenScript system available: `(ql:quickload :parenscript)`.

## Provided ASDF systems
- `capnweb` – core ParenScript wrappers and helper utilities.
- `capnweb/examples` – convenience entry points for producing runnable examples.

Load the systems with:
```lisp
(ql:quickload :capnweb/examples)
```

## Quick start
1. Load the example helper:
   ```lisp
   (asdf:load-system "capnweb/examples")
   ```
2. Emit the example JavaScript module (defaults to `basic-example.js` in the current directory):
   ```lisp
   (capnweb.examples:write-basic-example-file)
   ```
3. Bundle or run the generated module in an environment where the `capnweb` npm package is available.

The generated JavaScript will import the published capnweb API and execute RPC calls using the ParenScript macros exported from the `capnweb.parenscript` package.

## Node HTTP batch demo
To spin up both a minimal RPC server and client targeting the HTTP batch transport:

```lisp
(capnweb.examples:write-node-client-and-server-files)
```

This writes `node-server.mjs` and `node-client.mjs` into the current directory. Run `npm run build` from the repo root to compile the JS bundle, then:

```bash
node node-server.mjs
node node-client.mjs
```

The server attaches to `http://localhost:3000/rpc` and exposes a `ping` method implemented via the `capnweb.parenscript:rpc-target` helper. The client uses `capnweb.parenscript:new-http-batch-session` to call the endpoint and logs the response.

## Using the ParenScript helpers
Import the `capnweb.parenscript` package in the forms you compile with `parenscript:ps`. The most immediately useful macros are:
- `capnweb.parenscript:new-websocket-session`
- `capnweb.parenscript:new-http-batch-session`
- `capnweb.parenscript:new-messageport-session`
- `capnweb.parenscript:rpc-main`
- `capnweb.parenscript:rpc-session-stats`
- `capnweb.parenscript:rpc-session-drain`
- `capnweb.parenscript:serialize`
- `capnweb.parenscript:deserialize`

See `lisp/examples/basic-example.lisp` for a concrete ParenScript program that targets the browser or Node.js environments.
