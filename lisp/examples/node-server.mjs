import http from "node:http";
import { fileURLToPath } from "node:url";
import { resolve } from "node:path";
import * as capnweb from "capnweb";

export function makeApi() {
  class PingApi extends capnweb.RpcTarget {
    async ping(message) {
      const reply = message != null ? message : "pong from server";
      console.log("server ping ->", reply);
      return reply;
    }
  }

  return new PingApi();
}

export function startServer(options = {}) {
  const opts = options || {};
  const api = opts.api || makeApi();
  const endpointPath = typeof opts.path === "string" ? opts.path : "/rpc";
  let portValue = opts.port;
  if (portValue === undefined || portValue === null) {
    portValue = process.env.PORT;
  }
  if (portValue === undefined || portValue === null) {
    portValue = 3000;
  }
  let port = typeof portValue === "number" ? portValue : Number(portValue);
  if (!Number.isFinite(port)) {
    port = 3000;
  }
  const server = http.createServer((req, res) => {
    if (req.method !== "POST" || req.url !== endpointPath) {
      res.writeHead(404, { "content-type": "text/plain" });
      res.end("Not Found");
      return;
    }
    capnweb.nodeHttpBatchRpcResponse(req, res, api).catch((err) => {
      res.writeHead(500, { "content-type": "text/plain" });
      const message = err && err.stack ? String(err.stack) : String(err);
      res.end(message);
    });
  });
  server.listen(port, () => {
    console.log(`capnweb RPC server listening on http://localhost:${port}${endpointPath}`);
  });
  return server;
}

const THIS_MODULE = fileURLToPath(import.meta.url);
const ENTRY = process.argv[1] ? resolve(process.cwd(), process.argv[1]) : null;
if (ENTRY && THIS_MODULE === ENTRY) {
  startServer();
}
