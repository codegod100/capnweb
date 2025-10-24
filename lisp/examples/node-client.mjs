import { fileURLToPath } from "node:url";
import * as capnweb from "capnweb";

const DEFAULT_ENDPOINT = "http://localhost:3000/rpc";

export async function runClient({ endpoint = DEFAULT_ENDPOINT, message = "ping" } = {}) {
  const remote = capnweb.newHttpBatchRpcSession(endpoint);
  const pong = await remote.ping(message);
  console.log("capnweb client received:", pong);
  return pong;
}

export function createStub(endpoint = DEFAULT_ENDPOINT) {
  return capnweb.newHttpBatchRpcSession(endpoint);
}

const THIS_MODULE = fileURLToPath(import.meta.url);
if (process.argv[1] && THIS_MODULE === process.argv[1]) {
  const message = process.argv.length > 2 ? process.argv[2] : "ping";
  runClient({ message }).catch((err) => {
    console.error(err);
    process.exitCode = 1;
  });
}
