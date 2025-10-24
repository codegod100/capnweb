import { fileURLToPath } from "node:url";
import { resolve } from "node:path";
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
const ENTRY = process.argv[1] ? resolve(process.cwd(), process.argv[1]) : null;
if (ENTRY && THIS_MODULE === ENTRY) {
  const message = process.argv.length > 2 ? process.argv[2] : "ping";
  runClient({ message }).catch((err) => {
    console.error(err);
    process.exitCode = 1;
  });
}
