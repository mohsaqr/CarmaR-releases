#!/usr/bin/env node
// carmar-mcp.mjs — the MCP stdio server that connects an agent CLI (Claude
// Code, Codex, anything speaking MCP) to the user's running CarmaR notebook.
//
//   claude mcp add carmar -- node /abs/path/tools/mcp/carmar-mcp.mjs
//   codex  mcp add carmar -- node /abs/path/tools/mcp/carmar-mcp.mjs
//
// The CLI spawns this process and speaks JSON-RPC over stdio; this process
// joins the kernel's WebSocket — the SAME loopback socket a notebook page uses —
// declares itself with mcp-hello, and asks. serve.R routes each mcp-request
// to the active notebook window; the page answers through the notebook's own
// insert/run machinery, so everything an agent does is visible in the UI.
//
// Boundary, by design: this file never reads CLI credentials, never talks to
// Anthropic or OpenAI, never proxies a subscription. Authentication stays
// inside the official CLI. Kernel discovery uses the same-user runtime file
// (~/.carmar/run, mode 0600); no credential is placed in a URL.
//
// Zero dependencies: Node >= 22 (built-in WebSocket and fetch).

import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import process from "node:process";
// `../../lib/` resolves to the repo's lib/ from tools/mcp/, and to inst/app/lib/
// from the R package's inst/app/kernel/mcp/ — build-r-pkg.sh stages the module
// at that depth on purpose, so one path serves both layouts.
import { authoringInstructions } from "../../lib/agent-authoring-contract.js";

const log = (...parts) => console.error("[carmar-mcp]", ...parts);

// MCP initialization instructions are the durable contract shared by Codex,
// Claude Code, and any other compliant client. Keep the one-block authoring
// rule in the first 512 characters: Codex explicitly uses that prefix while
// deciding which server tools belong in a workflow.
//
// The text is NOT written here. It comes from lib/agent-authoring-contract.js,
// the same module the notebook validates against, so what an agent is told and
// what it is held to cannot drift apart. Importing a local ESM file keeps this
// server dependency-free — the "zero-dependency" promise is about npm.
const SERVER_INSTRUCTIONS = authoringInstructions();

// ── kernel discovery ─────────────────────────────────────────────────────────

const RUNTIME_DIR = process.env.CARMAR_RUNTIME_DIR
  || path.join(os.homedir(), ".carmar", "run");

const argUrl = (() => {
  const at = process.argv.indexOf("--url");
  return at >= 0 ? process.argv[at + 1] : null;
})();

/** A candidate kernel URL → its ws:// form, or null when it makes no sense. */
function wsUrlFrom(pageUrl) {
  try {
    const u = new URL(pageUrl);
    const hostname = u.hostname.toLowerCase();
    if (!/^https?:$/.test(u.protocol)
        || !["127.0.0.1", "localhost", "::1", "[::1]"].includes(hostname)) return null;
    return {
      ws: `${u.protocol === "https:" ? "wss" : "ws"}://${u.host}/ws`,
      host: u.host,
    };
  } catch {
    return null;
  }
}

async function healthy(host) {
  try {
    const controller = new AbortController();
    const timer = setTimeout(() => controller.abort(), 2000);
    const reply = await fetch(`http://${host}/health`, { signal: controller.signal });
    clearTimeout(timer);
    if (!reply.ok) return false;
    const body = await reply.json();
    return body && body.ok === true;
  } catch {
    return false;
  }
}

/**
 * Find a live kernel: explicit env/arg first, then the runtime files newest
 * first. Files that fail the health check are STALE LITTER from a killed
 * kernel (a SIGKILL skips serve.R's cleanup) and are removed here — this
 * process owns the same user account that wrote them.
 */
async function discoverKernel() {
  const explicit = process.env.CARMAR_MCP_URL || argUrl;
  if (explicit) {
    const candidate = wsUrlFrom(explicit);
    if (candidate && await healthy(candidate.host)) return candidate;
    throw new Error(`No healthy CarmaR kernel at ${explicit}.`);
  }
  let names = [];
  try {
    names = fs.readdirSync(RUNTIME_DIR).filter((name) => /^kernel-\d+\.json$/.test(name));
  } catch {
    names = [];
  }
  const files = names
    .map((name) => {
      const file = path.join(RUNTIME_DIR, name);
      try { return { file, mtime: fs.statSync(file).mtimeMs }; }
      catch { return null; }
    })
    .filter(Boolean)
    .sort((a, b) => b.mtime - a.mtime);
  for (const { file } of files) {
    let record = null;
    try { record = JSON.parse(fs.readFileSync(file, "utf8")); } catch { record = null; }
    const candidate = record && record.url ? wsUrlFrom(record.url) : null;
    if (candidate && await healthy(candidate.host)) return candidate;
    try { fs.unlinkSync(file); log("removed stale runtime file", file); } catch { /* not ours to force */ }
  }
  throw new Error(
    "No running CarmaR kernel found. Start one (CarmaR.app, carmar::run(), or "
    + "`npm run kernel`) and open the notebook it prints, then try again.");
}

// ── the kernel connection ────────────────────────────────────────────────────

let clientName = "agent";        // learned from the CLI's initialize call
let connection = null;           // { sock, pending: Map, hello }

function dropConnection() {
  if (connection && connection.sock) { try { connection.sock.close(); } catch { /* gone */ } }
  connection = null;
}

/** Connect (or reuse) the WebSocket to the kernel; resolves after mcp-hello. */
async function kernelConnection() {
  if (connection && connection.sock.readyState === 1) return connection;
  dropConnection();
  const { ws, host } = await discoverKernel();
  const sock = new WebSocket(ws);
  const pending = new Map();     // frame id → {resolve, reject, timer}

  const conn = { sock, pending, host, hello: null };
  sock.addEventListener("message", (event) => {
    let frame = null;
    try { frame = JSON.parse(event.data); } catch { return; }
    if (!frame || typeof frame.id !== "string") return;
    const waiter = pending.get(frame.id);
    if (!waiter) return;
    pending.delete(frame.id);
    clearTimeout(waiter.timer);
    waiter.resolve(frame);
  });
  sock.addEventListener("close", () => {
    for (const waiter of pending.values()) {
      clearTimeout(waiter.timer);
      waiter.reject(new Error("The kernel connection closed."));
    }
    pending.clear();
    if (connection === conn) connection = null;
  });

  await new Promise((resolve, reject) => {
    const timer = setTimeout(() => reject(new Error("Timed out opening the kernel socket.")), 8000);
    sock.addEventListener("open", () => { clearTimeout(timer); resolve(); });
    sock.addEventListener("error", () => { clearTimeout(timer); reject(new Error("Could not open the kernel socket.")); });
  });

  conn.hello = await sendAndWait(conn, {
    type: "mcp-hello", id: `hello-${Date.now()}`, client: clientName,
  }, 8000);
  connection = conn;
  log(`connected to kernel at ${host} (${conn.hello.pages} notebook page(s))`);
  return conn;
}

let frameSeq = 0;
const frameId = (kind) => `mcp-${kind}-${++frameSeq}-${Math.random().toString(36).slice(2, 8)}`;

function sendAndWait(conn, frame, timeoutMs) {
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => {
      conn.pending.delete(frame.id);
      reject(new Error(`The kernel did not answer within ${Math.round(timeoutMs / 1000)}s.`));
    }, timeoutMs);
    conn.pending.set(frame.id, { resolve, reject, timer });
    try {
      conn.sock.send(JSON.stringify(frame));
    } catch (e) {
      conn.pending.delete(frame.id);
      clearTimeout(timer);
      reject(e);
    }
  });
}

/** Ask the active notebook page, through serve.R's routing. */
async function askPage(tool, args, timeoutMs) {
  const conn = await kernelConnection();
  const reply = await sendAndWait(conn, {
    type: "mcp-request", id: frameId("req"), tool, args: args || {},
  }, timeoutMs);
  if (!reply.ok) throw new Error(reply.error || `The notebook could not answer ${tool}.`);
  return reply;
}

/** Ask the R worker directly (read-side FORWARDED ops only). */
async function askWorker(type, payload, timeoutMs = 30000) {
  const conn = await kernelConnection();
  const reply = await sendAndWait(conn, { type, id: frameId(type), ...payload }, timeoutMs);
  if (reply.error) throw new Error(reply.error);
  return reply;
}

// ── the tools ────────────────────────────────────────────────────────────────

const TOOLS = [
  {
    name: "carmar_status",
    description:
      "Check the CarmaR connection: whether a local R kernel is running and "
      + "whether a notebook window is open. Call this first when other tools fail.",
    inputSchema: { type: "object", properties: {}, additionalProperties: false },
    async run() {
      try {
        const conn = await kernelConnection();
        const hello = await sendAndWait(conn, {
          type: "mcp-hello", id: frameId("hello"), client: clientName,
        }, 8000);
        const pages = Number(hello.pages) || 0;
        return {
          summary: pages > 0
            ? `Connected: kernel at ${conn.host}, ${pages} notebook page(s) open.`
            : `The kernel at ${conn.host} is running, but NO notebook window is open — `
              + "notebook tools will fail until the user opens CarmaR in a browser.",
          data: { kernel: conn.host, pages },
        };
      } catch (e) {
        return {
          summary: `Not connected: ${String((e && e.message) || e)}`,
          data: { kernel: null, pages: 0 },
        };
      }
    },
  },
  {
    name: "notebook_read",
    description:
      "Read the open CarmaR notebook: every chunk in order (address, name, R "
      + "source, latest output summary) plus which chunk is active. Chunk "
      + "addresses like \"3\" or \"7A\" are the handles other tools accept.",
    inputSchema: {
      type: "object",
      properties: {
        include_output: { type: "boolean", description: "Include each chunk's latest output summary (default true)." },
      },
      additionalProperties: false,
    },
    run: (args) => askPage("notebook_read", args, 30000),
  },
  {
    name: "chunk_read",
    description: "Read one chunk of the notebook — its R source and latest output — by address (\"3\", \"7A\") or name.",
    inputSchema: {
      type: "object",
      properties: { chunk: { type: "string", description: "Chunk address or name." } },
      required: ["chunk"],
      additionalProperties: false,
    },
    run: (args) => askPage("chunk_read", args, 30000),
  },
  {
    name: "chunk_insert",
    description:
      "Insert exactly ONE deliberately authored document block into the notebook. "
      + "For a longer analysis, call this tool once per finished prose section or runnable R step, "
      + "using each returned revisionId as the next base_revision. Do not send a whole chat reply "
      + "or multiple blocks as one source string. By default this block lands after the "
      + "active chunk, or at the beginning when none is active — the same rule "
      + "the notebook's own Add button follows. Inserted code is NOT run; use "
      + "chunk_run on the returned address.",
    inputSchema: {
      type: "object",
      properties: {
        code: { type: "string", description: "The chunk's source (R code, or markdown for kind \"text\")." },
        kind: { type: "string", enum: ["r", "text"], description: "Chunk kind (default \"r\")." },
        where: {
          type: "string", enum: ["auto", "beginning", "end"],
          description: "Placement (default \"auto\": after the active chunk, else beginning).",
        },
        after: { type: "string", description: "Optional chunk address or name to place this block after." },
        base_revision: {
          type: "string",
          description: "The `document.revisionId` from your most recent notebook_read (or the "
            + "`revisionId` an earlier insert returned). If the user has changed the notebook "
            + "since, the insert is refused instead of landing against a document you have not "
            + "seen. Always send it.",
        },
      },
      required: ["code"],
      additionalProperties: false,
    },
    run: (args) => askPage("chunk_insert", args, 30000),
  },
  {
    name: "chunk_run",
    description:
      "Run one chunk in the user's live R session and return its output "
      + "(stdout, messages, errors, table/plot summaries). The run is visible "
      + "in the notebook exactly as if the user pressed Run.",
    inputSchema: {
      type: "object",
      properties: {
        chunk: { type: "string", description: "Chunk address (\"3\", \"7A\") or name." },
        timeout_s: { type: "number", description: "Seconds to wait (default 300)." },
      },
      required: ["chunk"],
      additionalProperties: false,
    },
    run: (args) => {
      const seconds = Math.min(3600, Math.max(5, Number(args.timeout_s) || 300));
      return askPage("chunk_run", { chunk: args.chunk }, seconds * 1000);
    },
  },
  {
    name: "chunk_update",
    description:
      "Replace exactly ONE existing Text or R block with a deliberately authored document "
      + "revision. Preserve the block's kind. The replacement is revision-guarded, visible "
      + "immediately, and provisional: the user receives Keep/Reject controls and Reject restores "
      + "the previous source. Use this when the user asks to revise or edit the document; do not "
      + "describe the replacement only in chat.",
    inputSchema: {
      type: "object",
      properties: {
        chunk: { type: "string", description: "Chunk address (\"3\", \"7A\") or name to revise." },
        code: { type: "string", description: "Complete replacement source, without Markdown fences around R code." },
        base_revision: {
          type: "string",
          description: "Required `document.revisionId` from the most recent notebook_read. A stale replacement is refused.",
        },
      },
      required: ["chunk", "code", "base_revision"],
      additionalProperties: false,
    },
    run: (args) => askPage("chunk_update", args, 30000),
  },
  {
    name: "file_list",
    description:
      "List a directory as R sees it (defaults to R's working directory). "
      + "Paths are subject to the kernel's own root rules.",
    inputSchema: {
      type: "object",
      properties: { path: { type: "string", description: "Directory to list (default: R's getwd())." } },
      additionalProperties: false,
    },
    async run(args) {
      const reply = await askWorker("files", args && args.path ? { path: args.path } : {});
      const entries = Array.isArray(reply.entries) ? reply.entries : [];
      return {
        summary: `${reply.path}: ${entries.length} entr${entries.length === 1 ? "y" : "ies"}.`,
        data: {
          path: reply.path,
          entries: entries.map((entry) => ({
            name: entry.name, isdir: !!entry.isdir, size: entry.size ?? null,
          })),
        },
      };
    },
  },
  {
    name: "file_read",
    description:
      "Read a text file through the R kernel (project scripts, data files, "
      + "up to 4 MB). Same path rules as the notebook's own editor.",
    inputSchema: {
      type: "object",
      properties: { path: { type: "string", description: "File to read; ~ is expanded." } },
      required: ["path"],
      additionalProperties: false,
    },
    async run(args) {
      const reply = await askWorker("readfile", { path: String(args.path || "") });
      return {
        summary: `Read ${reply.path} (${reply.text.length} characters).`,
        data: { path: reply.path, text: reply.text },
      };
    },
  },
  {
    name: "file_open",
    description:
      "Open a notebook document (.qmd, .Rmd, .md) in the CarmaR window as "
      + "chunks. For plain reading use file_read instead.",
    inputSchema: {
      type: "object",
      properties: { path: { type: "string", description: "Document to open in the notebook." } },
      required: ["path"],
      additionalProperties: false,
    },
    run: (args) => askPage("file_open", args, 30000),
  },
];

// ── MCP over stdio: newline-delimited JSON-RPC 2.0 ──────────────────────────

const PROTOCOL_FALLBACK = "2025-06-18";

function reply(id, result) {
  process.stdout.write(`${JSON.stringify({ jsonrpc: "2.0", id, result })}\n`);
}
function replyError(id, code, message) {
  process.stdout.write(`${JSON.stringify({ jsonrpc: "2.0", id, error: { code, message } })}\n`);
}

/** Tool output → MCP content. One text block: the summary line, then data. */
function toolContent(out) {
  const text = out && out.data !== undefined
    ? `${out.summary || "ok"}\n${JSON.stringify(out.data, null, 2)}`
    : String((out && out.summary) || "ok");
  return { content: [{ type: "text", text }] };
}

async function onRequest(msg) {
  const { id, method, params } = msg;
  if (method === "initialize") {
    clientName = String(params?.clientInfo?.name || "agent").slice(0, 64);
    reply(id, {
      protocolVersion: typeof params?.protocolVersion === "string"
        ? params.protocolVersion : PROTOCOL_FALLBACK,
      capabilities: { tools: {} },
      serverInfo: { name: "carmar", version: "0.1.0" },
      instructions: SERVER_INSTRUCTIONS,
    });
    return;
  }
  if (method === "ping") { reply(id, {}); return; }
  if (method === "tools/list") {
    reply(id, {
      tools: TOOLS.map(({ name, description, inputSchema }) => ({ name, description, inputSchema })),
    });
    return;
  }
  if (method === "tools/call") {
    const tool = TOOLS.find((candidate) => candidate.name === params?.name);
    if (!tool) { replyError(id, -32602, `Unknown tool: ${params?.name}`); return; }
    try {
      const out = await tool.run(params?.arguments || {});
      // Page replies arrive as {ok, summary, data}; local tools return the
      // same envelope — one shape, whoever answered.
      reply(id, toolContent(out));
    } catch (e) {
      reply(id, {
        content: [{ type: "text", text: String((e && e.message) || e) }],
        isError: true,
      });
    }
    return;
  }
  if (id !== undefined) replyError(id, -32601, `Method not found: ${method}`);
}

let buffer = "";
process.stdin.setEncoding("utf8");
process.stdin.on("data", (data) => {
  buffer += data;
  let newline;
  while ((newline = buffer.indexOf("\n")) >= 0) {
    const line = buffer.slice(0, newline).trim();
    buffer = buffer.slice(newline + 1);
    if (!line) continue;
    let msg = null;
    try { msg = JSON.parse(line); } catch { continue; }
    if (!msg || msg.jsonrpc !== "2.0") continue;
    if (msg.method !== undefined) {
      if (msg.id === undefined) continue;          // notifications need no answer
      onRequest(msg).catch((e) => replyError(msg.id, -32603, String((e && e.message) || e)));
    }
    // Responses to server-initiated requests: this server never sends any.
  }
});
process.stdin.on("end", () => { dropConnection(); process.exit(0); });
process.on("SIGTERM", () => { dropConnection(); process.exit(0); });
process.on("SIGINT", () => { dropConnection(); process.exit(0); });
