// agent-authoring-contract.js — the one place that says how a model may write
// into a CarmaR document, and the only one that decides whether it did.
//
// WHY THIS IS A MODULE AND NOT A PROMPT. The rules below were previously
// stated in three unsynchronised places — the MCP server's initialize text,
// the in-browser system prompts, and each tool's description — and stated is
// all they were. A model that ignored "never bury runnable R inside a text
// cell's code fence" was not stopped by anything; the sentence was advice.
// Instructions teach the workflow, schemas restrict the shape, and THIS decides
// whether a proposal is valid. Only the third one is enforcement.
//
// Nothing here touches the DOM, the notebook, or the network, because
// `tools/mcp/carmar-mcp.mjs` imports it in Node (outside the browser bundle)
// and `lib/mcp-bridge.js` imports it in the page. One module, both sides, so
// the rule an agent is told and the rule it is held to cannot drift apart.

export const CONTRACT = "carmar.authoring/v1";
export const SCHEMA_VERSION = 1;

/** A browser answer may still be reviewed as one bounded user-requested set. */
export const MAX_BLOCKS = 24;

/** The whole block vocabulary. `r` runs; `text` is prose. */
export const BLOCK_KINDS = ["r", "text"];

/**
 * The model-visible workflow. Kept as sentences rather than one blob so a
 * caller can take the prefix it needs: Codex reads only the first 512
 * characters of the MCP instructions when deciding which server tools belong
 * in a workflow, so the one-block authoring rule has to live in sentence one.
 */
export const AUTHORING_RULES = [
  // The wording and the available tool now agree: terminal agents author one
  // document unit at a time. The removed batch tool changed the model's unit
  // of composition from "this notebook block" to "my reply, split into an
  // array", which is how chat scaffolding reached saved documents.
  "CarmaR is the user's live R notebook. Start with notebook_read. Build the analysis ONE "
  + "document block at a time: call chunk_insert once for each finished prose section or runnable "
  + "R step, in reading and execution order. AUTHOR every text block as part of the saved .qmd, "
  + "not as a chat reply. Never paste or divide your chat response into notebook blocks. Keep "
  + "explanation in text blocks and executable R in r blocks—never bury runnable R in prose.",

  "Use the address returned by chunk_insert for chunk_run. After inserting an R chunk, run it "
  + "before authoring any later block that depends on its result; do not pre-write claimed results. "
  + "Runs are visible in the notebook. If the user requested authoring only, do not run.",

  "Inserted cells remain marked as pending until the user decides. You may run pending R cells, "
  + "but never claim that the user kept or accepted them unless a later notebook_read no longer "
  + "reports review pending.",

  "Preserve execution order: setup/import, transformation, model, then output or visualization. "
  + "Put prose immediately before the code it explains. Use the revisionId returned by each "
  + "chunk_insert as base_revision for the next call.",

  "A text block is DOCUMENT PROSE, in the notebook's voice. Write headings, sentences and lists "
  + "as they should appear in the finished .qmd. Never carry chat formatting into it: no banner "
  + "rules, no box-drawing lines, no \"Insight\"/\"Note to self\" framing devices, no addressing "
  + "the reader as if answering a message. The notebook is a document someone else will read, "
  + "not a transcript of your reply.",

  "notebook_read reports the document's identity, source format and revisionId. Pass that "
  + "revisionId as base_revision on every insert so a notebook the user has changed underneath "
  + "you is refused rather than written to; each insert returns the new revisionId to use next. "
  + "Never change the source format — it is the user's decision, and it is recorded on the "
  + "document.",

  "To revise an existing block, call chunk_update with the current document revision and the "
  + "complete replacement source. Preserve the block's kind. An update is provisional and "
  + "reversible until the user keeps or rejects it; never describe a proposed edit only in chat "
  + "when the user asked you to change the document.",
];

/** The MCP `initialize` instructions, and any other single-string surface. */
export function authoringInstructions() {
  return AUTHORING_RULES.join(" ");
}

// A BRACED fence is an executable chunk: ```{r}, ```{r label}, ```{r, echo=FALSE}.
// An unbraced ```r fence is plain Markdown and runs nowhere — which is exactly
// the distinction lib/qmd.js preserves as `braced`, so the two agree about what
// "executable" means rather than each having an opinion.
const BRACED_R = /^\{\s*[rR][\s,}]/;
const PLAIN_R = /^[rR]$/;
const FENCE_LINE = /^[ \t]*(`{3,}|~{3,})(.*)$/;

/**
 * The info strings of this content's TOP-LEVEL fences.
 *
 * Nesting is the whole reason this is a scan and not a regex. An answer that
 * TEACHES chunk syntax puts a ```{r} inside an outer ```` fence, and that inner
 * line is content, not a chunk — refusing it would mean the notebook cannot
 * explain itself. A rule that fires on correct input gets switched off by the
 * first person it annoys, so the false-positive rate has to be zero.
 */
function topLevelFenceInfo(content) {
  const info = [];
  let open = null;
  String(content).split("\n").forEach((line) => {
    const match = FENCE_LINE.exec(line);
    if (!match) return;
    const [, marker, rest] = match;
    if (open) {
      // A closing fence: same character, at least as long, nothing trailing.
      if (marker[0] === open[0] && marker.length >= open.length && !rest.trim()) open = null;
      return;
    }
    open = marker;
    info.push(rest.trim());
  });
  return info;
}
// An R block whose source IS a Markdown fence: the model wrapped its code the
// way it would in chat. R cannot parse a backtick fence, so the chunk is dead
// on arrival — and the failure looks like a syntax error in the user's code.
const WRAPPED_IN_FENCE = /^[ \t]*(?:`{3,}|~{3,})/;

// ── chat furniture ───────────────────────────────────────────────────────────
//
// An assistant's TERMINAL voice is decorated: banner rules, "★ Insight ────"
// headers, a matching rule to close. In a chat window that is a reading aid.
// Written into a notebook it is a document wearing someone else's conversation
// — it survives the save, lands in the .qmd, and the author has to delete it by
// hand from work they did not write.
//
// Only HORIZONTAL rule characters count. A model drawing an actual diagram uses
// corners and verticals (┌ │ └), and that is legitimate content; a run of three
// or more horizontal line characters is never prose.
const RULE_CHARS = "─━═┄┅┈┉╌╍";
const BARE_RULE = new RegExp(`^[${RULE_CHARS}]{3,}$`);
// A labelled banner: a short label, then the rule. "★ Insight ─────────".
const LABELLED_RULE = new RegExp(`^\\S[^${RULE_CHARS}]{0,40}?[${RULE_CHARS}]{3,}$`);

/**
 * A line with its Markdown wrapping removed, for rule-matching only.
 *
 * The first version matched the bare form and missed every real case, because
 * assistants emit the banner INSIDE a code span — `` `★ Insight ────────` `` —
 * and a trailing backtick meant the line no longer ended in rule characters.
 * The pattern was written from a screenshot of the rendered block instead of
 * from the source, which is exactly the mistake it exists to catch.
 */
const unwrapLine = (line) => String(line).replace(/^[\s`*_~>]+/, "").replace(/[\s`*_~]+$/, "");

/**
 * Remove banner rules from prose, keeping what they framed.
 *
 * Stripping rather than refusing, deliberately: the decoration wraps REAL
 * content — the sentence inside the banner is the useful part. Refusing the
 * whole transaction over a cosmetic line would throw the analysis away, and
 * silently keeping it puts it in the author's document. So the lines come out,
 * the prose stays, and the reply says what was removed.
 *
 * @returns {{text: string, removed: number}}
 */
export function stripChatDecoration(content) {
  const lines = String(content == null ? "" : content).split("\n");
  const kept = lines.filter((line) => {
    const bare = unwrapLine(line);
    return !(BARE_RULE.test(bare) || LABELLED_RULE.test(bare));
  });
  const removed = lines.length - kept.length;
  if (!removed) return { text: String(content == null ? "" : content), removed: 0 };
  // Collapse the blank lines the banners leave behind, and trim the ends, so a
  // stripped block does not arrive padded with the gaps its frame occupied.
  const text = kept.join("\n").replace(/\n{3,}/g, "\n\n").trim();
  return { text, removed };
}

// ── what an agent gets wrong, enumerated ─────────────────────────────────────
//
// The first version of this module held five rules, all of them copied from
// the plan's validation section, all of them prohibitions on STRUCTURE. It had
// no positive model of the artifact — nothing saying a text block is document
// prose a stranger will read — so an assistant's chat decoration walked
// straight through and into a saved .qmd. A contract made only of "not X"
// catches exactly the X's somebody already named.
//
// So the failure space is written down here instead of assumed, and each entry
// is placed by one rule:
//
//   REFUSE  what CANNOT WORK OR IS NOT DOCUMENT PROSE — a fence-wrapped chunk,
//           a pasted console prompt, a duplicate label, assistant/process
//           register. The model can repair these without the host guessing how
//           to rewrite a sentence that also carries real analytical facts.
//   STRIP   what is decoration AROUND content and safely separable. The prose
//           is the useful part; throwing the transaction away over a frame
//           would cost the author their analysis.
//   LINT    what works but may be intentional — an install, a setwd, pasted
//           output. The user reviews every block before it is theirs, so the
//           honest move is to SAY so rather than guess at authorization.
//
// Nothing here guesses at intent, and nothing calls another model.

/**
 * Assistant register: talking TO the reader instead of writing the document.
 *
 * Each pattern carries its own plain-English `quote`. The first version built
 * that string by stripping punctuation out of the regex source, which produced
 * "would you lke me to" and "hope ths|that helps" — user-facing text must be
 * written, not derived from the thing that matched it.
 *
 * `['’]` throughout: models emit curly apostrophes constantly, and a rule that
 * only knows the straight one silently misses most of what it is looking for.
 */
const ASSISTANT_VOICE = [
  { re: /\blet me know\b/i, quote: "let me know" },
  { re: /\bwould you like me to\b/i, quote: "would you like me to" },
  { re: /\bhere['’]?s what i (found|did)\b/i, quote: "here's what I found" },
  { re: /\bi hope (this|that) helps\b/i, quote: "I hope this helps" },
  { re: /\bfeel free to\b/i, quote: "feel free to" },
  { re: /\bas requested\b/i, quote: "as requested" },
  {
    re: /\bi\s?['’]?ve (added|created|inserted|updated|written)\b/i,
    quote: "I've added / created / inserted",
  },
  {
    re: /\bi (ran|tested|verified|checked|executed)\b/i,
    quote: "I ran / tested / verified",
  },
  { re: /\byour installed\b/i, quote: "your installed" },
  { re: /\bshall i\b/i, quote: "shall I" },
];

// Deictic notebook-UI language has no stable meaning in a saved document.
// "That last block" depends on a vanished conversation and also calls an R
// chunk by the browser's generic internal noun. Refuse it so the author can
// name the actual step or chunk; do not silently substitute a possibly wrong
// reference.
const UI_REGISTER = /\b(?:this|that|last|previous|next)\s+(?:code\s+)?block\b/i;

// Recipe voice is still chat voice even when it avoids "I". These are the
// forms seen in real leaked prose: "without you assembling anything", "you
// place and run them", and "your notebook/session". A saved methods document
// names the analyst, reader, R session, object, or step; it does not address
// the person who happened to ask the pane a question.
const SECOND_PERSON = [
  { re: /\bwithout you\b/i, quote: "without you" },
  {
    re: /\byou\s+(?:can|could|should|must|need|may|might|will|would|have|want|place|run|decide|use|inspect|assemble|copy|paste|add|edit|change|choose|see)\b/i,
    quote: "you can / should / need",
  },
  {
    re: /\byour\s+(?:notebook|document|session|installation|installed|code|analysis|model|result|output|data|machine|project|file)\b/i,
    quote: "your notebook / session / analysis",
  },
];

/**
 * The agent narrating its OWN affordances — "I can't place or run chunks from
 * this pane", "each block below gets a Make this a chunk button", "I have not
 * run any of it". A reader opening the .qmd next year has no pane, no buttons
 * and no conversation with anyone.
 *
 * These are REMOVED, not linted, and a whole paragraph at a time. Linting them
 * was the first attempt and a Playwright run through the real answer door
 * showed why it was not enough: a note in the tool reply does not stop the
 * sentence landing in the document, which is the entire complaint. A sentence
 * about a button is never part of the analysis, so the paragraph goes whole;
 * mixed assistant/process prose is refused below because deleting part of a
 * factual paragraph would be an unsafe rewrite.
 */
const TOOLING_TALK = [
  /\bi (can['’]?t|cannot|can not) (place|run|insert|add|create|execute)\b/i,
  /\b(this|the) pane\b/i,
  /\bmake this a chunk\b/i,
  /\beach (block|step) below\b/i,
  /\bi have not run (any of )?(it|them|this)\b/i,
  /\b(button|buttons) (above|below)\b/i,
  /\bone step per block\b/i,
];

/**
 * Drop paragraphs that talk about the tool instead of the analysis.
 *
 * Paragraph-at-a-time because that is the unit these arrive in: an assistant's
 * closing "here is how to use what I just gave you" is its own block of prose,
 * and cutting a sentence out of the middle of a real paragraph would be the
 * riskier edit.
 *
 * @returns {{text: string, removed: number}}
 */
export function stripToolingTalk(content) {
  const source = String(content == null ? "" : content);
  const paragraphs = source.split(/\n{2,}/);
  const kept = paragraphs.filter((para) => !TOOLING_TALK.some((re) => re.test(para)));
  const removed = paragraphs.length - kept.length;
  if (!removed) return { text: source, removed: 0 };
  return { text: kept.join("\n\n").replace(/\n{3,}/g, "\n\n").trim(), removed };
}

// `[1] 42` is a printed R value. In a TEXT block it is output pasted as a
// picture of a result — the notebook runs the chunk and shows the real thing.
// A leading `> ` is deliberately NOT checked here: in Markdown that is a
// blockquote, and quoting is ordinary prose.
const PASTED_VALUE = /^\s*\[\d+\]\s+\S/m;

// In R SOURCE, a leading `> ` or `[1] ` is a transcript someone copied out of
// a console. Neither parses — `> x <- 1` is a syntax error — so this is a
// refusal, not a note. `#> ...` (the reprex convention) starts with `#` and is
// a comment, so it is untouched.
const CONSOLE_TRANSCRIPT = /^\s*(>\s+\S|\[\d+\]\s+\S)/m;

/** Side effects on the user's machine that a chunk should not quietly carry. */
const SIDE_EFFECTS = [
  { re: /\binstall\.packages\s*\(/, what: "installs packages into the user's R library" },
  { re: /\bsetwd\s*\(/, what: "changes the working directory for the whole session" },
  { re: /\brm\s*\(\s*list\s*=\s*ls\s*\(/, what: "erases every object in the user's session" },
  { re: /\bunlink\s*\(|\bfile\.remove\s*\(/, what: "deletes files from disk" },
];

// A path out of the project, in a string. `~` alone is not checked: in R it is
// the formula operator, and `y ~ x` is not a path.
const ABSOLUTE_PATH = /["'](?:\/Users\/|\/home\/|~\/|[A-Za-z]:[\\/])/;

/** `#| label: fig-x` — the label a Quarto chunk carries in its body. */
const HASH_PIPE_LABEL = /^\s*#\|\s*label\s*:\s*(\S+)/m;

/** An error a model can act on: what is wrong, where, and what to do instead. */
function repair(message) {
  return Object.assign(new Error(message), { code: "contract_violation", contract: CONTRACT });
}

const ordinal = (index) => `block ${index + 1}`;

/**
 * Validate and normalize a proposed sequence of blocks.
 *
 * Hard rules throw — the proposal does not reach the notebook. Softer
 * conventions come back as `lints`, reported to the model in the reply so it
 * can do better next time without the write being refused.
 *
 * @param {Array<{kind?: string, content?: string}>} raw
 * @param {Object} [opts]
 * @param {string} [opts.tool]  the tool name, for messages that name it
 * @param {number} [opts.max]   block ceiling (a single-block tool passes 1)
 * @returns {{blocks: Array<{kind: 'rcode'|'md', content: string}>, lints: string[]}}
 * @throws {Error} with `code: "contract_violation"` and a repair message
 */
export function normalizeBlocks(raw, { tool = "This answer", max = MAX_BLOCKS } = {}) {
  if (!Array.isArray(raw) || !raw.length) {
    throw repair(`${tool} needs a non-empty list of blocks.`);
  }
  if (raw.length > max) {
    throw repair(max === 1
      ? `${tool} takes exactly one block. Author the next document block in a later chunk_insert call.`
      : `${tool} accepts at most ${max} blocks at once; you sent ${raw.length}. `
        + "Send the analysis as one coherent set within that limit.");
  }

  const lints = [];
  const blocks = raw.map((item, index) => {          // null → dropped, see below
    const declared = item && item.kind != null ? String(item.kind) : "r";
    // "md" is the internal name for a text block; accept it so an in-page
    // caller and a terminal agent can share this function.
    const isText = declared === "text" || declared === "md";
    if (!isText && declared !== "r" && declared !== "rcode") {
      throw repair(`${ordinal(index)} has kind "${declared}". `
        + `CarmaR blocks are ${BLOCK_KINDS.map((k) => `"${k}"`).join(" or ")}.`);
    }
    const content = String(item && item.content != null ? item.content : "");
    if (!content.trim()) {
      throw repair(`${ordinal(index)} is empty. Every block must carry content.`);
    }

    if (isText) {
      const fences = topLevelFenceInfo(content);
      // The rule the instructions could only ask for. A ```{r} fence inside
      // prose is a chunk the user cannot run, cannot see the output of, and
      // cannot review as code — the analysis arrives as a picture of itself.
      if (fences.some((each) => BRACED_R.test(each))) {
        throw repair(`${ordinal(index)} is a text block containing an executable \`\`\`{r} `
          + "chunk. Runnable R belongs in its own block with kind \"r\"; split this into a "
          + "text block for the prose and an r block for the code, in that order.");
      }
      if (fences.some((each) => PLAIN_R.test(each))) {
        lints.push(`${ordinal(index)} embeds a \`\`\`r fence in prose. That is a literal `
          + "example and will never run — if it was meant to run, send it as an r block.");
      }
      const banners = stripChatDecoration(content);
      const cleaned = stripToolingTalk(banners.text);
      cleaned.removed = banners.removed;          // banner count, for the lint below
      if (cleaned.text !== banners.text) {
        lints.push(`${ordinal(index)} contained prose about the notebook's own buttons and `
          + "panes, which was removed. The saved document is read by people who have no pane "
          + "and were not in this conversation — write about the analysis only.");
      }
      if (banners.removed) {
        lints.push(`${ordinal(index)} arrived wrapped in ${cleaned.removed} chat banner `
          + `rule${cleaned.removed === 1 ? "" : "s"}, which were removed. A text block is `
          + "document prose — write it as it should read in the saved .qmd, with no "
          + "decoration around it.");
      }
      // A block that was ENTIRELY chat — a banner with nothing in it, or a
      // paragraph purely about the buttons — is dropped, not refused.
      //
      // Refusing was the first behaviour and it broke the pane's own Keep
      // button: an answer whose opening line happened to be "here is the
      // workflow, one step per block" failed WHOLE, and the user lost every
      // good block with it. The model's phrasing is not the user's mistake.
      // Dropping an empty block cannot damage the document; refusing the
      // transaction can.
      if (!cleaned.text.trim()) {
        lints.push(`${ordinal(index)} was entirely chat — a banner or a note about the `
          + "notebook's own controls — so it was dropped. The remaining blocks were kept.");
        return null;
      }
      const voice = ASSISTANT_VOICE.find((pattern) => pattern.re.test(cleaned.text));
      if (voice) {
        throw repair(`${ordinal(index)} uses assistant or process-reporting register `
          + `("${voice.quote}"). The document outlives the conversation and is read by people `
          + "who were not in it. Rewrite the complete block as facts about the analysis, with "
          + "no claims about what you did, the user's installation, or what you can do next.");
      }
      const recipe = SECOND_PERSON.find((pattern) => pattern.re.test(cleaned.text));
      if (recipe) {
        throw repair(`${ordinal(index)} addresses the user in recipe/chat register `
          + `("${recipe.quote}"). Rewrite it as standalone document prose that names the `
          + "method, function, object, analyst, or reader instead of the current conversation.");
      }
      if (UI_REGISTER.test(cleaned.text)) {
        throw repair(`${ordinal(index)} uses notebook UI/conversation vocabulary such as `
          + '"this/that/last block". In saved prose, call runnable code an R chunk and identify '
          + "the step, object or chunk explicitly instead of referring to a vanished screen position.");
      }
      if (PASTED_VALUE.test(cleaned.text)) {
        lints.push(`${ordinal(index)} contains printed R output pasted into prose. The `
          + "notebook runs the chunk and shows the real result; a copy goes stale the "
          + "moment the data changes.");
      }
      return { kind: "md", content: cleaned.text };
    }

    if (WRAPPED_IN_FENCE.test(content)) {
      throw repair(`${ordinal(index)} is an r block whose source starts with a Markdown fence. `
        + "Send the R source itself — the notebook supplies the chunk, so ``` lines become "
        + "syntax errors in the user's session.");
    }
    if (CONSOLE_TRANSCRIPT.test(content)) {
      throw repair(`${ordinal(index)} is an r block holding a console TRANSCRIPT — lines `
        + "beginning `> ` or `[1] `. R cannot parse those; send the source alone and let the "
        + "notebook produce the output by running it.");
    }
    SIDE_EFFECTS.forEach(({ re, what }) => {
      if (re.test(content)) {
        lints.push(`${ordinal(index)} ${what}. The user reviews this block before it is `
          + "theirs, but do not include it unless they asked for it — a chunk should compute, "
          + "not change the machine it runs on.");
      }
    });
    if (ABSOLUTE_PATH.test(content)) {
      lints.push(`${ordinal(index)} names an absolute path. Use a project-relative one, or the `
        + "document stops working on any other machine — including the author's, tomorrow.");
    }

    return { kind: "rcode", content };
  }).filter(Boolean);

  // Everything was chat and nothing was analysis. Now it IS a refusal — there
  // is no document left to insert, and silently adding nothing while reporting
  // success would be the worst of the three options.
  if (!blocks.length) {
    throw repair(`${tool} received nothing but chat — banners and notes about the notebook's `
      + "own controls, with no analysis in any block. Send the prose and the R itself.");
  }

  // Cross-block, so it cannot be checked while walking one block at a time.
  // knitr errors on a duplicate label by default, which makes the whole
  // document unrenderable — the failure surfaces at Render, far from the
  // transaction that caused it, so it is refused here instead.
  const labels = new Map();
  blocks.forEach((block, index) => {
    if (block.kind !== "rcode") return;
    const found = HASH_PIPE_LABEL.exec(block.content);
    if (!found) return;
    const label = found[1];
    if (labels.has(label)) {
      throw repair(`${ordinal(index)} reuses the chunk label "${label}", already used by `
        + `${ordinal(labels.get(label))}. knitr refuses a document with duplicate labels, so `
        + "this would fail at render. Give each chunk its own descriptive label.");
    }
    labels.set(label, index);
  });

  return { blocks, lints };
}

export default {
  CONTRACT, SCHEMA_VERSION, MAX_BLOCKS, BLOCK_KINDS, AUTHORING_RULES,
  authoringInstructions, normalizeBlocks,
};
