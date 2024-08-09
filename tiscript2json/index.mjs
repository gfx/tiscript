#!/usr/bin/env node
import * as fs from "node:fs";
import * as path from "node:path";
import * as os from "node:os";
import * as ts from "typescript";

function die(msg) {
  console.error(msg);
  process.exit(1);
}

const progname = path.basename(process.argv[1]);

const tsFile = process.argv[2] ?? die("No file provided.");
const TMPDIR = `${os.tmpdir() ?? die("No TMPDIR provided.")}/${progname}`;
fs.mkdirSync(TMPDIR, { recursive: true });
const jsFile = `${TMPDIR}/${path.basename(tsFile).replace(/\.m?tsx?$/, ".js")}`;
const mjsFile = `${TMPDIR}/${path
  .basename(tsFile)
  .replace(/\.m?tsx?$/, ".mjs")}`;
process.on("exit", () => {
  for (const file of [jsFile, mjsFile]) {
    if (fs.existsSync(file)) {
      fs.unlinkSync(file);
    }
  }
});

// load the typescript file
const program = ts.createProgram([tsFile], {
  strict: true,
  noEmitOnError: true,
  target: ts.ScriptTarget.ESNext,
  module: ts.ModuleKind.ESNext,
  outDir: TMPDIR,
});

let emitResult = program.emit();

let allDiagnostics = ts
  .getPreEmitDiagnostics(program)
  .concat(emitResult.diagnostics);

const uniq = new Set();
for (const diagnostic of allDiagnostics) {
  if (uniq.has(diagnostic.code)) {
    continue;
  }
  uniq.add(diagnostic.code);

  if (diagnostic.file) {
    let { line, character } = ts.getLineAndCharacterOfPosition(
      diagnostic.file,
      diagnostic.start
    );
    let message = ts.flattenDiagnosticMessageText(diagnostic.messageText, "\n");
    console.error(
      `${diagnostic.file.fileName}:${line + 1}:${character + 1}: ${message}`
    );
  } else {
    console.error(
      ts.flattenDiagnosticMessageText(diagnostic.messageText, "\n")
    );
  }
}

if (emitResult.emitSkipped) {
  process.exit(1);
} else {
  if (fs.existsSync(jsFile)) {
    fs.renameSync(jsFile, mjsFile);
  }
  const result = await import(mjsFile);
  console.log(JSON.stringify(result, (_key, value) => {
    if (typeof value === "bigint") {
      return value.toString();
    } else {
      return value;
    }
  }, 2));
}
