#!/usr/bin/env node
// demo.mjs — JS version running phone.n3 + scanner.n3
// Usage:
//   node demo.mjs           # default session S1
//   node demo.mjs S42       # custom session
//
// Requires: EYE on PATH.

import { execFile } from "node:child_process";
import { mkdir, writeFile, readFile, access } from "node:fs/promises";
import { constants as fsConstants } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, resolve, join } from "node:path";

const HERE = dirname(fileURLToPath(import.meta.url));
const SESSION = process.argv[2] || "S1";
const OUTDIR = resolve(HERE, "bus", SESSION);

// ---- helpers ----------------------------------------------------------
const sh = (cmd, args, opts = {}) =>
  new Promise((resolveP, reject) => {
    execFile(cmd, args, { ...opts }, (err, stdout, stderr) => {
      if (err) {
        err.stdout = stdout + "";
        err.stderr = stderr + "";
        reject(err);
      } else {
        resolveP({ stdout: stdout + "", stderr: stderr + "" });
      }
    });
  });

async function existsOnPath(bin) {
  try {
    if (process.platform === "win32") {
      await sh("where", [bin]);
    } else {
      await sh("which", [bin]);
    }
    return true;
  } catch {
    return false;
  }
}

const isoNow = () => new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
const isoPlusHours = (h) => new Date(Date.now() + h * 3600_000).toISOString().replace(/\.\d{3}Z$/, "Z");

// ---- main -------------------------------------------------------------
(async () => {
  // 0) check EYE
  if (!(await existsOnPath("eye"))) {
    console.error("ERROR: EYE not found on PATH. Please install EYE and retry.");
    process.exit(1);
  }

  // 1) input graphs
  await mkdir(OUTDIR, { recursive: true });

  //const CREATED_AT = isoNow();
  //const EXPIRES_AT = isoPlusHours(2);
  //const NOW_TS = isoNow();
  const CREATED_AT = "2025-10-05T11:22:27Z";
  const EXPIRES_AT = "2025-10-05T13:22:27Z";
  const NOW_TS = "2025-10-05T11:22:27Z";

  const files = [
    [
      "profile.ttl",
      `@prefix :       <https://example.org/person#> .
@prefix health: <https://example.org/health#> .

:me  health:householdCondition  health:Diabetes .
`,
    ],
    [
      "context.ttl",
      `@prefix ctx: <https://example.org/context#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

[] ctx:retailer "Delfour" ;
   ctx:device   "self-scanner" ;
   ctx:event    "pick_up_scanner" ;
   ctx:createdAt "${CREATED_AT}"^^xsd:dateTime ;
   ctx:expiresAt "${EXPIRES_AT}"^^xsd:dateTime .
`,
    ],
    [
      "now.ttl",
      `@prefix ex:  <https://example.org/enforce#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
[] ex:now "${NOW_TS}"^^xsd:dateTime .
`,
    ],
    [
      "request.ttl",
      `@prefix odrl: <http://www.w3.org/ns/odrl/2/> .
@prefix req:  <https://example.org/request#> .

req:r1 odrl:action odrl:use ;
      odrl:constraint [
        odrl:leftOperand  odrl:purpose ;
        odrl:rightOperand "shopping_assist"
      ] .
`,
    ],
    [
      "catalog.ttl",
      `@prefix prod:   <https://example.org/product#> .
@prefix schema: <http://schema.org/> .
@prefix xsd:    <http://www.w3.org/2001/XMLSchema#> .

prod:BIS_001 a schema:Product ; schema:name "Classic Tea Biscuits" ; schema:sugarContent "12.0"^^xsd:decimal .
prod:BIS_101 a schema:Product ; schema:name "Low-Sugar Tea Biscuits" ; schema:sugarContent "3.0"^^xsd:decimal .
prod:CHOC_050 a schema:Product ; schema:name "Milk Chocolate Bar"   ; schema:sugarContent "15.0"^^xsd:decimal .
prod:CHOC_150 a schema:Product ; schema:name "85% Dark Chocolate"   ; schema:sugarContent "6.0"^^xsd:decimal .
`,
    ],
    [
      "scan.ttl",
      `@prefix shop: <https://example.org/shop#> .
@prefix prod: <https://example.org/product#> .

[] shop:scannedProduct prod:BIS_001 .
`,
    ],
  ];

  await Promise.all(
    files.map(([name, content]) => writeFile(join(OUTDIR, name), content, "utf8"))
  );

  // 2) PHONE: derive neutral Insight + ODRL policy
  console.log("[phone] deriving neutral insight + policy …");
  const phoneOutPath = join(OUTDIR, "insight_policy.ttl");
  try {
    const { stdout } = await sh("eye", [
      "--quiet",
      "--nope",
      "--pass-only-new",
      join(OUTDIR, "profile.ttl"),
      join(OUTDIR, "context.ttl"),
      resolve(HERE, "phone.n3"),
    ]);
    await writeFile(phoneOutPath, stdout, "utf8");
  } catch (e) {
    console.error("ERROR: EYE failed during phone phase.");
    console.error(e.stderr || e);
    process.exit(2);
  }

  // 3) SCANNER: auth + audit + suggestion
  console.log("[scanner] running enforcement + suggestion …");
  const scanOutPath = join(OUTDIR, "scanner_out.n3");
  try {
    const { stdout } = await sh("eye", [
      "--quiet",
      "--nope",
      "--pass-only-new",
      phoneOutPath,
      join(OUTDIR, "now.ttl"),
      join(OUTDIR, "request.ttl"),
      join(OUTDIR, "catalog.ttl"),
      join(OUTDIR, "scan.ttl"),
      resolve(HERE, "scanner.n3"),
    ]);
    await writeFile(scanOutPath, stdout, "utf8");
  } catch (e) {
    console.error("ERROR: EYE failed during scanner phase.");
    console.error(e.stderr || e);
    process.exit(3);
  }

  // 4) Summary
  const summary = await readFile(scanOutPath, "utf8");
  const decision = /act:outcome\s+"(Allowed|Blocked)"/.exec(summary)?.[1] || "(none)";
  const altLine = summary.match(/shop:suggestedAlternative[^\n]+/);
  const suggestion = altLine ? altLine[0].trim() : "none";

  console.log("\n=== SUMMARY (session %s) ===", SESSION);
  console.log("Created:  %s", CREATED_AT);
  console.log("Expires:  %s", EXPIRES_AT);
  console.log("Now:      %s\n", NOW_TS);
  console.log("- Insight & Policy:   %s", phoneOutPath);
  console.log("- Scanner derivation: %s\n", scanOutPath);
  console.log("Decision: %s", decision);
  console.log("Suggestion: %s\n", suggestion);
})().catch((e) => {
  console.error("Unexpected error:", e);
  process.exit(99);
});

