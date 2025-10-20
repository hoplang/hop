const { existsSync, mkdirSync, copyFileSync, chmodSync } = require("fs");
const { join } = require("path");

const PLATFORM_MAP = {
  "darwin-arm64": "@hoplang/darwin-arm64",
  "linux-arm64": "@hoplang/linux-arm64",
  "linux-x64": "@hoplang/linux-x64",
};

const platformKey = `${process.platform}-${process.arch}`;
const platformPackage = PLATFORM_MAP[platformKey];

if (!platformPackage) {
  console.error(`[hop] Unsupported platform: ${platformKey}`);
  console.error(`[hop] Supported platforms: ${Object.keys(PLATFORM_MAP).join(", ")}`);
  process.exit(1);
}

const source = join(__dirname, "..", platformKey, "bin", "hop");
const dest = join(__dirname, "bin", "hop");

if (existsSync(source)) {
  mkdirSync(join(__dirname, "bin"), { recursive: true });
  copyFileSync(source, dest);
  chmodSync(dest, 0o755);
} else {
  console.error(`[hop] Binary not found at: ${source}`);
  console.error(`[hop] Platform package ${platformPackage} may not be installed`);
  process.exit(1);
}
