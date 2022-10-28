const fs = require("fs");
const path = require("path");

const isalpha = (s) => !/[^0-9a-z\xDF-\xFF]/.test(s);

const grammar = fs.readFileSync(path.join(__dirname, "grammar.ebnf"), "utf8");
const keywords = new Set();
const symbols = new Set();
for (let [terminal] of grammar.matchAll(/"[^"]+"/g)) {
  terminal = terminal.slice(1, -1);
  if (isalpha(terminal)) {
    keywords.add(terminal);
  } else {
    symbols.add(terminal);
  }
}
console.log([...keywords, ...symbols].join("\n"));

