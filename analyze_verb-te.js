// This script analyzes verbて-form patterns to understand how to detect sentence-final position

const examples = [
  "ちょっとまって。",
  "部屋を片付けてね。",
  "助けて！",
  "落ち着いて。",
  "このポスターを貼っておいて。",
  "そこで少し待ってて。すぐに戻るから。",
  "駅まで走っていってて...",  // NOT a request - thinking/incomplete
  "約束を守れなくてすみません。",  // NOT a request - apology
];

console.log("Verbて-request pattern analysis:");
console.log("- Must be verb in te-form");
console.log("- Must be sentence-final (or followed only by sentence-final particles like ね, よ)");
console.log("- Must NOT have auxiliary verbs attached (ている, てある, てしまう, etc.)");
console.log("- Used as casual request meaning 'please do X'");
