import { useSharedEngine } from "./src/rules/bunpro/_test/engine.js";
import { BUNPRO_JLPT4 } from "./src/rules/bunpro/jlpt4/index.js";

const engineGetter = useSharedEngine([BUNPRO_JLPT4]);
const engine = await engineGetter.get();

const sentence = "文化祭の準備をしている生徒：「だいたいでいいから、午前中までにはおわらせておいて。」";
console.log("Testing:", sentence);
console.log("Length:", sentence.length);

const hits = await engine.match(sentence);
console.log("Total hits:", hits.length);
for (const hit of hits) {
  console.log("  - ruleId:", hit.ruleId, "captures:", hit.captures);
}

const daitaiHit = hits.find((h) => h.ruleId === 'だいたい');
if (daitaiHit) {
  console.log("FOUND!");
} else {
  console.log("NOT FOUND");
  const explain = await engine.explainMatch(sentence, 'だいたい');
  console.log("Reason:", explain.reason);
}
