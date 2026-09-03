export type RomanizationName =
  | 'hepburn-basic'
  | 'hepburn-simple'
  | 'hepburn-passport'
  | 'hepburn-traditional'
  | 'hepburn-modified'
  | 'kunrei-siki';

export function joinRomanizedParts(parts: readonly string[]): string {
  let output = '';
  let lastWasSpace = true;
  for (const part of parts) {
    if (part.length === 0) continue;
    if (!lastWasSpace && /[a-zA-Z0-9]/.test(part[0]!)) output += ' ';
    output += part;
    lastWasSpace = /\s/.test(part[part.length - 1]!);
  }
  return output;
}
