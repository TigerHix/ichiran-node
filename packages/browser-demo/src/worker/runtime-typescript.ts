import { TypeScriptOracleRuntime } from '@ichiran/core/qualification';
import type { TokenDetails, TokenDetailsOptions } from '@ichiran/core';
import type { InstalledFiles } from './install.js';
import { decodeGzip, randomAccessSource } from './runtime.js';

/** Qualification-only host for the frozen TypeScript differential oracle. */
export async function openTypeScriptAnalyzerRuntime(
  files: InstalledFiles
): Promise<{
  analyze: TypeScriptOracleRuntime['analyze'];
  romanize: TypeScriptOracleRuntime['romanize'];
  entry: TypeScriptOracleRuntime['describe'];
  details(text: string, options: TokenDetailsOptions): Promise<TokenDetails>;
}> {
  const [lexicon, locales] = await Promise.all([
    randomAccessSource(files.lexicon),
    Promise.all(Object.entries(files.locales).map(async ([locale, handle]) => (
      [locale, await randomAccessSource(handle)] as const
    ))).then(entries => Object.fromEntries(entries))
  ]);
  const oracle = await TypeScriptOracleRuntime.open({
    hot: new Uint8Array(await (await files.hot.getFile()).arrayBuffer()),
    lexicon: {
      source: lexicon,
      sha256: files.manifest.lexicon.installedSha256
    },
    locales,
    decodeGzip
  });
  return {
    analyze: oracle.analyze.bind(oracle),
    romanize: oracle.romanize.bind(oracle),
    entry: oracle.describe.bind(oracle),
    details: async () => {
      throw new Error('Token details are provided by the product Rust runtime');
    }
  };
}
