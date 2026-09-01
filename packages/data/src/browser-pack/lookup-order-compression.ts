import { createHash } from 'node:crypto';
import type { AnalyzerSupportGeneratedSource } from './analyzer-support.js';

/** Pure semantic row consumed by the query-free lookup-order compressor. */
export interface LookupOrderRow {
  rootSeq: number;
  firstAlias: number | null;
  secondAlias: number | null;
  route: 'kana' | 'kanji';
  surface: string;
  rank: number;
  physicalClasses: number;
  locatedClasses: number;
  ambiguousSurfaces: number;
  loadedPatches: number;
}

interface LookupOrderLocator {
  readonly rootSeq: number;
  readonly firstAlias: number | null;
  readonly secondAlias: number | null;
}

class LookupOrderEquivalence {
  readonly #parents = new Map<string, string>();
  readonly #sizes = new Map<string, number>();

  add(value: string): void {
    if (this.#parents.has(value)) return;
    this.#parents.set(value, value);
    this.#sizes.set(value, 1);
  }

  find(value: string): string {
    const parent = this.#parents.get(value);
    if (parent === undefined) throw new Error(`Unknown lookup-order locator ${value}`);
    if (parent === value) return value;
    const root = this.find(parent);
    this.#parents.set(value, root);
    return root;
  }

  union(left: string, right: string): void {
    let leftRoot = this.find(left);
    let rightRoot = this.find(right);
    if (leftRoot === rightRoot) return;
    const leftSize = this.#sizes.get(leftRoot)!;
    const rightSize = this.#sizes.get(rightRoot)!;
    // Size keeps paths shallow; the semantic key makes an equal-size union
    // independent of SQL/input iteration order.
    if (leftSize < rightSize || (leftSize === rightSize && leftRoot > rightRoot)) {
      [leftRoot, rightRoot] = [rightRoot, leftRoot];
    }
    this.#parents.set(rightRoot, leftRoot);
    this.#sizes.set(leftRoot, this.#sizes.get(leftRoot)! + this.#sizes.get(rightRoot)!);
  }

  values(): IterableIterator<string> {
    return this.#parents.keys();
  }
}


function compareText(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

function compareLookupOrderLocator(
  left: LookupOrderLocator,
  right: LookupOrderLocator
): number {
  return left.rootSeq - right.rootSeq
    || (left.firstAlias ?? -1) - (right.firstAlias ?? -1)
    || (left.secondAlias ?? -1) - (right.secondAlias ?? -1);
}

function lookupOrderLocatorKey(value: LookupOrderLocator): string {
  return `${value.rootSeq}\u0000${value.firstAlias ?? -1}\u0000${value.secondAlias ?? -1}`;
}

export function compileLookupOrders(
  rows: readonly LookupOrderRow[],
  aliasCount: number,
  expectedPatchCount: number
): {
  readonly values: AnalyzerSupportGeneratedSource['lookupOrders'];
  readonly sourceRows: number;
  readonly sourceSha256: string;
  readonly surfaces: number;
  readonly physicalClasses: number;
  readonly equivalenceClasses: number;
  readonly components: number;
  readonly cyclicComponents: number;
  readonly edges: number;
  readonly maxRank: number;
  readonly sha256: string;
  readonly exceptions: AnalyzerSupportGeneratedSource['lookupOrderExceptions'];
  readonly exceptionClasses: number;
  readonly exceptionLocators: number;
} {
  if (rows.length === 0 || rows.some(row => row.loadedPatches !== expectedPatchCount)) {
    throw new Error(
      `Generated SQL loaded an incomplete manual-patch projection; expected ${expectedPatchCount}`
    );
  }

  const physicalClasses = rows[0]!.physicalClasses;
  const locatedClasses = rows[0]!.locatedClasses;
  const ambiguousSurfaces = rows[0]!.ambiguousSurfaces;
  if (physicalClasses !== locatedClasses || rows.some(value =>
    value.physicalClasses !== physicalClasses
    || value.locatedClasses !== locatedClasses
    || value.ambiguousSurfaces !== ambiguousSurfaces)) {
    throw new Error(
      `Lookup-order physical coverage failed: ${locatedClasses}/${physicalClasses} classes`
    );
  }

  const locatorByKey = new Map<string, LookupOrderLocator>();
  const surfaces = new Map<string, Map<number, Set<string>>>();
  const surfaceLocators = new Map<string, Map<string, number>>();
  for (const value of rows) {
    if (!Number.isSafeInteger(value.rootSeq) || value.rootSeq <= 0
      || !Number.isSafeInteger(value.rank) || value.rank < 0
      || (value.firstAlias === null && value.secondAlias !== null)
      || (value.route !== 'kana' && value.route !== 'kanji')
      || value.surface.length === 0
      || (value.firstAlias !== null && (value.firstAlias < 0 || value.firstAlias >= aliasCount))
      || (value.secondAlias !== null && (value.secondAlias < 0 || value.secondAlias >= aliasCount))) {
      throw new Error(`Invalid semantic lookup-order locator ${JSON.stringify(value)}`);
    }
    const locator: LookupOrderLocator = {
      rootSeq: value.rootSeq,
      firstAlias: value.firstAlias,
      secondAlias: value.secondAlias
    };
    const locatorKey = lookupOrderLocatorKey(locator);
    locatorByKey.set(locatorKey, locator);
    const surfaceKey = `${value.route}\u0000${value.surface}`;
    const priorRanks = surfaceLocators.get(surfaceKey) ?? new Map<string, number>();
    const priorRank = priorRanks.get(locatorKey);
    if (priorRank !== undefined && priorRank !== value.rank) {
      throw new Error(
        `Lookup-order locator maps to physical classes ${priorRank} and ${value.rank} on ${JSON.stringify(surfaceKey)}`
      );
    }
    priorRanks.set(locatorKey, value.rank);
    surfaceLocators.set(surfaceKey, priorRanks);
    const classes = surfaces.get(surfaceKey) ?? new Map<number, Set<string>>();
    const locators = classes.get(value.rank) ?? new Set<string>();
    locators.add(locatorKey);
    classes.set(value.rank, locators);
    surfaces.set(surfaceKey, classes);
  }

  let coveredClasses = 0;
  const equivalence = new LookupOrderEquivalence();
  for (const [surface, classes] of surfaces) {
    const ranks = [...classes.keys()].sort((left, right) => left - right);
    if (ranks.length < 2 || ranks.some((rank, index) => rank !== index)) {
      throw new Error(`Lookup-order ranks are not dense for ${JSON.stringify(surface)}`);
    }
    coveredClasses += ranks.length;
    for (const rank of ranks) {
      const locators = [...classes.get(rank)!];
      if (locators.length === 0) {
        throw new Error(`Lookup-order physical class ${rank} is empty for ${JSON.stringify(surface)}`);
      }
      for (const locator of locators) equivalence.add(locator);
      for (let index = 1; index < locators.length; index++) {
        equivalence.union(locators[0]!, locators[index]!);
      }
    }
  }
  if (surfaces.size !== ambiguousSurfaces || coveredClasses !== physicalClasses) {
    throw new Error(
      `Lookup-order coverage disagrees: ${surfaces.size}/${ambiguousSurfaces} surfaces, `
        + `${coveredClasses}/${physicalClasses} classes`
    );
  }
  const sourceProjection = createHash('sha256');
  let sourceProjectionRows = 0;
  for (const [surfaceKey, classes] of [...surfaces].sort((left, right) =>
    compareText(left[0], right[0]))) {
    const delimiter = surfaceKey.indexOf('\u0000');
    const route = surfaceKey.slice(0, delimiter);
    const surface = surfaceKey.slice(delimiter + 1);
    for (const [rank, locators] of [...classes].sort((left, right) => left[0] - right[0])) {
      for (const locatorKey of [...locators].sort((left, right) =>
        compareLookupOrderLocator(locatorByKey.get(left)!, locatorByKey.get(right)!))) {
        const locator = locatorByKey.get(locatorKey)!;
        sourceProjection.update(JSON.stringify([
          route, surface, rank, locator.rootSeq,
          locator.firstAlias ?? -1, locator.secondAlias ?? -1
        ]) + '\n');
        sourceProjectionRows++;
      }
    }
  }
  if (sourceProjectionRows !== rows.length) {
    throw new Error(
      `Lookup-order semantic source contains ${rows.length - sourceProjectionRows} duplicate row(s)`
    );
  }

  const membersByRoot = new Map<string, string[]>();
  for (const locator of equivalence.values()) {
    const root = equivalence.find(locator);
    const members = membersByRoot.get(root) ?? [];
    members.push(locator);
    membersByRoot.set(root, members);
  }
  const quotient = [...membersByRoot.entries()].map(([root, members]) => {
    members.sort((left, right) => compareLookupOrderLocator(
      locatorByKey.get(left)!, locatorByKey.get(right)!
    ));
    return { root, members, canonical: locatorByKey.get(members[0]!)! };
  }).sort((left, right) => compareLookupOrderLocator(left.canonical, right.canonical));
  const nodeByRoot = new Map(quotient.map((value, index) => [value.root, index]));
  const nodeForLocator = (locator: string): number => nodeByRoot.get(equivalence.find(locator))!;
  const adjacencySets = Array.from({ length: quotient.length }, () => new Set<number>());
  let duplicateSurfaceClasses = 0;
  let selfEdges = 0;
  const orderedSurfaceNodes = new Map<string, number[]>();
  for (const [surface, classes] of [...surfaces].sort((left, right) =>
    compareText(left[0], right[0]))) {
    const nodes = [...classes.entries()]
      .sort((left, right) => left[0] - right[0])
      .map(([, locators]) => {
        const values = [...locators].map(nodeForLocator);
        if (new Set(values).size !== 1) {
          throw new Error(`Lookup-order equivalence split one physical class on ${JSON.stringify(surface)}`);
        }
        return values[0]!;
      });
    if (new Set(nodes).size !== nodes.length) duplicateSurfaceClasses++;
    for (let index = 1; index < nodes.length; index++) {
      const from = nodes[index - 1]!;
      const to = nodes[index]!;
      if (from === to) selfEdges++;
      else adjacencySets[from]!.add(to);
    }
    orderedSurfaceNodes.set(surface, nodes);
  }
  if (duplicateSurfaceClasses !== 0 || selfEdges !== 0) {
    throw new Error(
      `Lookup-order quotient has ${duplicateSurfaceClasses} duplicate surface classes and ${selfEdges} self edges`
    );
  }

  const reverse = Array.from({ length: quotient.length }, () => [] as number[]);
  let edgeCount = 0;
  const adjacency = adjacencySets.map((values, from) => {
    const result = [...values].sort((left, right) => left - right);
    edgeCount += result.length;
    for (const to of result) reverse[to]!.push(from);
    return result;
  });

  // Explicit iterative Kosaraju audit keeps a corrupt/cyclic source from being
  // hidden behind the later topological-rank failure.
  const seen = new Uint8Array(quotient.length);
  const finish: number[] = [];
  for (let start = 0; start < quotient.length; start++) {
    if (seen[start]) continue;
    const stack = [start];
    while (stack.length > 0) {
      const encoded = stack.pop()!;
      if (encoded < 0) {
        finish.push(~encoded);
        continue;
      }
      if (seen[encoded]) continue;
      seen[encoded] = 1;
      stack.push(~encoded);
      const next = adjacency[encoded]!;
      for (let index = next.length - 1; index >= 0; index--) {
        if (!seen[next[index]!]) stack.push(next[index]!);
      }
    }
  }
  seen.fill(0);
  const componentByNode = new Int32Array(quotient.length);
  componentByNode.fill(-1);
  const componentMembers: number[][] = [];
  let cyclicComponents = 0;
  for (let index = finish.length - 1; index >= 0; index--) {
    const start = finish[index]!;
    if (seen[start]) continue;
    const component = componentMembers.length;
    const members: number[] = [];
    const stack = [start];
    seen[start] = 1;
    while (stack.length > 0) {
      const node = stack.pop()!;
      componentByNode[node] = component;
      members.push(node);
      for (const prior of reverse[node]!) {
        if (seen[prior]) continue;
        seen[prior] = 1;
        stack.push(prior);
      }
    }
    if (members.length > 1) cyclicComponents++;
    componentMembers.push(members);
  }
  if (componentMembers.reduce((sum, values) => sum + values.length, 0) !== quotient.length) {
    throw new Error(
      `Lookup-order SCC traversal covered an incomplete quotient`
    );
  }

  const componentAdjacencySets = Array.from(
    { length: componentMembers.length },
    () => new Set<number>()
  );
  for (let from = 0; from < adjacency.length; from++) {
    const fromComponent = componentByNode[from]!;
    for (const to of adjacency[from]!) {
      const toComponent = componentByNode[to]!;
      if (fromComponent !== toComponent) componentAdjacencySets[fromComponent]!.add(toComponent);
    }
  }
  const componentAdjacency = componentAdjacencySets.map(values => [...values].sort((a, b) => a - b));
  const indegree = new Uint32Array(componentMembers.length);
  for (const next of componentAdjacency) for (const node of next) indegree[node]++;
  const ready: number[] = [];
  for (let index = 0; index < indegree.length; index++) {
    if (indegree[index] === 0) ready.push(index);
  }
  const componentLevels = new Uint8Array(componentMembers.length);
  let visited = 0;
  let maxRank = 0;
  while (ready.length > 0) {
    const node = ready.pop()!;
    visited++;
    for (const next of componentAdjacency[node]!) {
      const rank = componentLevels[node]! + 1;
      if (rank > 0x3f) throw new Error('Lookup-order global rank exceeds six bits');
      if (rank > componentLevels[next]!) componentLevels[next] = rank;
      maxRank = Math.max(maxRank, componentLevels[next]!);
      if (--indegree[next] === 0) ready.push(next);
    }
  }
  if (visited !== componentMembers.length) {
    throw new Error(`Lookup-order Kahn traversal covered ${visited}/${componentMembers.length} SCCs`);
  }
  const levels = new Uint8Array(quotient.length);
  for (let node = 0; node < quotient.length; node++) {
    levels[node] = componentLevels[componentByNode[node]!]!;
  }

  const exceptionSurfaces: string[] = [];
  for (const [surface, nodes] of orderedSurfaceNodes) {
    let exact = true;
    for (let index = 1; index < nodes.length; index++) {
      if (levels[nodes[index - 1]!]! >= levels[nodes[index]!]!) {
        exact = false;
        break;
      }
    }
    if (!exact) exceptionSurfaces.push(surface);
  }

  let exceptionClasses = 0;
  let exceptionLocators = 0;
  const exceptions = exceptionSurfaces.sort(compareText).map(surfaceKey => {
    const delimiter = surfaceKey.indexOf('\u0000');
    const route = surfaceKey.slice(0, delimiter);
    const surface = surfaceKey.slice(delimiter + 1);
    if ((route !== 'kana' && route !== 'kanji') || surface.length === 0) {
      throw new Error(`Invalid lookup-order exception key ${JSON.stringify(surfaceKey)}`);
    }
    const exceptionRoute: 'kana' | 'kanji' = route;
    const classes = surfaces.get(surfaceKey)!;
    exceptionClasses += classes.size;
    const orders = [] as Array<LookupOrderLocator & { rank: number }>;
    for (const [rank, locators] of [...classes].sort((a, b) => a[0] - b[0])) {
      for (const locatorKey of [...locators].sort((left, right) =>
        compareLookupOrderLocator(locatorByKey.get(left)!, locatorByKey.get(right)!))) {
        const locator = locatorByKey.get(locatorKey)!;
        orders.push({ ...locator, rank });
      }
    }
    exceptionLocators += orders.length;
    return { route: exceptionRoute, surface, orders };
  });

  const values = [...locatorByKey.entries()].map(([key, locator]) => ({
    ...locator,
    rank: levels[nodeForLocator(key)]!
  })).sort((left, right) => compareLookupOrderLocator(left, right));
  const projection = createHash('sha256');
  // This JSON-lines spelling is part of the release-lock ABI. Changing only
  // its punctuation changes the provenance digest even when every rank and
  // emitted byte is identical, so migrations must be explicit lock updates.
  for (const value of values) {
    projection.update(JSON.stringify([
      'global', value.rootSeq, value.firstAlias ?? -1,
      value.secondAlias ?? -1, value.rank
    ]) + '\n');
  }
  for (const exception of exceptions) {
    for (const value of exception.orders) {
      projection.update(JSON.stringify([
        'exception', exception.route, exception.surface, value.rootSeq,
        value.firstAlias ?? -1, value.secondAlias ?? -1, value.rank
      ]) + '\n');
    }
  }

  // Final compiler gate emulates the runtime's atomic choice: exact local
  // ranks for an exception surface, SCC-global levels everywhere else.
  const exceptionsBySurface = new Map(exceptions.map(value => [
    `${value.route}\u0000${value.surface}`,
    new Map(value.orders.map(order => [lookupOrderLocatorKey(order), order.rank]))
  ]));
  let replayedSurfaces = 0;
  for (const [surfaceKey, classes] of surfaces) {
    const local = exceptionsBySurface.get(surfaceKey);
    for (const [expectedRank, locators] of [...classes].sort((a, b) => a[0] - b[0])) {
      const ranks = new Set([...locators].map(locator => local
        ? local.get(locator)
        : levels[nodeForLocator(locator)]));
      if (ranks.size !== 1 || ranks.has(undefined)) {
        throw new Error(`Lookup-order runtime replay has incomplete class on ${JSON.stringify(surfaceKey)}`);
      }
      const actualRank = ranks.values().next().value as number;
      if (local && actualRank !== expectedRank) {
        throw new Error(`Lookup-order runtime replay has invalid rank on ${JSON.stringify(surfaceKey)}`);
      }
    }
    const physicalRanks = [...classes.entries()]
      .sort((left, right) => left[0] - right[0])
      .map(([, locators]) => {
        const locator = locators.values().next().value as string;
        return local ? local.get(locator)! : levels[nodeForLocator(locator)]!;
      });
    if (physicalRanks.some((rank, index) => index > 0 && rank <= physicalRanks[index - 1]!)) {
      throw new Error(`Lookup-order runtime ranks do not replay ${JSON.stringify(surfaceKey)}`);
    }
    if (local && local.size !== [...classes.values()].reduce((sum, locators) => sum + locators.size, 0)) {
      throw new Error(`Lookup-order exception is not complete for ${JSON.stringify(surfaceKey)}`);
    }
    replayedSurfaces++;
  }
  if (replayedSurfaces !== ambiguousSurfaces) {
    throw new Error(`Lookup-order runtime replay covered ${replayedSurfaces}/${ambiguousSurfaces} surfaces`);
  }
  return {
    values,
    sourceRows: rows.length,
    sourceSha256: sourceProjection.digest('hex'),
    surfaces: ambiguousSurfaces,
    physicalClasses,
    equivalenceClasses: quotient.length,
    components: componentMembers.length,
    cyclicComponents,
    edges: edgeCount,
    maxRank,
    sha256: projection.digest('hex'),
    exceptions,
    exceptionClasses,
    exceptionLocators
  };
}
