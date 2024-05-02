import type { any, mutable, empty } from "any-ts"

export declare namespace Tree {
  type fromPaths<paths extends Tree.pathable | any.array<Tree.pathable>> =
    mutable<Tree.go<paths>>;

  type pathable<
    type extends empty.path | Tree.nonempty = empty.path | Tree.nonempty
  > = type;
  type nonempty<
    last = any.nonnullable,
    lead extends any.path = any.path
  > = readonly [...lead, last];

  type shift<xs extends Tree.pathable> = xs extends readonly [
    any.index<infer head>,
    ...Tree.pathable<infer tail>
  ]
    ? any.two<head, tail>
    : never;

  type go<paths extends Tree.pathable | any.array<Tree.pathable>>
    = Tree.breadthFirst<paths extends Tree.pathable ? paths : paths[number]>

  type breadthFirst<xs extends Tree.pathable>
    = [xs] extends [any.one] ? xs[0]
    : ({ [e in Tree.shift<xs> as e[0]]: Tree.breadthFirst<e[1]> })
}

export namespace Tree {
  export function fromPaths<const paths extends any.array<pathable>>(
    ...paths: paths
  ): fromPaths<paths>;
  export function fromPaths<const paths extends any.array<pathable>>(
    ...paths: paths
  ) {
    const group = new globalThis.Map();
    for (const path of paths) {
      if (path.length === 1) return path[0];
      if (path.length > 1) {
        const [k, ...p] = path;
        group.get(k)?.push(p) ?? group.set(k, [p]);
      }
    }
    const entries: any.entries = globalThis.Array.from(
      group.entries(),
      ([k, v]) => [k, !globalThis.Array.isArray(v) ? v : fromPaths(...v)]
    );

    return globalThis.Object.fromEntries(entries);
  }
}
