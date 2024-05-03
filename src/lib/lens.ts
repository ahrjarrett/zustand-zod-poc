import type { Widen, any, Kind, nonempty, traversable, traversal, mut, pathsof } from "any-ts"
import { z } from "zod"

export const uri = "Lens" as const
export type uri = typeof uri

export { sym as symbol }
const sym = Symbol.for(uri)
type sym = typeof sym

export interface Lens<S, A> {
  [sym]: uri
  get(s: S): A
  set(a: A): (s: S) => S
}

type AnyRecord = Record<keyof any, any>
const isObject = (u: unknown): u is Record<string, any> =>
  typeof u === "object" && u !== null && !Array.isArray(u)

export type { Any as any }
type Any = Lens<any, any>

export const is
  : <S, A>(u: unknown) => u is Lens<S, A>
  = (u: unknown): u is never => z.object({
    [sym]: z.literal(uri),
    get: z.function(),
    set: z.function()
  }).safeParse(u).success

export type Has<P extends any.keys, A = any> = traversable.unfold<Widen<A>, P>
export function Has<const K extends any.index>(key: K): <O>(o: O) => o is O & Record<K, unknown>
export function Has<const K extends any.index>(key: K) {
  return (object: {}): object is Record<K, unknown> => isObject(object) && key in object
}


export type get<S, P extends any.keys> = traversal.of<S, P>
export type getter<P extends any.keys, S extends traversable.by<P> = traversable.by<P>> = <const T extends S>(object: T) => get<T, P>
export type setter<P extends any.keys, R extends traversable.by<P> = traversable.by<P>>
  = <const A extends Widen<traversal.of<R, P>>>(a: A)
    => <const S extends R>(s: S)
      => set<S, P, A>

export type indexable<ix extends any.index> = ix extends any.key ? `${ix}` : ix
export type pathable<P extends any.keys> = {
  [ix in keyof P]: indexable<P[ix]>
}
export const pathable: <const P extends any.keys>(path: P) => pathable<P> = (path) =>
  path.map((p) => (typeof p === "number" ? `${p}` : p)) as never


export type set<S, P extends any.keys, A> = set.go<S, pathable<P>, A>
export declare namespace set {
  type go<S, P extends any.keys, A> = P extends nonempty.arrayOf<any.key, infer head, infer tail>
    ? { -readonly [k in keyof S]: [indexable<k>, indexable<head>] extends [indexable<head>, indexable<k>]
      ? go<S[k], tail, A>
      : S[k] }
    : A
}


export declare namespace pathsOf {
  type path = mut.keys
  type paths = mut.array<pathsOf.path>
  type tree = any.primitive | { [x: string]: tree }
  type stack = mut.array<frame>
  type frame = { tree: tree, path: path }
}

export function pathsOf<const T>(object: T): pathsof<T>[] { return pathsOf_(object as never) }
export function createPaths(ps: pathsOf.paths): pathsOf.paths { return ps }
export function createFrame(tree: pathsOf.tree, path: pathsOf.path): pathsOf.frame { return ({ tree, path }) }
export function pathsOf_<const T>(object: T) {
  let stack: pathsOf.stack = [{ tree: object as never, path: [] }]
  let paths = createPaths([])

  while (stack.length > 0) {
    const { tree, path } = stack.pop()!

    if (isObject(tree))
      for (const k in tree)
        stack.push(createFrame(tree[k], [...path, k]))

    else paths.push(path)
  }

  return paths as never
}

export function get<const P extends any.keys>(...path: P): getter<P>
export function get<const P extends any.keys>(path: P): getter<P>
export function get<const P extends any.keys>(...path: [P] | P) {
  return (object: traversable.by<P>) =>
    path.flat().reduce((acc: unknown, key) => (Has(key)(acc) ? acc[key] : void 0), object)
}

export function unsafeSet<const P extends any.keys>(path: P, a: {}) {
  return (s: AnyRecord) => path.reduce((o, k, i) => (i === path.length - 1 ? void (o[k] = a) : o?.[k]), s)
}

export function set<const P extends any.keys>(...path: P): setter<P>
export function set<const P extends any.keys>(path: P): setter<P>
export function set<const P extends any.keys>(...path: [P] | P): unknown {
  return (a: {}) => (s: AnyRecord) => {
    const p: P = path.flat() as never
    const v = get(p)(s as never)
    if (globalThis.Object.is(a, v)) return s
    else {
      const o = { ...s } // globalThis.structuredClone(s)
      unsafeSet(p, a)(o)
      return o
    }
  }
}

export type FromPath<P, S> = Kind.apply$<fromPathKind, [P, S]>
export interface fromPath<P extends any.keys, S extends Has<P> = Has<P>> {
  [sym]: uri,
  get: getter<P, S>
  set: setter<P, S>
}

export function fromPath<const P extends any.keys>(...path: P): fromPath<P> {
  return {
    [sym]: uri,
    get: get(path),
    set: set(path),
  }
}

export interface fromPathKind extends Kind<[P: any.keys, S: object]> {
  [-1]
  : [this[0]] extends [any.keys & infer P extends any.keys]
  ? [this[1]] extends [object & infer S extends Has<P>]
  ? fromPath<P, S>
  : never
  : never
  ;
}
