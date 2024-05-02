import type { any, some, object, to, traversable, traversal } from "any-ts"

export function map<K extends string, A, B>(
  obj: { [k in K]: A },
  fn: (a: A, k: K) => B
): { [k in K]: B };
export function map<K extends string, A, B>(
  obj: { [k in K]: A },
  fn: (a: A, k: K) => B
) {
  let out: Record<string, B> = {};
  for (const k in obj) out[k] = fn(obj[k], k);
  return out;
}

export { objectKeys as keys }
const objectKeys: <const T extends object>(object: T) => any.keysOf<T>
  = globalThis.Object.keys

export namespace nonempty {
  export type keys<T> = never | readonly [keyof T, ...(keyof T)[]]
  export const keys: <const T extends object>(x: T) => nonempty.keys<T>
    = (x, keys = objectKeys(x)) => [keys[0], ...keys.slice(1)]
}

export function softPick<KS extends any.keys>(...keys: KS): <T extends object>(object: T) =>
  { [K in keyof T as K extends KS[number] ? K : never]: T[K] }

export function softPick<KS extends any.keys>(...keys: KS) {
  return <T extends object>(object: T) => {
    let out = {}
    for (const key of keys)
      if (key in object)
        out[key] = object[key]
    return out
  }
}

export const pick
  : <KS extends any.keys>(...keys: KS) => <T extends any.indexedBy<KS[number]>>(object: T) => { [K in KS[number]]: T[K] }
  = softPick as never

declare namespace filter {
  type eval<t> = never | { [k in keyof t]: t[k] }
  export type { object_ as object }
  type object_<t, bound> =
    | never
    | eval<
      filter.valuesSatisfy<t, bound> &
      filter.valuesPartiallySatisfy<t, bound> &
      filter.valuesMightSatisfy<t, bound>
    >

  export type cast<type, invariant, distribute = never> = [distribute] extends [never]
    ? (type extends invariant ? type : never) extends infer narrow
    ? [narrow] extends [never]
    ? invariant
    : narrow
    : never
    : type extends type
    ? filter.cast<type, invariant, never>
    : never

  export type allSatisfy<t, bound> = (
    bound extends bound
    ? (t extends t & bound ? t : never) extends infer out
    ? out
    : never
    : never
  ) extends infer out
    ? [t, out] extends [out, t]
    ? true
    : false
    : never

  export type onlySomeSatisfy<t, bound> = filter.allSatisfy<t, bound> extends true
    ? false
    : [bound extends bound ? (t extends bound ? true : never) : never] extends [never]
    ? false
    : true

  export type mightSatisfy<t, bound> = [t] extends [bound]
    ? never
    : [t extends t ? (bound extends t ? true : never) : never] extends [never]
    ? false
    : true

  export type valuesSatisfy<t, bound> = {
    -readonly [k in keyof t as filter.allSatisfy<t[k], bound> extends true ? k : never]: t[k]
  }

  export type valuesPartiallySatisfy<t, bound> = {
    -readonly [k in keyof t as filter.onlySomeSatisfy<t[k], bound> extends true ? k : never]+?: t[k]
  }

  export type valuesMightSatisfy<t, bound> = {
    -readonly [k in keyof t as filter.mightSatisfy<t[k], bound> extends true ? k : never]+?: filter.cast<
      t[k],
      bound
    >
  }

  export { objectKeys as keys }
  type objectKeys<t, bound extends any.index> =
    | never
    | eval<
      & filter.keysSatisfy<t, bound>
      & filter.keysPartiallySatisfy<t, bound>
      & filter.keysMightSatisfy<t, bound>
    >

  export type keysSatisfy<t, bound extends any.index> = {
    -readonly [k in keyof t as filter.allSatisfy<k, bound> extends true ? k : never]: t[k]
  }
  export type keysPartiallySatisfy<t, bound extends any.index> = {
    -readonly [k in keyof t as filter.onlySomeSatisfy<k, bound> extends true ? k : never]: t[k]
  }
  export type keysMightSatisfy<t, bound extends any.index> = {
    -readonly [k in keyof t as filter.mightSatisfy<k, bound> extends true ? k : never]: t[k]
  }
}

/** @category selectors */
export function filter<A, B>(
  guard: any.typeguard<A, B>,
): <const T extends any.dict<A>>(object: T) => object.filterValues<T, B>

/** @category selectors */
export function filter<A>(
  predicate: some.predicate<A>,
): <const T extends any.dict<A>>(object: T) => object.filterValues<T, A>

/** @category selectors */
export function filter<A, B, const T extends any.dict<A>>(
  object: T,
  guard: any.typeguard<A, B>,
): filter.object<T, B>

/** @category selectors */
export function filter<A, const T extends any.dict<A>>(
  object: T,
  predicate: some.predicate<A>,
): object.filterValues<T, A>

/** impl. */
export function filter<A, B, const T extends any.dict<A>>(
  ...args:
    | [predicate: some.predicate<A>]
    | [guard: any.typeguard<A, B>]
    | [object: T, predicate: some.predicate<A>]
) {
  if (args.length === 1) return (object: T) => filter(object, args[0])
  else {
    const [object, predicate] = args
    const keep = globalThis.Object.entries(object).filter(([k, v]) => predicate(v))
    /**
     * **Optimization:**
     * If applying the filter did not remove any keys, we want to
     * _preserve the original reference_: just return the original object
     */
    if (globalThis.Object.keys(object).length === keep.length) return object
    else return Object.fromEntries(keep)
  }
}

export const entries
  : <const T extends any.object>(object: T) => to.entries<T>[]
  = globalThis.Object.entries

/** @category selectors */
export function filterKeys<A extends any.index, B extends any.index>(
  guard: any.typeguard<A, B>,
): <const T extends any.indexedBy<A>>(object: T) => object.filterKeys<T, B>

/** @category selectors */
export function filterKeys<A extends any.index>(
  predicate: some.predicate<A>,
): <const T extends any.indexedBy<A>>(object: T) => object.filterKeys<T, A>

/** @category selectors */
export function filterKeys<A extends any.index, B extends A, const T extends any.indexedBy<A>>(
  object: T,
  guard: any.typeguard<A, B>,
): object.filterKeys<T, B>

/** @category selectors */
export function filterKeys<A extends any.index, const T extends any.indexedBy<A>>(
  object: T,
  predicate: some.predicate<A>,
): object.filterKeys<T, A>

/** impl. */
export function filterKeys<A extends any.index, B extends A, const T extends any.indexedBy<A>>(
  ...args:
    | [predicate: some.predicate<A>]
    | [guard: any.typeguard<A, B>]
    | [object: T, predicate: some.predicate<A>]
) {
  if (args.length === 1) return (object: T) => filterKeys(object, args[0])
  else {
    const [object, predicate] = args
    const keep = entries(object).filter(([k, v]) => predicate(k as A))

    /**
     * **Optimization:**
     * If applying the filter did not remove any keys, we want to
     * _preserve the original reference_: just return the original object
     */
    if (globalThis.Object.keys(object).length === keep.length) return object
    else return Object.fromEntries(keep)
  }
}

export function fromKeys<KS extends any.keys>(...keys: KS): { [K in KS[number]]: K }
export function fromKeys<KS extends any.keys>(keys: KS): { [K in KS[number]]: K }
export function fromKeys(...keys: [any.keys] | (any.keys)) {
  return keys.flat(1).reduce((acc, key: string | number) => { acc[key] = key; return acc }, {})
}

export function get<K extends any.index>(key: K): <const T extends any.indexedBy<K>>(object: T) => T[K]
export function get<P extends any.path>(...path: P): <const T extends traversable.by<P>>(object: T) => traversal.of<T, P>
export function get<P extends any.path>(...path: P): unknown {
  return <const T extends traversable.by<P>>(object: T) => path.reduce((prev, k) => prev[k as never], object)
}

export function invert<const T extends any.invertible>(object: T): { [K in keyof T as T[K]]: K }
export function invert<const T extends any.invertible>(object: T) {
  return globalThis.Object.fromEntries(globalThis.Object.entries(object).map(([k, v]) => [v, k] as const))
}
