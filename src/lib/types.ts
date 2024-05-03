import type { any } from "any-ts"

export type Bivariant<type> = never | { _: type }["_"]
export type Contravariant<type> = (_: type) => never
export type evaluate<type> = never | { [k in keyof type]: type[k] }
export type parseInt<type> = type extends `${infer x extends number}` ? x : type

export type Basecase =
  | any.primitive
  | any.array
  | any.function
  | globalThis.RegExp
  | globalThis.Date
  | globalThis.Generator
  | { readonly [Symbol.toStringTag]: string }

export type Partial<T>
  = T extends Basecase ? T
  : { [K in keyof T]+?: Partial<T[K]> }
