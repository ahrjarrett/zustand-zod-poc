import type { any, Kind, mut, traversable } from "any-ts"
import { z } from "zod"

import { adaptor, z as Z } from "./schema"
import * as object from "./object"
import * as fn from "./function"
import { Tree } from "./tree"
import * as Lens from "./lens"

const isObject = (u: unknown): u is Record<string, any> =>
  typeof u === "object" && u !== null && !Array.isArray(u)
const isPrimitive = (u: unknown): u is any.primitive =>
  ["string", "number", "boolean", "symbol", "bigint"].includes(typeof u) || u == null

type SchemaKind = typeof SchemaKind[keyof typeof SchemaKind]
const SchemaKind = {
  Array: "Array",
  Tuple: "Tuple",
  Object: "Object",
  Leaf: "Leaf",
} as const
const SchemaKindToProp = {
  Array: "element",
  Tuple: "items",
  Object: "shape",
  Leaf: null,
} as const

export const SchemaKindToType = {
  [z.ZodFirstPartyTypeKind.ZodString]: "string",
  [z.ZodFirstPartyTypeKind.ZodNumber]: "number",
  [z.ZodFirstPartyTypeKind.ZodBoolean]: "boolean",
  [z.ZodFirstPartyTypeKind.ZodNull]: "null",
  [z.ZodFirstPartyTypeKind.ZodUndefined]: "undefined",
  [z.ZodFirstPartyTypeKind.ZodArray]: "array",
  [z.ZodFirstPartyTypeKind.ZodObject]: "object",
  [z.ZodFirstPartyTypeKind.ZodTuple]: "tuple",
} as const

export const TypeToSchemaKind = object.invert(SchemaKindToType)

export namespace Context {
  export type empty<S extends Z.any> =
    { schema: S; path: [] }
  export const empty = <S extends Z.any>(schema: S): Context.empty<S> =>
    ({ schema, path: [] })
  export interface Leaf<S extends Z.any = Z.any> { schema: S, path: any.keys }
  export type next<_kind extends SchemaKind, prev extends Context.Leaf, schema, ix extends any.key> = never |
  { schema: schema; path: [...prev['path'], ix] }
  export const next = (kind: SchemaKind, prev: Context.Leaf, schema: any.type, segment?: any.key): Context.Leaf =>
  ({
    schema: kind === SchemaKind.Leaf ? schema : (schema as any)[SchemaKindToProp[kind]],
    path: [...prev.path, ...(segment ? [segment] : [])]
  })
}

export declare namespace Model {
  interface leaf<T, S extends Z.any> {
    value: T,
    schema: S
    errors: any[],
    touched: boolean
  }
}

export namespace Model {
  export const leaf = <S extends Z.any, context extends Context.Leaf<S>>(ctx: context) => ({
    type: SchemaKindToType[Z.typeName.get(ctx.schema)],
    schema: ctx.schema,
    validate: fn.flow(ctx.schema.safeParse, x => x.success),
    path: ctx.path,
    valueLens: Lens.fromPath,
    errors: [],
    touched: false,
  })
  // export const leaf = <T, S extends Z.any>(value: T, schema: S): Model.leaf<T, S> => ({
  //   value,
  //   schema,
  //   errors: [] as any[],
  //   touched: false,
  // })
}

// function generateSchemaLens(path: any.keys) { return Lens.fromPath(generateSchemaPath(path)) }
// function generateSchemaPath(path: any.keys) {
//   let out: mut.array<string> = []
//   for(const p of path.map(globalThis.String)) {
//     if(p === "[]") out.push("element")
//     else if(isBetweenBrackets(p)) out.push("items", removeBrackets(p))
//     else out.push("shape", p)
//   }
//   return out
// }
// function isBetweenBrackets(s: string) { return s.startsWith("[") && s.endsWith("]") }
// function removeBrackets(s: string) { return s.slice("[".length, -("]".length)) }

export namespace Handlers {
  export type Any = {
    [k in keyof Handlers.Leaf]: Kind<1> | ((ctx: Context.Leaf) => unknown);
  };
  export interface Leaf<S = {}, N = {}, B = {}, Nil = unknown, Void = unknown> {
    string(s: Context.Leaf): S;
    number(s: Context.Leaf): N;
    boolean(s: Context.Leaf): B;
    null(s: Context.Leaf): Nil;
    undefined(s: Context.Leaf): Void;
  }

  export type Kinds = { [k in keyof Handlers.Leaf]: Kind<1> }
  export type Fns = { [k in keyof Handlers.Leaf]: (ctx: Context.Leaf) => unknown }

  export interface Identity extends Kind<1> { [-1]: this[0] }

  export type context = { [k in keyof Handlers.Leaf]: Identity };
  export const context = {
    string: (ctx) => ({ ...ctx, schema: Z.typeName.softGet(ctx.schema) }),
    number: (ctx) => ({ ...ctx, schema: Z.typeName.softGet(ctx.schema) }),
    boolean: (ctx) => ({ ...ctx, schema: Z.typeName.softGet(ctx.schema) }),
    null: (ctx) => ({ ...ctx, schema: Z.typeName.softGet(ctx.schema) }),
    undefined: (ctx) => ({ ...ctx, schema: Z.typeName.softGet(ctx.schema) }),
  } satisfies Handlers.Leaf;

  interface LensKind extends Kind<[Context.Leaf]> {
    [-1]
    : [this[0]] extends [Context.Leaf & infer context extends Context.Leaf]
    ? [context] extends [{ path: any.keys & infer path extends any.keys, schema: infer schema extends Z.any }]
    ? [schema] extends [{ _type: infer type extends traversable.by<path> }]
    ? Lens.fromPath<path, type>
    : Lens.fromPath<path, traversable.unfold<schema[Extract<"_type", keyof schema>], path>>
    : never
    : never
  }

  type myschema = typeof myschema
  const myschema = z.object({ a: z.object({ b: z.number(), c: z.string() }), d: z.boolean(), e: z.object({ f: z.string(), g: z.string() }) })

  type __lenskind__ = Kind.apply$<LensKind, [{ path: ["a", "b"], schema: myschema }]>

  // = [H[adaptor[Z.typeName.get<T>]]] extends [infer handler]


  export type lenses = { [k in keyof Handlers.Leaf]: LensKind }
  export const lenses = {
    boolean: (ctx) => Lens.fromPath(...ctx.path),
    null: (ctx) => Lens.fromPath(...ctx.path),
    number: (ctx) => Lens.fromPath(...ctx.path),
    string: (ctx) => Lens.fromPath(...ctx.path),
    undefined: (ctx) => Lens.fromPath(...ctx.path),
  } satisfies Handlers.Leaf

  export type empty = typeof Handlers.empty
  export const empty = {
    boolean: () => false,
    number: () => 0,
    string: () => '',
    null: () => null,
    undefined: () => undefined,
  } satisfies Handlers.Leaf


  export type leaves = typeof Handlers.leaves
  export const leaves = {
    boolean: Model.leaf,
    null: Model.leaf,
    number: Model.leaf,
    string: Model.leaf,
    undefined: Model.leaf,
  } satisfies Handlers.Leaf

  export type defaults = typeof Handlers.defaults
  export const defaults = object.map(
    Handlers.context,
    () => () => ({})
  ) satisfies Handlers.Leaf

  export type touched = typeof Handlers.touched
  export const touched = object.map(
    Handlers.context,
    () => () => false as boolean
  ) satisfies Handlers.Leaf
}

export namespace Parser {
  export type SchemaTag = typeof SchemaTag
  export const SchemaTag = "SchemaTag"
  export const SchemaType = Symbol.for(SchemaTag)
  export type SchemaType = typeof SchemaType

  export type ArrayType = typeof ArrayType
  export const ArrayType = "ArrayType"
  export type ObjectType = typeof ObjectType
  export const ObjectType = "ObjectType"
  export type TupleType = typeof TupleType
  export const TupleType = "TupleType"

  export type errors<errs extends any.array<Z.issue>>
    = Tree.fromPaths<{ [ix in keyof errs]: [...errs[ix]['path'], errs[ix]] }>

  export function errors<const errs extends any.array<Z.issue>>(errs: errs): Parser.errors<errs>;
  export function errors<const errs extends any.array<Z.issue>>(errs: errs) {
    return Tree.fromPaths(...errs.map((e) => [...e.path, e] as const))
  }

  export type leaf<T extends Z.any.leaf, H extends Handlers.Any, Ctx>
    = [H[adaptor[Z.typeName.get<T>]]] extends [infer handler]
    ? [handler] extends [Kind<1>] ? Kind.apply$<handler, [Ctx]>
    : [handler] extends [any.function] ? ReturnType<handler>
    : handler
    : Ctx
    ;
  export function leaf<H extends Handlers.Leaf, S extends Z.any.leaf>
    (handlers: Partial<H>, ctx: Context.Leaf, s: S) {
    if (Z.leaf.string.is(s))
      return (handlers.string ?? Handlers.defaults.string)(Context.next(SchemaKind.Leaf, ctx, s))
    else if (Z.leaf.number.is(s))
      return (handlers.number ?? Handlers.defaults.number)(Context.next(SchemaKind.Leaf, ctx, s))
    else if (Z.leaf.boolean.is(s))
      return (handlers.boolean ?? Handlers.defaults.boolean)(Context.next(SchemaKind.Leaf, ctx, s))
    else if (Z.leaf.null.is(s))
      return (handlers.null ?? Handlers.defaults.null)(Context.next(SchemaKind.Leaf, ctx, s));
    else if (Z.leaf.undefined.is(s))
      return (handlers.undefined ?? Handlers.defaults.undefined)(Context.next(SchemaKind.Leaf, ctx, s))
    else return fn.exhaustive(s)
  }

  export type parse<S extends Z.any, H extends Handlers.Any, Ctx extends Context.Leaf = Context.empty<S>>
    = [S] extends [Z.any.leaf] ? Parser.leaf<S, H, Ctx>
    : [S] extends [z.ZodArray<infer elements>] ? (
      & { [SchemaType]: ArrayType } // , [-1]: (create: z.infer<elements>) => z.infer<elements> }
      & {
        [ix: number]:
        Parser.parse<elements, H, Context.next<typeof SchemaKind.Array, Ctx, elements, number>>
      }
    ) : [S] extends [z.ZodTuple<infer items>] ? (
      & { [SchemaType]: TupleType }
      & { [ix in keyof items]:
        Parser.parse<items[ix], H, Context.next<typeof SchemaKind.Tuple, Ctx, items[ix], ix>> }
    ) : [S] extends [z.ZodObject<infer shape>] ? (
      & { [SchemaType]: ObjectType }
      & { [k in keyof shape]:
        Parser.parse<shape[k], H, Context.next<typeof SchemaKind.Object, Ctx, shape[k], k & any.key>> }
    ) : never

  export function parse
    <H extends Handlers.Kinds = never>(handlers: Partial<Handlers.Leaf>):
    <S extends Z.any>(s: S) => Parser.parse<S, H, Context.empty<S>>
  export function parse
    <H extends Handlers.Fns = never>(handlers: Partial<H>):
    <S extends Z.any>(s: S) => Parser.parse<S, H, Context.empty<S>>

  /** impl. */
  export function parse(handlers: Partial<Handlers.Leaf>) {
    const go =
      (ctx: Context.Leaf) =>
        (s: Z.any): unknown => {
          if (Z.array.is(s))
            return { ["[]"]: go(Context.next(SchemaKind.Array, ctx, Z.array.get(s), "[]"))(Z.array.get(s)) }
          else if (Z.tuple.is(s))
            return Z.tuple.get(s).map((v, ix) => go(Context.next(SchemaKind.Tuple, ctx, s, ix))(v))
          else if (Z.object.is(s))
            return object.map(Z.object.get(s), (v, k) => go(Context.next(SchemaKind.Object, ctx, s, k))(v))
          else if (Z.leaf.is(s))
            return Parser.leaf(handlers, ctx, s);
          else // TODO: remove type assertion:
            return fn.exhaustive(s as never)
        }
    return (s: Z.any) => go(Context.empty(s))(s);
  }

  export type concat<prev extends any.index, next extends any.index, delimiter extends any.showable = ".">
    = `${prev & any.key}${prev extends "" ? "" : next extends `[${string}` ? "" : delimiter}${next & any.key}`

  export const concat = (prev: any.key, next: any.key, delimiter: any.showable = ".") =>
    `${prev}${`${prev}` === "" ? "" : `${next}`.startsWith("[") ? "" : delimiter}${next}`

  /** TODO: get this working when working on arrays */
  // export type correctLens<tree> = never | (
  //   tree extends Lens.fromPath<infer path, infer schema> // infer schema extends traversable.by<path>>
  //   ? Lens.fromPath<{ [ix in keyof path]: number extends path[ix] ? "[]" : path[ix] }, schema>
  //   : tree
  // )

  type cleanUp<type>
    = [type] extends [any.primitive] ? type
    : [type] extends [any.indexedBy<Parser.SchemaType>]
    ? { [k in Exclude<keyof type, Parser.SchemaType>]: cleanUp<type[k]> }
    : never
    ;

  export type flatten<tree, prev extends any.key = "">
    = [tree] extends [Lens.any] ? [key: prev, lens: tree] // correctLens<tree>]   /** TODO: turn this back on when `correctLens` is fixed */
    : [keyof tree] extends [any.keyof<tree, infer next>]
    ? next extends next
    /** TODO: get Arrays working again */
    // ? tree extends (
    //   & { [Parser.SchemaType]: Parser.ArrayType } 
    //   & infer type extends any.indexedBy<next>
    // ) ? flatten<type[Extract<number, keyof type>], Parser.concat<prev, `[]`, ``>>
    /** TODO: get Tuples working again */
    // : tree extends (
    //   & { [Parser.SchemaType]: Parser.TupleType } 
    //   & infer type extends any.indexedBy<next>
    // ) ? flatten<type[next], Parser.concat<prev, `[${number extends next ? never : next & any.key}]`, ``>>
    ? tree extends (
      & { [Parser.SchemaType]: Parser.ObjectType }
      & infer type extends any.indexedBy<next>
    ) ? flatten<type[next], Parser.concat<prev, next>>
    : { [k in keyof tree]: flatten<tree[k], prev> }
    : never
    : never
    ;

  export function flatten(object: unknown) {
    let entries: mut.entries = []
    const go = (path: any.key) => (x: unknown) => {
      if (Lens.is(x)) entries.push([path, x])
      if (isPrimitive(x)) entries.push([path, x])
      if (Array.isArray(x)) x.forEach((v, ix) => go(concat(path, `[${ix}]`, ""))(v))
      if (isObject(x)) globalThis.Object.entries(x).forEach(([k, v]) => go(concat(path, k))(v))
    }
    void go("")(object)
    return globalThis.Object.fromEntries(entries)
  }

  export type lenses_<S extends Z.any> = Parser.parse<S, Handlers.lenses>
  export const lenses_ = Parser.parse<Handlers.lenses>(Handlers.lenses)
  export type flatLens<tree>
    = flatten<tree> extends any.entry<infer entry>
    ? { [e in entry as e[0]]: e[1] } : never
  export const flatLens
    : <S extends Z.any>(schema: S) => Parser.lenses<S>
    = fn.flow(Parser.lenses_, flatten) as never

  export type lenses<S extends Z.any> = flatLens<Parser.lenses_<S>>
  export const lenses = Parser.flatLens
  export type leaves<S extends Z.any> = Parser.parse<S, Handlers.leaves>
  export const leaves = Parser.parse(Handlers.leaves)
  export type empty<S extends Z.any> = Parser.parse<S, Handlers.empty>
  export const empty = Parser.parse<Handlers.empty>(Handlers.empty)
  export type deriveEmpty<S extends Z.any> = cleanUp<Parser.empty<S>>
  export type context<S extends Z.any> = Parser.parse<S, Handlers.context>
  export const context = Parser.parse<Handlers.context>(Handlers.context)
  export type touched<S extends Z.any> = Parser.parse<S, Handlers.touched>
  export const touched = Parser.parse<Handlers.touched>(Handlers.touched)
}


const dateStruct = z.object({
  mm: z.number(),
  dd: z.number(),
  yyyy: z.number(),
})


// export type flatten<tree, prev extends any.key = "">
//   = [tree] extends [Lens.any] ? [key: prev, lens: correctLens<tree>]
//   : [keyof tree] extends [any.propertyOf<tree, infer next>]
//   ? next extends next
//   ? tree extends (
//     & { [Parser.SchemaType]: Parser.ArrayType }
//     & infer type extends any.indexedBy<next>
//   ) ? flatten<type[Extract<number, keyof type>], Parser.concat<prev, `[]`, ``>>
//   : tree extends (
//     & { [Parser.SchemaType]: Parser.TupleType }
//     & infer type extends any.indexedBy<next>
//   ) ? flatten<type[next], Parser.concat<prev, `[${number extends next ? never : next & any.key}]`, ``>>
//   : tree extends (
//     & { [Parser.SchemaType]: Parser.ObjectType }
//     & infer type extends any.indexedBy<next>
//   ) ? flatten<type[next], Parser.concat<prev, next>>
//   : never.close.distributive<"next">
//   : never.close.inline_var<"next">
//   : never.close.unmatched_expr
//   ;