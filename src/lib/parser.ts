import type { any, Kind, mut, traversable } from "any-ts"
import { z } from "zod"

import { adaptor, z as zod } from "./schema"
import * as object from "./object"
import * as fn from "./function"
import { Tree } from "./tree"
import * as Lens from "./lens"
import type { StoreApi, UseBoundStore } from "zustand"
import { Partial } from "./types"

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
  export type empty<S extends zod.any> =
    { schema: S; path: [] }
  export const empty = <S extends zod.any>(schema: S): Context.empty<S> =>
    ({ schema, path: [] })
  export interface Leaf<S extends zod.any = zod.any> { schema: S, path: any.keys, shadow?: any }
  export type next<_kind extends SchemaKind, prev extends Context.Leaf, schema, ix extends any.key> = never |
  { schema: schema; path: [...prev['path'], ix] }
  export const next = (kind: SchemaKind, prev: Context.Leaf, schema: any.type, segment?: any.key, shadow?: any): Context.Leaf =>
  ({
    schema: kind === SchemaKind.Leaf ? schema : (schema as any)[SchemaKindToProp[kind]],
    path: [...prev.path, ...(segment ? [segment] : [])],
    ...(shadow ? { shadow } : {})
  })
}

export declare namespace Model {
  interface leaf<T, S extends zod.any> {
    value: T,
    schema: S
    errors: any[],
    touched: boolean
  }
}

export namespace Model {
  export const leaf = <S extends zod.any, context extends Context.Leaf<S>>(ctx: context) => ({
    type: SchemaKindToType[zod.typeName.get(ctx.schema)],
    schema: ctx.schema,
    validate: fn.flow(ctx.schema.safeParse, x => x.success),
    path: ctx.path,
    valueLens: Lens.fromPath,
    errors: [],
    touched: false,
  })
  // export const leaf = <T, S extends zod.any>(value: T, schema: S): Model.leaf<T, S> => ({
  //   value,
  //   schema,
  //   errors: [] as any[],
  //   touched: false,
  // })
}

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
    string: (ctx) => ({ ...ctx, schema: zod.typeName.softGet(ctx.schema) }),
    number: (ctx) => ({ ...ctx, schema: zod.typeName.softGet(ctx.schema) }),
    boolean: (ctx) => ({ ...ctx, schema: zod.typeName.softGet(ctx.schema) }),
    null: (ctx) => ({ ...ctx, schema: zod.typeName.softGet(ctx.schema) }),
    undefined: (ctx) => ({ ...ctx, schema: zod.typeName.softGet(ctx.schema) }),
  } satisfies Handlers.Leaf;

  interface LensKind extends Kind<[Context.Leaf]> {
    [-1]
    : [this[0]] extends [Context.Leaf & infer context extends Context.Leaf]
    ? [context] extends [{ path: any.keys & infer path extends any.keys, schema: infer schema extends zod.any }]
    ? [schema] extends [{ _type: infer type extends traversable.by<path> }]
    ? Lens.fromPath<path, type>
    : Lens.fromPath<path, traversable.unfold<schema[Extract<"_type", keyof schema>], path>>
    : never
    : never
  }

  export type StoreLens<target> = never | {
    get(): target
    set(a: target): void
    [Lens.symbol]: Lens.uri
  }

  export interface StoreLensKind extends Kind<[Ctx: Context.Leaf]> {
    [-1]: this[0]["schema"] extends
    | zod.any & infer target extends zod.any
    ? StoreLens<target["_type"]>
    : never
  }


  export type lenses = { [k in keyof Handlers.Leaf]: LensKind }
  export const lenses = {
    boolean: (ctx) => Lens.fromPath(...ctx.path),
    null: (ctx) => Lens.fromPath(...ctx.path),
    number: (ctx) => Lens.fromPath(...ctx.path),
    string: (ctx) => Lens.fromPath(...ctx.path),
    undefined: (ctx) => Lens.fromPath(...ctx.path),
  } satisfies Handlers.Leaf

  function deriveLens<Z extends zod.any = never>():
    (store: StoreApi<Z>) => (ctx: Context.Leaf<z.ZodTypeAny>) => StoreLens<Z>
  function deriveLens<Z extends zod.any>(schema: Z):
    (store: StoreApi<Z>) => (ctx: Context.Leaf<z.ZodTypeAny>) => StoreLens<Z>
  function deriveLens<Z extends zod.any = never>(_?: Z):
    (store: StoreApi<Z>) => (ctx: Context.Leaf<z.ZodTypeAny>) => StoreLens<Z> {
    return (store) => (ctx) => {
      const lens = Lens.fromPath<any>(...ctx.path)
      return {
        ...lens,
        get: () => lens.get(store.getState()),
        set: (a: never) => store.setState((lens.set as Lens.setter<any, any>)(a)),
      }
    }
  }

  export type deriveLenses<Z extends zod.any> = { [k in keyof Handlers.Leaf]: StoreLensKind }
  export const deriveLenses =
    <Z extends zod.any>() => (store: StoreApi<Z>) => object.map(
      Handlers.context,
      () => deriveLens<Z>()(store)
    ) satisfies Handlers.Leaf

  export type empty = typeof Handlers.empty
  export const empty = {
    boolean: (ctx): boolean => ctx.shadow ?? false,
    number: (ctx): number => ctx.shadow ?? 0,
    string: (ctx): string => ctx.shadow ?? "",
    null: (ctx): null => ctx.shadow ?? null,
    undefined: (ctx): undefined => ctx.shadow ?? undefined,
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

  export type errors<errs extends any.array<zod.issue>>
    = Tree.fromPaths<{ [ix in keyof errs]: [...errs[ix]['path'], errs[ix]] }>

  export function errors<const errs extends any.array<zod.issue>>(errs: errs): Parser.errors<errs>;
  export function errors<const errs extends any.array<zod.issue>>(errs: errs) {
    return Tree.fromPaths(...errs.map((e) => [...e.path, e] as const))
  }

  export type leaf<T extends zod.any.leaf, H extends Handlers.Any, Ctx>
    = [H[adaptor[zod.typeName.get<T>]]] extends [infer handler]
    ? [handler] extends [Kind<1>] ? Kind.apply$<handler, [Ctx]>
    : [handler] extends [any.function] ? ReturnType<handler>
    : handler
    : Ctx
    ;
  export function leaf<H extends Handlers.Leaf, S extends zod.any.leaf>
    (handlers: Partial<H>, ctx: Context.Leaf, s: S, shadow?: any) {
    const shadowSpread = ctx.shadow ? { shadow: ctx.shadow } : {}
    if (zod.leaf.string.is(s))
      return (handlers.string ?? Handlers.defaults.string)(Context.next(SchemaKind.Leaf, ctx, s, void 0, ctx.shadow))
    else if (zod.leaf.number.is(s))
      return (handlers.number ?? Handlers.defaults.number)(Context.next(SchemaKind.Leaf, ctx, s, void 0, ctx.shadow))
    else if (zod.leaf.boolean.is(s))
      return (handlers.boolean ?? Handlers.defaults.boolean)(Context.next(SchemaKind.Leaf, ctx, s, void 0, ctx.shadow))
    else if (zod.leaf.null.is(s))
      return (handlers.null ?? Handlers.defaults.null)(Context.next(SchemaKind.Leaf, ctx, s, void 0, ctx.shadow));
    else if (zod.leaf.undefined.is(s))
      return (handlers.undefined ?? Handlers.defaults.undefined)(Context.next(SchemaKind.Leaf, ctx, s, void 0, ctx.shadow))
    else return fn.exhaustive(s)
  }

  export type parse<S extends zod.any, H extends Handlers.Any, Ctx extends Context.Leaf = Context.empty<S>>
    = [S] extends [zod.any.leaf] ? Parser.leaf<S, H, Ctx>
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

  const nextShadow = (prop: any.key, shadow: any) => isObject(shadow) && prop in shadow ? shadow[prop] : void 0

  export function parse
    <H extends Handlers.Kinds = never>(handlers: Partial<Handlers.Leaf>, shadow?: any):
    <S extends zod.any>(s: S) => Parser.parse<S, H, Context.empty<S>>
  export function parse
    <H extends Handlers.Fns = never>(handlers: Partial<H>, shadow?: any):
    <S extends zod.any>(s: S) => Parser.parse<S, H, Context.empty<S>>
  /** impl. */
  export function parse(handlers: Partial<Handlers.Leaf>, shadow?: any) {
    const go =
      (ctx: Context.Leaf, shadow?: any) =>
        (s: zod.any): unknown => {
          if (zod.array.is(s))
            /** TODO: Figure out how you want to  turn on "shadow" for array schemas */
            return { ["[]"]: go(Context.next(SchemaKind.Array, ctx, zod.array.get(s), "[]", void 0), void 0)(zod.array.get(s)) }
          else if (zod.tuple.is(s))
            return zod.tuple.get(s).map((v, ix) => go(Context.next(SchemaKind.Tuple, ctx, s, ix, nextShadow(ix, shadow)), nextShadow(ix, shadow))(v))
          else if (zod.object.is(s))
            return object.map(zod.object.get(s), (v, k) => go(Context.next(SchemaKind.Object, ctx, s, k, nextShadow(k, shadow)), nextShadow(k, shadow))(v))
          else if (zod.leaf.is(s))
            return Parser.leaf(handlers, ctx, s)
          else // TODO: remove type assertion:
            return fn.exhaustive(s as never)
        }
    return (s: zod.any) => go(Context.empty(s), shadow)(s)
  }

  export type concat<prev extends any.index, next extends any.index, delimiter extends any.showable = ".">
    = `${prev & any.key}${prev extends "" ? "" : next extends `[${string}` ? "" : delimiter}${next & any.key}`

  export const concat = (prev: any.key, next: any.key, delimiter: any.showable = ".") =>
    `${prev}${`${prev}` === "" ? "" : `${next}`.startsWith("[") ? "" : delimiter}${next}`

  /** TODO: Will need this when you switch to supporting arrays */
  // export type correctLens<tree> = never | (
  //   tree extends Lens.fromPath<infer P, infer S>
  //   ? { [ix in keyof P]: number extends P[ix] ? "[]" : P[ix] } extends
  //   | infer Q extends { [ix in keyof P]: number extends P[ix] ? "[]" : P[ix] }
  //   ? Lens.fromPath<Q, traversable.by<Q>>
  //   : tree
  //   : never
  // )

  export type flatten<tree, basecase, prev extends any.key = "">
    = [tree] extends [basecase] ? [key: prev, lens: tree]
    : [keyof tree] extends [any.keyof<tree, infer next>]
    ? next extends next
    /** TODO: turn on after adding support for tuples and arrays */
    // ? tree extends (
    //   & { [Parser.SchemaType]: Parser.ArrayType }
    //   & infer type extends any.indexedBy<next>
    // ) ? flatten<type[Extract<number, keyof type>], Parser.concat<prev, `[]`, ``>>
    // : tree extends (
    //   & { [Parser.SchemaType]: Parser.TupleType }
    //   & infer type extends any.indexedBy<next>
    // ) ? flatten<type[next], Parser.concat<prev, `[${number extends next ? never : next & any.key}]`, ``>>
    ? tree extends (
      & { [Parser.SchemaType]: Parser.ObjectType }
      & infer type extends any.indexedBy<next>
    ) ? flatten<type[next], basecase, Parser.concat<prev, next>>
    : tree
    : never
    : never
    ;

  export function flatten(object: unknown): Record<string, any> {
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

  export type leaves<S extends zod.any> = Parser.parse<S, Handlers.leaves>
  export const leaves = Parser.parse(Handlers.leaves)

  export type context<S extends zod.any> = Parser.parse<S, Handlers.context>
  export const context = Parser.parse<Handlers.context>(Handlers.context)
  export type touched<S extends zod.any> = Parser.parse<S, Handlers.touched>
  export const touched = Parser.parse<Handlers.touched>(Handlers.touched)

  export type deriveEmpty<S extends zod.any> = internal.cleanUp<Parser.parse<S, Handlers.empty>>
  export const deriveEmpty = <S extends z.ZodTypeAny>(s: S, initialValues?: Partial<S["_type"]>) =>
    Parser.parse<Handlers.empty>(Handlers.empty, initialValues)(s)

  export type deriveLenses<Z extends zod.any> = never | { [e in internal.deriveLenses<Z> as e[0]]: e[1] }
  export const deriveLenses = <Z extends zod.any>(schema: Z) =>
    (store: UseBoundStore<StoreApi<Z>>): Parser.deriveLenses<Z> => {
      const lenses = flatten(internal.deriveLenses(schema))
      return object.map(
        lenses,
        (l: Lens.fromPath<any, any>) => ({
          ...l,
          get: () => store(l.get),
          set: (a: any) => store.setState((l.set as Lens.setter<any, any>)(a))
        })
      ) as never
    }
}

declare namespace internal {
  type deriveLenses<Z extends zod.any>
    = Extract<Parser.flatten<
      Parser.parse<Z, Handlers.deriveLenses<Z>>,
      Handlers.StoreLens<any>
    >, any.entry>
  type deriveEmpty<S extends zod.any> = cleanUp<Parser.deriveEmpty<S>>

  type cleanUp<type>
    = [type] extends [any.primitive] ? type
    : [type] extends [any.indexedBy<Parser.SchemaType>]
    ? { [k in Exclude<keyof type, Parser.SchemaType>]: cleanUp<type[k]> }
    : never
    ;
}
namespace internal {
  export const deriveLenses = Parser.parse<Handlers.lenses>(Handlers.lenses)
}

// = [tree] extends [Lens.any] ? [key: prev, lens: correctLens<tree>]
// : [keyof tree] extends [any.propertyOf<tree, infer next>]
// ? next extends next
// ? tree extends (
//   & { [Parser.SchemaType]: Parser.ArrayType }
//   & infer type extends any.indexedBy<next>
// ) ? flatten<type[Extract<number, keyof type>], Parser.concat<prev, `[]`, ``>>
// : tree extends (
//   & { [Parser.SchemaType]: Parser.TupleType }
//   & infer type extends any.indexedBy<next>
// ) ? flatten<type[next], Parser.concat<prev, `[${number extends next ? never : next & any.key}]`, ``>>
// : tree extends (
//   & { [Parser.SchemaType]: Parser.ObjectType }
//   & infer type extends any.indexedBy<next>
// ) ? flatten<type[next], Parser.concat<prev, next>>
// : 1
// // never.close.distributive<"next">
// : 2 // never.close.inline_var<"next">
// : 3 // never.close.unmatched_expr
// ;
// export type lenses_<S extends zod.any> = Parser.parse<S, Handlers.lenses>
// export const lenses_ = Parser.parse<Handlers.lenses>(Handlers.lenses)
// // export type flatLens<tree>
// //   = flatten<tree> extends any.entry<infer entry>
//   ? { [e in entry as e[0]]: e[1] } : never
// export const flatLens
//   : <S extends zod.any>(schema: S) => Parser.lenses<S>
//   = fn.flow(Parser.lenses_, flatten) as never
// export type lenses<S extends zod.any> = { [e in flatten<Parser.lenses_<S>, Lens.any>> as e[0]]: }
// export const lenses = Parser.flatLens
// export type flatten<tree, prev extends any.key = "">
//   = [tree] extends [Lens.any] ? [key: prev, lens: tree] // correctLens<tree>]   /** TODO: turn this back on when `correctLens` is fixed */
//   : [keyof tree] extends [any.propertyOf<tree, infer next>]
//   // next extends next
//   ? next
//   : never
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
// ? tree extends (
//   & { [Parser.SchemaType]: Parser.ObjectType }
//   & infer type extends any.indexedBy<next>
// ) ? flatten<type[next], Parser.concat<prev, next>>
// : { [k in keyof tree]: flatten<tree[k], prev> }
// : never
// : never
// ;
// {
//   boolean: (ctx: Context.Leaf<z.ZodTypeAny>) => StoreLens<Z>;
//   null: (ctx: Context.Leaf<z.ZodTypeAny>) => StoreLens<Z>;
//   number: (ctx: Context.Leaf<z.ZodTypeAny>) => StoreLens<Z>;
//   string: (ctx: Context.Leaf<z.ZodTypeAny>) => StoreLens<Z>;
//   undefined: (ctx: Context.Leaf<z.ZodTypeAny>) => StoreLens<Z>;
// }
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


// import type { any, Kind, mut, traversable } from "any-ts"
// import { z } from "zod"

// import { adaptor, z as zod } from "./schema"
// import * as object from "./object"
// import * as fn from "./function"
// import { Tree } from "./tree"
// import * as Lens from "./lens"
// import type { StoreApi, UseBoundStore } from "zustand"

// const isObject = (u: unknown): u is Record<string, any> =>
//   typeof u === "object" && u !== null && !Array.isArray(u)
// const isPrimitive = (u: unknown): u is any.primitive =>
//   ["string", "number", "boolean", "symbol", "bigint"].includes(typeof u) || u == null

// type SchemaKind = typeof SchemaKind[keyof typeof SchemaKind]
// const SchemaKind = {
//   Array: "Array",
//   Tuple: "Tuple",
//   Object: "Object",
//   Leaf: "Leaf",
// } as const
// const SchemaKindToProp = {
//   Array: "element",
//   Tuple: "items",
//   Object: "shape",
//   Leaf: null,
// } as const

// export const SchemaKindToType = {
//   [z.ZodFirstPartyTypeKind.ZodString]: "string",
//   [z.ZodFirstPartyTypeKind.ZodNumber]: "number",
//   [z.ZodFirstPartyTypeKind.ZodBoolean]: "boolean",
//   [z.ZodFirstPartyTypeKind.ZodNull]: "null",
//   [z.ZodFirstPartyTypeKind.ZodUndefined]: "undefined",
//   [z.ZodFirstPartyTypeKind.ZodArray]: "array",
//   [z.ZodFirstPartyTypeKind.ZodObject]: "object",
//   [z.ZodFirstPartyTypeKind.ZodTuple]: "tuple",
// } as const

// export const TypeToSchemaKind = object.invert(SchemaKindToType)

// export namespace Context {
//   export type empty<S extends zod.any> =
//     { schema: S; path: [] }
//   export const empty = <S extends zod.any>(schema: S): Context.empty<S> =>
//     ({ schema, path: [] })
//   export interface Leaf<S extends zod.any = zod.any> { schema: S, path: any.keys }
//   export type next<_kind extends SchemaKind, prev extends Context.Leaf, schema, ix extends any.key> = never |
//   { schema: schema; path: [...prev['path'], ix] }
//   export const next = (kind: SchemaKind, prev: Context.Leaf, schema: any.type, segment?: any.key): Context.Leaf =>
//   ({
//     schema: kind === SchemaKind.Leaf ? schema : (schema as any)[SchemaKindToProp[kind]],
//     path: [...prev.path, ...(segment ? [segment] : [])]
//   })
// }

// export declare namespace Model {
//   interface leaf<T, S extends zod.any> {
//     value: T,
//     schema: S
//     errors: any[],
//     touched: boolean
//   }
// }

// export namespace Model {
//   export const leaf = <S extends zod.any, context extends Context.Leaf<S>>(ctx: context) => ({
//     type: SchemaKindToType[zod.typeName.get(ctx.schema)],
//     schema: ctx.schema,
//     validate: fn.flow(ctx.schema.safeParse, x => x.success),
//     path: ctx.path,
//     valueLens: Lens.fromPath,
//     errors: [],
//     touched: false,
//   })
//   // export const leaf = <T, S extends zod.any>(value: T, schema: S): Model.leaf<T, S> => ({
//   //   value,
//   //   schema,
//   //   errors: [] as any[],
//   //   touched: false,
//   // })
// }

// export namespace Handlers {
//   export type Any = {
//     [k in keyof Handlers.Leaf]: Kind<1> | ((ctx: Context.Leaf) => unknown);
//   };
//   export interface Leaf<S = {}, N = {}, B = {}, Nil = unknown, Void = unknown> {
//     string(s: Context.Leaf): S;
//     number(s: Context.Leaf): N;
//     boolean(s: Context.Leaf): B;
//     null(s: Context.Leaf): Nil;
//     undefined(s: Context.Leaf): Void;
//   }

//   export type Kinds = { [k in keyof Handlers.Leaf]: Kind<1> }
//   export type Fns = { [k in keyof Handlers.Leaf]: (ctx: Context.Leaf) => unknown }

//   export interface Identity extends Kind<1> { [-1]: this[0] }

//   export type context = { [k in keyof Handlers.Leaf]: Identity };
//   export const context = {
//     string: (ctx) => ({ ...ctx, schema: zod.typeName.softGet(ctx.schema) }),
//     number: (ctx) => ({ ...ctx, schema: zod.typeName.softGet(ctx.schema) }),
//     boolean: (ctx) => ({ ...ctx, schema: zod.typeName.softGet(ctx.schema) }),
//     null: (ctx) => ({ ...ctx, schema: zod.typeName.softGet(ctx.schema) }),
//     undefined: (ctx) => ({ ...ctx, schema: zod.typeName.softGet(ctx.schema) }),
//   } satisfies Handlers.Leaf;

//   interface LensKind extends Kind<[Context.Leaf]> {
//     [-1]
//     : [this[0]] extends [Context.Leaf & infer context extends Context.Leaf]
//     ? [context] extends [{ path: any.keys & infer path extends any.keys, schema: infer schema extends zod.any }]
//     ? [schema] extends [{ _type: infer type extends traversable.by<path> }]
//     ? Lens.fromPath<path, type>
//     : Lens.fromPath<path, traversable.unfold<schema[Extract<"_type", keyof schema>], path>>
//     : never
//     : never
//   }


//   export interface StoreLens<target> {
//     get(): target
//     set(a: target): void
//     [Lens.symbol]: Lens.uri
//   }

//   export interface StoreLensKind extends Kind<[Ctx: Context.Leaf]> {
//     [-1]: this[0]["schema"] extends
//     | zod.any & infer target extends zod.any
//     ? StoreLens<target["_type"]>
//     : never
//   }


//   export type lenses = { [k in keyof Handlers.Leaf]: LensKind }
//   export const lenses = {
//     boolean: (ctx) => Lens.fromPath(...ctx.path),
//     null: (ctx) => Lens.fromPath(...ctx.path),
//     number: (ctx) => Lens.fromPath(...ctx.path),
//     string: (ctx) => Lens.fromPath(...ctx.path),
//     undefined: (ctx) => Lens.fromPath(...ctx.path),
//   } satisfies Handlers.Leaf

//   function deriveLens<Z extends zod.any = never>():
//     (store: StoreApi<Z>) => (ctx: Context.Leaf<z.ZodTypeAny>) => StoreLens<Z>
//   function deriveLens<Z extends zod.any>(schema: Z):
//     (store: StoreApi<Z>) => (ctx: Context.Leaf<z.ZodTypeAny>) => StoreLens<Z>
//   function deriveLens<Z extends zod.any = never>(_?: Z):
//     (store: StoreApi<Z>) => (ctx: Context.Leaf<z.ZodTypeAny>) => StoreLens<Z> {
//     return (store) => (ctx) => {
//       const lens = Lens.fromPath<any>(...ctx.path)
//       return {
//         ...lens,
//         get: () => lens.get(store.getState()),
//         set: (a: never) => store.setState((lens.set as Lens.setter<any, any>)(a)),
//       }
//     }
//   }

//   export type deriveLenses<Z extends zod.any> = { [k in keyof Handlers.Leaf]: StoreLensKind }
//   export const deriveLenses =
//     <Z extends zod.any>() => (store: StoreApi<Z>) => object.map(
//       Handlers.context,
//       () => deriveLens<Z>()(store)
//     ) satisfies Handlers.Leaf

//   export type empty = typeof Handlers.empty
//   export const empty = {
//     boolean: () => false,
//     number: () => 0,
//     string: () => '',
//     null: () => null,
//     undefined: () => undefined,
//   } satisfies Handlers.Leaf


//   export type leaves = typeof Handlers.leaves
//   export const leaves = {
//     boolean: Model.leaf,
//     null: Model.leaf,
//     number: Model.leaf,
//     string: Model.leaf,
//     undefined: Model.leaf,
//   } satisfies Handlers.Leaf

//   export type defaults = typeof Handlers.defaults
//   export const defaults = object.map(
//     Handlers.context,
//     () => () => ({})
//   ) satisfies Handlers.Leaf

//   export type touched = typeof Handlers.touched
//   export const touched = object.map(
//     Handlers.context,
//     () => () => false as boolean
//   ) satisfies Handlers.Leaf
// }

// export namespace Parser {
//   export type SchemaTag = typeof SchemaTag
//   export const SchemaTag = "SchemaTag"
//   export const SchemaType = Symbol.for(SchemaTag)
//   export type SchemaType = typeof SchemaType

//   export type ArrayType = typeof ArrayType
//   export const ArrayType = "ArrayType"
//   export type ObjectType = typeof ObjectType
//   export const ObjectType = "ObjectType"
//   export type TupleType = typeof TupleType
//   export const TupleType = "TupleType"

//   export type errors<errs extends any.array<zod.issue>>
//     = Tree.fromPaths<{ [ix in keyof errs]: [...errs[ix]['path'], errs[ix]] }>

//   export function errors<const errs extends any.array<zod.issue>>(errs: errs): Parser.errors<errs>;
//   export function errors<const errs extends any.array<zod.issue>>(errs: errs) {
//     return Tree.fromPaths(...errs.map((e) => [...e.path, e] as const))
//   }

//   export type leaf<T extends zod.any.leaf, H extends Handlers.Any, Ctx>
//     = [H[adaptor[zod.typeName.get<T>]]] extends [infer handler]
//     ? [handler] extends [Kind<1>] ? Kind.apply$<handler, [Ctx]>
//     : [handler] extends [any.function] ? ReturnType<handler>
//     : handler
//     : Ctx
//     ;
//   export function leaf<H extends Handlers.Leaf, S extends zod.any.leaf>
//     (handlers: Partial<H>, ctx: Context.Leaf, s: S) {
//     if (zod.leaf.string.is(s))
//       return (handlers.string ?? Handlers.defaults.string)(Context.next(SchemaKind.Leaf, ctx, s))
//     else if (zod.leaf.number.is(s))
//       return (handlers.number ?? Handlers.defaults.number)(Context.next(SchemaKind.Leaf, ctx, s))
//     else if (zod.leaf.boolean.is(s))
//       return (handlers.boolean ?? Handlers.defaults.boolean)(Context.next(SchemaKind.Leaf, ctx, s))
//     else if (zod.leaf.null.is(s))
//       return (handlers.null ?? Handlers.defaults.null)(Context.next(SchemaKind.Leaf, ctx, s));
//     else if (zod.leaf.undefined.is(s))
//       return (handlers.undefined ?? Handlers.defaults.undefined)(Context.next(SchemaKind.Leaf, ctx, s))
//     else return fn.exhaustive(s)
//   }

//   export type parse<S extends zod.any, H extends Handlers.Any, Ctx extends Context.Leaf = Context.empty<S>>
//     = [S] extends [zod.any.leaf] ? Parser.leaf<S, H, Ctx>
//     : [S] extends [z.ZodArray<infer elements>] ? (
//       & { [SchemaType]: ArrayType } // , [-1]: (create: z.infer<elements>) => z.infer<elements> }
//       & {
//         [ix: number]:
//         Parser.parse<elements, H, Context.next<typeof SchemaKind.Array, Ctx, elements, number>>
//       }
//     ) : [S] extends [z.ZodTuple<infer items>] ? (
//       & { [SchemaType]: TupleType }
//       & { [ix in keyof items]:
//         Parser.parse<items[ix], H, Context.next<typeof SchemaKind.Tuple, Ctx, items[ix], ix>> }
//     ) : [S] extends [z.ZodObject<infer shape>] ? (
//       & { [SchemaType]: ObjectType }
//       & { [k in keyof shape]:
//         Parser.parse<shape[k], H, Context.next<typeof SchemaKind.Object, Ctx, shape[k], k & any.key>> }
//     ) : never

//   export function parse
//     <H extends Handlers.Kinds = never>(handlers: Partial<Handlers.Leaf>):
//     <S extends zod.any>(s: S) => Parser.parse<S, H, Context.empty<S>>
//   export function parse
//     <H extends Handlers.Fns = never>(handlers: Partial<H>):
//     <S extends zod.any>(s: S) => Parser.parse<S, H, Context.empty<S>>
//   /** impl. */
//   export function parse(handlers: Partial<Handlers.Leaf>) {
//     const go =
//       (ctx: Context.Leaf) =>
//         (s: zod.any): unknown => {
//           if (zod.array.is(s))
//             return { ["[]"]: go(Context.next(SchemaKind.Array, ctx, zod.array.get(s), "[]"))(zod.array.get(s)) }
//           else if (zod.tuple.is(s))
//             return zod.tuple.get(s).map((v, ix) => go(Context.next(SchemaKind.Tuple, ctx, s, ix))(v))
//           else if (zod.object.is(s))
//             return object.map(zod.object.get(s), (v, k) => go(Context.next(SchemaKind.Object, ctx, s, k))(v))
//           else if (zod.leaf.is(s))
//             return Parser.leaf(handlers, ctx, s);
//           else // TODO: remove type assertion:
//             return fn.exhaustive(s as never)
//         }
//     return (s: zod.any) => go(Context.empty(s))(s);
//   }

//   export type concat<prev extends any.index, next extends any.index, delimiter extends any.showable = ".">
//     = `${prev & any.key}${prev extends "" ? "" : next extends `[${string}` ? "" : delimiter}${next & any.key}`

//   export const concat = (prev: any.key, next: any.key, delimiter: any.showable = ".") =>
//     `${prev}${`${prev}` === "" ? "" : `${next}`.startsWith("[") ? "" : delimiter}${next}`

//   /** TODO: Will need this when you switch to supporting arrays */
//   // export type correctLens<tree> = never | (
//   //   tree extends Lens.fromPath<infer P, infer S>
//   //   ? { [ix in keyof P]: number extends P[ix] ? "[]" : P[ix] } extends
//   //   | infer Q extends { [ix in keyof P]: number extends P[ix] ? "[]" : P[ix] }
//   //   ? Lens.fromPath<Q, traversable.by<Q>>
//   //   : tree
//   //   : never
//   // )

//   export type flatten<tree, basecase, prev extends any.key = "">
//     = [tree] extends [basecase] ? [key: prev, lens: tree]
//     : [keyof tree] extends [any.keyof<tree, infer next>]
//     ? next extends next
//     /** TODO: turn on after adding support for tuples and arrays */
//     // ? tree extends (
//     //   & { [Parser.SchemaType]: Parser.ArrayType }
//     //   & infer type extends any.indexedBy<next>
//     // ) ? flatten<type[Extract<number, keyof type>], Parser.concat<prev, `[]`, ``>>
//     // : tree extends (
//     //   & { [Parser.SchemaType]: Parser.TupleType }
//     //   & infer type extends any.indexedBy<next>
//     // ) ? flatten<type[next], Parser.concat<prev, `[${number extends next ? never : next & any.key}]`, ``>>
//     ? tree extends (
//       & { [Parser.SchemaType]: Parser.ObjectType }
//       & infer type extends any.indexedBy<next>
//     ) ? flatten<type[next], basecase, Parser.concat<prev, next>>
//     : tree
//     : never
//     : never
//     ;

//   export function flatten(object: unknown): Record<string, any> {
//     let entries: mut.entries = []
//     const go = (path: any.key) => (x: unknown) => {
//       if (Lens.is(x)) entries.push([path, x])
//       if (isPrimitive(x)) entries.push([path, x])
//       if (Array.isArray(x)) x.forEach((v, ix) => go(concat(path, `[${ix}]`, ""))(v))
//       if (isObject(x)) globalThis.Object.entries(x).forEach(([k, v]) => go(concat(path, k))(v))
//     }
//     void go("")(object)
//     return globalThis.Object.fromEntries(entries)
//   }

//   export type leaves<S extends zod.any> = Parser.parse<S, Handlers.leaves>
//   export const leaves = Parser.parse(Handlers.leaves)

//   export type context<S extends zod.any> = Parser.parse<S, Handlers.context>
//   export const context = Parser.parse<Handlers.context>(Handlers.context)
//   export type touched<S extends zod.any> = Parser.parse<S, Handlers.touched>
//   export const touched = Parser.parse<Handlers.touched>(Handlers.touched)

//   export type deriveEmpty<S extends zod.any> = internal.cleanUp<Parser.parse<S, Handlers.empty>>
//   export const deriveEmpty = Parser.parse<Handlers.empty>(Handlers.empty)

//   export type deriveLenses<Z extends zod.any> = never | { [e in internal.deriveLenses<Z> as e[0]]: e[1] }
//   export const deriveLenses = <Z extends zod.any>(schema: Z) =>
//     (store: UseBoundStore<StoreApi<Z>>): Parser.deriveLenses<Z> => {
//       const lenses = flatten(internal.deriveLenses(schema))
//       return object.map(
//         lenses,
//         (l: Lens.fromPath<any, any>) => ({
//           ...l,
//           get: () => store(l.get),
//           set: (a: any) => store.setState((l.set as Lens.setter<any, any>)(a))
//         })
//       ) as never
//     }
// }

// declare namespace internal {
//   type deriveLenses<Z extends zod.any>
//     = Extract<Parser.flatten<
//       Parser.parse<Z, Handlers.deriveLenses<Z>>,
//       Handlers.StoreLens<any>
//     >, any.entry>
//   type deriveEmpty<S extends zod.any> = cleanUp<Parser.deriveEmpty<S>>

//   type cleanUp<type>
//     = [type] extends [any.primitive] ? type
//     : [type] extends [any.indexedBy<Parser.SchemaType>]
//     ? { [k in Exclude<keyof type, Parser.SchemaType>]: cleanUp<type[k]> }
//     : never
//     ;
// }
// namespace internal {
//   export const deriveLenses = Parser.parse<Handlers.lenses>(Handlers.lenses)
// }

// // = [tree] extends [Lens.any] ? [key: prev, lens: correctLens<tree>]
// // : [keyof tree] extends [any.propertyOf<tree, infer next>]
// // ? next extends next
// // ? tree extends (
// //   & { [Parser.SchemaType]: Parser.ArrayType }
// //   & infer type extends any.indexedBy<next>
// // ) ? flatten<type[Extract<number, keyof type>], Parser.concat<prev, `[]`, ``>>
// // : tree extends (
// //   & { [Parser.SchemaType]: Parser.TupleType }
// //   & infer type extends any.indexedBy<next>
// // ) ? flatten<type[next], Parser.concat<prev, `[${number extends next ? never : next & any.key}]`, ``>>
// // : tree extends (
// //   & { [Parser.SchemaType]: Parser.ObjectType }
// //   & infer type extends any.indexedBy<next>
// // ) ? flatten<type[next], Parser.concat<prev, next>>
// // : 1
// // // never.close.distributive<"next">
// // : 2 // never.close.inline_var<"next">
// // : 3 // never.close.unmatched_expr
// // ;
// // export type lenses_<S extends zod.any> = Parser.parse<S, Handlers.lenses>
// // export const lenses_ = Parser.parse<Handlers.lenses>(Handlers.lenses)
// // // export type flatLens<tree>
// // //   = flatten<tree> extends any.entry<infer entry>
// //   ? { [e in entry as e[0]]: e[1] } : never
// // export const flatLens
// //   : <S extends zod.any>(schema: S) => Parser.lenses<S>
// //   = fn.flow(Parser.lenses_, flatten) as never
// // export type lenses<S extends zod.any> = { [e in flatten<Parser.lenses_<S>, Lens.any>> as e[0]]: }
// // export const lenses = Parser.flatLens
// // export type flatten<tree, prev extends any.key = "">
// //   = [tree] extends [Lens.any] ? [key: prev, lens: tree] // correctLens<tree>]   /** TODO: turn this back on when `correctLens` is fixed */
// //   : [keyof tree] extends [any.propertyOf<tree, infer next>]
// //   // next extends next
// //   ? next
// //   : never
// /** TODO: get Arrays working again */
// // ? tree extends (
// //   & { [Parser.SchemaType]: Parser.ArrayType }
// //   & infer type extends any.indexedBy<next>
// // ) ? flatten<type[Extract<number, keyof type>], Parser.concat<prev, `[]`, ``>>
// /** TODO: get Tuples working again */
// // : tree extends (
// //   & { [Parser.SchemaType]: Parser.TupleType }
// //   & infer type extends any.indexedBy<next>
// // ) ? flatten<type[next], Parser.concat<prev, `[${number extends next ? never : next & any.key}]`, ``>>
// // ? tree extends (
// //   & { [Parser.SchemaType]: Parser.ObjectType }
// //   & infer type extends any.indexedBy<next>
// // ) ? flatten<type[next], Parser.concat<prev, next>>
// // : { [k in keyof tree]: flatten<tree[k], prev> }
// // : never
// // : never
// // ;
// // {
// //   boolean: (ctx: Context.Leaf<z.ZodTypeAny>) => StoreLens<Z>;
// //   null: (ctx: Context.Leaf<z.ZodTypeAny>) => StoreLens<Z>;
// //   number: (ctx: Context.Leaf<z.ZodTypeAny>) => StoreLens<Z>;
// //   string: (ctx: Context.Leaf<z.ZodTypeAny>) => StoreLens<Z>;
// //   undefined: (ctx: Context.Leaf<z.ZodTypeAny>) => StoreLens<Z>;
// // }
// // function generateSchemaLens(path: any.keys) { return Lens.fromPath(generateSchemaPath(path)) }
// // function generateSchemaPath(path: any.keys) {
// //   let out: mut.array<string> = []
// //   for(const p of path.map(globalThis.String)) {
// //     if(p === "[]") out.push("element")
// //     else if(isBetweenBrackets(p)) out.push("items", removeBrackets(p))
// //     else out.push("shape", p)
// //   }
// //   return out
// // }
// // function isBetweenBrackets(s: string) { return s.startsWith("[") && s.endsWith("]") }
// // function removeBrackets(s: string) { return s.slice("[".length, -("]".length)) }
