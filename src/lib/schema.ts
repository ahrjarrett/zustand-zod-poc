import { z } from "zod"
import type { any, nonempty, TypeError, to } from "any-ts"
import type { evaluate, parseInt } from "./types"
import * as object from "./object"

/** @internal */
const leafTypeNames = [
  z.ZodFirstPartyTypeKind.ZodString,
  z.ZodFirstPartyTypeKind.ZodNumber,
  z.ZodFirstPartyTypeKind.ZodBoolean,
] as const;
/** @internal */
const ZodIssue = z.object({
  code: z.enum(object.nonempty.keys(z.ZodIssueCode)),
  message: z.string(),
  path: z.array(z.union([z.string(), z.number()])),
});
/** @internal */
const ZodIssues = z.array(ZodIssue);
/** @internal */
const ZodError = z.object({ issues: ZodIssues });
/** @internal */
const getTypeName = <T extends hasTypeName>(s: T): T['_def']['typeName'] => s._def.typeName
/** @internal */
const softGetTypeName = (u: unknown) =>
  typeof u === 'object' && u !== null ? hasTypeName(u) ? getTypeName(u) : void 0 : void 0

/** @internal */
type hasTypeName<typeName extends string = string> = never | { _def: { typeName: typeName } }
/** @internal */
function hasTypeName(u: unknown): u is hasTypeName
/** @internal */
function hasTypeName<name extends string>(u: unknown, typeName: name): u is hasTypeName<name>
/** @internal */
function hasTypeName<const names extends any.array<string>>(u: unknown, ...typeNames: names): u is hasTypeName<names[number]>
/** @internal */
function hasTypeName(u: unknown, ...typeNames: any.array<string>): u is never {
  return (
    typeof u === 'object' &&
    u !== null &&
    '_def' in u &&
    typeof u._def === 'object' &&
    u._def !== null &&
    'typeName' in u._def &&
    typeof u._def.typeName === 'string' &&
    (typeNames.length === 0 ? true : typeNames.includes(u._def.typeName))
  )
}

export type adaptor = typeof adaptor;
export const adaptor = {
  ZodString: 'string',
  ZodNumber: 'number',
  ZodBoolean: 'boolean',
  ZodNull: 'null',
  ZodUndefined: 'undefined',
} as const;

export { Z as z }
declare namespace Z {
  export type issue = evaluate<Omit<z.ZodIssue, 'path'> & { path: any.keys }>;
  export namespace typeName {
    type get<T extends hasTypeName> = T['_def']['typeName'];
  }
  export { Any as any }
  type Any<type extends z.ZodTypeAny = z.ZodTypeAny> = type
  namespace Any {
    export { Object as object }
    type Object<type extends z.AnyZodObject = z.AnyZodObject> = type
    export type array<type extends z.ZodArray<any> = z.ZodArray<any>> = type
    export type tuple<type extends z.AnyZodTuple = z.AnyZodTuple> = type

    export type schema<
      type extends
      | Z.any.leaf
      | z.ZodTuple<any>
      | z.ZodArray<any>
      | z.ZodObject<any>
      = Z.any.leaf
      | z.ZodTuple<any>
      | z.ZodArray<any>
      | z.ZodObject<any>
    > = type;
    export type leaf<
      type extends
      | z.ZodString
      | z.ZodNumber
      | z.ZodBoolean
      | z.ZodNull
      | z.ZodUndefined
      = z.ZodString
      | z.ZodNumber
      | z.ZodBoolean
      | z.ZodNull
      | z.ZodUndefined
    > = type;
    export type items<
      type extends
      | [] | nonempty.mut.array<z.ZodTypeAny>
      = [] | nonempty.mut.array<z.ZodTypeAny>
    > = type;
    export type shape<type extends z.ZodRawShape = z.ZodRawShape> = type;
  }
  namespace Infer {
    type go<type extends Z.any.schema> = type extends z.ZodObject<
      Z.any.shape<infer shape>
    >
      ? { [ix in keyof shape]: Z.infer.go<shape[ix]> }
      : type extends z.ZodTuple<Z.any.items<infer items>>
      ? { [ix in keyof items]: Z.infer.go<items[ix]> }
      : type extends z.ZodArray<Z.any.schema<infer schema>>
      ? any.array<Z.infer.go<schema>>
      : type extends Z.any.leaf<infer leaf>
      ? Z.infer.leaf<leaf>
      : TypeError<['Unsupported schema', type]>;
    export type leaf<type extends Z.any.leaf> = z.infer<type>;
    export type elements<type extends z.ZodArray<any>> =
      type extends z.ZodArray<Z.any.schema<infer schema>> ? schema : never;
    export type items<type extends z.ZodTuple<any>> = type extends z.ZodTuple<
      Z.any.items<infer items>
    >
      ? { [ix in keyof items]: [index: parseInt<ix>, item: items[ix]] }
      : never;
    export type shape<type extends z.ZodObject<any>> = type extends z.ZodObject<
      Z.any.shape<infer props>
    >
      ? to.entries<props>
      : never;
  }
  export { Infer as infer };
}

export namespace guard {
  export const fromSchema: <S extends z.ZodSchema>(
    schema: S
  ) => any.typeguard<unknown, z.infer<S>> =
    (schema) =>
      (u): u is never =>
        schema.safeParse(u).success;
}

const Z = {
  typeName: {
    has: hasTypeName,
    get: getTypeName,
    softGet: softGetTypeName,
  },
  issue: { schema: ZodIssue, is: guard.fromSchema(ZodIssue) },
  issues: { schema: ZodIssues, is: guard.fromSchema(ZodIssues) },
  error: { schema: ZodError, is: guard.fromSchema(ZodError) },
  leaf: {
    is: (u: object): u is Z.any.leaf => hasTypeName(u, ...leafTypeNames),
    boolean: { is: (u: object): u is z.ZodBoolean => hasTypeName(u, z.ZodFirstPartyTypeKind.ZodBoolean) },
    number: { is: (u: object): u is z.ZodNumber => hasTypeName(u, z.ZodFirstPartyTypeKind.ZodNumber) },
    string: { is: (u: object): u is z.ZodString => hasTypeName(u, z.ZodFirstPartyTypeKind.ZodString) },
    null: { is: (u: object): u is z.ZodNull => hasTypeName(u, z.ZodFirstPartyTypeKind.ZodNull) },
    undefined: { is: (u: object): u is z.ZodUndefined => hasTypeName(u, z.ZodFirstPartyTypeKind.ZodUndefined) },
  },
  array: {
    get: <T extends Z.any.schema>(a: z.ZodArray<T>): T => a._def.type,
    is: <T extends Z.any.schema>(u: unknown): u is z.ZodArray<T> => hasTypeName(u, z.ZodFirstPartyTypeKind.ZodArray),
  },
  tuple: {
    get: <T extends Z.any.items>(t: z.ZodTuple<T>): T => t._def.items,
    is: <T extends Z.any.items>(u: unknown): u is z.ZodTuple<T> => hasTypeName(u, z.ZodFirstPartyTypeKind.ZodTuple),
  },
  object: {
    get: <T extends Z.any.shape>(o: z.ZodObject<T>): T => o.shape,
    is: <T extends Z.any.shape>(u: unknown): u is z.ZodObject<T> => hasTypeName(u, z.ZodFirstPartyTypeKind.ZodObject),
  },
} as const;
