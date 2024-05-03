import * as zustand from "zustand"
import type { StoreApi, UseBoundStore } from "zustand"

import { Parser } from "./parser"
import * as fn from "./function"
import { z as zod } from "./schema"
import { Middleware } from './middleware'
import type { Partial } from "./types"

export { Middleware } from "./middleware"

export const create
  : <Z extends zod.any>(schema: Z, initialValues?: Partial<Z["_type"]>) => UseBoundStore<Middleware.deriveLenses<StoreApi<Parser.deriveEmpty<Z>>, Z>>
  = (schema, initialValues) =>
    fn.pipe(
      /** TODO: fix type assertion */
      (() => Parser.deriveEmpty(schema, initialValues) as never),
      zustand.create(),
      Middleware.deriveLenses(schema),
    )
