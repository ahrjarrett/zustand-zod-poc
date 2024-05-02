import * as zustand from "zustand"
import type { StoreApi } from "zustand"

import { Parser } from "../lib/parser"
import * as fn from "../lib/function"
import { z as zod } from "../lib/schema"
import { Middleware } from '../lib/middleware'

export const create
  : <Z extends zod.any>(schema: Z) => Middleware.withLenses<StoreApi<Parser.deriveEmpty<Z>>, Z>
  = (schema) =>
    fn.pipe(
      /** TODO: fix type assertion */
      (() => Parser.empty(schema) as never),
      zustand.create(),
      Middleware.withLenses(schema),
    )
