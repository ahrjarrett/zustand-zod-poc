import type * as zustand from "zustand"

import { Parser } from "./parser"
import * as Lens from "./lens"
import * as object from "./object"
import { z } from "./schema"

export declare namespace Middleware {
  type SetState<Z extends z.any> = StoreApi<Z>["setState"]
  type GetState<Z extends z.any> = StoreApi<Z>["getState"]
  type StoreApi<Z extends z.any> = Middleware.deriveLenses<zustand.StoreApi<Z["_type"]>, Z>

  type deriveLenses<store, Z extends z.any> = never | (
    & store
    & { use: Parser.deriveLenses<Z> }
  )
}

export namespace Middleware {
  export function deriveLenses<schema extends z.any>(schema: schema):
    <store>(store: store) => Middleware.deriveLenses<store, schema>

  export function deriveLenses<Z extends z.any>(schema: Z) {
    return <store extends zustand.UseBoundStore<StoreApi<Z>>>(store: store) => {
      let next = { ...store } as store & { use: any }
      next.use = Parser.deriveLenses<Z>(schema)(store)
      return next
    }
  }
}
