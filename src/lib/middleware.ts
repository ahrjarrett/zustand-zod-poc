import type { StoreApi } from "zustand"

import { Parser } from "./parser"
import * as Lens from "./lens"
import * as object from "./object"
import { z as Z } from "./schema"

export declare namespace Middleware {
  type withLenses<store, schema extends Z.any> = never | (
    & store
    & { use: Parser.lenses<schema> }
  )
}

export namespace Middleware {
  export function withLenses<schema extends Z.any>(schema: schema):
    <store>(store: store) => Middleware.withLenses<store, schema>

  export function withLenses<schema extends Z.any>(schema: schema) {
    return <store extends StoreApi<schema["_type"]>>(store: store) => {
      let next = store as store & { use: unknown }
      const lenses = Parser.lenses(schema)
      next.use = object.map(
        lenses as never,
        (l: Lens.fromPath<any, any>) => ({
          ...l,
          get: () => l.get(store.getState()),
          set: (a: any) => store.setState((l.set as Lens.setter<any, any>)(a))
        })
      )
      return next
    }
  }
}
