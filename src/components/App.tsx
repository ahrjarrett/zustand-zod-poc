import './App.css'
import * as R from "react"
import * as zustand from "zustand"
import type { UseBoundStore, StoreApi } from "zustand"
import * as z from "zod"
import type { Kind, any } from 'any-ts'

import { Parser } from "../lib/parser"
import * as Lens from "../lib/lens"
import * as object from "../lib/object"
import * as fn from "../lib/function"
import { create } from '../lib/create'
import { z as Z } from "../lib/schema"
import { Middleware } from '../lib/middleware'

const parseInt = (u: unknown) => Number.isNaN(+String(u)) ? 0 : +String(u)

interface UseInput {
  value: string
  onChange: R.ChangeEventHandler<HTMLInputElement>
  type: "number" | "string"
}
function useInput(state: string, setState: (s: string) => void): UseInput {
  const onChange
    : UseInput["onChange"]
    = (e) => { console.log("e", e.target.value, e.currentTarget.value); setState(e.target.value) }
  return {
    type: "number",
    value: state,
    onChange,
  }
}

const Title = (props: { title: string }) => <p>{props.title}</p>

interface InputProps<S, P extends any.keys, V extends any.showable> {
  store: StoreApi<S>,
  path: P
  setState(v: any.showable): void
}
const Input = <S, P extends any.keys, V extends any.showable>(props: InputProps<S, P, V>) => {
  const selector = object.get(...props.path)
  const state = JSON.stringify((props.store as any)(selector as never))
  const input = useInput(state, props.setState)
  return <>
    <input {...input} />
  </>
}


/** TODO: fix this mess */
type AttributeProps<P extends any.keys, S = unknown> = Kind.apply$<attributeProps, [P, S]>
interface attributeProps extends Kind<[path: any.keys, state: unknown, value: unknown]> {
  [-1]
  : [this[0]] extends [any.keys & infer P extends any.keys]
  ? [this[1]] extends [Lens.Has<P> & infer S]
  ? [this[2]] extends [infer V]
  ? {
    path: P
    // store: (store: (selector: S) => V) => V /** all that for nothing */
    store: any /** all that for nothing */
  }
  : never
  : never
  : never
}

const Attribute = <P extends any.keys, S extends Lens.Has<P>>(props: AttributeProps<P, S>) => {
  const selector = object.get(...props.path)
  const state = props.store(selector as never)
  return <code>{JSON.stringify(state, null, 2)}</code>
}

export interface Address extends z.infer<typeof Address> { }
export const Address = z.object({
  street: z.string(),
  city: z.string(),
  zip: z.string(),
})

interface User extends z.infer<typeof User> { }
const User = z.object({
  name: z.object({
    first: z.string(),
    last: z.string(),
  }),
  dob: z.object({
    mm: z.number(),
    dd: z.number(),
    yyyy: z.number(),
  }),
  /** TODO: get arrays working */
  // trips: z.array(Address),
  /** TODO: get tuples working */
  // last3Trips: z.tuple([Address, Address, Address]),
})


const Store = create(User)
console.log("Store", Store)
  ; (window as any).Store = Store

export function App() {
  return (
    <main>
      <header>
        <h1>User data</h1>
      </header>
      <article>
        <section>
          <h2>Birth date</h2>
          <Attribute path={["dob", "dd"]} store={Store} />
          {`-`}
          <Attribute path={["dob", "mm"]} store={Store} />
          {`-`}
          <Attribute path={["dob", "yyyy"]} store={Store} />
        </section>
        <section>
          <Title title="dob-dd" />
          <Input
            path={["dob", "dd"]}
            setState={fn.flow(parseInt, Store.use["dob.dd"].set)}
            store={Store}
          />
          <Title title="dob-mm" />
          <Input
            path={["dob", "mm"]}
            setState={fn.flow(parseInt, Store.use["dob.mm"].set)}
            store={Store}
          />
          <Title title="dob-yyyy" />
          <Input
            path={["dob", "yyyy"]}
            setState={fn.flow(parseInt, Store.use["dob.yyyy"].set)}
            store={Store}
          />
        </section>

      </article>
    </main>
  )
}

// store: UseBoundStore<StoreApi<S>>,
// getState: () => V
// setState(v: any.showable): void
// title: string
// }
// type storeFromSchema<schema extends Z.any>
//   = Store<Middleware.withLenses<schema, cleanUp<Parser.empty<schema>>>>
//   ;
// type boundStoreFromSchema<schema extends Z.any>
//   = BoundStore<UseBoundStore<StoreApi<schema["_type"]>> & storeFromSchema<schema>>
//         <section>
//           <h2>Name</h2>
//           <Attribute path={["name", "first"]} store={Store} />
//           <Attribute path={["name", "last"]} store={Store} />
//         </section>
// type id<x> = x
// /* @ts-expect-error hush */
// interface Store<S extends {}> extends id<S> { }