import './App.css'
import * as R from "react"
import * as z from "zod"

import type { StoreLens } from "./lens"
import { create } from '../lib/create'
import { InputIntrinsics, supportIntrinsics } from "./intrinsics"
import { Handlers } from '../lib/parser'
import { useInput } from "./useInput"

const Label = (props: { title: string }) => <label>{props.title}</label>

type ShowProps = { lens: StoreLens }
const Show = (props: ShowProps) => <code>{props.lens.get()}</code>

interface StringInputProps extends InputIntrinsics {
  lens: Handlers.StoreLens<string>
  type: "string"
}
interface NumericInputProps extends InputIntrinsics {
  lens: Handlers.StoreLens<number>
  type: "number"
}

function Input(props: StringInputProps): R.ReactElement
function Input(props: NumericInputProps): R.ReactElement
function Input(props: StringInputProps | NumericInputProps): R.ReactElement {
  const input = useInput(props.type, props.lens)
  const allProps = { ...supportIntrinsics(props), ...input }
  return <input {...allProps} />
}

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

export interface Address extends z.infer<typeof Address> { }
export const Address = z.object({
  street: z.string(),
  city: z.string(),
  zip: z.string(),
})

type Store = typeof Store
const Store = create(User)
  ; (window as any).Store = Store

export function App() {
  return (
    <main>
      <header>
        <h1>User data</h1>
      </header>
      <article>
        <div>
          <section>
            <h2>Birth date</h2>
            <p>Your birthday:</p>
            <Show lens={Store.use["dob.dd"]} />-<Show lens={Store.use["dob.mm"]} />-<Show lens={Store.use["dob.yyyy"]} />
          </section>
          <section>
            <h2>Name</h2>
            <p>Your name:</p>
            <Show lens={Store.use["name.first"]} /> <Show lens={Store.use["name.last"]} />
          </section>
        </div>
        <br />
        <br />
        <div>
          <section style={{ display: "flex", flexDirection: "column" }}>
            <Label title="birth month" />
            <Input type="number" lens={Store.use["dob.dd"]} />
            <Label title="birth date" />
            <Input type="number" lens={Store.use["dob.mm"]} />
            <Label title="birth year" />
            <Input type="number" lens={Store.use["dob.yyyy"]} />
            <br />
            <Label title="first name" />
            <Input type="string" lens={Store.use["name.first"]} />
            <Label title="last name" />
            <Input type="string" lens={Store.use["name.last"]} />
          </section>
        </div>
      </article>
    </main>
  )
}
