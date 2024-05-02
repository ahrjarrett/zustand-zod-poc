import * as R from "react"
import { Handlers, Parser } from '../lib/parser'
import { stringLens, numberLens, StoreLens } from "./lens"

export interface UseStringInput {
  type: "string"
  value: string
  onChange: R.ChangeEventHandler<HTMLInputElement>
}

export function useStringInput(lens: Handlers.StoreLens<string>): UseStringInput {
  const onChange
    : UseStringInput["onChange"]
    = (e) => lens.set(e.target.value)
  return {
    type: "string",
    value: stringLens(lens),
    onChange,
  }
}

export interface UseNumericInput {
  type: "number"
  value: number
  onChange: R.ChangeEventHandler<HTMLInputElement>
}

export function useNumericInput(lens: Handlers.StoreLens<number>): UseNumericInput {
  const onChange
    : UseNumericInput["onChange"]
    = (e) => lens.set(parseInt(e.target.value))
  return {
    type: "number",
    value: numberLens(lens),
    onChange,
  }
}

export const useInput = (
  type: "string" | "number",
  lens: StoreLens,
) => type === "number"
    ? useNumericInput(lens as never) // eslint-disable-line react-hooks/rules-of-hooks
    : useStringInput(lens as never) // eslint-disable-line react-hooks/rules-of-hooks
