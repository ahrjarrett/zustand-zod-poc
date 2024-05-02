import { Handlers, Parser } from '../lib/parser'
import * as fn from "../lib/function"

export type StringLens = Handlers.StoreLens<string>
export type NumberLens = Handlers.StoreLens<number>

export type StoreLens =
  | StringLens
  | NumberLens

export const toString
  : (ns: number | string) => string
  = globalThis.String
export const parseInt
  : (ns: number | string) => number
  = (ns) => Number.isNaN(+toString(ns)) ? 0 : +toString(ns)

export const stringLens
  : (lens: StoreLens) => string
  = fn.flow(lens => lens.get(), toString)
export const numberLens
  : (lens: StoreLens) => number
  = fn.flow(lens => lens.get(), parseInt)
