export type Bivariant<type> = never | { _: type }["_"]
export type Contravariant<type> = (_: type) => never
export type evaluate<type> = never | { [k in keyof type]: type[k] }
export type parseInt<type> = type extends `${infer x extends number}` ? x : type