import * as z from "zod"
import * as object from "../lib/object"

export interface InputIntrinsics extends z.infer<typeof InputIntrinsics> { }
export const InputIntrinsics = z.object({
  id: z.string().optional(),
  name: z.string().optional(),
})

export const supportIntrinsics = object.softPick(...object.keys(InputIntrinsics.shape))
