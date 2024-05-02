# zustand-zod-lenses

### Overview

This is a POC demonstrating the viability of a piece of zustand middleware
that accepts a zod schema that describes an HTML form as input, and parses the schema to generate:

- an empty default value for the form
- optimized/flattened getters/setters (lenses) 

The lenses are bound to the store's `use` property.

### Try it out

#### Online:
- [Stackblitz](https://stackblitz.com/~/github.com/ahrjarrett/zustand-zod-poc)

#### Local:
1. Clone the project:
  ```bash
  git clone https://github.com/ahrjarrett/zustand-zod-poc
  ```
2. Install deps:
  ```bash
  pnpm install && pnpm dev
  ```
3. Open [localhost:5173](http://localhost:5173/)


#### Walk-through

Run the demo. Note that the `Store` value is bound to the global window.

Then, do the following.

1. Make sure your React devtools have "show re-renders" turned on
2. Open the browser console
3. Run the following script to set the store state from the console:
  ```javascript
  Store.use["dob.mm"].set(9000)
  ```
4. Note the surface area of the re-renders

#### TODO:
- [ ] Use `Parser.leaves` and `Parser.errors` to validate
- [ ] Add error lenses
