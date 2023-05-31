## This file will be removed

- [x] :analyze-meta
  - It's a hook
  - It checks the content in-place of a `def` based on the metadata
  - An arbitrary metadata (symbol or keyword) is checked, being mapped
    to a hook
  - [x] Wrap the body of the def with an imagined symbol
  - [x] Map the meta identifier to the hook

{:hooks {:analyze-meta {:omg hooks.foo/meta-handler}}}
