# CMark Lens

(Incomplete) Collection of Lens for [cmark](https://hackage.haskell.org/package/cmark)

This package is written in the style of [writing lenses without lens](https://github.com/ekmett/lens/wiki/How-can-I-write-lenses-without-depending-on-lens%3F) making it a lightweight dependency.

# Example

```haskell
import CMark
import CMark.Lens
import Control.Lens

foo :: CMark.Node -> ()
foo node =
  let posInfo = node ^. _posInfo       -- Lens' PosInfo
      nodeType = node ^. _nodeType     -- Lens' NodeType
      text = nodeType ^. _TEXT         -- Prism' NodeType Text
      childNodes' = node ^.. _nodes    -- Traversal' Node [Node]
      childNodes  = node ^. _nodesLens -- Lens' Node [Node]
      -- Remove all the TEXTs at depth 2
      node'' = set (_nodes . _nodes . _nodeType . _TEXT) empty node
   in ()
```

For more extensive examples, go to [/test/Main.hs](/test/Main.hs).

# CMark-GFM Lens

For the lenses for `cmark-gfm-lens` go https://github.com/ingun37/cmark-gfm-lens