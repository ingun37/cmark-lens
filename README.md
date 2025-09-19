# CMark Lens

```haskell
import CMark
import CMark.Lens
import Control.Lens

foo :: CMark.Node -> ()
foo node =
  let posInfo = node ^. _posInfo             -- get PosInfo
      nodeType = node ^. _nodeType           -- get NodeType
      text = node ^. _nodeType . _TEXT       -- get content of TEXT with Prism' NodeType Text
      childNodes  = node ^. _nodesLens       -- get [Node] with Lens' Node [Node]
      childNodes' = node ^.. _nodesTraversal -- get [Node] with Traversal' Node [Node]
      -- Remove all the TEXTs at depth 2
      node'' = set (_nodesTraversal . _nodesTraversal . _nodeType . _TEXT) "" node
   in ()
```