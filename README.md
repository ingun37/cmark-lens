# CMark Lens

Unofficial collection of Lens for [cmark](https://hackage.haskell.org/package/cmark).

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

# Manual Deployment

CI/CD is work in progress so currently the deployment is done manually

## Requirements

I use these versions because they seem to be the latest versions that works with Hackage/Haddock CI

- Cabal 3.8
- GHC 9.8
- GHC2021

Changing the toolchain versions affects

- [cmark-lens.cabal](/cmark-lens.cabal)
  - `cabal-version`
  - `default-language`
- [nix/hix.nix](/nix/hix.nix)
  - `compiler-nix-name`
- [cabal.project](/cabal.project)
  - `with-compiler`

## Steps

Update the `version` in [cmark-lens.cabal](/cmark-lens.cabal).

Commit.

Update the `tag` of `source-repository this` in [cmark-lens.cabal](/cmark-lens.cabal).

```
source-repository this
    type:     git
    location: https://github.com/ingun37/cmark-lens.git
    tag:      f33793a7e46ac299796d82a16ccfafb9c18ab1c8
```

Commit again.

Run
```bash
cabal check # Just in case
cabal build # Just in case
cabal sdist
```

Upload the produced tarball to Hackage.

# CMark-GFM Lens

For the lenses for `cmark-gfm-lens` go https://github.com/ingun37/cmark-gfm-lens
