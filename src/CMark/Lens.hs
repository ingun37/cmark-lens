module CMark.Lens
  ( _posInfo,
    _nodeType,
    _nodesTraversal,
    _nodesLens,
    _HEADING,
    _PARAGRAPH,
    _TEXT,
  )
where

import CMark
import Data.Profunctor
import Data.Text qualified as T

_posInfo :: (Functor f) => (Maybe PosInfo -> f (Maybe PosInfo)) -> Node -> f Node
_posInfo inj (Node pi a b) =
  let set x = Node x a b
   in set <$> inj pi
{-# INLINE _posInfo #-}

_nodeType :: (Functor f) => (NodeType -> f NodeType) -> Node -> f Node
_nodeType inj (Node a nt b) =
  let set x = Node a x b
   in set <$> inj nt
{-# INLINE _nodeType #-}

_nodesTraversal :: (Applicative f) => (Node -> f Node) -> Node -> f Node
_nodesTraversal inj (Node a b nodes) = Node a b <$> traverse inj nodes
{-# INLINE _nodesTraversal #-}

_nodesLens :: (Functor f) => ([Node] -> f [Node]) -> Node -> f Node
_nodesLens inj (Node a b nodes) = Node a b <$> inj nodes
{-# INLINE _nodesLens #-}

prism0 :: (Choice p, Applicative f) => NodeType -> p () (f ()) -> p NodeType (f NodeType)
prism0 c = dimap to fro . right'
  where
    -- to DOCUMENT = Right ()
    to x = if x == c then Right () else Left x
    fro (Left it) = pure it
    fro (Right fa) = c <$ fa

_DOCUMENT :: (Choice p, Applicative f) => p () (f ()) -> p NodeType (f NodeType)
_DOCUMENT = prism0 DOCUMENT
{-# INLINE _DOCUMENT #-}

_THEMATIC_BREAK :: (Choice p, Applicative f) => p () (f ()) -> p NodeType (f NodeType)
_THEMATIC_BREAK = prism0 THEMATIC_BREAK
{-# INLINE _THEMATIC_BREAK #-}

_PARAGRAPH :: (Choice p, Applicative f) => p () (f ()) -> p NodeType (f NodeType)
_PARAGRAPH = prism0 PARAGRAPH
{-# INLINE _PARAGRAPH #-}

_BLOCK_QUOTE :: (Choice p, Applicative f) => p () (f ()) -> p NodeType (f NodeType)
_BLOCK_QUOTE = prism0 BLOCK_QUOTE
{-# INLINE _BLOCK_QUOTE #-}

_ITEM :: (Choice p, Applicative f) => p () (f ()) -> p NodeType (f NodeType)
_ITEM = prism0 ITEM
{-# INLINE _ITEM #-}

_SOFTBREAK :: (Choice p, Applicative f) => p () (f ()) -> p NodeType (f NodeType)
_SOFTBREAK = prism0 SOFTBREAK
{-# INLINE _SOFTBREAK #-}

_LINEBREAK :: (Choice p, Applicative f) => p () (f ()) -> p NodeType (f NodeType)
_LINEBREAK = prism0 LINEBREAK
{-# INLINE _LINEBREAK #-}

_EMPH :: (Choice p, Applicative f) => p () (f ()) -> p NodeType (f NodeType)
_EMPH = prism0 EMPH
{-# INLINE _EMPH #-}

_STRONG :: (Choice p, Applicative f) => p () (f ()) -> p NodeType (f NodeType)
_STRONG = prism0 STRONG
{-# INLINE _STRONG #-}

_HTML_BLOCK :: (Choice p, Applicative f) => p T.Text (f T.Text) -> p NodeType (f NodeType)
_HTML_BLOCK = dimap to fro . right'
  where
    to (HTML_BLOCK a) = Right a
    to x = Left x
    fro (Left it) = pure it
    fro (Right fa) = HTML_BLOCK <$> fa
{-# INLINE _HTML_BLOCK #-}

_TEXT :: (Choice p, Applicative f) => p T.Text (f T.Text) -> p NodeType (f NodeType)
_TEXT = dimap to fro . right'
  where
    to (TEXT a) = Right a
    to x = Left x
    fro (Left it) = pure it
    fro (Right fa) = TEXT <$> fa
{-# INLINE _TEXT #-}

_HTML_INLINE :: (Choice p, Applicative f) => p T.Text (f T.Text) -> p NodeType (f NodeType)
_HTML_INLINE = dimap to fro . right'
  where
    to (HTML_INLINE a) = Right a
    to x = Left x
    fro (Left it) = pure it
    fro (Right fa) = HTML_INLINE <$> fa
{-# INLINE _HTML_INLINE #-}

_CODE :: (Choice p, Applicative f) => p T.Text (f T.Text) -> p NodeType (f NodeType)
_CODE = dimap to fro . right'
  where
    to (CODE a) = Right a
    to x = Left x
    fro (Left it) = pure it
    fro (Right fa) = CODE <$> fa
{-# INLINE _CODE #-}

_HEADING :: (Choice p, Applicative f) => p Level (f Level) -> p NodeType (f NodeType)
_HEADING = dimap to fro . right'
  where
    to (HEADING a) = Right a
    to x = Left x
    fro (Left it) = pure it
    fro (Right fa) = HEADING <$> fa
{-# INLINE _HEADING #-}

_LIST :: (Choice p, Applicative f) => p ListAttributes (f ListAttributes) -> p NodeType (f NodeType)
_LIST = dimap to fro . right'
  where
    to (LIST a) = Right a
    to x = Left x
    fro (Left it) = pure it
    fro (Right fa) = LIST <$> fa
{-# INLINE _LIST #-}

_CUSTOM_BLOCK :: (Choice p, Applicative f) => p (T.Text, T.Text) (f (T.Text, T.Text)) -> p NodeType (f NodeType)
_CUSTOM_BLOCK = dimap to fro . right'
  where
    to (CUSTOM_BLOCK a b) = Right (a, b)
    to x = Left x
    fro (Left it) = pure it
    fro (Right fa) = uncurry CUSTOM_BLOCK <$> fa
{-# INLINE _CUSTOM_BLOCK #-}

_CUSTOM_INLINE :: (Choice p, Applicative f) => p (T.Text, T.Text) (f (T.Text, T.Text)) -> p NodeType (f NodeType)
_CUSTOM_INLINE = dimap to fro . right'
  where
    to (CUSTOM_INLINE a b) = Right (a, b)
    to x = Left x
    fro (Left it) = pure it
    fro (Right fa) = uncurry CUSTOM_INLINE <$> fa
{-# INLINE _CUSTOM_INLINE #-}

_CODE_BLOCK :: (Choice p, Applicative f) => p (Info, T.Text) (f (Info, T.Text)) -> p NodeType (f NodeType)
_CODE_BLOCK = dimap to fro . right'
  where
    to (CODE_BLOCK a b) = Right (a, b)
    to x = Left x
    fro (Left it) = pure it
    fro (Right fa) = uncurry CODE_BLOCK <$> fa
{-# INLINE _CODE_BLOCK #-}

_LINK :: (Choice p, Applicative f) => p (Url, Title) (f (Url, Title)) -> p NodeType (f NodeType)
_LINK = dimap to fro . right'
  where
    to (LINK a b) = Right (a, b)
    to x = Left x
    fro (Left it) = pure it
    fro (Right fa) = uncurry LINK <$> fa
{-# INLINE _LINK #-}

_IMAGE :: (Choice p, Applicative f) => p (Url, Title) (f (Url, Title)) -> p NodeType (f NodeType)
_IMAGE = dimap to fro . right'
  where
    to (IMAGE a b) = Right (a, b)
    to x = Left x
    fro (Left it) = pure it
    fro (Right fa) = uncurry IMAGE <$> fa
{-# INLINE _IMAGE #-}