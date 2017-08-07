module Halogen.VDom.StringRenderer
  ( render
  , TagType(..)
  ) where

import Prelude

import Data.Array as A
import Data.Maybe (maybe)
import Data.String as S
import Data.Tuple (snd)

import Halogen.VDom (VDom(..), ElemSpec(..), ElemName(..), Namespace(..), runGraft)
import Halogen.VDom.StringRenderer.Util (escape)

-- | Type used to determine whether an element can be rendered as self-closing
-- | element, for example, "<br/>".
data TagType
  = NormalTag
  | SelfClosingTag

derive instance eqTagType ∷ Eq TagType
derive instance ordTagType ∷ Ord TagType

-- | Renders a `VDom` tree to a string using the specified tag type scheme,
-- | attribute renderer, and widget renderer.
render
  ∷ ∀ a w
  . (ElemName → TagType)
  → (a → String)
  → (w → String)
  → VDom a w
  → String
render getTagType renderAttrs renderWidget = go
  where
  go ∷ VDom a w → String
  go = case _ of
    Text s → escape s
    Elem elem children → renderElement elem children
    Keyed elem kchildren → renderElement elem (map snd kchildren)
    Widget w → renderWidget w
    Grafted g → go (runGraft g)

  renderElement ∷ ElemSpec a → Array (VDom a w) → String
  renderElement (ElemSpec mns en@(ElemName name) attrs) children =
    let
      as = renderAttrs attrs
      as' = maybe as (\(Namespace ns) -> "xmlns=\"" <> escape ns <> "\"" <> if S.null as then "" else " " <> as) mns
    in
      "<" <> name <> (if S.null as then "" else " ") <> as <>
        if A.null children
        then if getTagType en == SelfClosingTag then "/>" else "></" <> name <> ">"
        else ">" <> S.joinWith "" (map go children) <> "</" <> name <> ">"
