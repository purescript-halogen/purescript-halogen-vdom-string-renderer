module Halogen.VDom.StringRenderer
  ( render
  , TagType(..)
  ) where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe, maybe)
import Data.String as S
import Data.Tuple (snd)

import Halogen.VDom (VDom(..), ElemName(..), Namespace(..), runGraft)
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
  ∷ ∀ attrs widget
  . (ElemName → TagType)
  → (attrs → String)
  → (widget → String)
  → VDom attrs widget
  → String
render getTagType renderAttrs renderWidget = go
  where
  go ∷ VDom attrs widget → String
  go = case _ of
    Text s → escape s
    Elem namespace elementName attrs children → renderElement namespace elementName attrs children
    Keyed namespace elementName attrs kchildren → renderElement namespace elementName attrs (map snd kchildren)
    Widget widget → renderWidget widget
    Grafted g → go (runGraft g)

  renderElement ∷ (Maybe Namespace) -> ElemName -> attrs → Array (VDom attrs widget) → String
  renderElement maybeNamespace elemName@(ElemName name) attrs children =
    let
      as = renderAttrs attrs
      as' = maybe as (\(Namespace ns) -> "xmlns=\"" <> escape ns <> "\"" <> if S.null as then "" else " " <> as) maybeNamespace
    in
      "<" <> name <> (if S.null as' then "" else " ") <> as' <>
        if A.null children
        then if getTagType elemName == SelfClosingTag then "/>" else "></" <> name <> ">"
        else ">" <> S.joinWith "" (map go children) <> "</" <> name <> ">"
