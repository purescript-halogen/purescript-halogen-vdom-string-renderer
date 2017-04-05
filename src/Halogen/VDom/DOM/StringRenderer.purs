module Halogen.VDom.DOM.StringRenderer where

import Prelude
import Halogen.VDom (VDom(..), ElemSpec(..), ElemName(..), Namespace(..), runGraft)
import Halogen.VDom.DOM.Prop (Prop(..))
import Data.Tuple (snd)
import Data.String as S
import Data.Maybe (maybe)
import Data.Array as A
import Data.Foldable as F
import Halogen.VDom.StringRenderer as VSR


render ∷ ∀ i w. (w → String) → VDom (Array (Prop i)) w → String
render renderWidget = VSR.render getTagType renderProps renderWidget
  where
  getTagType :: ElemName -> VSR.TagType
  getTagType en
    | F.elem en voidElements = VSR.SelfClosingTag
    | otherwise = VSR.NormalTag
  renderProps :: Array (Prop i) -> String
  renderProps = S.joinWith " " <<< A.mapMaybe renderProp
  renderProp :: Prop i -> Maybe String
  renderProp = case _ of
    Attribute (Maybe Namespace) String String
    Property String PropValue
    Handler _ _ → Nothing
    Ref _ → Nothing

voidElements :: Array ElemName
voidElements =
  [ ElemName "area"
  , ElemName "base"
  , ElemName "br"
  , ElemName "col"
  , ElemName "embed"
  , ElemName "hr"
  , ElemName "img"
  , ElemName "input"
  , ElemName "isindex" -- legacy XHTML
  , ElemName "keygen"
  , ElemName "link"
  , ElemName "meta"
  , ElemName "param"
  , ElemName "source"
  , ElemName "track"
  , ElemName "wbr"
  ]
