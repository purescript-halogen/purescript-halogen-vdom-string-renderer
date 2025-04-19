module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Halogen.VDom (VDom, ElemName(..))
import Halogen.VDom as V
import Halogen.VDom.DOM.Prop (Prop(..), PropValue, propFromBoolean, propFromInt, propFromNumber, propFromString)
import Halogen.VDom.DOM.StringRenderer (render)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Unsafe.Coerce (unsafeCoerce)

attr ∷ ∀ a. String → String → Prop a
attr key value = Attribute Nothing key value

class IsPropValue v where
  toPropValue :: v -> PropValue

instance isPropValueString :: IsPropValue String where
  toPropValue = propFromString

instance isPropValueBoolean :: IsPropValue Boolean where
  toPropValue = propFromBoolean

instance isPropValueInt :: IsPropValue Int where
  toPropValue = propFromInt

instance isPropValueNumber :: IsPropValue Number where
  toPropValue = propFromNumber

prop :: forall a v. IsPropValue v => String -> v -> Prop a
prop key value = Property key (toPropValue value)

infixr 1 attr as :=
infixr 1 prop as .=

elem ∷ ∀ a. String → Array (Prop a) → Array (VDom (Array (Prop a)) a) → VDom (Array (Prop a)) a
elem n a c = V.Elem Nothing (ElemName n) a (unsafeCoerce c)

keyed ∷ ∀ a. String → Array (Prop a) → Array (Tuple String (VDom (Array (Prop a)) a)) → VDom (Array (Prop a)) a
keyed n a c = V.Keyed Nothing (ElemName n) a (unsafeCoerce c)

text ∷ ∀ a. String → VDom (Array (Prop Void)) a
text a = V.Text a

main ∷ Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  it "works" do
    let
      html ∷ VDom (Array (Prop Void)) Void
      html =
        elem "div" [ "className" .= "container", "id" := "root" ]
          [ elem "label" [ "htmlFor" .= "username" ]
              [ text "Username" ]
          , elem "input" [ "id" := "input" ] []
          , elem "a" [ "href" := "index" ] [ text "Inbox" ]
          , keyed "div" []
              [ "0" /\ elem "span" [] [ text "0" ]
              , "1" /\ elem "span" [] [ text "1" ]
              ]
          ]
    render absurd html `shouldEqual` """<div class="container" id="root"><label for="username">Username</label><input id="input"/><a href="index">Inbox</a><div><span>0</span><span>1</span></div></div>"""
  it "should not escape links in prop attrs" do
    let
      html ∷ VDom (Array (Prop Void)) Void
      html =
        elem "script"
          [ "async" := ""
          , "type" := "application/javascript"
          , "src" := "chunks/defaultVendors~main-063769ad5d827b791041.js"
          ]
          []
    render absurd html `shouldEqual` """<script async="" type="application/javascript" src="chunks/defaultVendors~main-063769ad5d827b791041.js"></script>"""
  it "but should escape links prop names" do
    let
      html ∷ VDom (Array (Prop Void)) Void
      html =
        elem "script"
          [ "application/javascript" := "type"
          , "chunks/defaultVendors~main-063769ad5d827b791041.js" := "src"
          ]
          []
    render absurd html `shouldEqual` """<script application&#x2F;javascript="type" chunks&#x2F;defaultVendors~main-063769ad5d827b791041.js="src"></script>"""
  it "but should render int, boolean, number props" do
    let
      html ∷ VDom (Array (Prop Void)) Void
      html =
        elem "input"
          [ "maxLength" .= 255 -- Int
          , "step" .= 0.5 -- Number
          , "disabled" .= true -- Boolean
          , "readonly" .= false -- Boolean
          ]
          []
    render absurd html `shouldEqual` """<input maxLength="255" step="0.5" disabled/>"""
  describe "escape chars" do
    let
      mkElem char = elem ("div" <> char <> "test") [ "attr" <> char <> "key" := "attr" <> char <> "val", "prop" <> char <> "key" .= "prop" <> char <> "val" ] []
      mkTest char expected = it char $ render absurd (mkElem char) `shouldEqual` expected

    mkTest "\"" """<divtest attr&quot;key="attr&quot;val" prop&quot;key="prop&quot;val"></divtest>"""
    mkTest "'" """<divtest attr&#39;key="attr'val" prop&#39;key="prop'val"></divtest>"""
    mkTest "/" """<divtest attr&#x2F;key="attr/val" prop&#x2F;key="prop/val"></divtest>"""
    mkTest "\\" """<divtest attr\key="attr\val" prop\key="prop\val"></divtest>"""
    mkTest "`" """<divtest attr`key="attr`val" prop`key="prop`val"></divtest>"""
    mkTest "?" """<divtest attr?key="attr?val" prop?key="prop?val"></divtest>"""
    mkTest "!" """<divtest attr!key="attr!val" prop!key="prop!val"></divtest>"""
    mkTest "@" """<divtest attr@key="attr@val" prop@key="prop@val"></divtest>"""
    mkTest "#" """<divtest attr#key="attr#val" prop#key="prop#val"></divtest>"""
    mkTest "$" """<divtest attr$key="attr$val" prop$key="prop$val"></divtest>"""
