module Halogen.VDom.StringRenderer.Util (escape) where

import Prelude
import Data.String.Regex (Regex, replace')
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)

escapeRegex ∷ Regex
escapeRegex = unsafeRegex "[\\\"\\\'/&<>]" global

escapeChar ∷ String → String
escapeChar = case _ of
  "\"" → "&quot;"
  "'"  → "&#39;"
  "/"  → "&#x2F;"
  "&"  → "&amp;"
  "<"  → "&lt;"
  ">"  → "&gt;"
  ch   → ch

escape ∷ String → String
escape = replace' escapeRegex (const <<< escapeChar)
