module Halogen.VDom.StringRenderer.Util where

import Prelude

import Data.String.Regex (Regex, replace', replace)
import Data.String.Regex.Flags (global, unicode)
import Data.String.Regex.Unsafe (unsafeRegex)

-- TODO: use https://github.com/mathiasbynens/he ?
escapeHtmlEntity ∷ String → String
escapeHtmlEntity = replace' escapeRegex (const <<< escapeChar)
  where
  escapeRegex ∷ Regex
  escapeRegex = unsafeRegex """[&"'<>\t\n\r\/]""" global

  escapeChar ∷ String → String
  escapeChar = case _ of
    "&" -> "&amp;"
    "\"" -> "&quot;"
    "'" → "&#39;"
    "<" -> "&lt;"
    ">" -> "&gt;"
    "\t" -> "&#x9;"
    "\n" -> "&#xA;"
    "\r" -> "&#xD;"
    "/" → "&#x2F;"
    ch → ch

-- just like https://github.com/jsdom/w3c-xmlserializer/blob/83115f8ecce8ed77a2a907c74407b2c671751463/lib/attributes.js#L24
-- NOTE: it will not escape / in https://mywebsite.com (check this test https://github.com/jsdom/w3c-xmlserializer/blob/83115f8ecce8ed77a2a907c74407b2c671751463/test/test.js#L58-L70)
escapeAttributeValue ∷ String → String
escapeAttributeValue = replace' escapeRegex (const <<< escapeChar)
  where
  escapeRegex ∷ Regex
  escapeRegex = unsafeRegex """[&"<>\t\n\r]""" (global <> unicode)

  escapeChar ∷ String → String
  escapeChar = case _ of
    "&" -> "&amp;"
    "\"" -> "&quot;"
    "<" -> "&lt;"
    ">" -> "&gt;"
    "\t" -> "&#x9;"
    "\n" -> "&#xA;"
    "\r" -> "&#xD;"
    ch → ch

-- just like https://github.com/jsdom/xml-name-validator/blob/836f307eec81279d2b1655587892e38a1effe039/lib/xml-name-validator.js#L4
-- but will just remove all chars that are not `name`
--
-- Where we use it in `<my-element my-attr="my-attr-value"></my-element>`? We use it to clean `my-element` and `my-attr`.
-- Just like w3c-xmlserializer does for element names (https://github.com/jsdom/w3c-xmlserializer/blob/83115f8ecce8ed77a2a907c74407b2c671751463/lib/serialize.js#L178) and attribute names (https://github.com/jsdom/w3c-xmlserializer/blob/83115f8ecce8ed77a2a907c74407b2c671751463/lib/attributes.js#L112).
cleanNonXnvName ∷ String → String
cleanNonXnvName = replace invalidCharsRegex ""
  where
  invalidCharsRegex = unsafeRegex """[^[:A-Z_a-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\u{10000}-\u{EFFFF}\-.0-9\u00B7\u0300-\u036F\u203F-\u2040]""" (global <> unicode)

-- just like https://github.com/jsdom/w3c-xmlserializer/blob/83115f8ecce8ed77a2a907c74407b2c671751463/lib/serialize.js#L155
-- removes all non XML_CHAR characters and changes escapes &<>
cleanAndEscapeTextNode ∷ String → String
cleanAndEscapeTextNode = replace' escapeRegex (const <<< escapeChar) <<< cleanInvalidChars
  where
  cleanInvalidChars :: String -> String
  cleanInvalidChars = replace invalidCharsRegex ""
    where
    invalidCharsRegex = unsafeRegex """[^\x09\x0A\x0D\x20-\uD7FF\uE000-\uFFFD\u{10000}-\u{10FFFF}]""" (global <> unicode)

  escapeRegex ∷ Regex
  escapeRegex = unsafeRegex "[&<>]" global

  escapeChar ∷ String → String
  escapeChar = case _ of
    "&" → "&amp;"
    "<" → "&lt;"
    ">" → "&gt;"
    ch → ch
