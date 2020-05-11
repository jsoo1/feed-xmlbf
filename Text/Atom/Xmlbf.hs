{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Text.Atom.Xmlbf where

import Data.Foldable (fold)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (maybeToList)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy
import Data.Text.Lazy (fromStrict, toStrict)
import qualified Text.Atom.Feed as Atom
import qualified Text.Atom.Feed.Export as Atom
import qualified Xmlbf as Xml
import Xmlbf (ToXml (..))

instance ToXml Atom.Feed where
  toXml Atom.Feed {..} =
    Xml.element "feed" [("xmlns", Atom.atomNS)] $
      Xml.element "id" [] (Xml.text (fromStrict feedId))
        <> xmlTextContent "title" feedTitle
        <> Xml.element "updated" [] (Xml.text (fromStrict feedUpdated))
        <> (xmlLink =<< feedLinks)

xmlTextContent :: Text -> Atom.TextContent -> [Xml.Node]
xmlTextContent name = \case
  Atom.TextString s ->
    Xml.element name [("type", "text")] $ Xml.text $ fromStrict s
  Atom.HTMLString s ->
    Xml.element name [("type", "html")] $ Xml.text $ fromStrict s
  Atom.XHTMLString e ->
    Xml.element name [("type", "xhtml")] $ toXml e

xmlLink :: Atom.Link -> [Xml.Node]
xmlLink Atom.Link {..} =
  Xml.element "link" attrs (toXml =<< linkOther)
  where
    attrs =
      HashMap.fromList $
        fold
          [ pure ("href", linkHref),
            ("rel",) . either id id <$> maybeToList linkRel,
            ("type",) <$> maybeToList linkHrefLang,
            ("title",) <$> maybeToList linkTitle,
            ("length",) <$> maybeToList linkLength,
            (\(name, content) -> (Text.pack (show name), foldMap _ content)) <$> linkAttrs
          ]

xmlAuthor :: Atom.Person -> Xml.Node
xmlAuthor =
  Xml.element "author" [] . toXml

instance ToXml Atom.Person where
  toXml Atom.Person {..} =
    Xml.element "name" personName $
      [ Xml.element "uri" [] . Xml.text <$> maybeToList personURI,
        Xml.element "email" [] . Xml.text <$> maybeToList personEmail
      ]
        <> toXml personOther

instance Xml.FromXml Atom.Feed where
  fromXml = _
