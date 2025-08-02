{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Text.Atom.Xmlbf where

import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.XML.Types as Data.XML
import Data.XML.Types.Xmlbf ()
import qualified Data.XML.Types.Xmlbf as XMLInstances
import qualified Text.Atom.Feed as Atom
import qualified Text.Atom.Feed.Export as Atom
import qualified Xmlbf as Xml
import Xmlbf (ToXml (..))

instance ToXml Atom.Feed where
  toXml Atom.Feed {..} =
    Xml.element "feed" [("xmlns", Atom.atomNS), ("xmlns:thr", Atom.atomThreadNS)] $
      Xml.element "id" [] (Xml.text feedId)
        <> xmlTextContent "title" feedTitle
        <> Xml.element "updated" [] (Xml.text feedUpdated)
        <> (toXml =<< feedLinks)
        <> (Xml.element "author" [] . toXml =<< feedAuthors)
        <> (toXml =<< feedCategories)
        <> (Xml.element "contributor" [] . toXml =<< feedContributors)
        <> maybe [] toXml feedGenerator
        <> maybe [] (Xml.element "icon" [] . Xml.text) feedIcon
        <> maybe [] (Xml.element "logo" [] . Xml.text) feedLogo
        <> maybe [] (xmlTextContent "rights") feedRights
        <> maybe [] (xmlTextContent "subtitle") feedSubtitle
        <> (toXml =<< feedEntries)
        <> (toXml =<< feedOther)

xmlTextContent :: Text -> Atom.TextContent -> [Xml.Node]
xmlTextContent name = \case
  Atom.TextString s ->
    Xml.element name [("type", "text")] $ Xml.text s
  Atom.HTMLString s ->
    Xml.element name [("type", "html")] $ Xml.text s
  Atom.XHTMLString e ->
    Xml.element name [("type", "xhtml")] $ toXml e

instance ToXml Atom.Link where
  toXml Atom.Link {..} =
    Xml.element "link" (HashMap.fromList attrs) (toXml =<< linkOther)
    where
      attrs :: [(Text, Text)]
      attrs =
        ("href", linkHref)
          : maybe [] (pure . ("rel",) . either id id) linkRel
            <> maybe [] (pure . ("type",)) linkHrefLang
            <> maybe [] (pure . ("title",)) linkTitle
            <> maybe [] (pure . ("length",)) linkLength
            <> (XMLInstances.xmlbfAttr <$> linkAttrs)

xmlAuthor :: Atom.Person -> [Xml.Node]
xmlAuthor =
  Xml.element "author" [] . toXml

instance ToXml Atom.Person where
  toXml Atom.Person {..} =
    Xml.element "name" mempty (Xml.text personName)
      <> maybe [] (Xml.element "uri" [] . Xml.text) personURI
      <> maybe [] (Xml.element "email" [] . Xml.text) personEmail
      <> (toXml =<< personOther)

instance ToXml Atom.Category where
  toXml Atom.Category {..} =
    Xml.element "category" (HashMap.fromList attrs) $ toXml =<< catOther
    where
      attrs =
        ("term", catTerm)
          : maybe [] (pure . ("scheme",)) catScheme
          <> maybe [] (pure . ("label",)) catLabel

instance Xml.ToXml Atom.Generator where
  toXml Atom.Generator {..} =
    Xml.element "generator" (HashMap.fromList attrs)
      $ Xml.text genText
    where
      attrs =
        maybe [] (pure . ("uri",)) genURI
          <> maybe [] (pure . ("version",)) genVersion

instance Xml.ToXml Atom.Entry where
  toXml Atom.Entry {..} =
    Xml.element "entry" (HashMap.fromList attrs) $
      Xml.element "id" [] (Xml.text entryId)
        <> xmlTextContent "title" entryTitle
        <> Xml.element "updated" [] (Xml.text entryUpdated)
        <> (Xml.element "author" [] . toXml =<< entryAuthors)
        <> (toXml =<< entryCategories)
        <> maybe [] toXml entryContent
        <> (Xml.element "contributor" [] . toXml =<< entryContributor)
        <> (toXml =<< entryLinks)
        <> maybe [] (Xml.element "published" [] . Xml.text) entryPublished
        <> maybe [] (xmlTextContent "rights") entryRights
        <> maybe [] toXml entrySource
        <> maybe [] (xmlTextContent "summary") entrySummary
        <> maybe [] toXml entryInReplyTo
        <> maybe [] toXml entryInReplyTotal
        <> (toXml =<< entryOther)
    where
      attrs = XMLInstances.xmlbfAttr <$> entryAttrs

instance Xml.ToXml Atom.EntryContent where
  toXml = \case
    Atom.TextContent t ->
      Xml.element "content" [("type", "text")] $ Xml.text t
    Atom.HTMLContent t ->
      Xml.element "content" [("type", "html")] $ Xml.text t
    Atom.XHTMLContent x ->
      Xml.element "content" [("type", "xhtml")] $ toXml x
    Atom.MixedContent ty cs ->
      Xml.element "content" (maybe [] (\t -> [("type", t)]) ty) $ toXml =<< cs
    Atom.ExternalContent ty src ->
      Xml.element "content" (HashMap.fromList attrs) []
      where
        attrs = ("src", src) : maybe [] (pure . ("type",)) ty

instance Xml.ToXml Atom.Source where
  toXml Atom.Source {..} =
    Xml.element "source" [] $
      (toXml =<< sourceOther)
        <> (Xml.element "author" [] . toXml =<< sourceAuthors)
        <> (toXml =<< sourceCategories)
        <> maybe [] toXml sourceGenerator
        <> maybe [] (Xml.element "icon" [] . Xml.text) sourceIcon
        <> maybe [] (Xml.element "id" [] . Xml.text) sourceId
        <> (toXml =<< sourceLinks)
        <> maybe [] (Xml.element "logo" [] . Xml.text) sourceLogo
        <> maybe [] (xmlTextContent "rights") sourceRights
        <> maybe [] (xmlTextContent "subtitle") sourceSubtitle
        <> maybe [] (xmlTextContent "title") sourceTitle
        <> maybe [] (Xml.element "updated" [] . Xml.text) sourceUpdated

instance Xml.ToXml Atom.InReplyTo where
  toXml Atom.InReplyTo {..} =
    Xml.element "thr:in-reply-to" (HashMap.fromList attrs) $
      toXml =<< replyToContent
    where
      attrs =
        ("thr:ref", replyToRef)
          : maybe [] (pure . ("thr:href",)) replyToHRef
          <> maybe [] (pure . ("thr:type",)) replyToType
          <> maybe [] (pure . ("thr:source",)) replyToSource
          <> (XMLInstances.xmlbfAttr <$> replyToOther)

instance Xml.ToXml Atom.InReplyTotal where
  toXml Atom.InReplyTotal {..} =
    Xml.element "thr:total" (HashMap.fromList attrs)
      $ Xml.text
      $ Text.pack
      $ show replyToTotal
    where
      attrs = XMLInstances.xmlbfAttr <$> replyToTotalOther
