---
date: 2025-05-13
tags: haskell self-hosting programming
---

# Reading Time Estimates for Pandoc Based Blog Generators

If you, like me, are one of those who have written their own static-site generators based on Pandoc using Haskell, or use a framework made on top of it[^1], this post provides a little code snippet to compute the reading time estimate for your blog posts. You can stick this in your setup at the right place, and get pretty accurate estimates[^2].

[^1]: Like [Hakyll](https://jaspervdj.be/hakyll/), [Slick](https://github.com/ChrisPenner/slick) or [Ema](https://ema.srid.ca/).

[^2]: I cross-checked the estimates with those provided by the [Miniflux](https://miniflux.app/) feed reader, and they are quite close.

```haskell
module Pandoc.ReadingTimeEstimate (readingTimeEstimateMinutes) where

import Data.Char (isAlphaNum)
import Data.Monoid (Sum (..))
import Data.Text qualified as Text
import Text.HTML.TagSoup (innerText, parseTags)
import Text.Pandoc.Definition

readingTimeEstimateMinutes :: Pandoc -> Int
readingTimeEstimateMinutes = round . (/ wpm) . fromIntegral . wordCount
  where
    wpm = 220

wordCount :: Pandoc -> Int
wordCount (Pandoc _ blocks) = getSum $ foldMap blockWCount blocks
  where
    blockWCount = \case
      Plain is -> inlinesWCount is
      Para is -> inlinesWCount is
      CodeBlock _ s -> tWordCount s
      RawBlock (Format "html") s -> tWordCount . innerText $ parseTags s
      RawBlock _ s -> tWordCount s
      LineBlock iss -> inlinesListWCount iss
      BlockQuote bs -> blocksWCount bs
      OrderedList _ bss -> blocksListWCount bss
      BulletList bss -> blocksListWCount bss
      DefinitionList ls ->
        foldMap (\(is, bss) -> inlinesWCount is + blocksListWCount bss) ls
      Header _ _ is -> inlinesWCount is
      HorizontalRule -> 0
      Table _ cap@(Caption _ _) _ (TableHead _ thrs) tbs (TableFoot _ tfrs) ->
        captionWCount cap
          + rowsWCount thrs
          + rowsWCount tfrs
          + foldMap tableBodyWCount tbs
      Div (id', classes, _) bs'
        | id' == "refs" && "references" `elem` classes -> 0
        | otherwise -> blocksWCount bs'
      Figure _ cap@(Caption _ _) bs' ->
        captionWCount cap + blocksWCount bs'

    blocksWCount = foldMap blockWCount
    blocksListWCount = foldMap blocksWCount

    captionWCount (Caption msc bs) =
      maybe (0) inlinesWCount msc + blocksWCount bs

    rowsWCount = foldMap rowWCount
    rowWCount (Row _ cells) = foldMap cellWCount cells
    cellWCount (Cell _ _ _ _ bs) = blocksWCount bs
    tableBodyWCount (TableBody _ _ hr br) = rowsWCount hr + rowsWCount br

    inlineWCount = \case
      Str s -> tWordCount s
      Emph is -> inlinesWCount is
      Strong is -> inlinesWCount is
      Strikeout is -> inlinesWCount is
      Underline is -> inlinesWCount is
      Superscript is -> inlinesWCount is
      Subscript is -> inlinesWCount is
      SmallCaps is -> inlinesWCount is
      Quoted _ is -> inlinesWCount is
      Cite _ is -> inlinesWCount is
      Code _ s -> tWordCount s
      Span _ s -> inlinesWCount s
      Space -> 0
      SoftBreak -> 0
      LineBreak -> 0
      Math _ s -> tWordCount s
      RawInline (Format "html") s -> tWordCount . innerText $ parseTags s
      RawInline _ s -> tWordCount s
      Link _ is _ -> inlinesWCount is
      Image _ is _ -> inlinesWCount is
      Note bs -> blocksWCount bs

    inlinesWCount = foldMap inlineWCount
    inlinesListWCount = foldMap inlinesWCount

    tWordCount =
      Sum
        . length
        . filter (not . Text.all (not . isAlphaNum))
        . Text.words
```

The `wordCount` function takes a [`Pandoc`](https://hackage.haskell.org/package/pandoc-types/docs/Text-Pandoc-Definition.html#t:Pandoc) tree data structure, and recursively traverses it to get the count of textual words. For raw HTML code embedded in the tree, it parses the HTML code using the [tagsoup](https://github.com/ndmitchell/tagsoup) library, and extracts the inner text after removing the tags. The words are counted after removing words that are composed of only punctuation.

The `readingTimeEstimateMinutes` function then divides the word count by a words-per-minute (`wpm`) parameter — that I've set here to 220, close to the average reading speed of literate adult humans — to return the reading time estimate in minutes. You can change the `wpm` parameter as per your liking.

That's it. I hope this is helpful to you.

Like, share, or comment on this post on [Mastodon](https://fantastic.earth/@abnv/114501531258312548){:class="mastodon-link"}.
