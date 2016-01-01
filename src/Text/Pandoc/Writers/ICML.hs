{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

{- |
   Module      : Text.Pandoc.Writers.ICML
   Copyright   : Copyright (C) 2013 github.com/mb21
   License     : GNU GPL, version 2 or above

   Stability   : alpha

Conversion of 'Pandoc' documents to Adobe InCopy ICML, a stand-alone XML format
which is a subset of the zipped IDML format for which the documentation is
available here: http://wwwimages.adobe.com/www.adobe.com/content/dam/Adobe/en/devnet/indesign/sdk/cs6/idml/idml-specification.pdf
InCopy is the companion word-processor to Adobe InDesign and ICML documents can be integrated
into InDesign with File -> Place.
-}
module Text.Pandoc.Writers.ICML (writeICML) where
import qualified Text.Pandoc.Definition as P
import Text.Pandoc.XML
import Text.Pandoc.Readers.TeXMath (texMathToInlines)
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Shared (fetchItem, warn)
import Text.Pandoc.Options
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Pretty
import Text.Pandoc.ImageSize
import Data.List (intersperse)
import Control.Monad.State
import Network.URI (isURI)
import qualified Data.Set as Set

type CharStyle = [CharName]
type ParStyle  = [ParName]
type Hyperlink = [(Int, String)]

data WriterState = WriterState{
    blockStyles  :: Set.Set ParStyle
  , inlineStyles :: Set.Set CharStyle
  , links        :: Hyperlink
  , listDepth    :: Int
  , maxListDepth :: Int
  , stOpts       :: WriterOptions
  }

type WS a = StateT WriterState IO a

defaultWriterState :: WriterState
defaultWriterState = WriterState{
    blockStyles  = Set.empty
  , inlineStyles = Set.empty
  , links        = []
  , listDepth    = 1
  , maxListDepth = 0
  , stOpts       = def
  }

-- inline names (appear in InDesign's character styles pane)
data CharName = Italic
              | Bold
              | Strikeout
              | Superscript
              | Subscript
              | SmallCaps
              | Code
              | Link
              | Cite
              deriving (Show, Eq, Ord)

-- block element names (appear in InDesign's paragraph styles pane)
data ParName  = Paragraph
              | Figure
              | Caption
              | CodeBlock
              | Blockquote
              | NumList
              | BulList
              | DefListTerm
              | DefListDef
              | Header Int
              | TablePar
              | TableHeader
              | TableCaption
              | LeftAlign
              | RightAlign
              | CenterAlign
              | Footnote
              | BeginsWith Int
              | ListLowerRoman
              | ListUpperRoman
              | ListLowerAlpha
              | ListUpperAlpha
              | ListSubParagraph
              | First
              deriving (Show, Eq, Ord)


-- | Convert Pandoc document to string in ICML format.
writeICML :: WriterOptions -> P.Pandoc -> IO String
writeICML opts (P.Pandoc meta blocks) = do
  let sst = defaultWriterState{ stOpts = opts }
      colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
      render' = render colwidth
      renderMeta f s = liftM (render' . fst) $ runStateT (f [] s) sst
  metadata <- metaToJSON opts
             (renderMeta blocksToICML)
             (renderMeta inlinesToICML)
             meta
  (doc, st) <- runStateT (blocksToICML [] blocks) sst
  let main    = render' doc
      context = defField "body" main
              $ defField "charStyles" (render' $ charStylesToDoc $ inlineStyles st)
              $ defField "parStyles"  (render' $ parStylesToDoc  $ blockStyles  st)
              $ defField "hyperlinks" (render' $ hyperlinksToDoc $ links st)
              $ metadata
  return $ if writerStandalone opts
              then renderTemplate' (writerTemplate opts) context
              else main

-- | Auxilary functions for parStylesToDoc and charStylesToDoc.
contains :: (Eq a) => [a] -> (a, (String, String)) -> [(String, String)]
contains s rule =
  if (fst rule) `elem` s
     then [snd rule]
     else []

-- | The monospaced font to use as default.
monospacedFont :: Doc
monospacedFont = inTags False "AppliedFont" [("type", "string")] $ text "Courier New"

-- | How much to indent blockquotes etc.
defaultIndent :: Int
defaultIndent = 20

-- | How much to indent numbered lists before the number.
defaultListIndent :: Int
defaultListIndent = 10

-- other constants
lineSeparator :: String
lineSeparator = "&#x2028;"

-- | Convert a set of block styles to the ICML listing of Paragraph Styles.
parStylesToDoc :: Set.Set ParStyle -> Doc
parStylesToDoc = vcat . (map makeStyle) . Set.toAscList
  where
    makeStyle s =
      let count key = length . filter (== key)
          attrs = concat $ map (contains s) $ [
                               (DefListTerm, ("BulletsAndNumberingListType", "BulletList"))
                             , (DefListTerm, ("FontStyle", "Bold"))
                             , (TableHeader, ("FontStyle", "Bold"))
                             , (LeftAlign,   ("Justification", "LeftAlign"))
                             , (RightAlign,  ("Justification", "RightAlign"))
                             , (CenterAlign, ("Justification", "CenterAlign"))
                             , (Header 1, ("PointSize", "36"))
                             , (Header 2, ("PointSize", "30"))
                             , (Header 3, ("PointSize", "24"))
                             , (Header 4, ("PointSize", "18"))
                             , (Header 5, ("PointSize", "14"))
                             ]
          -- what is the most nested list type, if any?
          (isBulletList, isOrderedList) = findList s
            where
              findList [] = (False, False)
              findList (x:xs) | x == BulList = (True, False)
                              | x == NumList = (False, True)
                              | otherwise = findList xs
          nBuls = count BulList s
          nOrds = count NumList s
          attrs' = numbering ++ listType ++ indent ++ attrs
            where
              numbering | isOrderedList = [("NumberingExpression", "^#.^t"), ("NumberingLevel", show nOrds)]
                        | otherwise     = []
              notSubPar = not $ ListSubParagraph `elem` s
              listType | notSubPar && isOrderedList
                           = [("BulletsAndNumberingListType", "NumberedList")]
                       | notSubPar && isBulletList
                           = [("BulletsAndNumberingListType", "BulletList")]
                       | otherwise = []
              indent = [("LeftIndent", show i)]
                where
                  i = max 0 $ defaultListIndent*(nBuls + nOrds - 1)
                            + defaultIndent*(count Blockquote s + count DefListDef s)
          props = inTags True "Properties" [] $ (basedOn $$ tabList $$ numbForm)
            where
              font = if CodeBlock `elem` s
                        then monospacedFont
                        else empty
              basedOn = inTags False "BasedOn" [("type", "object")] (text "$ID/NormalParagraphStyle") $$ font
              tabList = if isBulletList
                           then inTags True "TabList" [("type","list")] $ inTags True "ListItem" [("type","record")]
                                $ vcat [
                                    inTags False "Alignment" [("type","enumeration")] $ text "LeftAlign"
                                  , inTags False "AlignmentCharacter" [("type","string")] $ text "."
                                  , selfClosingTag "Leader" [("type","string")]
                                  , inTags False "Position" [("type","unit")] $ text
                                      $ show $ defaultListIndent * (nBuls + nOrds)
                                  ]
                           else empty
              makeNumb name = inTags False "NumberingFormat" [("type", "string")] (text name)
              numbForm | ListLowerRoman `elem` s = makeNumb "i, ii, iii, iv..."
                       | ListUpperRoman `elem` s = makeNumb "I, II, III, IV..."
                       | ListLowerAlpha `elem` s = makeNumb "a, b, c, d..."
                       | ListUpperAlpha `elem` s = makeNumb "A, B, C, D..."
                       | otherwise = empty
          stlStr = showParStyle s
      in  inTags True "ParagraphStyle" ([("Self", "ParagraphStyle/"++stlStr), ("Name", stlStr)] ++ attrs') props

-- | Convert a set of inline styles to the ICML listing of Character Styles.
charStylesToDoc :: Set.Set CharStyle -> Doc
charStylesToDoc = vcat . (map makeStyle) . Set.toAscList
  where
    makeStyle s =
      let attrs = concat $ map (contains s) [
                               (Strikeout,   ("StrikeThru", "true"))
                             , (Superscript, ("Position", "Superscript"))
                             , (Subscript,   ("Position", "Subscript"))
                             , (SmallCaps,   ("Capitalization", "SmallCaps"))
                             ]
          attrs' | Italic `elem` s && Bold `elem` s = ("FontStyle", "Bold Italic") : attrs
                 | Bold   `elem` s                  = ("FontStyle", "Bold") : attrs
                 | Italic `elem` s                  = ("FontStyle", "Italic") : attrs
                 | otherwise                        = attrs
          props = inTags True "Properties" [] $
                    inTags False "BasedOn" [("type", "object")] (text "$ID/NormalCharacterStyle") $$ font
                  where
                    font =
                      if Code `elem` s
                         then monospacedFont
                         else empty
          stlStr = showCharStyle s
      in  inTags True "CharacterStyle" ([("Self", "CharacterStyle/"++stlStr), ("Name", stlStr)] ++ attrs') props

-- | Escape colon characters as %3a
escapeColons :: String -> String
escapeColons (x:xs)
  | x == ':' = "%3a" ++ escapeColons xs
  | otherwise = x : escapeColons xs
escapeColons []     = []

-- | Convert a list of (identifier, url) pairs to the ICML listing of hyperlinks.
hyperlinksToDoc :: Hyperlink -> Doc
hyperlinksToDoc []     = empty
hyperlinksToDoc (x:xs) = hyp x $$ hyperlinksToDoc xs
  where
    hyp (ident, url) = hdest $$ hlink
      where
        hdest = selfClosingTag "HyperlinkURLDestination"
                  [("Self", "HyperlinkURLDestination/"++(escapeColons url)), ("Name","link"), ("DestinationURL",url), ("DestinationUniqueKey","1")] -- HyperlinkURLDestination with more than one colon crashes CS6
        hlink = inTags True "Hyperlink" [("Self","uf-"++show ident),  ("Name",url),
                    ("Source","htss-"++show ident), ("Visible","true"), ("DestinationUniqueKey","1")]
                  $ inTags True "Properties" []
                  $ inTags False "BorderColor" [("type","enumeration")] (text "Black")
                  $$ (inTags False "Destination" [("type","object")]
                  $ text $ "HyperlinkURLDestination/"++(escapeColons (escapeStringForXML url))) -- HyperlinkURLDestination with more than one colon crashes CS6


-- | Convert a list of Pandoc blocks to ICML.
blocksToICML :: ParStyle -> [P.Block] -> WS Doc
blocksToICML style lst = do
  docs <- mapM (blockToICML style) lst
  return $ intersperseBrs docs

-- | Convert a Pandoc block element to ICML.
blockToICML :: ParStyle -> P.Block -> WS Doc
blockToICML style (P.Plain lst) = parStyle style lst
-- title beginning with fig: indicates that the image is a figure
blockToICML style (P.Para img@[P.Image _ txt (_,'f':'i':'g':':':_)]) = do
  figure  <- parStyle (Figure:style) img
  caption <- parStyle (Caption:style) txt
  return $ intersperseBrs [figure, caption]
blockToICML style (P.Para lst) = parStyle (Paragraph:style) lst
blockToICML style (P.CodeBlock _ str) = parStyle (CodeBlock:style) $ [P.Str str]
blockToICML _ (P.RawBlock f str)
  | f == P.Format "icml" = return $ text str
  | otherwise          = return empty
blockToICML style (P.BlockQuote blocks) = blocksToICML (Blockquote:style) blocks
blockToICML style (P.OrderedList attribs lst) = listItemsToICML NumList style (Just attribs) lst
blockToICML style (P.BulletList lst) = listItemsToICML BulList style Nothing lst
blockToICML style (P.DefinitionList lst) = intersperseBrs `fmap` mapM (definitionListItemToICML style) lst
blockToICML style (P.Header lvl _ lst) =
  let stl = (Header lvl):style
  in parStyle stl lst
blockToICML _ P.HorizontalRule = return empty -- we could insert a page break instead
blockToICML style (P.Table caption aligns widths headers rows) =
  let style' = TablePar : style
      noHeader  = all null headers
      nrHeaders = if noHeader
                     then "0"
                     else "1"
      nrRows = length rows
      nrCols = if null rows
                  then 0
                  else length $ head rows
      rowsToICML [] _ = return empty
      rowsToICML (col:rest) rowNr =
        liftM2 ($$) (colsToICML col rowNr (0::Int)) $ rowsToICML rest (rowNr+1)
      colsToICML [] _ _ = return empty
      colsToICML (cell:rest) rowNr colNr = do
        let stl  = if rowNr == 0 && not noHeader
                      then TableHeader:style'
                      else style'
            alig = aligns !! colNr
            stl' | alig == P.AlignLeft = LeftAlign : stl
                 | alig == P.AlignRight = RightAlign : stl
                 | alig == P.AlignCenter = CenterAlign : stl
                 | otherwise = stl
        c <- blocksToICML stl' cell
        let cl = return $ inTags True "Cell"
                   [("Name", show colNr ++":"++ show rowNr), ("AppliedCellStyle","CellStyle/Cell")] c
        liftM2 ($$) cl $ colsToICML rest rowNr (colNr+1)
  in  do
      let tabl = if noHeader
                    then rows
                    else headers:rows
      cells <- rowsToICML tabl (0::Int)
      let colWidths w = if w > 0
                           then [("SingleColumnWidth",show $ 500 * w)]
                           else []
      let tupToDoc tup = selfClosingTag "Column" $ [("Name",show $ fst tup)] ++ (colWidths $ snd tup)
      let colDescs = vcat $ map tupToDoc $ zip [0..nrCols-1] widths
      let tableDoc = return $ inTags True "Table" [
                         ("AppliedTableStyle","TableStyle/Table")
                       , ("HeaderRowCount", nrHeaders)
                       , ("BodyRowCount", show nrRows)
                       , ("ColumnCount", show nrCols)
                       ] (colDescs $$ cells)
      liftM2 ($$) tableDoc $ parStyle (TableCaption:style) caption
blockToICML style (P.Div _ lst) = blocksToICML style lst
blockToICML _ P.Null = return empty

-- | Convert a list of lists of blocks to ICML list items.
listItemsToICML :: ParName -> ParStyle -> Maybe P.ListAttributes -> [[P.Block]] -> WS Doc
listItemsToICML _ _ _ [] = return empty
listItemsToICML listType style attribs (first:rest) = do
  st <- get
  put st{ listDepth = 1 + listDepth st}
  let stl = listType:style
  let f = listItemToICML stl True attribs first
  let r = map (listItemToICML stl False attribs) rest
  docs <- sequence $ f:r
  s    <- get
  let maxD = max (maxListDepth s) (listDepth s)
  put s{ listDepth = 1, maxListDepth = maxD }
  return $ intersperseBrs docs

-- | Convert a list of blocks to ICML list items.
listItemToICML :: ParStyle -> Bool -> Maybe P.ListAttributes -> [P.Block] -> WS Doc
listItemToICML style isFirst attribs item =
  let makeNumbStart (Just (i, numbStl, _)) =
        let doN P.DefaultStyle = []
            doN P.LowerRoman = [ListLowerRoman]
            doN P.UpperRoman = [ListUpperRoman]
            doN P.LowerAlpha = [ListLowerAlpha]
            doN P.UpperAlpha = [ListUpperAlpha]
            doN _ = []
            bw = if i > 1
                    then [BeginsWith i]
                    else []
        in  doN numbStl ++ bw
      makeNumbStart Nothing = []
      stl = if isFirst
               then First:style
               else style
      stl' = makeNumbStart attribs ++ stl
  in  if length item > 1
         then do
           let insertTab (P.Para lst) = blockToICML (ListSubParagraph:style) $ P.Para $ (P.Str "\t"):lst
               insertTab block      = blockToICML style block
           f <- blockToICML stl' $ head item
           r <- mapM insertTab $ tail item
           return $ intersperseBrs (f : r)
         else blocksToICML stl' item

definitionListItemToICML :: ParStyle -> ([P.Inline],[[P.Block]]) -> WS Doc
definitionListItemToICML style (term,defs) = do
  term' <- parStyle (DefListTerm:style) term
  defs' <- mapM (blocksToICML (DefListDef:style)) defs
  return $ intersperseBrs $ (term' : defs')


-- | Convert a list of inline elements to ICML.
inlinesToICML :: CharStyle -> [P.Inline] -> WS Doc
inlinesToICML style lst = vcat `fmap` mapM (inlineToICML style) (mergeSpaces lst)

-- | Convert an inline element to ICML.
inlineToICML :: CharStyle -> P.Inline -> WS Doc
inlineToICML style (P.Str str) = charStyle style $ text $ escapeStringForXML str
inlineToICML style (P.Emph lst) = inlinesToICML (Italic:style) lst
inlineToICML style (P.Strong lst) = inlinesToICML (Bold:style) lst
inlineToICML style (P.Strikeout lst) = inlinesToICML (Strikeout:style) lst
inlineToICML style (P.Superscript lst) = inlinesToICML (Superscript:style) lst
inlineToICML style (P.Subscript lst) = inlinesToICML (Subscript:style) lst
inlineToICML style (P.SmallCaps lst) = inlinesToICML (SmallCaps:style) lst
inlineToICML style (P.Quoted P.SingleQuote lst) = inlinesToICML style $ [P.Str "‘"] ++ lst ++ [P.Str "’"]
inlineToICML style (P.Quoted P.DoubleQuote lst) = inlinesToICML style $ [P.Str "“"] ++ lst ++ [P.Str "”"]
inlineToICML style (P.Cite _ lst) = inlinesToICML (Cite:style) lst
inlineToICML style (P.Code _ str) = charStyle (Code:style) $ text $ escapeStringForXML str
inlineToICML style P.Space = charStyle style space
inlineToICML style P.SoftBreak = do
  st <- get
  case writerWrapText (stOpts st) of
       WrapAuto     -> charStyle style space
       WrapNone     -> charStyle style space
       WrapPreserve -> charStyle style cr
inlineToICML style P.LineBreak = charStyle style $ text lineSeparator
inlineToICML style (P.Math mt str) =
  cat <$> mapM (inlineToICML style) (texMathToInlines mt str)
inlineToICML _ (P.RawInline f str)
  | f == P.Format "icml" = return $ text str
  | otherwise          = return empty
inlineToICML style (P.Link _ lst (url, title)) = do
  content <- inlinesToICML (Link:style) lst
  state $ \st ->
            let ident = if null $ links st
                           then 1::Int
                           else 1 + (fst $ head $ links st)
                newst = st{ links = (ident, url):(links st) }
                cont  = inTags True "HyperlinkTextSource"
                         [("Self","htss-"++show ident), ("Name",title), ("Hidden","false")] content
            in  (cont, newst)
inlineToICML style (P.Image attr _ target) = imageICML style attr target
inlineToICML _ (P.Note lst) = footnoteToICML lst
inlineToICML style (P.Span _ lst) = inlinesToICML style lst

-- | Convert a list of block elements to an ICML footnote.
footnoteToICML :: [P.Block] -> WS Doc
footnoteToICML lst =
  let insertTab (P.Para ls) = blockToICML [Footnote] $ P.Para $ (P.Str "\t"):ls
      insertTab block       = blockToICML [Footnote] block
  in  do
    contents <- mapM insertTab lst
    let number = inTags True "ParagraphStyleRange" [] $
                   inTags True "CharacterStyleRange" [] $ inTagsSimple "Content" "<?ACE 4?>"
    return $ inTags True "CharacterStyleRange"
      [("AppliedCharacterStyle","$ID/NormalCharacterStyle"), ("Position","Superscript")]
      $ inTags True "Footnote" [] $ number $$ intersperseBrs contents

-- | Auxiliary function to merge Space elements into the adjacent Strs.
mergeSpaces :: [P.Inline] -> [P.Inline]
mergeSpaces ((P.Str s):(x:((P.Str s'):xs))) | isSp x =
  mergeSpaces $ P.Str (s++" "++s') : xs
mergeSpaces (x:((P.Str s):xs)) | isSp x = mergeSpaces $ P.Str (" "++s) : xs
mergeSpaces ((P.Str s):(x:xs)) | isSp x = mergeSpaces $ P.Str (s++" ") : xs
mergeSpaces (x:xs) = x : (mergeSpaces xs)
mergeSpaces []     = []

isSp :: P.Inline -> Bool
isSp P.Space = True
isSp P.SoftBreak = True
isSp _ = False

-- | Intersperse line breaks
intersperseBrs :: [Doc] -> Doc
intersperseBrs = vcat . intersperse (selfClosingTag "Br" []) . filter (not . isEmpty)

-- | Wrap a list of inline elements in an ICML Paragraph Style
parStyle :: ParStyle -> [P.Inline] -> WS Doc
parStyle style lst =
  let stlStr = showParStyle style
      stl    = if null stlStr
                  then ""
                  else "ParagraphStyle/" ++ stlStr
      attrs  = ("AppliedParagraphStyle", stl)
      attrs' =  if First `elem` style
                   then case deepestBeginsWith style of
                           Just i  -> ("NumberingStartAt", show i) : ats
                           Nothing -> ats
                   else [attrs]
                     where
                       ats = attrs : [("NumberingContinue", "false")]
                       deepestBeginsWith []                 = Nothing
                       deepestBeginsWith ((BeginsWith i):_) = Just i
                       deepestBeginsWith (_:xs)             = deepestBeginsWith xs
  in  do
      content <- inlinesToICML [] lst
      let cont = inTags True "ParagraphStyleRange" attrs' content
      state $ \st -> (cont, st{ blockStyles = Set.insert style $ blockStyles st })

showParStyle :: ParStyle -> String
showParStyle style = foldr slipIn [] $ reverse style
  where
    slipIn x y = if null y
                    then show x
                    else show x ++ " > " ++ y

-- | Wrap a Doc in an ICML Character Style.
charStyle :: CharStyle -> Doc -> WS Doc
charStyle style content =
  let (stlStr, attrs) = styleToStrAttr style
      doc = inTags True "CharacterStyleRange" attrs $ inTagsSimple "Content" $ flush content
  in  do
      state $ \st ->
        let styles = if null stlStr
                        then st
                        else st{ inlineStyles = Set.insert style $ inlineStyles st }
        in  (doc, styles)

showCharStyle :: CharStyle -> String
showCharStyle = unwords . map show . Set.toAscList . Set.fromList

-- | Transform a Style to a tuple of String (eliminating duplicates and ordered) and corresponding attribute.
styleToStrAttr :: CharStyle -> (String, [(String, String)])
styleToStrAttr style =
  let stlStr = showCharStyle style
      stl    = if null style
                  then "$ID/NormalCharacterStyle"
                  else "CharacterStyle/" ++ stlStr
      attrs = [("AppliedCharacterStyle", stl)]
  in  (stlStr, attrs)

-- | Assemble an ICML Image.
imageICML :: CharStyle -> P.Attr -> P.Target -> WS Doc
imageICML style attr (src, _) = do
  st <- get
  let opts = stOpts st
  res  <- liftIO $ fetchItem (writerSourceURL opts) src
  imgS <- case res of
            Left (_) -> do
              liftIO $ warn $ "Could not find image `" ++ src ++ "', skipping..."
              return def
            Right (img, _) -> do
              case imageSize img of
                Right size -> return size
                Left msg   -> do
                  return $ warn $ "Could not determine image size in `" ++
                    src ++ "': " ++ msg
                  return def
  let (ow, oh) = sizeInPoints imgS
      (imgWidth, imgHeight) = desiredSizeInPoints opts attr imgS
      hw = showFl $ ow / 2
      hh = showFl $ oh / 2
      scale = showFl (imgWidth / ow) ++ " 0 0 " ++ showFl (imgHeight / oh)
      src' = if isURI src then src else "file:" ++ src
      (_, attrs) = styleToStrAttr style
      props  = inTags True "Properties" [] $ inTags True "PathGeometry" []
                 $ inTags True "GeometryPathType" [("PathOpen","false")]
                 $ inTags True "PathPointArray" []
                 $ vcat [
                     selfClosingTag "PathPointType" [("Anchor", "-"++hw++" -"++hh),
                       ("LeftDirection", "-"++hw++" -"++hh), ("RightDirection", "-"++hw++" -"++hh)]
                   , selfClosingTag "PathPointType" [("Anchor", "-"++hw++" "++hh),
                       ("LeftDirection", "-"++hw++" "++hh), ("RightDirection", "-"++hw++" "++hh)]
                   , selfClosingTag "PathPointType" [("Anchor", hw++" "++hh),
                       ("LeftDirection", hw++" "++hh), ("RightDirection", hw++" "++hh)]
                   , selfClosingTag "PathPointType" [("Anchor", hw++" -"++hh),
                       ("LeftDirection", hw++" -"++hh), ("RightDirection", hw++" -"++hh)]
                   ]
      image  = inTags True "Image"
                   [("Self","ue6"), ("ItemTransform", scale++" -"++hw++" -"++hh)]
                 $ vcat [
                     inTags True "Properties" [] $ inTags True "Profile" [("type","string")] $ text "$ID/Embedded"
                   , selfClosingTag "Link" [("Self", "ueb"), ("LinkResourceURI", src')]
                   ]
      doc    = inTags True "CharacterStyleRange" attrs
                 $ inTags True "Rectangle" [("Self","uec"), ("StrokeWeight", "0"),
                     ("ItemTransform", scale++" "++hw++" -"++hh)]
                 $ (props $$ image)
  state $ \st' -> (doc, st{ inlineStyles = Set.insert style $ inlineStyles st' } )
