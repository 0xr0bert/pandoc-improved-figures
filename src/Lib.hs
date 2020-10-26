module Lib
    ( imageFilter
    ) where

import Text.Pandoc.JSON
import qualified Data.Text as T

toLatex :: T.Text -> Inline
toLatex x = RawInline (Format (T.pack "latex")) x

data FigureType = StandardFigure | MarginFigure | WideFigure

environmentName :: FigureType -> T.Text
environmentName StandardFigure = T.pack "figure"
environmentName MarginFigure = T.pack "marginfigure"
environmentName WideFigure = T.pack "figure*"

startEnvironment :: FigureType -> T.Text
startEnvironment x = (T.pack "\\begin{") <> (environmentName x) <> (T.pack "}")

endEnvironment :: FigureType -> T.Text
endEnvironment x = (T.pack "\\end{") <> (environmentName x) <> (T.pack "}")

createFigure :: FigureType -> Inline -> Block
createFigure figType img = 
    Para [
        toLatex $ getHypertarget img <> startEnvironment figType <> T.pack "\n\\centering\n",
        img,
        toLatex $ T.pack "\n\\caption",
        toLatex $ shortCap $ getAttrsOfImage img,
        caption img,
        toLatex $ getLabel img <> T.pack "\n" <> endEnvironment figType <> T.pack "\n}\n"
    ] where
        caption (Image _ alt _) = Span nullAttr alt

getAttrsOfImage :: Inline -> Attr
getAttrsOfImage (Image attr _ _) = attr

getAttr :: [(T.Text, T.Text)] -> T.Text -> Maybe T.Text
getAttr ((x1, x2):_) attr | x1 == attr = Just x2
getAttr (_:xs) attr = getAttr xs attr
getAttr [] _ = Nothing

shortCap :: Attr -> T.Text
shortCap (_,_,kvp) = f $ getAttr kvp (T.pack "short-caption") where
    f Nothing = T.pack ""
    f (Just x) = T.pack "[" <> x <> T.pack "]"

getHypertarget :: Inline -> T.Text
getHypertarget (Image (id, _, kvp) _ _) =
    if id /= (getTitle kvp) then T.pack "\\hypertarget{" <> id <> T.pack "}{%%\n"
    else T.pack "{%%\n"

getLabel :: Inline -> T.Text
getLabel (Image (id, _, kvp) _ _) =
    if id /= (getTitle kvp) then T.pack "\n\\label{" <> id <> T.pack "}"
    else T.pack "\n"

getTitle :: [(T.Text, T.Text)] -> T.Text
getTitle kvp = f $ getAttr kvp $ T.pack "title" where
    f (Just x) = x
    f Nothing = T.pack ""

getEnvironment :: Attr -> FigureType
getEnvironment (_, cls, _) | elem (T.pack "wide") cls = WideFigure
getEnvironment (_, cls, _) | elem (T.pack "margin") cls = MarginFigure
getEnvironment _ = StandardFigure

imageFilter :: Block -> Block
imageFilter (Para ((Image attr alt target):_)) = createFigure figType (Image attr alt target) where
        figType = getEnvironment attr
imageFilter x = x