--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Control.Monad (foldM)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  
  match "img/**"   static
--  match "artwork/**" static
  match "js/*"       static
  match "css/*"      static

  match "templates/*" $ compile templateCompiler

  match "index.html" $ do
    route idRoute
    compile $ do
      getResourceBody
        >>= applyAsTemplate postCtx
        >>= loadAndApplyTemplate "templates/layout.html" (postCtx' "index")
        >>= relativizeUrls

  create ["work.html"] $ do
    route idRoute
    compile $ do makeItem "<div class='span2'/>"
      >>= loadAndApplyTemplate "templates/work-link.html" (wpCtx "current"  "cu" "current" $ head (filter isCurrent paintings))
--      >>= loadAndApplyTemplate "templates/work-link.html" (wpCtx "currenta" "ca" "Older" $ head $ filter isCurrentA paintings)
      >>= loadAndApplyTemplate "templates/work-link.html" (wpCtx "archive"  "ar" "archive" $ head $ filter isArchive paintings)
--      >>= makeItem "<div class='span2'/>"
      >>= loadAndApplyTemplate "templates/work.html" postCtx
      >>= loadAndApplyTemplate "templates/layout.html" (postCtx' "work")
      >>= relativizeUrls
  
  create ["cu.html"] $ buildGallery "current"  "cu" isCurrent
--  create ["ca.html"] $ buildGallery "currenta" "ca" isCurrentA
  create ["ar.html"] $ buildGallery "archive"  "ar" isArchive

  match "bio.html" $ do
    route idRoute
    compile $ do
      getResourceBody
        >>= applyAsTemplate postCtx
        >>= loadAndApplyTemplate "templates/layout.html" (postCtx' "bio")
        >>= relativizeUrls

static :: Rules ()
static = route idRoute >> compile copyFileCompiler

buildGallery :: String -> String -> (Painting -> Bool) -> Rules ()
buildGallery dir active p = do 
  route idRoute
  compile $ do
    let ctx = galleryCtx dir p `mappend` constField "work" "active" `mappend` (postCtx' active)

    makeItem ""
      >>= loadAndApplyTemplate "templates/gallery.html" ctx
      >>= loadAndApplyTemplate "templates/layout.html"  ctx
      >>= relativizeUrls

paintingToCompiler2 :: String -> String -> Painting -> Compiler String
paintingToCompiler2 dir s p = do 
  item <- loadAndApplyTemplate "templates/thumbnail.html" (paintingCtx dir p) =<< makeItem ""
  return (s ++ (itemBody item))


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    constField "index" ""  `mappend`
    constField "work"  ""  `mappend`
    constField "bio"   ""  `mappend`
    constField "cu"    ""  `mappend`
    constField "ca"    ""  `mappend`
    constField "ar"    ""  `mappend`
    defaultContext

postCtx' :: String -> Context String
postCtx' active = constField active "active"  `mappend` postCtx

galleryCtx :: String -> (Painting -> Bool) -> Context String
galleryCtx dir p = field "pics" $ \_ -> foldM (paintingToCompiler2 dir) "" (filter p paintings) 

paintingCtx :: String -> Painting -> Context String
paintingCtx d (Painting _ name size medium)  = constCtx d name size medium

wpCtx :: String -> String -> String -> Painting -> Context String
wpCtx dir page text (Painting _ name _ _)  = 
  constField "dir"   dir  `mappend`
  constField "page"  page `mappend`
  constField "text"  text `mappend`
  constField "name"  name `mappend`
  defaultContext

constCtx :: String -> String -> String -> String -> Context String
constCtx d n s m = 
  constField "dir"    d `mappend`
  constField "name"   n `mappend`
  constField "size"   s `mappend`
  constField "medium" m

--------------------------------------------------------------------------------

isType :: PaintingType -> Painting -> Bool
isType t (Painting pt _ _ _) = t == pt

isCurrent :: Painting -> Bool
isCurrent p = (isType C p) || (isType CA p)

isCurrentA :: Painting -> Bool
isCurrentA = isType CA

isArchive :: Painting -> Bool
isArchive = isType A

data PaintingType = C | CA | A deriving Eq
data Painting = Painting PaintingType String String String 

paintings :: [Painting]
paintings = [
   Painting A "buttercup.jpg" "8\" x 10\"" "Oil on Canvas"
    , Painting A "drawing archive.jpg" "9.25\" x 8.5\"" "Ink on Paper"
    , Painting A "oil stick 1.jpg" "33.5\" x 25\"" "Oil Stick on Paper"
    , Painting A "oil stick 2.jpg" "25\" x 25\"" "Oil Stick on Paper"
    , Painting A "oil stick on board 1.jpg" "11\" x 9\"" "Oil Stick on Canvas"
    , Painting A "one night.jpg" "11\" x 14\"" "Oil on Canvas"
    , Painting A "Orange & Black.jpg" "38.25\" x 30\"" "Ink & Oil Pastel on Paper"
    , Painting A "Drawing Circle.jpg" "37.25\" x 30.25\"" "Ink on Paper"
    , Painting A "Drawing Circle 2.jpg" "12.75\" x 12.5\"" "Ink on Paper"
    , Painting A "Drawing Circle 4.jpg" "14\" x 13.5\"" "Ink on Paper"

    , Painting A "drawing 1.jpg" "9.5\" x 8\"" "Ink on Paper"
    , Painting A "drawing circle 5.jpg" "14\" x 12.5\"" "Ink on Paper"

    , Painting A "drawing archive 2.jpg" "6\" x 5.5\"" "Graphite on Paper"

    , Painting A "painting 1.jpg" "11.75\" x 12\"" "Oil on Canvas"
    , Painting A "painting archive 1.jpg" "8\" x 9\"" "Oil on Canvas"
    , Painting CA "Jen 01-cropped" "30\" x 40\"" "Graphite on Paper"
    , Painting A "drawing circle yellow.jpg" "26.5\" x 22.5\"" "Ink & Oil Pastel on Paper"
    
    , Painting CA "Jen 02-cropped" "35\" x 27.5\"" "Charcoal on Paper"
    , Painting CA "Jen 03-cropped" "36\" x 29.5\"" "Charcoal on Paper"
    , Painting CA "Jen 04-cropped" "22.5\" x 22.75\"" "Charcoal on Paper"
    , Painting CA "Jen 05-cropped" "25\" x 22\"" "Acrylic & Ink on Paper"
    , Painting CA "Jen 06-cropped" "19.5\" x 22.25\"" "Oil Slick on Paper"
    , Painting C "Jen 07-cropped" "25\" x 22\"" "Acrylic, Ink & Graphite on Paper"
    , Painting C "Jen 08-cropped" "25\" x 22\"" "Acrylic, Ink & Graphite on Paper"
    , Painting C "Jen 09-cropped" "25\" x 22\"" "Acrylic, Ink & Graphite on Paper"

    , Painting C "Jen 12-cropped" "10\" x 9\"" "Acrylic, Ink & Graphite on Paper"
    , Painting CA "Jen 13-cropped" "11.5\" x 9.25\"" "Acrylic & Ink on Paper"
    , Painting CA "Jen 14-cropped" "11.5\" x 9.25\"" "Acrylic & Ink on Paper"
    , Painting C "Jen 11-cropped" "9\" x 10\"" "Acrylic, Ink & Graphite on Paper"
    , Painting C "Jen 15-cropped" "\" x \"" "Dunno"
    , Painting C "_DSC0385-cropped" "\" x \"" "Dunno"
    , Painting C "_DSC0386-cropped" "\" x \"" "Dunno"
    , Painting C "_DSC0387-cropped" "\" x \"" "Dunno"
    , Painting C "_DSC0388-cropped" "\" x \"" "Dunno"
    , Painting C "JENBRA_0101-cropped" "\" x \"" "Dunno"
    , Painting C "JENBRA_0115-cropped" "\" x \"" "Dunno"
    , Painting C "JENBRA_0121-cropped" "\" x \"" "Dunno"
    , Painting C "JENBRA_0138-cropped" "\" x \"" "Dunno"
    , Painting C "JENBRA_0145-cropped" "\" x \"" "Dunno"
    ]

paintingName :: Painting -> String
paintingName (Painting _ n _ _)  = n
