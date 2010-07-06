module Main where

import Control.Monad (forM_, forM, filterM, foldM, when)
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
import Text.CSV (parseCSVFromFile)
import Text.Hakyll (hakyll)
import Control.Monad.Reader (liftIO)
import Text.Hakyll.Render (renderChain, css, static)
import Text.Hakyll.CreateContext (createPage, combine, combineWithUrl, createCustomPage)
import Text.Hakyll.ContextManipulations (renderValue)
import Text.Hakyll.Context 
import Text.Hakyll.File (directory)
import Text.Hakyll.HakyllAction 
import Data.List (dropWhile)
import Data.Either
import Data.List (intercalate, sortBy)
import qualified Data.Map as M

-- Get Just Two Levels

getDirs directory = do
    files <- getDirectoryContents directory
    let filterDirs b ds = filterM (\d -> doesDirectoryExist $ b ++ "/" ++ d) ds
        removeDots = filter (\d -> (d /= ".") && (d /= ".."))
    dirs <- (filterDirs directory).removeDots $ files
    forM dirs $ \d -> do
        files <- getDirectoryContents $ directory ++ "/" ++ d
        secDirs <- (filterDirs $ directory ++ "/" ++ d).removeDots $ files
        return (d, secDirs)

-- Create the dictionary from 'menu' files

dirsDict directory = do
    let cleanStrings = map $ map (dropWhile (== ' '))
    dirs <- getDirs directory
    dictData <- foldM (\l (d, s) -> do
                Right pf <- parseCSVFromFile (directory ++ "/" ++ d ++ "/menu")
                let pd = cleanStrings $ filter (/= [""]) pf
                sd <- forM s $ \s' -> do
                    Right sf <- parseCSVFromFile (directory ++ "/" ++ 
                                                  d ++ "/" ++ s' ++ "/menu")
                    return $ (s', cleanStrings $ filter (/= [""]) sf)
                return $ ((d, pd):sd) ++ l) [] dirs
    let dict = M.fromListWith M.union $ [ (l, M.fromList [ (d, (menu, dir)) ])
                                        | (d, pd) <- dictData, 
                                          [l, dir, menu] <- pd ]
    return dict

-- Return the URL's directory in a lang
        
urlInLang dict primDir secDir lang = (transDir primDir) ++ "/" ++ (transDir secDir)
  where
    transDir [] = ""
    transDir dir = case M.lookup dir (dict M.! lang) of
                        Nothing -> "?"
                        Just d -> fst $ d

-- Return the Menu in a lang

menuInLang dirs dict primDir secDir lang = (primMenu ordDirs, secMenu ordDirs)
  where
    linkName d = case M.lookup d (dict M.! lang) of
                    Nothing -> ("?","?")
                    Just ln -> ln

    ordDirs = sortBy (\x y -> compare (fst x) (fst y)) dirs

    select d p | p == d    =  " class=\"selected\"" 
               | otherwise =  ""

    secMenu = concatMap sm  

    sm = \(prim, sec) -> "<ul" ++ select primDir prim ++ " id=\"" ++
                         (fst $ linkName prim) ++ "\"" ++
                         (foldl (\s d -> s ++ "<li" ++ select secDir d ++
                         ">" ++ slink prim d ++ "</li>\n") "" sec) ++ "</ul>"

    slink p s | s == secDir  =  snd $ linkName s
              | otherwise    =  "<a href=\"/" ++ url p s ++ "\">" ++ 
                                (snd $ linkName s) ++ "</a>"

    url prim sec = urlInLang dict prim sec lang

    primMenu = foldl (\s (d,_) -> s ++ "<li" ++ select primDir d  ++ 
                                   " id=\"" ++ (fst $ linkName d) ++ 
                                   "\"><a href=\"javascript:selectMenu('" ++ 
                                   (fst $ linkName d) ++ "')\">" ++ 
                                   (snd $ linkName d) ++ "</a></li>\n") ""

languages = ["pt", "en", "de"]

main = hakyll "http://it3s.org" $ do
    directory css "css"
    directory static "images"
    directory static "js"
    dirs <- liftIO $ getDirs "conteudo"
    dirsDict <- liftIO $ dirsDict "conteudo"
    forM_ dirs $ \(primaryDir, secondaryDirs)  -> do
        let createHtml secondaryDir = 
                forM_ languages $ \lang -> do
                    let url = urlInLang dirsDict primaryDir secondaryDir lang

                        textFile l = "conteudo/" ++ primaryDir ++ 
                                    maybeSecondary ++
                                    "/" ++ l ++ ".text.markdown"
                        
                        maybeSecondary = case secondaryDir of
                                "" -> ""
                                _  -> '/':secondaryDir

                        menu = createCustomPage "" [ ("menuPrim", Left $ primMenu)
                                                   , ("menuSec", Left $ secMenu) ] 

                        (primMenu, secMenu) = menuInLang dirs dirsDict 
                                                         primaryDir secondaryDir lang

                        footer   = createPage $ "conteudo/" ++ lang ++ 
                                                ".footer.markdown"

                        createLangButtons = do
                            langLinks <- forM languages $ \l -> do
                                if l == lang
                                  then return $ "<b>" ++ lang ++ "</b>"
                                  else do
                                    haveFile <- liftIO $ doesFileExist $ textFile l
                                    if haveFile
                                      then do
                                        return $ "<a href=\"/" ++
                                                (urlInLang dirsDict primaryDir
                                                           secondaryDir l) ++
                                                "/\">" ++ l ++ "</a>"
                                      else return ""
                            return $ createCustomPage "" [ ("changeLanguage", Left $ 
                                                           intercalate " | " $ 
                                                           filter (/= "") langLinks) ]
                    
                    changeLanguage <- createLangButtons
                    haveFile <- liftIO $ doesFileExist $ textFile lang
                    when haveFile $ do
                        renderChain [ "templates/index.html" ] $ 
                                    combineWithUrl (url ++ "/index.html")
                                    (createPage $ textFile lang) 
                                    footer 
                                    `combine`
                                    menu
                                    `combine`
                                    changeLanguage  
        case secondaryDirs of
            [] -> createHtml ""
            _  -> forM_ secondaryDirs createHtml
