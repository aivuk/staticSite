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
import Data.List (dropWhile, sort, intercalate, sortBy)
import Data.Either
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
                    Right pf <- parseCSVFromFile (directory ++ "/" ++ d ++ 
                                                  "/menu")
                    let pd = cleanStrings $ filter (/= [""]) pf
                    sd <- forM s $ \s' -> do
                        Right sf <- parseCSVFromFile (directory ++ "/" ++ 
                                                      d ++ "/" ++ s' ++ 
                                                      "/menu")
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

menuInLang dirs dict primDir secDir lang = primMenu ordDirs 
  where
    linkName d = case M.lookup d (dict M.! lang) of
                    Nothing -> ("?","?")
                    Just ln -> ln

    ordDirs = map (fmap sort) $ sortBy (\x y -> compare (fst x) (fst y)) dirs

    sec prim sl = "<div class=\"menuContent" ++ isSelected prim ++ "\">" ++
		  "<ul>" ++ (foldl (\s d -> s ++ "<li" ++
                         ">" ++ slink prim d ++ "</li>\n") "" sl) ++ "</ul></div>"

    slink p s | s == secDir  =  "<span style=\"color: black;\">" ++ 
                                (snd $ linkName s) ++ "</span>"
              | otherwise    =  "<a href=\"/" ++ url p s ++ "\">" ++ 
                                (snd $ linkName s) ++ "</a>"

    plink d sl | sl == []      =  "<a href=\"/" ++ (snd $ linkName d) ++ "\">" ++ 
					(fst $ linkName d) ++ "</a>"
	       | d == primDir  =  "<div class=\"menuButton\"><span style=\"color: black;\">" ++ 
                               	   (fst $ linkName d) ++ "</span></div>"
               | otherwise     =  "<div class=\"menuButton\">" ++ (fst $ linkName d) ++ "</div>"

    url prim sec = urlInLang dict prim sec lang

    primMenu = foldl (\s (d,sl) -> s ++ (plink d sl) ++ (sec d sl)) ""

    isSelected d | d == primDir  =  " selected"
                 | otherwise     =  ""

-- Return the breadcrumb levels

createBreadCrumb dict primDir secDir lang = bc
    where bc = (snd $ linkName primDir, snd $ linkName secDir)
          linkName d = case M.lookup d (dict M.! lang) of
                          Nothing -> ("?","?")
                          Just ln -> ln

languages = ["pt", "en", "de"]

main = hakyll "http://it3s.org" $ do
    directory css "css"
    directory static "images"
    directory static "js"
    dirsMenu <- liftIO $ getDirs "conteudo/left"
    dirsMenuDict <- liftIO $ dirsDict "conteudo/left"
    forM_ dirsMenu $ \(primaryDir, secondaryDirs)  -> do
        let createHtml secondaryDir = 
                forM_ languages $ \lang -> do
                    let url = urlInLang dirsMenuDict primaryDir secondaryDir lang

                        textFile l = "conteudo/left/" ++ primaryDir ++ 
                                    maybeSecondary ++
                                    "/" ++ l ++ ".text.markdown"
                        
                        maybeSecondary = case secondaryDir of
                                "" -> ""
                                _  -> '/':secondaryDir

                        menu = createCustomPage "" [ ("menuLeft", Left $ menuContent) ]

                        breadCrumb = createCustomPage "" [ ("firstLevel", Left fstLevel), 
                                                           ("secondLevel", Left sndLevel)]

                        (fstLevel, sndLevel) = createBreadCrumb dirsMenuDict primaryDir 
                                                                secondaryDir lang

                        menuContent = menuInLang dirsMenu dirsMenuDict 
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
                                                (urlInLang dirsMenuDict primaryDir
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
                                    `combine`
                                    breadCrumb
        case secondaryDirs of
            [] -> createHtml ""
            _  -> forM_ secondaryDirs createHtml
