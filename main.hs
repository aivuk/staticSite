module Main where

import Control.Monad (forM_, forM, filterM, foldM, when)
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
import Text.CSV (parseCSVFromFile)
import Text.Hakyll (hakyll)
import Control.Monad.Reader (liftIO)
import Text.Hakyll.Render (renderChain)
import Text.Hakyll.CreateContext (createPage, combine, combineWithUrl, createCustomPage)
import Text.Hakyll.ContextManipulations (renderValue)
import Text.Hakyll.Context 
import Text.Hakyll.HakyllAction 
import Data.List (dropWhile)
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
                Right pf <- parseCSVFromFile (directory ++ "/" ++ d ++ "/menu")
                let pd = cleanStrings $ filter (/= [""]) pf
                sd <- forM s $ \s' -> do
                    Right sf <- parseCSVFromFile (directory ++ "/" ++ d ++ "/" ++ s' ++ "/menu")
                    return $ (s', cleanStrings $ filter (/= [""]) sf)
                return $ ((d, pd):sd) ++ l) [] dirs
    let dict = M.fromListWith M.union $ [ (l, M.fromList [ (d, (menu, dir)) ])
                                        | (d, pd) <- dictData, [l, dir, menu] <- pd ]
    return dict

-- Return the URL's directory in a lang
        
urlInLang dict primDir secDir lang = (transDir primDir) ++ "/" ++ (transDir secDir)
  where
    transDir dir = fst $ (dict M.! lang) M.! dir

createMenu dirs dict primDir secDir lang = (primMenu, secMenu)
  where
    primMenu = foldl (\s (d,_) -> s ++ "<li>" ++ (snd $ (dict M.! lang) M.! d) ++ "</li>\n") "" dirs
    secMenu = concatMap ((foldl (\s d -> s ++ "<li>" ++ (snd $ (dict M.! lang) M.! d) ++ "</li>\n") "").snd) dirs

main = hakyll "http://it3s.org" $ do
    dirs <- liftIO $ getDirs "conteudo"
    dirsDict <- liftIO $ dirsDict "conteudo"
    forM_ dirs $ \(primaryDir, secondaryDirs)  -> do
        forM_ secondaryDirs $ \secondaryDir -> do
            forM_ ["pt", "de", "en"] $ \lang -> do
                let url = urlInLang dirsDict primaryDir secondaryDir lang

                    textFile = "conteudo/" ++ primaryDir ++ 
                                maybeSecondary ++
                                "/" ++ lang ++ ".text.markdown"
                    
                    maybeSecondary = case secondaryDir of
                            [] -> ""
                            _  -> '/':secondaryDir

                    menu = createCustomPage "" [ ("menuPrim", Left $ primMenu)
                                               , ("menuSec", Left $ secMenu) ] 

                    (primMenu, secMenu) = createMenu dirs dirsDict primaryDir secondaryDir lang

                    footer   = createPage $ "conteudo/" ++ lang ++ ".footer.markdown"

                haveFile <- liftIO $ doesFileExist textFile
                when haveFile $ do
                    renderChain [ "templates/index.html" ] $ 
                                combineWithUrl (url ++ "/index.html")
                                (createPage textFile)
                                footer 
                                `combine`
                                menu
