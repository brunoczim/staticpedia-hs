module Staticpedia.Site
  ( Directory
  , dirToMap
  , dirFromMap
  , Node (..)
  , insertNode
  , insertPage
  , insertDir
  , generate
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Staticpedia.Location as Location
import Staticpedia.Component.Page (Page)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as B
import TextShow (TextShow, showb, showt)
import Data.Foldable (foldl')
import Data.Text.IO (writeFile)
import System.Directory
  ( createDirectoryIfMissing
  , removeDirectoryRecursive
  , getDirectoryContents
  , doesDirectoryExist
  , copyFileWithMetadata
  )
import Control.Monad (forM_, when)
import Staticpedia.Component (Component, render)
import qualified Staticpedia.Component as Component
import Prelude hiding (writeFile)
  
newtype Directory = Directory
  { dirToMap :: Map Location.Fragment Node }
  deriving (Eq, Ord)

dirFromMap :: Map Location.Fragment Node -> Directory
dirFromMap = Directory

emptyDir :: Directory
emptyDir = dirFromMap Map.empty

dirAsMap :: (Map Location.Fragment Node -> Map Location.Fragment Node) 
  -> Directory
  -> Directory
dirAsMap f = dirFromMap . f . dirToMap

data Node = PageNode Page | DirNode Directory deriving (Eq, Ord)

data AlreadyExists
  = PageAlreadyExists { existingFragment :: Location.Fragment }
  | DirAlreadyExists { existingFragment :: Location.Fragment }
  deriving (Eq, Ord)

instance TextShow AlreadyExists where
  showb (PageAlreadyExists frag) = B.fromText
    ( Text.concat
      [ "Attempt to insert at an existing page of fragment path "
      , Location.fragmentText frag
      , "."
      ]
    )
  showb (DirAlreadyExists frag) = B.fromText
    ( Text.concat
      [ "Attempt to insert at an existing directory of fragment path "
      , Location.fragmentText frag
      , "."
      ]
    )

withDir :: (Directory -> Either AlreadyExists Directory)
  -> Location.Path
  -> Directory
  -> Either AlreadyExists Directory
withDir f p d = case Location.pathFragments p of
  [] -> f d
  (x : xs) -> do
    d' <- case Map.lookup x (dirToMap d) of
          Just (DirNode d') -> Right d'
          Just (PageNode _) -> Left (PageAlreadyExists x)
          Nothing -> Right emptyDir
    d'' <- withDir f (Location.Path xs) d'
    return (dirAsMap (Map.insert x (DirNode d')) d)

insertNode :: Fragment
  -> Node
  -> Directory
  -> Either AlreadyExists Directory
insertNode frag n dir = case Map.lookup frag (dirToMap dir) of
  Just (DirNode _) -> Left (DirAlreadyExists frag)
  Just (PageNode _) -> Left (PageAlreadyExists frag)
  Nothing -> Right (dirAsMap (Map.insert frag n) dir)

mergeDirs :: Directory -> Directory -> Either AlreadyExists Directory
mergeDirs d0 =
  let singleMerge acc key node =  acc >>= insertNode key node
  in Map.foldlWithKey' singleMerge (Right d0) . dirToMap

insertPage :: Path
  -> Fragment
  -> Page
  -> Directory
  -> Either AlreadyExists Directory
insertPage path frag page = withDir (insertNode frag (PageNode page)) path

insertDir :: Path
  -> Directory
  -> Directory
  -> Either AlreadyExists Directory
insertDir path d0 d1 = withDir (mergeDirs d1) path d0

newtype Site = Site { root :: Directory }

data GenConfig = GenConfig
  { outputSysDir :: FilePath 
  , assetsSysDir :: FilePath
  } deriving (Eq, Ord)

copyDirRecursive :: FilePath  -> FilePath  -> IO ()
copyDirRecursive src dest = do
  entries <- getDirectoryContents src
  forM_ entries $ \path -> do
    isDir <- doesDirectoryExist path
    if isDir
      then do
        createDirectoryIfMissing True (dest ++ path)
        copyDirRecursive (src ++ "/" ++ path) (dest ++ "/" ++ path)
      else copyFileWithMetadata (src ++ "/" ++ path) (dest ++ "/" ++ path)


generate :: GenConfig -> Site -> IO ()
generate cfg site = do
  when (outputSysDir cfg /= assetsSysDir cfg)
    ( do
      removeDirectoryRecursive (outputSysDir cfg)
      createDirectoryIfMissing True (outputSysDir cfg)
      copyDirRecursive (assetsSysDir cfg) (outputSysDir cfg)
    )
  generateDir (outputSysDir cfg) (root site)


generateDir :: FilePath -> Directory -> IO ()
generateDir path dir = do
  createDirectoryIfMissing True path
  forM_ (Map.toAscList (dirToMap dir)) $ \(frag, node) ->
    generateNode (path ++ "/" ++ show (Location.fragmentText frag)) node

generatePage :: FilePath -> Page -> IO ()
generatePage path = writeFile path . render Component.initialCtx

generateNode :: FilePath -> Node -> IO ()
generateNode path (PageNode page) = generatePage path page
generateNode path (DirNode dir) = generateDir path dir
