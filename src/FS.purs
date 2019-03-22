module FS
  ( isDirectory
  , readDirectory
  , readTextFile
  , writeTextFile
  ) where

import Prelude

import Effect (Effect)
import Node.Encoding as Encoding
import Node.FS.Stats as Stats
import Node.FS.Sync as FS
import Node.Path as Path

isDirectory :: String -> Effect Boolean
isDirectory path = map Stats.isDirectory (FS.stat path)

mkdirp :: String -> Effect Unit
mkdirp path = do
  let parent = Path.dirname path
  if parent == path
    then pure unit
    else do
      existsParent <- FS.exists parent
      if existsParent then pure unit else mkdirp parent
  exists <- FS.exists path
  if exists then pure unit else FS.mkdir path

readDirectory :: String -> Effect (Array String)
readDirectory path = map (map (\p -> Path.concat [path, p])) (FS.readdir path)

readTextFile :: String -> Effect String
readTextFile = FS.readTextFile Encoding.UTF8

writeTextFile :: String -> String -> Effect Unit
writeTextFile path content = do
  mkdirp (Path.dirname path)
  FS.writeTextFile Encoding.UTF8 path content
