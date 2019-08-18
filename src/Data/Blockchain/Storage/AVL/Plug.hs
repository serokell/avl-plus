
module Data.Blockchain.Storage.AVL.Plug
    ( KVRetrieve   (..)
    , KVAppend     (..)
    , KVErase      (..)
    , ProvidesHash (..)
    , notFound
    , NotFound
    , NoRootExists
    ) where

import Data.Tree.AVL