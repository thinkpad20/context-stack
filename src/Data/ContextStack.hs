{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.ContextStack (
    Stack(..), KeyValueStore(..),
    withFrame, withBindings, modifyTopM, store, buildMap, loadBindingsM,
    find, findOnTop, findOrError, findOrHardError, topFrame
  ) where

import qualified Prelude as P
import qualified Data.HashMap.Strict as H
import Data.Default
import Control.Monad.State.Strict
import Control.Monad.Except
import ClassyPrelude hiding (find, asList)

-- | An abstract stack, supporting push/pop.
class Stack s where
  -- | Frames are things which appear in a stack.
  type family Frame s :: *
  -- | Pushing onto the stack.
  push :: Frame s -> s -> s
  -- | Popping off of the stack.
  pop :: s -> (Frame s, s)
  -- | Viewing the stack as a list of frames.
  asList :: s -> [Frame s]
  -- | Applying a function to the top of the stack.
  modifyTop :: (Frame s -> Frame s) -> s -> s

-- | Lists are stacks, of course.
instance Stack [a] where
  type Frame [a] = a
  push = (:)
  pop (x:xs) = (x, xs)
  asList = id
  modifyTop func (x:xs) = func x : xs

-- | Gets the top frame of the stack.
topFrame :: Stack s => s -> Frame s
topFrame = P.head . asList

-- | Pushing in the state monad.
pushM :: (Stack s, MonadState s m) => Frame s -> m ()
pushM = modify . push

-- | Popping in the state monad.
popM :: (Stack s, MonadState s m) => m (Frame s)
popM = gets pop >>= \(top, newstack) -> put newstack >> return top

-- | Modifying the top in the state monad.
modifyTopM :: (Stack s, MonadState s m) => (Frame s -> Frame s) -> m ()
modifyTopM = modify . modifyTop

-- | An abstract key-value store.
class KeyValueStore s where
  -- | The keys of the store.
  type family LookupKey s :: *
  -- | The values of the store.
  type family StoredValue s :: *
  -- | An empty "default" map.
  empty :: s
  -- | Looking up a value.
  getValue :: LookupKey s -> s -> Maybe (StoredValue s)
  -- | Inserting a value.
  putValue :: LookupKey s -> StoredValue s -> s -> s
  -- | Loading bindings from a hashmap.
  loadBindings :: HashMap (LookupKey s) (StoredValue s) -> s -> s

-- | HashMaps are key-value stores.
instance (Hashable key, Eq key) => KeyValueStore (HashMap key val) where
  type LookupKey (HashMap key val) = key
  type StoredValue (HashMap key val) = val
  empty = H.empty
  putValue = H.insert
  getValue = H.lookup
  loadBindings = H.union

-- | Converts a list into a `KeyValueStore`.
buildMap :: (Default s, KeyValueStore s, MonoFoldable collection,
               Element collection ~ (LookupKey s, StoredValue s))
            => collection -> s
buildMap = foldr (uncurry putValue) def

-- | Stores @name => item@ on the top of the stack.
store :: (Stack s, KeyValueStore (Frame s), MonadState s m)
      => LookupKey (Frame s) -> StoredValue (Frame s) -> m ()
store key item = modifyTopM $ putValue key item

loadBindingsM :: (Stack s, KeyValueStore (Frame s), MonadState s m)
              => HashMap (LookupKey (Frame s)) (StoredValue (Frame s)) -> m ()
loadBindingsM map = modifyTopM $ loadBindings map

-- | Looks up @key@ somewhere in the stack, starting from the top.
find :: (Stack s, KeyValueStore (Frame s), MonadState s m)
     => LookupKey (Frame s) -> m (Maybe (StoredValue (Frame s)))
find key = gets asList >>= go where
  go [] = return Nothing
  go (top:rest) = case getValue key top of
    Nothing -> go rest
    Just result -> return $ Just result

-- | Looks up @key@ on the top of the stack only.
findOnTop :: (Stack s, KeyValueStore (Frame s), MonadState s m)
          => LookupKey (Frame s) -> m (Maybe (StoredValue (Frame s)))
findOnTop key = gets asList >>= \case
  [] -> return Nothing
  top:_ -> return $ getValue key top

-- | Finds an identifier in the current scope, or throws an error.
findOrError :: (Stack s, KeyValueStore (Frame s), MonadState s m,
                MonadError e m)
            => e -- * Error to throw
            -> LookupKey (Frame s)        -- * Key to look up
            -> m (StoredValue (Frame s))  -- * Result
findOrError err key = find key >>= \case
  Nothing -> throwError err
  Just result -> return result

-- | Finds an identifier in the current scope, or throws an IO error.
findOrHardError :: (Stack s, KeyValueStore (Frame s), MonadState s m,
                    Show (LookupKey (Frame s)))
                => LookupKey (Frame s)             -- * The key to look up
                -> m (StoredValue (Frame s))       -- * The retrieved value
findOrHardError key = find key >>= \case
  Nothing -> error $ concat ["Key ", show key, " not found"]
  Just result -> return result

-- | Performs an action with the given frame on the top of the stack.
withFrame :: (Stack s, MonadState s m, Applicative m)
          => Frame s -> m a -> m a
withFrame elem action = pushM elem *> action <* popM

-- | Performs an action with the given bindings in scope.
withBindings :: (Stack s, KeyValueStore (Frame s), MonadState s m,
                 Applicative m, Default (Frame s), MonoFoldable collection,
                 Element collection ~ (LookupKey (Frame s), StoredValue (Frame s)))
             => collection -> m a -> m a
withBindings bindings = withFrame (buildMap bindings)
