# context-stack

#### A type class for a stack, and for a monadic context using the state monad with that stack holding an environment.

### Motivation

One thing I've encountered time and time again, especially writing language implementations, is the idea of a stack-based context. For example, when type checking, you need to have a mapping from names to types. There is lexical scope in your functions, so that when you look up a name, you only get the most recent name. When you exit a scope, you want the existing previous names to still be there. Using a stack for this makes sense, with its push-pop API.

The issue is that quite often, the stack is only part of your state; for example, when type checking your state might store variable types as well as type aliases, and the type aliases are fixed while the variable types use a stack. You still want to "push" and "pop", but only the stack-part of your state. But conceptually, the whole thing is still a stack.

Another issue is that the stack might not only be a stack of dictionaries. For example when evaluating, each frame of the stack might have a lookup table as well as the current argument(s). The former is a dictionary, but the latter is not. And yet, the idea of "find in the current scope" is still quite well-defined.

Another issue is that the stack is probably monomorphic: within the context of a single usage, you're probably only going to have one stack and it will be of kind `*`, making it less straightforward to abstract.

### Solution

This library provides an abstract `Stack` class, as well as a `KeyValueStore` class, and provides a lot of functionality that combines the two. Each uses type families to provide the abstraction: a stack has a `Frame` type, and a `KeyValueStore` has `LookupKey` and `StoredValue` types.


```haskell
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
```

The simplest stack is a list:

```haskell
instance Stack [a] where
  type Frame [a] = a
  push = (:)
  pop (x:xs) = (x, xs)
  asList = id
  modifyTop func (x:xs) = func x : xs
```

And a useful key-value store is a `HashMap`:

```haskell
instance (Hashable key, Eq key) => KeyValueStore (HashMap key val) where
  type LookupKey (HashMap key val) = key
  type StoredValue (HashMap key val) = val
  empty = H.empty
  putValue = H.insert
  getValue = H.lookup
  loadBindings = H.union
```

With these classes we can write generic `Stack` functions; for example, operating in the `State` monad, which was a major goal of this package:

```haskell
-- | Pushing in the state monad.
pushM :: (Stack s, MonadState s m) => Frame s -> m ()
pushM = modify . push

-- | Popping in the state monad.
popM :: (Stack s, MonadState s m) => m (Frame s)
popM = gets pop >>= \(top, newstack) -> put newstack >> return top

-- | Looks up @key@ somewhere in the stack, starting from the top.
find :: (Stack s, KeyValueStore (Frame s), MonadState s m)
     => LookupKey (Frame s) -> m (Maybe (StoredValue (Frame s)))
find key = gets asList >>= go where
  go [] = return Nothing
  go (top:rest) = case getValue key top of
    Nothing -> go rest
    Just result -> return $ Just result

-- | Performs an action with the given frame on the top of the stack.
withFrame :: (Stack s, MonadState s m, Applicative m)
          => Frame s -> m a -> m a
withFrame elem action = pushM elem *> action <* popM
```

### Example

So, let's say we're writing an evaluator. We need a state representing our current context:

```haskell
data EvalState = EvalState {context :: [EvalFrame]}
```

And a frame is the current argument, and the environment:

```haskell
data EvalFrame = EvalFrame {
  argument :: Value,
  environment :: HashMap Text Value
}
```

So here the stack is our `EvalState`:

```haskell
-- | The evaluator state is a stack of `EvalFrame`s.
instance Stack EvalState where
  type Frame EvalState = EvalFrame
  push frame state = state {context = push frame $ context state}
  pop state = (top, state {context=rest}) where
    top:rest = context state
  asList = context
  modifyTop func state = state {context = modifyTop func $ context state}
```

And the key-value store is an `EvalFrame`:

```haskell
instance KeyValueStore EvalFrame where
  type LookupKey EvalFrame = Name
  type StoredValue EvalFrame = Value
  getValue name = lookup name . environment
  putValue name val f = f {environment = insertMap name val $ environment f}
  loadBindings bs f = f {environment = union bs $ environment f}
```

And we can now fully generically use many useful functions like `find`, `store`, perform an action in a fresh context with `withFrame`, etc!

```haskell
type Eval = State EvalState

eval :: Expression -> Eval Value
eval (Variable name) = fromJust $ find name
eval (Let name e1 e2) = do
  val <- eval e1
  store name val
  eval e2
eval (Apply e1 e2) = do
  func <- eval e1
  arg <- eval e2
  case func of 
    Closure env body -> withFrame (EvalFrame arg env) $ eval body
eval ...
```

### Install

```
$ cabal install context-stack
```

```haskell
import Data.ContextStack
```

Enjoy!
