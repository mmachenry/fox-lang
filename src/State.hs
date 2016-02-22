module State (
    FoxState,
    ReferenceId,
    addReference,
    lookupReference,
    updateReference,
    emptyState,
    pushHeap,
    popHeap
    ) where

import qualified Data.Vector.Persistent as V

newtype FoxState a = FoxState (V.Vector (Heap a))

type Heap a = V.Vector a

data ReferenceId = ReferenceId Int Int

emptyState :: FoxState a
emptyState = FoxState V.empty

pushHeap :: FoxState a -> FoxState a
pushHeap (FoxState state) = FoxState $ V.snoc state V.empty

popHeap :: FoxState a -> FoxState a
popHeap (FoxState state) = FoxState $ V.take (V.length state - 1) state

addReference :: a -> FoxState a -> (ReferenceId, FoxState a)
addReference val (FoxState state) =
    let topHeapId = V.length state - 1
        topHeap = V.unsafeIndex state topHeapId
        newState = V.update topHeapId (V.snoc topHeap val) state
    in (ReferenceId topHeapId (V.length topHeap), FoxState newState)

updateReference :: ReferenceId -> a -> FoxState a -> FoxState a
updateReference (ReferenceId heapid refid) val (FoxState state) =
    let heap = V.unsafeIndex state heapid
        newHeap = V.update refid val heap
    in FoxState (V.update heapid newHeap state)

lookupReference :: ReferenceId -> FoxState a -> a
lookupReference (ReferenceId heapid refid) (FoxState state) =
    let heap = V.unsafeIndex state heapid
    in V.unsafeIndex heap refid

