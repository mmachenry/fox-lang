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

type FoxState a = V.Vector (Heap a)
type Heap a = V.Vector a
type ReferenceId = (Int, Int)

emptyState :: FoxState a
emptyState = V.empty

pushHeap :: FoxState a -> FoxState a
pushHeap state = V.snoc state V.empty

popHeap :: FoxState a -> FoxState a
popHeap state = V.take (V.length state - 1) state

addReference :: a -> FoxState a -> (ReferenceId, FoxState a)
addReference val state =
    let topHeapId = V.length state - 1
        topHeap = V.unsafeIndex state topHeapId
        newState = V.update topHeapId (V.snoc topHeap val) state
    in ((topHeapId, V.length topHeap), newState)

updateReference :: ReferenceId -> a -> FoxState a -> FoxState a
updateReference (heapid, refid) val state =
    let heap = V.unsafeIndex state heapid
        newHeap = V.update refid val heap
    in V.update heapid newHeap state

lookupReference :: ReferenceId -> FoxState a -> a
lookupReference (heapid, refid) state =
    let heap = V.unsafeIndex state heapid
    in V.unsafeIndex heap refid

