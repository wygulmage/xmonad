
module XMonad.Internal.Type.WorkspaceId where

newtype WorkspaceId = WorkspaceId String
    deriving (Eq, Ord, Show, Read)
