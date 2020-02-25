module TypesTTG where

data TcPrimT
  = BooleanT
  | ByteT
  | ShortT
  | IntT
  | LongT
  | CharT
  | FloatT
  | DoubleT
    -- Paragon
  | ActorT
  | PolicyT

data TcRefT
    = TcClsRefT TcClassType
    | TcArrayT TcType ActorPolicy
    | TcTypeVar B.ByteString
    | TcNullT


data TcClassType
    = TcClassT (Name SourcePos) [TcTypeArg]
