{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Main where
import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Catch         (MonadThrow)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (MonadReader, ReaderT, runReaderT)
import           Control.Monad.Trans.Control (MonadBaseControl, StM,
                                              liftBaseWith, restoreM)
import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy        as BSL (pack, unpack)
import           Data.Text                   (Text)
import           GHC.Generics                (Generic)
import           Data.Word                       (Word8)
import qualified Codec.Compression.BZip      as BZip
import qualified Data.Text                   as T
import qualified Data.Map as Map
import Data.Char (toLower)
import           Path
import           Streamly
import qualified Streamly.Internal.Data.Unicode.Stream      as SString
import qualified Streamly.FileSystem.Handle  as FH
import qualified Streamly.Data.Fold            as FL

import           Streamly.Prelude            as S hiding (replicate)
import qualified Data.Set as Set
import Data.Maybe
import           System.IO                       (IOMode (..), openFile)

enwiki8FP = [absfile|/Users/timpierson/arity/hasktorch/examples/static-lstm/data/enwik8.bz2|]

{-# INLINE tshow #-}
tshow :: Show a => a -> Text
tshow = T.pack . show

{-# INLINE joinly #-}
joinly :: (IsStream t, Monad m, Monad (t m)) => m (t m b) -> t m b
joinly x = S.concatMapM (const x) (return ())

fromByteString :: (MonadIO m) => ByteString -> AheadT m Word8
fromByteString = S.fromList . BSL.unpack

toByteString :: (Monad m) => SerialT m Word8 -> m ByteString
toByteString stream = BSL.pack <$> S.toList stream

unzipFile :: (MonadIO m) => Path b File -> m ByteString
unzipFile f = do
    cb  <- liftIO $ openFile (toFilePath f) ReadMode
    cb' <- liftIO . toByteString $ S.unfold FH.read cb
    return $ BZip.decompress cb'

dontUnzipFile :: (MonadIO m) => Path b File -> m ByteString
dontUnzipFile f = do
    cb  <- liftIO $ openFile (toFilePath f) ReadMode
    liftIO . toByteString $ S.unfold FH.read cb


data StrictTuple a = StrictTuple {firstOne :: !a, secondOne:: !a}
data StrictEither a b = Left' !a | Right' !b

data StrictMaybe a = Nothing' | Just' !a

data EnWiki9 

enwikiSeqLen = 256
alphabet = Set.fromList $ ['a'..'z'] <> [' ']

ilphabet = Map.fromList $ zip (Set.toList alphabet) [0..] 

dumbOneHot c = replicate i 0 <> [1] <> replicate (size - i - 1) 0
    where 
        size = Map.size ilphabet
        (Just i) = Map.lookup c ilphabet

lockAndLoad :: (MonadAsync m) => Path a File -> AheadT m (StrictTuple Char)
lockAndLoad fp =
    S.map fromRight . S.filter isRight . S.postscanl' step' init' $ prep
  where
    init' = Left' Nothing'
    step' (Left'  Nothing'          ) r = Left' $ Just' r
    step' (Left'  (Just' r'        )) r = Right' (StrictTuple r' r)
    step' (Right' (StrictTuple r' r)) n = Right' (StrictTuple r n)
    isRight (Right' _) = True
    isRight _          = False
    isNormalRight (Right _) = True
    isNormalRight _         = False
    fromNormalRight (Right s) = s
    fromRight (Right' s) = s
    prep =
        S.filter (flip Set.member alphabet)
            .   S.map toLower
            -- .   S.map fromNormalRight
            -- .   S.filter isNormalRight
            .   SString.decodeUtf8 . maxThreads 1 
            $   joinly 
            $   (S.fromList . BSL.unpack) 
            <$> unzipFile fp
 
type BatchSize = 24 
type SeqLen = 128 

kitAndKaboodle :: IO ()
kitAndKaboodle = S.drain . serially . aheadly $ batch
  where
    -- batch
    --     :: forall m
    --      . (MonadAsync m)
    --     => AheadT
    --            m
    --            ( Tensor
    --                  '( 'D.CPU, 0)
    --                  'D.Float
    --                  '[SeqLen, BatchSize, NumInputs]
    --            , Tensor '( 'D.CPU, 0) 'D.Int64 '[SeqLen, BatchSize]
    --            )
    batch = -- S.map (\t -> (reshape @'[SeqLen, BatchSize, NumInputs] $ procX t, reshape @'[SeqLen, BatchSize] $  procY t)) $ 
        S.chunksOf 24 FL.toList xyStream
    --   where
    --     procX :: [([Float], [Int])] -> Tensor '( 'D.CPU, 0) 'D.Float '[SeqLen * BatchSize * NumInputs]
    --     procX = UnsafeMkTensor . D.asTensor . concat . fmap fst 
    --     procY :: [([Float], [Int])] -> Tensor '( 'D.CPU, 0) 'D.Int64 '[SeqLen * BatchSize]
    --     procY = UnsafeMkTensor . D.asTensor . concat . fmap snd
    xyStream
        :: forall m
         . (MonadAsync m)
        => AheadT
               m
               ( [Float]
               , [Int]
               )
    xyStream = S.map (\v -> (procX v, procY v)) xyStream'
      where
        procX = concat . fmap fst
        procY = fmap (fromJust . flip Map.lookup ilphabet . snd)

    xyStream'
        :: (MonadAsync m)
        => AheadT m [([Float], Char)]
    xyStream' = S.chunksOf 128 FL.toList
        $ S.mapM (\(StrictTuple x y) -> do 
            -- liftIO . print $ P.length $ toOneHot x
            return (toOneHot x, y)
            ) cycleData
    data'
        = enwiki8FP
    cycleData :: (MonadAsync m) => AheadT m (StrictTuple Char)
    cycleData = (lockAndLoad data')  -- <> cycleData

toOneHot :: Char -> [Float]
toOneHot =
    fmap (fromIntegral :: Integer -> Float)
        . dumbOneHot

main :: IO ()
main = kitAndKaboodle
