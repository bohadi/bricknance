{-# LANGUAGE
    OverloadedStrings
  , DuplicateRecordFields
#-}

module Util (
    StreamEvent(..)
  , CmdEvent(..)
  , SymbolPair
  , StreamTarget
  , StreamEndpoint(..)
  , defaultEndpoints
) where

import Data.Text (Text, pack, unpack)
import qualified Data.Aeson as A
import Data.Aeson ((.:))
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

-- TODO read from config
defaultEndpoints = [
    StreamEndpoint (BTC_USDT  , MiniTicker)
  , StreamEndpoint (ADA_USDT  , AggTrade)
  , StreamEndpoint (ETH_USDT  , Trade)
  , StreamEndpoint (TUSD_USDT , Kline1m)
  ] :: [StreamEndpoint]

newtype StreamEndpoint = StreamEndpoint (SymbolPair, StreamTarget)
instance Show StreamEndpoint where
  show (StreamEndpoint (s,AllTickers))     = "!ticker@arr"
  show (StreamEndpoint (s,AllMiniTickers)) = "!miniTicker@arr"
  show (StreamEndpoint (s,t))              = (show s) <> "@" <> (show t) <> "/"

data SymbolPair =
    BTC_USDT  | ETH_USDT  | ADA_USDT
  | ADA_BTC   | ADA_ETH 
  | TUSD_USDT
instance Show SymbolPair where
  show BTC_USDT  = "btcusdt"
  show ETH_USDT  = "ethusdt"
  show ADA_USDT  = "adausdt"
  show ADA_BTC   = "adabtc"
  show ADA_ETH   = "adaeth"
  show TUSD_USDT = "tusdusdt"
enumSym :: String -> SymbolPair
enumSym "btcusdt"  = BTC_USDT
enumSym "ethusdt"  = ETH_USDT
enumSym "adausdt"  = ADA_USDT
enumSym "adabtc"   = ADA_BTC
enumSym "adaeth"  = ADA_ETH
enumSym "tusdusdt" = TUSD_USDT

data StreamTarget =
    Ticker | MiniTicker
  | AllTickers | AllMiniTickers
  | Trade | AggTrade
  | Depth | Depth5 | Depth10 | Depth20
  | Kline1m | Kline3m | Kline5m | Kline15m | Kline30m
  | Kline1h | Kline2h | Kline4h | Kline6h | Kline8h | Kline12h
  | Kline1d | Kline3d
  | Kline1w | Kline1M
instance Show StreamTarget where
  show MiniTicker = "miniTicker"
  show AggTrade   = "aggTrade"
  show Trade      = "trade"
  show Depth10    = "depth10"
  show Kline1m    = "kline_1m"
enumTgt :: String -> StreamTarget
enumTgt "miniTicker" = MiniTicker 
enumTgt "aggTrade"   = AggTrade
enumTgt "trade"      = Trade
enumTgt "depth10"    = Depth10 
enumTgt "kline_1m"   = Kline1m 

-- trade payload params
type Price   = Float
type Quantity = Float
type BuyerIsMaker = Bool
-- miniticker payload params
type SpanOpen  = Float
type SpanClose = Float
type SpanLow   = Float
type SpanHigh  = Float
type BaseVol   = Float
type QuoteVol  = Float
-- topdepth payload params
type Bid = Order
type Ask = Order
type Order = (Price, Quantity)
-- kline payload params
type KlineIsClosed = Bool

data StreamEvent =
    MiniTickerPayload {
        s :: SymbolPair
      , c :: SpanClose
      , o :: SpanOpen
      , h :: SpanHigh
      , l :: SpanLow
      , v :: BaseVol
      , q :: QuoteVol
    }
  | AggTradePayload {
        s :: SymbolPair
      , p :: Price
      , q :: Quantity
      , m :: BuyerIsMaker
    }
  | TradePayload {
        s :: SymbolPair
      , p :: Price
      , q :: Quantity
      , m :: BuyerIsMaker
    }
  | TopDepthPayload {
        s :: SymbolPair
      , bids :: [Order]
      , asks :: [Order]
    }
  | KlinePayload {
        s :: SymbolPair
      , c :: SpanClose
      , o :: SpanOpen
      , h :: SpanHigh
      , l :: SpanLow
      , v :: BaseVol
      , q :: QuoteVol
      , x :: KlineIsClosed
    } deriving (Show)
-- https://github.com/binance-exchange/binance-official-api-docs/blob/master/web-socket-streams.md
instance A.FromJSON StreamEvent where
  parseJSON = A.withObject "StreamEvent" $ \v -> do
    ev <- v .: "stream"
    pl <- v .: "data"
    let [sym, event] = (splitOn "@" ev)
        symb = enumSym sym
    case event of
      "miniTicker" -> do
        c <- pl .: "c"
        o <- pl .: "o"
        h <- pl .: "h"
        l <- pl .: "l"
        v <- pl .: "v"
        q <- pl .: "q"
        return $ MiniTickerPayload symb (read c) (read o) (read h) (read l) (read v) (read q)
      "aggTrade" -> do
        p <- pl .: "p"
        q <- pl .: "q"
        m <- pl .: "m"
        return $ AggTradePayload symb (read p) (read q) m
      "trade" -> do
        p <- pl .: "p"
        q <- pl .: "q"
        m <- pl .: "m"
        return $ TradePayload symb (read p) (read q) m
   {- "depth10" -> do      --- TODO
        b <- pl .: "bids"
        a <- pl .: "asks"
        let bids = helper $ fromJust $ (A.decode b :: Maybe A.Array)
            asks = helper $ fromJust $ (A.decode a :: Maybe A.Array)
        return $ TopDepthPayload symb bids asks   -}
      "kline_1m" -> do
        k <- pl .: "k"
        c <-  k .: "c"
        o <-  k .: "o"
        h <-  k .: "h"
        l <-  k .: "l"
        v <-  k .: "v"
        q <-  k .: "q"
        x <-  k .: "x"
        return $ KlinePayload symb (read c) (read o) (read h) (read l) (read v) (read q) x

--helper os = (\(A.Array o) -> (read $ o ! 0, read $ o ! 1)) <$> os

data CmdEvent =
    Add [StreamEndpoint]
  | Rm  [StreamEndpoint]
