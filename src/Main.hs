module Main where

import Chess.FEN
import Chess
import Data.Array
import Data.List.Split
import Data.List
import System.Environment
import System.Console.ANSI hiding (White, Black)
import Options.Applicative.Simple hiding (Parser)
import Options.Applicative.Types hiding (Parser)

import qualified Options.Applicative.Types as OPT
import qualified System.Console.ANSI as ANSI


toSolidPiece :: PieceType -> String
toSolidPiece t =
              case t of
                King   -> "♚"
                Queen  -> "♛"
                Rook   -> "♜"
                Bishop -> "♝"
                Knight -> "♞"
                Pawn   -> "♟"

toHollowPiece :: PieceType -> String
toHollowPiece t =
              case t of
                King   -> "♔"
                Queen  -> "♕"
                Rook   -> "♖"
                Bishop -> "♗"
                Knight -> "♘"
                Pawn   -> "♙"

isLightSquare :: (Int, Int) -> Bool
isLightSquare (i, j) = odd (i + j)

darkOrLight :: (Int, Int) -> String -> String
darkOrLight (i, j) = if isLightSquare (i, j) then toLightSquare else toDarkSquare

toDarkSquare :: String -> String
toDarkSquare s =
             setSGRCode [SetColor Background Dull  ANSI.Black]
          ++ setSGRCode [SetColor Foreground Vivid ANSI.White]
          ++ s
          ++ setSGRCode [Reset]

toLightSquare :: String -> String
toLightSquare s =
             setSGRCode [SetColor Foreground Dull  ANSI.Black]
          ++ setSGRCode [SetColor Background Vivid ANSI.White]
          ++ s
          ++ setSGRCode [Reset]

toGraphic :: ((Int, Int), Maybe Piece) -> String
toGraphic ((i, j), mp) =
  darkOrLight (i, j) $
          case mp of
            (Just (Piece White t)) -> if isLightSquare (i, j) then toHollowPiece t else toSolidPiece  t
            (Just (Piece Black t)) -> if isLightSquare (i, j) then toSolidPiece  t else toHollowPiece t
            Nothing -> " "

displayBoard :: String -> String
displayBoard fen =
  case fromFEN fen of
    Just (Board turn _ _ a) ->
      unlines . map concat . reverse . transpose . chunksOf 8 . map toGraphic $ assocs a

optparser :: OPT.Parser [String]
optparser = many $ argument str (metavar "FEN...")

main :: IO ()
main = do
    args <- getArgs

    (fens,()) <- simpleOptions "0.0.1" "FEN displayer" "" optparser empty

    mapM_ (putStrLn . displayBoard) fens
