{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Main where

import Data.Char
import qualified Data.HashMap.Strict as M

data Token s n
  = Reserved s
  | Number   n
  | LeftParenthesis
  | RightParenthesis
  deriving (Show, Eq)

data ArithmeticReserved
  = Add
  | Subtract
  | Multiply
  | Divide
  | Identity String
  deriving (Eq, Show)

newtype UnaryFunction
  = U (forall a. Floating a => a -> a)

newtype HiddenNumber
  = N (forall a. (Floating a, Read a) => a)

evalSimple :: Floating a => ArithmeticReserved -> a -> a -> Either String a
evalSimple op v1 v2 =
  case op of
    Add       -> Right $ v1 + v2
    Subtract  -> Right $ v1 - v2
    Multiply  -> Right $ v1 * v2
    Divide    -> Right $ v1 / v2
    _ -> Left  $ show op ++ " is not a binary operator"

functionTable :: M.HashMap String UnaryFunction
functionTable = M.fromList
  [("sqrt", U sqrt)
  ,("sin",  U sin)
  ,("cos",  U cos)
  ,("tan",  U tan)]

constantTable :: M.HashMap String HiddenNumber
constantTable = M.fromList
  [("e",  N 2.718281828459)
  ,("pi", N 3.141592653589)]

tokenTable :: M.HashMap Char (Token ArithmeticReserved HiddenNumber)
tokenTable = M.fromList
  [('+', Reserved Add)
  ,('-', Reserved Subtract)
  ,('*', Reserved Multiply)
  ,('/', Reserved Divide)
  ,('(', LeftParenthesis)
  ,(')', RightParenthesis)]

fromString :: String -> Either String [Token ArithmeticReserved HiddenNumber]
fromString [] = Right []
fromString (' ':xs) = fromString xs
fromString (x:xs)
  | isDigit x =
    let (nums, others) = span isDigit xs
    in (:) <$> Right (Number (N $ read (x:nums))) <*> fromString others
  | isAlphaNum x = do
    let (name, others) = span isAlphaNum xs
    (:) <$> Right (Reserved $ Identity (x:name)) <*> fromString others
  | otherwise =
    case M.lookup x tokenTable of
      Just token -> (:) <$> Right token <*> fromString xs
      Nothing -> Left $ "unknown token: " ++ [x]

evaluate :: (Floating a, Read a) => [Token ArithmeticReserved HiddenNumber] -> Either String a
evaluate tokens = eval tokens [] [] 
  where
    eval [] [] [v] = Right v
    eval (x:xs) os vs =
      case x of
        Reserved op      -> eval xs (op:os) vs
        Number (N n)     -> eval xs os (n:vs)
        LeftParenthesis  -> eval xs os vs
        RightParenthesis -> do
          (op, oss) <- safeDestr "operator" os
          runOp op >>= eval xs oss
      where
        runOp (Identity idn) =
          case M.lookup idn functionTable of
            Just (U func) -> do
              (arg, vst) <- safeDestr "argument" vs
              return $ func arg : vst
            Nothing ->
              case M.lookup idn constantTable of
                Just (N value) -> return $ value : vs
                Nothing -> Left $ "unknown identity: " ++ idn
        runOp op = do
          (v1, vs1) <- safeDestr "left operand" vs
          (v2, vs2) <- safeDestr "right operand" vs1
          value <- evalSimple op v1 v2
          return $ value : vs2
    eval _ os _ = Left $ "unexpected error: " ++ show os

    safeDestr info []     = Left $ "missing value: " ++ info
    safeDestr _    (x:xs) = Right (x, xs)

calculate :: (Floating a, Read a) => String -> Either String a
calculate s = fromString s >>= evaluate

main :: IO ()
main = do
  let result = calculate "(* (+ 1 (e)) (* 8 12))" :: Either String Double
  print result
