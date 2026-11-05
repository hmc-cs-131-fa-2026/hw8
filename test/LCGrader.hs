import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List
import qualified System.Environment
import qualified System.IO
import qualified System.Timeout
import Control.Monad (unless)
import qualified Control.Concurrent
import qualified Control.Exception
import qualified System.Posix.Signals

import Core  -- the underlying lamba-calculus interpreter


data Test = DoTest (String, Integer, [String], Expr, [Expr])
          | PrintIt String
          | PrintTotal
          | AdjustBy String Integer


emptyCtx =  Ctx { shortcuts = False,
                  expandUnappliedNumerals = True,
                  environment = M.empty,
                  showTrace = NoTrace,
                  lambdaChar = '\\',
                  allParens = True,
                  showColor = False,
                  lastFilename = Nothing
                }

parseArgs :: [String] -> (String, Integer, String)
parseArgs args =
  let (testFile, args1) =
        case args of
          "-t" : testFile: rest -> (testFile, rest)
          _ -> ("cs131-tests", args)
      (budget, args2) =
        case args1 of
          "-b" : b : rest -> (read b, rest)
          _ -> (500000, args1)
      studentFile =
        case args2 of
          [sf] -> sf
          [] -> error "Missing student filename"
          _ -> error "Too many command-line arguments"
  in (testFile, budget, studentFile)

main =
  do
     System.IO.hSetBuffering System.IO.stdout System.IO.NoBuffering

     {- Incantation to prevent two ^c's from killing the program -}
     tid <- Control.Concurrent.myThreadId
     System.Posix.Signals.installHandler System.Posix.Signals.keyboardSignal (System.Posix.Signals.Catch (Control.Exception.throwTo tid Control.Exception.UserInterrupt)) Nothing

     args <- System.Environment.getArgs
     let (testFile, budget, studentFile) = parseArgs args
     env <- parseFile emptyCtx studentFile
{-
     (env, tests0) <- if (M.size env > 0) then return (env, [])
                        else do env' <- parseFile (emptyCtx { allParens = False } ) studentFile
                                return (env', [AdjustBy "Parenthesis problems" (-5)])
                                -}
     let tests0 = []
     let ctx = emptyCtx { environment = env }
     tests' <- parseTests testFile
     let tests = tests0 ++ expandTests ctx 20000 tests'
     let outOf = totalPoints tests
     (_, points) <- runTests ctx budget (0, outOf) tests
     return ()


handler :: Control.Exception.SomeException -> IO Expr
handler h = return (Macro "[error]") -- "[error"++ (show h) ++ "]"

timeout :: Int -> b -> IO b -> IO b
timeout msecs fail act = (maybe fail id) <$> System.Timeout.timeout msecs act


parseTests :: String -> IO [Test]
parseTests filename =
   do result <- parseFromFile testsParser filename
      case result of
        Left err -> error (show err)
        Right tests -> return tests

disables :: Parser [String]
disables =
  many (do char '-'; macroId)

disableEnv names ctx =
  ctx { environment = M.filterWithKey (\k -> \_ -> not (elem k names)) (environment ctx) }

expr_no_Parens =
  do e <- expr
     return (stripParens e)

testParser :: Parser Test
testParser =
  (do char '+'
      points <- number
      disables <- disables
      testName <- many (noneOf "\n")
      char '\n'
      ch '?'
      testExpr <- expr_no_Parens
      expectations <- many1 (do ch '!'; expr_no_Parens)
      return $ DoTest (testName, points, disables, testExpr, expectations)) <|>
  (do string "\"\"\""
      s <- many (noneOf "\"")
      string "\"\"\""
      whitespace
      return $ PrintIt s) <|>
  (do string "*SHOWPOINTS*"
      whitespace
      return $ PrintTotal)

testsParser :: Parser [Test]
testsParser =
  do result <- many testParser
     eof
     return result

expandTest :: Ctx -> Integer -> Test -> Test
expandTest ctx budget (DoTest (name, points, disables, input, expected)) =
  let ctx' = disableEnv disables ctx
  in DoTest (name, points, disables, input, Data.List.nub (concat (map (\x -> [x,  let (_,_,_,ans) = cbnLazy ctx' M.empty x (fv x) in ans]) expected)))
expandTest ctx _ t = t

expandTests ctx budget tests =
  map (expandTest ctx budget) tests

getPoints (DoTest(_,p,_,_,_)) = p
getPoints _ = 0

totalPoints tests = sum (map getPoints tests)

spacing str1 str3 =
  let linewidth = 72
      nspaces = 72 - length str1 - length str3
  in if nspaces >= 2 then
       take (linewidth - length str1 - length str3) (repeat ' ')
     else
       "\n" ++ take (linewidth - length str3) (repeat ' ')


doCommand ctx input = 
  do let (_, _, _, ans) = cbnLazy ctx M.empty input (fv input)
    --  return (show ans `seq` ans)
     return ((show ans) `seq` ans)

cbn :: Ctx -> Expr -> IO Expr
cbn ctx input =  
  do answer <- Control.Exception.catch (timeout 10000 (Macro "<timeout>") (doCommand ctx input)) handler
  -- do answer <- Control.Exception.handle f (do let (_, _, _, ans) = cbnLazy ctx M.empty input (fv input)
  --                                             return (show ans `seq` ans))
     System.IO.hFlush System.IO.stdout
     return answer
  where f :: Control.Exception.AsyncException -> IO Expr
        f _ = do putStrLn "<interrupt>"
                 return (Macro "<interrupt>")



runTests :: Ctx -> Integer -> (Integer,Integer) -> [Test] -> IO (Bool, Integer)
runTests ctx budget (pts,outOf) [] = return (True, pts)
runTests ctx budget (pts,outOf) (DoTest(name, points, disabled, input, expected) : rest)  =
  do -- putStrLn ("Test:" ++ name)
     let ctx' = disableEnv disabled ctx
     output <- cbn ctx' input

     let result = (elem output expected)
     let p = if result then points else 0
     let str1 = " " ++ show input ++ " --> " ++ show output
     let str3 = if result then ("+" ++ show p ++ " points") else (show p ++ "/" ++ show points ++ " points")
     let str2 = spacing str1 str3
     putStrLn (str1 ++ str2 ++ str3)

     unless (result) $ putStrLn ("  expected  " ++ Data.List.intercalate "  or  " (map show expected))


{-
     (result, points) <-
       case (cbn env' (budget, input) (fv input)) of
         Just (_, output) ->
           do
              let result = (elem output expected)
              let p = if result then points else 0
              let str1 = " " ++ show input ++ " --> " ++ show output
              let str3 = if result then ("+" ++ show p ++ " points") else (show p ++ " points")
              let str2 = spacing str1 str3
              putStrLn (str1 ++ str2 ++ str3)
              if (not result) then putStrLn ("  expected  " ++ Data.List.intercalate "  or  " (map show expected)) else return ()

              return (result, p)
         Nothing ->
           do
              let p = 0
              let str1 = " " ++ show input ++ " --> [TIMEOUT or BLOWUP]"
              let str3 = ("0/" ++ show points)
              let str2 = spacing str1 str3
              putStrLn (str1 ++ str2 ++ str3)
              putStrLn ("   expected  " ++ Data.List.intercalate "  or  " (map show expected))
              {-whnfTrace env' (1000, input) (fv input)-}
              return (False, 0)
-}
     System.IO.hFlush System.IO.stdout
     (results, pointss) <- runTests ctx budget (pts + p, outOf) rest
     return $ (result && results, pointss)

runTests env budget (pts,outOf) (PrintTotal : rest) =
  do putStrLn ("\nTOTAL POINTS: " ++ show pts ++ "/" ++ show outOf)
     System.IO.hFlush System.IO.stdout
     runTests env budget (pts,outOf) rest

runTests env budget (pts,outOf) (PrintIt s : rest) =
  do putStrLn s
     System.IO.hFlush System.IO.stdout
     runTests env budget (pts,outOf) rest

runTests ctx budget (pts,outOf) (AdjustBy str1 n : rest ) =
  do putStrLn ""
     let str3 = (show n ++ " points")
     let str2 = spacing str1 str2
     putStrLn (str1 ++ str2 ++ str3)
     System.IO.hFlush System.IO.stdout
     runTests ctx budget (pts + n, outOf) rest
