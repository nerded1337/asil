-- | Commandline options
module Options where

import System.Console.GetOpt
import System.Environment
import System.FilePath
import System.IO
import System.Directory

data Options
  = Options { optSourceFile  :: !FilePath
            , optOutputFile  :: !(Maybe FilePath)
            , optVerbose     :: !Bool
            , optDumpAst     :: !Bool
            , optDumpSym     :: !Bool
            , optGenLib      :: !(Maybe String)
            , optInjectAbc   :: ![FilePath]
            , optInjectRefl  :: !Bool
            , optEnv         :: ![FilePath]
            }

opts :: [OptDescr (Options -> IO Options)]
opts = [ Option "o" ["output"]      (ReqArg oOutput "path") "output .swf file"
       , Option "v" ["verbose"]     (NoArg oVerbose) "verbose output"
       , Option ""  ["dump-ast"]    (NoArg oDumpAst) "dump AST"
       , Option ""  ["dump-sym"]    (NoArg oDumpSym) "dump symbol table"
       , Option "l" ["gen-lib"]     (ReqArg oGenLib "name") "generate instrumentation support"
       , Option "i" ["inject-abc"]  (ReqArg oInjectAbc "path") "inject .abc byte code"
       , Option "r" ["inject-refl"] (NoArg oInjectRefl) "inject reflection information"
       , Option "e" ["env"]         (ReqArg oEnv "env") "environment info"
       ]

oOutput :: FilePath -> Options -> IO Options
oOutput s o = return (o { optOutputFile = Just s })

oVerbose :: Options -> IO Options
oVerbose o = return (o { optVerbose = True })

oDumpAst :: Options -> IO Options
oDumpAst o = return (o { optDumpAst = True })

oDumpSym :: Options -> IO Options
oDumpSym o = return (o { optDumpSym = True })

oGenLib :: String -> Options -> IO Options
oGenLib n o = return (o { optGenLib = Just n })

oInjectAbc :: String -> Options -> IO Options
oInjectAbc s o = return (o { optInjectAbc = splitSearchPath s })

oInjectRefl :: Options -> IO Options
oInjectRefl o = return (o { optInjectRefl = True })

oEnv :: String -> Options -> IO Options
oEnv fs o = return (o { optEnv = splitSearchPath fs })

defaultOpts :: Options
defaultOpts = Options { optSourceFile = "", optOutputFile = Nothing, optVerbose = False
                      , optDumpAst = False, optDumpSym = False
                      , optInjectAbc = [], optGenLib = Nothing, optEnv = []
                      , optInjectRefl = False }

commandlineArgs :: IO Options
commandlineArgs
  = do args <- getArgs
       let usage = usageInfo "Usage: asic <OPTION ...> <abc-file> ..." opts
       case getOpt Permute opts args of
         (actions, args', []) | null args' -> do hPutStrLn stderr ("No ABC source file specified.\n" ++ usage)
                                                 foldl (>>=) (return defaultOpts) actions
                              | otherwise  -> foldl (>>=) (return $ patch $ defaultOpts { optSourceFile = head args' }) actions
         (_, _, errs)                      -> do hPutStrLn stderr (unlines errs ++ "\n" ++ usage)
                                                 return defaultOpts
  where
    patch o = if optSourceFile o /= ""
              then o { optOutputFile = Just $ replaceExtension (optSourceFile o) ".out" }
              else o
