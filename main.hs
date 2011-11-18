module Main where
 
import Lex
import Parser
import System.Environment
import System.Console.GetOpt
import System.EasyFile
import Control.Monad
import IO
import Text.Show.Pretty


data Options = Options {
        optInformation :: Bool,
        optOutput :: String, 
        optInput :: String
    }


options :: [OptDescr (Options -> IO Options)]    
options = [
    Option ['o'] ["output"] (ReqArg writeOutput "FILE") "output file",
    Option ['i'] ["information"] (NoArg infOutput) "iformation output"
    ]    
    
writeOutput arg opt = return opt {optOutput = arg}    
infOutput opt = return opt {optInformation = True}
    
main :: IO()
main =  do 
    args <- getArgs
   -- let (actions, nonOpts, msgs) = 
    opts <- case getOpt Permute options args of  
                    (act,[path],[])  -> do o <- foldl (>>=) (return Options {optInformation = False, optOutput=path,optInput = path}) act
                                           return o  
                    (act,path:ps,[]) -> do putStrLn "Можно скомпилировать только один файл. остальные файлы проигнорированы"
                                           opts <- foldl (>>=) (return Options {optInformation = False, optOutput=path,optInput=path}) act
                                           return opts
                    (_,_,msgs)      ->  error $ concat msgs
    
    let oPath = replaceExtension (optOutput opts) ".out"
    let iPath = replaceExtension (optOutput opts) ".i"
    let inPath = optInput opts
    when (optInformation opts) $ writeFile iPath ""
    src <- readFile inPath
    when (optInformation opts) $ appendFile iPath (unlines ["Source code:","",src])
    let tokens = scan src
    when (optInformation opts) $ appendFile iPath (unlines ["","Token List:","",unlines$map show tokens])
    let ast = parse tokens
    when (optInformation opts) $ appendFile iPath (unlines ["","AST:","",ppShow ast])
    writeFile oPath "компилятор еще не закончен"
    
    
            
    
    
    
