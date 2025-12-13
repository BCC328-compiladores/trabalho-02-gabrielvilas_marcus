module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Tree (drawTree)

-- Importações corretas
import Frontend.Lexer.SlLexer (lexer, runAlex) -- Precisamos importar runAlex
import Frontend.Parser.SlParser (slParser)
import Frontend.Pretty.SlPretty (prettyPrint) 
import Frontend.Parser.AstToTree (astToTree)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--lexer", file]  -> runLexer file
        ["--parser", file] -> runParser file
        ["--pretty", file] -> runPretty file
        _ -> printUsage

printUsage :: IO ()
printUsage = do
    putStrLn "Uso: slc [opcao] <arquivo>"
    putStrLn "Opcoes:"
    putStrLn "  --lexer   : Executa apenas a analise lexica"
    putStrLn "  --parser  : Executa a analise sintatica"
    putStrLn "  --pretty  : Formata o codigo original"
    exitFailure

-- Lexer puro (continua usando a função lexer que retorna lista)
runLexer :: FilePath -> IO ()
runLexer file = do
    content <- readFile file
    case lexer content of
        Left err -> do
            putStrLn $ "Erro Lexico: " ++ err
            exitFailure
        Right tokens -> mapM_ print tokens 

-- Parser Monádico (Agora usamos runAlex passando o slParser)
runParser :: FilePath -> IO ()
runParser file = do
    content <- readFile file
    case runAlex content slParser of
        Left err -> do
            putStrLn $ "Erro: " ++ err
            exitFailure
        Right ast -> do
            -- Converte a AST em Data.Tree e usa drawTree para impressão legível
            let astStringTree = astToTree ast 
            putStrLn (drawTree astStringTree)

-- Pretty Printer (Usa o Parser Monádico e depois imprime)
runPretty :: FilePath -> IO ()
runPretty file = do
    content <- readFile file
    case runAlex content slParser of
        Left err -> do
            putStrLn $ "Erro: " ++ err
            exitFailure
        Right ast -> putStrLn (prettyPrint ast)