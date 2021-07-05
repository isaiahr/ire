module Pipeline.Pipeline (pipelineIO) where

{-
Pipeline.hs - functions for whole-program compilation

-}

import System.IO
import System.Exit
import System.IO.Error
import System.Console.ANSI
import Control.Monad.State
import Control.Exception (evaluate)
import Data.List

import Common.Common
import Common.Pass
import Parser.Lexer
import Parser.Parser
import AST.AST
import Pass.YieldInjection
import Pass.Typer
import Pass.SubScript
import Pass.UnSubScript
import Pass.Namer
import Pass.NameTyper
import Pass.TypeChecker
import IR.Monomorphization
import qualified IR.Syntax as IR.Syntax
import IR.DirectCall
import IR.Lowering
import IR.HeapConversion
import IR.LambdaLift
import IR.CodeGen
import Pipeline.Relations
import Pipeline.Target

data PFile = PFile {
    pLocation :: String,
    pObjLocation :: String,
    pExports :: [(String, Type)],
    pImports :: [String],
    pMsgs :: Messages,
    pFileInfo :: FileInfo
} 
instance Show PFile where
    show pf = show (pLocation pf) <> show (pExports pf) <> show (pImports pf)
 
pipelineIO target filename stage outfile = do
    -- todo target etc etc etc
    files <- importDag filename
    (errs, processed_files, (mtff, mnamef)) <- execStateT (forM (zip [1 .. (length files)] files) $ (\(idx, x) -> do 
        liftIO $ putStr $ "[" <> show idx <> " of " <> show (length files) <> "] Compiling " <> (uPath x)
        mpfiles <- get
        case mpfiles of
            ([], pfiles, (mtlf, mname)) -> do
                -- this selects what to do with output after running the compiler.
                let (tstage, out) = (case (idx == (length files), stage) of
                                            -- if we are compiling to executable, always produce an object (and link after all files compiled)
                                            (_, S_BIN) -> (Just $ S_OBJ, Nothing)
                                            -- if this is the last file to be processed, compile to what the cli option says.
                                            -- (for example, if we want llvm output, produce .ll, want obj produce .o)
                                            (True, _) -> (Just $ stage, Just outfile)
                                            -- otherwise, since this is not the last file, and we wont need output for linking, 
                                            -- just run a "dummy" pass and discard the result.
                                            (False, _) -> (Nothing, Nothing))
                epfile <- liftIO $ tryIOError (compile mtlf mname tstage target x pfiles idx out)
                case epfile of 
                    Left err -> do
                        put ([err], pfiles, (mtlf, mname))
                        liftIO $ putStr $ " ["
                        liftIO $ setSGR [SetColor Foreground Dull Red]
                        liftIO $ putStr "Failed" 
                        liftIO $ setSGR [Reset]
                        liftIO $ putStrLn "]" 
                        
                    Right (mtlf', mname', pfile) -> do
                        put ([], (pfile:pfiles), (mtlf', mname'))
                        liftIO $ putStr $ " ["
                        liftIO $ setSGR [SetColor Foreground Dull Green]
                        liftIO $ putStr "OK" 
                        liftIO $ setSGR [Reset]
                        liftIO $ putStrLn "]" 
            (errs, pfiles, _) -> do
                liftIO $ putStr $ " ["
                liftIO $ setSGR [SetColor Foreground Dull Yellow]
                liftIO $ putStr "Skipped" 
                liftIO $ setSGR [Reset]
                liftIO $ putStrLn "]" 
                
        )) ([], [], ([], []))
    if null errs then do
        if stage == S_BIN then do 
            libs <- getLinkedLibs target
            runLinker target (libs <> (map pObjLocation processed_files)) outfile
                          else return ()
    else do 
        forM errs $ \y -> hPutStrLn stderr (ioeGetErrorString y)
        exitFailure
    return $ foldl (<>) mempty (map pMsgs processed_files)


pipeline1 x = passLexer >>> 
              passParse >>>
              passYieldInj >>>
              passName x >>>
              passSubScript >>>
              passType -- >>>
              -- passUnSubScript

-- x = exported syms
pipeline2 x fi tlfs names = passTypeCheck >>> 
              passLower x fi >>>  
              passDCall >>> 
              passHConv >>>
              passLLift >>>
              passMonoM tlfs names >>>
              byPassWith (\(a, b, c) -> a) (\(newir,(a, b, c)) -> (newir, b, c)) passGenLLVM

{-
Generalized compilation function.
parameters: 
monoTLF: top-level functions that are candidates to monomorphization (havent been monomorphized, but may in the future when needed)
monoN: monomorphized versions of monoTLF, for re-use
stage: stage to which to compile to. Nothing -> no post-processing
target: target os-arch pair
ufile: unproccesed file to read from
processed: list of processed files.
idx: compilation index. used for file uniqueness.
mout: maybe output. will place output here. nothing -> temp file.
--}

compile :: [IR.Syntax.TLFunction] -> [IR.Syntax.Name] -> Maybe Stage -> Target -> UFile -> [PFile] -> Int -> Maybe String -> IO ([IR.Syntax.TLFunction], [IR.Syntax.Name], PFile)
compile monoTLF monoN stage target file processed idx mout = do
    inhandle <- openFile (uPath file) ReadMode
    hSetEncoding inhandle utf8
    contents <- hGetContents inhandle
    -- trick. lazy IO is dumb, so we force evaluation to actually close the handle.
    contents_sz <- evaluate (length contents)
    hClose inhandle
    importedsyms <- case importedSyms file processed of
        Left p -> ioError (userError p)
        Right t -> return t
    let (msg, result) = runPass contents (pipeline1 importedsyms)
    case result of
         Nothing -> ioError (userError $ disp (filterErrs msg))
         Just (AST ds) -> do
             
             let astsyms = map (\(TypedName t (Name s _)) -> (s, t)) (map (\(Plain p) -> p) (map (\d -> identifier d) ds))
             
             let allsyms = (astsyms ++ (map (\(x, y, t) -> (x, y)) importedsyms))
             if allsyms /= nub allsyms then
                 ioError (userError "Conflicting symbols")
                                       else 
                 return ()
             if (filter (\s -> s `elem` (map (\(s2, t2) -> s2) allsyms)) (uExports file)) == (uExports file) then
                 return () -- ok, all exports are imported
                                                                                          else 
                 ioError (userError "Exporting symbol not in file")
             let exportedsyms = map (\s -> (filter (\(s2, t) -> s == s2) allsyms) !! 0) (uExports file)
             let astexports = filter (\(s, i) -> s `elem` uExports file) (map (\(TypedName t (Name s i)) -> (s, i)) (map (\(Plain p) -> p) (map (\d -> identifier d) ds)))
             let (msg2, result2) = runPass (AST ds) (pipeline2 astexports FileInfo { fiSrcFileName = (uPath file), fiFileId = idx} monoTLF monoN)
             case result2 of
                 Nothing -> ioError (userError $ disp (filterErrs (msg <> msg2)))
                 Just (y, tlf0, name0) -> do 
                     tout <- case stage of 
                            Nothing -> return "" -- no outfile. 
                            (Just tar) -> do
                                case mout of
                                    (Just outfile) -> do 
                                        writeOutput (disp y) outfile target tar
                                        return outfile
                                    Nothing -> do
                                        outfile <- getTempFile
                                        writeOutput (disp y) outfile target tar
                                        return outfile
                     return $ (tlf0, name0, PFile {
                         pLocation = (uPath file),
                         pObjLocation = tout, 
                         pExports = exportedsyms, 
                         pImports = (uImports file), 
                         pMsgs = (msg <> msg2),
                         pFileInfo = FileInfo { fiSrcFileName = (uPath file), fiFileId = idx}
                     })

importedSyms :: UFile -> [PFile] -> Either String [(String, Type, FileInfo)]
importedSyms uf apf = case nub eof1 == eof1 of
                           True -> Right $ eof
                           False -> Left $ "Duplicate symbol"
    where eof = exportsOf (uImports uf) apf
          eof1 = map (\(x,y,z) -> x) eof

exportsOf :: [String] -> [PFile] -> [(String, Type, FileInfo)]
exportsOf s pf = execState (forM s $ \x -> do
    let t = (filter (\y -> (pLocation y) == x) pf) !! 0
    let yy = exportsOf (pImports t) pf
    (forM yy $ \xyz -> do
        st <- get
        if xyz `elem` st then do
            return ()
                         else do
            put $ xyz:st)
    forM (pExports t) $ \(str, ty) -> do
        st <- get
        if (str, ty) `elem` (map (\(x0, y0, z0) -> (x0, y0)) yy) then 
            return () -- reexport
        else
            if (str, ty, (pFileInfo t)) `elem` st then
                return ()
            else 
                put $ (str, ty, (pFileInfo t)):st
    ) []
