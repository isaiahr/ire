{-# LANGUAGE FlexibleInstances #-} -- for hack, see below
module Pipeline.Pipeline (pipelineIO) where

{-
Pipeline.hs - functions for whole-program compilation

-}

import System.FilePath
import System.IO
import System.Exit
import System.IO.Error
import System.CPUTime
import Control.Monad.State
import Control.Exception (evaluate)
import Data.List
import Text.Printf

import Common.Common
import Common.Pass
import Common.Terminal
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
import Pass.Specialization
import IR.Monomorphization
import qualified IR.Syntax as IR.Syntax
import IR.DirectCall
import IR.Lowering
import IR.HeapConversion
import IR.LambdaLift
import IR.CodeGen
import IR.GCPrepare
import Pipeline.Relations
import Pipeline.Target
import LLVM.Syntax

data PFile = PFile {
    pLocation :: String,
    pObjLocation :: String,
    pExports :: [(String, Type)],
    pImports :: [String],
    pMsgs :: Messages,
    pFileInfo :: FileInfo,
    pTimingInfo :: [(String, Double)]
} 
instance Show PFile where
    show pf = show (pLocation pf) <> show (pExports pf) <> show (pImports pf)
 
pipelineIO target filename stage opt time outfile = do
    -- todo target etc etc etc
    files <- importDag filename
    (errs, processed_files, (mtff, mnamef)) <- execStateT (forM (zip [1 .. (length files)] files) $ (\(idx, x) -> do 
        liftIO $ putStr $ "[" <> show idx <> " of " <> show (length files) <> "] Compiling " <> (uPath x)
        (errs2, pfiles, (mtlf, mname)) <- get
        case (whitelistSev (errs2) [Common.Pass.Error]) == mempty of
            True -> do
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
                epfile <- liftIO (compile mtlf mname tstage opt target x pfiles idx out)
                case epfile of 
                    Left err -> do
                        put (err, pfiles, (mtlf, mname))
                        liftIO $ putStr $ " ["
                        liftIO $ printColour Red "Failed" 
                        liftIO $ putStrLn "]" 
                    Right (mtlf', mname', pfile) -> do
                        put (mempty, (pfile:pfiles), (mtlf', mname'))
                        liftIO $ putStr $ " ["
                        liftIO $ printColour Green "OK"
                        liftIO $ putStrLn "]" 
            False -> do
                liftIO $ putStr $ " ["
                liftIO $ printColour Yellow "Skipped"
                liftIO $ putStrLn "]" 
                
        )) (mempty, [], ([], []))
    if mempty == (whitelistSev (errs) [Common.Pass.Error]) then do
        if stage == S_BIN then do 
            libs <- getLinkedLibs target
            runLinker target (libs <> (map pObjLocation processed_files)) outfile
                          else return ()
    else do 
        writeMessages (whitelistSev (errs) [Common.Pass.Error])
    if time then do
        writeTimingInfo processed_files
    else return ()
    return $ foldl (<>) mempty (map pMsgs processed_files)

writeTimingInfo :: [PFile] -> IO ()
writeTimingInfo pf = do
    putStr "Pass Name"
    forM pf $ \p -> do
        putStr ", "
        putStr $ takeFileName (pLocation p)
    putStrLn ""
    let passes = magicHelper pf
    forM passes $ \(p, data2) -> do
        putStr $ p
        forM data2 $ \(file, db) -> do
            putStr ", "
            putStr (printf "%.4f" db)
        putStrLn ""
        return ()
    return ()
        
-- magic. dont ask
-- ok. but what it does is sort of a matrix transpose on the things, and rearranges them
-- so that the first pass goes first. (note a < b and b < c does not imply a < c, so its not posible for it
-- to be perfect.)
magicHelper :: [PFile] -> [(String, [(String, Double)])]
magicHelper pfs = case foldr magic2 Nothing pfs of
                       Just lowest -> (lowest, map (\y -> case (filter (\z -> fst z == lowest) (pTimingInfo y)) of 
                                            [(p, t)] -> (pLocation y, t)
                                            -- below: n/a 
                                            _        -> (pLocation y, 0)) pfs): (magicHelper (remove lowest pfs))
                       Nothing -> []
    where magic2 pf1 (Just u) = case elemIndex u (map fst (pTimingInfo pf1)) of
                                    Nothing -> Just u
                                    Just 0 -> Just u
                                    Just i -> Just $ fst ((pTimingInfo pf1) !! 0)
          magic2 pf1 Nothing = case (pTimingInfo pf1) of
                                    [] -> Nothing
                                    ((a,b):_) -> Just a
          remove lowest pfs = map (rem2 lowest) pfs
          rem2 lowest pf = pf{pTimingInfo = filter (\y -> (fst y) /= lowest) (pTimingInfo pf)}

pipeline1 x pr = (pr >>>>
              passLexer >>>=
              passParse >>>=
              passYieldInj >>>=
              passName x >>>=
              passSpecialize) -- >>>

-- hack kind of 
instance Disp (IR.Syntax.IR, [IR.Syntax.TLFunction], [IR.Syntax.Name]) where
    disp (a, b, c) = disp a
    
instance Disp (LLVM.Syntax.LMod, [IR.Syntax.TLFunction], [IR.Syntax.Name]) where
    disp (a, b, c) = disp a
-- x = exported syms
pipeline2 x fi tlfs names = \pr -> (pr >>>>
              passTypeCheck >>>=
              passLower x fi >>>=  
              passDCall >>>=
              passHConv >>>=
              passLLift >>>=
              passMonoM tlfs names >>>=
              byPassWith (\(a, b, c) -> a) (\(newir, (a, b, c)) -> (newir, b, c)) passGCPrepare >>>=
              byPassWith (\(a, b, c) -> a) (\(newir,(a, b, c)) -> (newir, b, c)) passGenLLVM)

{-
Generalized compilation function.
parameters: 
monoTLF: top-level functions that are candidates to monomorphization (havent been monomorphized, but may in the future when needed)
monoN: monomorphized versions of monoTLF, for re-use
stage: stage to which to compile to. Nothing -> no post-processing
opt: run opt -O3 to optimize IR 
target: target os-arch pair
ufile: unproccesed file to read from
processed: list of processed files.
idx: compilation index. used for file uniqueness.
mout: maybe output. will place output here. nothing -> temp file.
--}

compile :: [IR.Syntax.TLFunction] ->
           [IR.Syntax.Name] ->
           Maybe Stage ->
           Bool ->
           Target ->
           UFile ->
           [PFile] ->
           Int ->
           Maybe String ->
           IO (Either Messages ([IR.Syntax.TLFunction], [IR.Syntax.Name], PFile))
compile monoTLF monoN stage opt target file processed idx mout = do
    inhandle <- openFile (uPath file) ReadMode
    hSetEncoding inhandle utf8
    contents <- hGetContents inhandle
    -- trick. lazy IO is dumb, so we force evaluation to actually close the handle.
    contents_sz <- evaluate (length contents)
    hClose inhandle
    importedsyms <- case importedSyms file processed of
        Left p -> ioError (userError p)
        Right t -> return t
    result <- (pipeline1 importedsyms) (mkPassResult contents (Just $ uPath file) )
    case (prPassResult result) of
         Nothing -> do 
             return $ Left (prPassMessages result)
         Just ast -> do
             let ds = astDefns ast
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
             result2 <- (pipeline2 astexports FileInfo { fiSrcFileName = (uPath file), fiFileId = idx} monoTLF monoN) result
             case (prPassResult result2) of
                 Nothing -> do
                     writeMessages (whitelistSev (prPassMessages result2) [Common.Pass.Error])
                     exitWith (ExitFailure 1)
                 Just (y, tlf0, name0) -> do 
                     (tout, ti2) <- case stage of 
                            Nothing -> return ("", []) -- no outfile. 
                            (Just tar) -> do
                                case mout of
                                    (Just outfile) -> do 
                                        a <- getCPUTime
                                        writeOutput (disp y) outfile target tar opt
                                        b <- getCPUTime                    
                                        let delta = b - a
                                        let millis = (fromIntegral(delta)/1000000000)::Double
                                        return (outfile, [("LLVM Toolchain", millis)])
                                    Nothing -> do
                                        outfile <- getTempFile
                                        a <- getCPUTime
                                        writeOutput (disp y) outfile target tar opt
                                        b <- getCPUTime                    
                                        let delta = b - a
                                        let millis = (fromIntegral(delta)/1000000000)::Double
                                        return (outfile, [("LLVM Toolchain", millis)])
                     return $ Right (tlf0, name0, PFile {
                         pLocation = (uPath file),
                         pObjLocation = tout, 
                         pExports = exportedsyms, 
                         pImports = (uImports file), 
                         pMsgs = (prPassMessages result2),
                         pFileInfo = FileInfo { fiSrcFileName = (uPath file), fiFileId = idx},
                         pTimingInfo = (prTime result2 <> ti2)
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
