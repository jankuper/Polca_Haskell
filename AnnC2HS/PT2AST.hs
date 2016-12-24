module PT2AST where

import Debug.Trace

import GHC.Generics
import FPPrac.Trees
import PolcaAlphabet
-- import PolcaGrammar
-- import Tokenizer
import TypesEtc
-- import ParserGen

str2op str = case str of
        "+"      -> Add
        "-"      -> Sub
        "*"      -> Mul
        "/"      -> Div
        "%"      -> Mod
        "^"      -> Exp
        "!"      -> Neg
        "&&"     -> And
        "||"     -> Or
        "=="     -> Eql
        "!="     -> NEq
        "<"      -> Lt
        ">"      -> Gt
        "<="     -> LEq
        ">="     -> GEq

-- =======================
-- ParseTree-2-Annotation/Statement
-- =======================

pt2inpAST (PNode P_Program stmnts) = map pt2AnnStmnt stmnts
pt2inpAST x = error $ show x

pt2AnnStmnt (PNode nt ts) = case nt of

  P_AnnStmnt     -> case ts of
                     [ PLeaf(Op,"#"), PLeaf(AnnWord,"pragma"), PLeaf(AnnWord,"polca"), ann, PLeaf(Sep,";") ]       -> pt2AnnStmnt ann

                     [ PLeaf(PrgWord,"fundef"), PLeaf(IdfTkn,name), PNode P_ParList parlist, PNode P_Body (hd:p_body) ]  -> FunDef (Idf name) pars body
                                                                                                                    where
                                                                                                                      -- pars = Tuple [ Idf x | PLeaf(IdfTkn,x) <- parlist ]

                                                                                                                      pars = Tuple $ map pt2expr $ tail  $ init $ filter (/=PLeaf(Sep,",")) parlist

                                                                                                                      body | hd == PLeaf(Bracket,"{") = map pt2AnnStmnt $ init p_body
                                                                                                                           | otherwise                = [ pt2AnnStmnt hd ]

        -- ,PLeaf (Sep,",")
        -- ,PLeaf (IdfTkn,"y")
        -- ,PLeaf (Sep,",")
        -- ,PLeaf (IdfTkn,"z")
        -- ,PLeaf (Bracket,")")
        -- ,PNode P_Body


                     [ PLeaf (PrgWord,"return"), expr, PLeaf(Sep,";") ]                  -> Return $ pt2expr expr

                     [ var, PLeaf(Op,"="), expr, PLeaf(Sep,";") ]                        -> Assign (pt2expr var) (pt2expr expr)

                     ( PLeaf(PrgWord,"if") : PNode P_ITE_Test tst : then_br : else_br )  -> IfS test_Expr then_Stmnts else_Stmnts
                                                                                         where
                                                                                           [  PLeaf(Bracket,"(")
                                                                                            , lhs
                                                                                            , PLeaf(BlnOp,r)
                                                                                            , rhs
                                                                                            , PLeaf(Bracket,")")
                                                                                            ]                           = tst
                                                                                           test_Expr  = BinInfix (str2op r) (pt2expr lhs) (pt2expr rhs)

                                                                                           PNode P_Body (t_hd:branch) = then_br
                                                                                           then_Stmnts | t_hd == PLeaf(Bracket,"{") = map pt2AnnStmnt $ init branch
                                                                                                       | otherwise                  = [ pt2AnnStmnt t_hd ]

                                                                                           else_Stmnts | null else_br               = []
                                                                                                       | e_hd == PLeaf(Bracket,"{") = map pt2AnnStmnt $ init branch
                                                                                                       | otherwise                  = [ pt2AnnStmnt e_hd ]
                                                                                                       where
                                                                                                         [ _ , PNode P_Body (e_hd:branch) ] = else_br

                     [ PLeaf(PrgWord,"for"), PNode P_LoopCntr cntr, PNode P_Body (hd:body) ]
                                                                                         -> For (LoopCntr
                                                                                                (Idf i)
                                                                                                (pt2expr n_start)
                                                                                                (pt2expr n_end)
                                                                                                (Const 1)
                                                                                                )
                                                                                                body_Expr
                                                                                         where
                                                                                           [  PLeaf(Bracket,"(")
                                                                                            , PLeaf(IdfTkn,i)
                                                                                            , PLeaf(Op,"=")
                                                                                            , n_start
                                                                                            , PLeaf(Sep,";")
                                                                                            , PLeaf(IdfTkn,_)
                                                                                            , PLeaf(BlnOp,r)
                                                                                            , n_end
                                                                                            , PLeaf (Sep,";")
                                                                                            , PLeaf (IdfTkn,_)
                                                                                            , PLeaf (Op,"++")
                                                                                            , PLeaf (Bracket,")")
                                                                                            ]                      = cntr

                                                                                           body_Expr | hd == PLeaf(Bracket,"{") = map pt2AnnStmnt $ init body
                                                                                                     | otherwise                = [ pt2AnnStmnt hd ]

                     _   -> error (show ts)

  P_Polca_ann    -> pt2AnnStmnt $ ts!!0         -- it's just a singleton list with the actual annotation

  P_Func_ann     -> PolcaFunc hof f args z
                 where
                   PLeaf (AnnWord,"func") : PLeaf (IdfTkn,hof) : PLeaf (IdfTkn,f) : pExprs = ts

                   exprs = map pt2expr pExprs
                   args  = init exprs
                   z     = last exprs

  P_Def_ann      -> PolcaDef f
                 where
                   [ PLeaf(AnnWord,"def"), PLeaf(IdfTkn,f) ] = ts

  P_IO_ann       -> PolcaIO inpoutp (map pt2expr ioVarList)
                 where
                   PLeaf (IOWord,inpoutp) : ioVarList = ts

  _              -> error ("pt2AnnStmnt: " ++ show nt ++ "\n" ++ show ts)





-- =======================
-- ParseTree-2-Expression
-- =======================

pt2expr (PLeaf (IdfTkn, x))                                  = Idf x
pt2expr (PLeaf (Nmbr, x))                                 = Const $ read x

pt2expr (PNode _ [t]) = pt2expr t
-- pt2expr (PNode _ [PNode nt ts]) = pt2expr (PNode nt ts)           -- singleton cases; not for PLeaf       -- don't remember: why not?

pt2expr (PNode nt ts) = case nt of

  -- Def_ann           -> ...
  -- Kernel_ann        -> ...
  -- Adapt_ann         -> ...
  -- Func_ann          -> ...
  -- Mem_ann           -> ...
  -- Varinfo_ann       -> ...
  -- Math_ann          -> ...
  -- HardwareIO_ann    -> ...
  -- Guard_ann         -> ...

  P_Expr         -> case ts of
               [ mTerm, PLeaf (AddOp,str), expr ]  -> BinInfix (str2op str) (pt2expr mTerm) (pt2expr expr)
               -- [ PLeaf (Fnc,f), PLeaf(Bracket,"("), expr, PLeaf(Bracket,")") ]   -> App (Function f) (pt2expr expr)
               _ -> error $ "Expr:: " ++ show nt ++ show ts

  P_Term         -> case ts of
               [ mFact, PLeaf (MulOp,str), mTerm ]  -> BinInfix (str2op str) (pt2expr mFact) (pt2expr mTerm)
               _ -> error $ "Term " ++ show nt ++ show ts

  P_Factor       -> case ts of

               ( PLeaf(IdfTkn,f) : pTrees )                             -> App (Idf f) $ pt2expr $ PNode P_Factor pTrees

               [ PLeaf(Bracket,"("), expr, PLeaf(Bracket,")") ]         -> pt2expr expr

               ( PLeaf(Bracket,"(") : pTrees )                          -> Tuple $ map pt2expr exprs            -- Tuple at least 2 elements long
                                                                        where
                                                                          exprs = filter (/= PLeaf(Sep,",")) $ init pTrees

               ( PLeaf(Bracket,"[") : pTrees )                          -> List $ map pt2expr exprs
                                                                        where
                                                                          exprs = filter (/= PLeaf(Sep,",")) $ init pTrees

               _ -> error $ "Factor: " ++ show nt ++ show ts

  P_Number       -> case ts of
               [ PLeaf (Nmbr,n) ]                   -> Const $ read n
               -- TODO: distinction Int, Float
               _ -> error $ "Number: " ++ show nt ++ show ts

  P_Var0         -> case ts of

               [ PLeaf (IdfTkn,x) ]                                -> Idf x
               [ PLeaf(Bracket,"("), var, PLeaf(Bracket,")") ]  -> pt2expr var
               ( PLeaf(Bracket,"(") : vars )                    -> Tuple $ map pt2expr $ filter (/= PLeaf(Sep,",")) $ init vars




  P_Var          -> case ts of

               [ PLeaf (MulOp,"*"), x ]              -> Ref $ pt2expr x
               [ PLeaf (Op,"&"), x ]                 -> Deref $ pt2expr x

               ( var0 : indexes )                    -> indxs v indexes
                                                     where
                                                       v = pt2expr var0

                                                       indxs v []                       = v
                                                       indxs v (marker : ind : indexes) = case marker of
                                                                PLeaf (Op,".")      -> indxs (RecSel v ind') indexes
                                                                                    where
                                                                                      PLeaf (IdfTkn,ind') = ind
                                                                PLeaf (Bracket,"[") -> indxs (LstSel v ind') indexes'
                                                                                    where
                                                                                      ind'     = pt2expr ind
                                                                                      indexes' | head indexes == PLeaf (Sep,",")     = PLeaf (Bracket,"[") : tail indexes
                                                                                               | head indexes == PLeaf (Bracket,"]") = tail indexes
                                                                                               | otherwise                           = error ("indxs: " ++ show indexes)

{-
               ( var0  : PLeaf (Op,".") : fldnm : indexes )
                                                    -> RecSel term $ pt2expr fldnm
                                                    where
                                                      noLastSqBracket | last indexes == rSqBrack   = init indexes                    -- remove "]"
                                                                      | otherwise                  = indexes
                                                      lastindex       = pt2expr $ last noLastSqBracket   -- process last "true index"
                                                      earlierIndexes  = init noLastSqBracket            -- remove last "true index"
                                                      term  | earlierIndexes == []
                                                                = pt2expr var0
                                                            | last earlierIndexes == PLeaf(Bracket,"[") 
                                                                =  pt2expr $ PNode P_Var $ var0 : PLeaf(Bracket,"[") : init earlierIndexes
                                                                                -- in case of a[i][j], etc
                                                            | last earlierIndexes == PLeaf(Sep,",") 
                                                                =  pt2expr $ PNode P_Var $ var0 : PLeaf(Bracket,"[") : init earlierIndexes ++ [PLeaf(Bracket,"]")]
                                                                                -- in case of a[i,j], etc
                                                            | otherwise
                                                                = error "P_Var: something wrong"

               ( var0  : PLeaf (Bracket,"[") : indexes )
                                                    -> LstSel term lastindex
                                                    where
                                                      noLastSqBracket = init indexes                    -- remove "]"
                                                      lastindex       = pt2expr $ last noLastSqBracket   -- process last "true index"
                                                      earlierIndexes  = init noLastSqBracket            -- remove last "true index"
                                                      term  | earlierIndexes == []
                                                                = pt2expr var0
                                                            | last earlierIndexes == PLeaf(Bracket,"[") 
                                                                =  pt2expr $ PNode P_Var $ var0 : PLeaf(Bracket,"[") : init earlierIndexes
                                                                                -- in case of a[i][j], etc
                                                            | last earlierIndexes == PLeaf(Sep,",") 
                                                                =  pt2expr $ PNode P_Var $ var0 : PLeaf(Bracket,"[") : init earlierIndexes ++ [PLeaf(Bracket,"]")]
                                                                                -- in case of a[i,j], etc
                                                            | otherwise
                                                                = error "P_Var: something wrong"
-}

               _                                     -> error $ "P_Var: " ++ show nt ++ show ts



  _            -> error ("pt2expr: " ++ show nt ++ " not defined yet")

pt2expr (PError pTree xs y message k) = error (show pTree ++ " ------- " ++ message)
