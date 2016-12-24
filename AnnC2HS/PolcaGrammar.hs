module PolcaGrammar where

import Debug.Trace
import PolcaAlphabet

polcaGrammar nt = case nt of

    -- *****************************************************************
    -- Statement and Annotation Syntax *********************************
    -- *****************************************************************

    P_Program             -> [[(*:)[P_AnnStmnt]               ]
                             ]
    P_AnnStmnt            -> [[ fundef, idf, P_ParList, P_Body ]
                             ,[ rtrn, P_Expr, semicolon ]
                             ,[ P_Var, eqSign, P_Expr, semicolon ]
                             ,[ if_kw, P_ITE_Test, P_Body, (?:)[else_kw,P_Body]    ]
                             ,[ for, P_LoopCntr, P_Body          ]
                             ,[ hash, pragma, polca, P_Polca_ann, semicolon      ]           -- "#" is an operation (see tokenizer) ??
                             ]
    P_ITE_Test            -> [[ lBrack, P_Expr, blnOp, P_Expr, rBrack              ]             -- ABSOLUTELY not complete
                             ]
    P_LoopCntr            -> [[ lBrack,
                                   idf, eqSign, nmbr, semicolon,
                                   idf, blnOp,  P_Expr, semicolon,
                                   idf, plusplus,
                                rBrack ]
                             ]

    P_ParList             -> [[ lBrack, rBrack ]
                             -- ,[ lBrack, idf, (*:)[comma,idf], rBrack ]
                             ,[ lBrack, P_Factor, (*:)[comma,P_Factor], rBrack ]
                             ]

    P_Body                -> [[ P_AnnStmnt                      ]
                             ,[ lCurly, (*:)[P_AnnStmnt], rCurly  ]
                             ]

    P_Polca_ann           -> [[ P_Func_ann                    ]   -- specific formats for HOFs (IO-names the same as in Def_ann)
                             ,[ P_Def_ann                     ]   -- defines a name for the block of C-code
                             ,[ P_IO_ann                      ]   -- related to a Def-ann, indicates input/output variables

                             -- ,[ Math_ann                    ]   -- properties of operators, algebraic structures
                             -- ,[ P_Guard_ann                   ]   -- to express branching
                             -- ,[ Smath                       ]   -- simple math, algebraic expressions on numbers
                             -- ,[ Kernel_ann                  ]   -- black box pieces of code
                             -- ,[ Adapt_ann                   ]   -- indicates the platform to which the C-code should be compiled
                             -- ,[ Mem_ann                     ]   -- size and number of variables (as needed for e.g. malloc)
                             -- ,[ Varinfo_ann                 ]   -- ibid, for existing variables
                             -- ,[ HardwareIO_ann              ]   -- reading/writing of the C-code
                             ]

    -- *****************************************************************
    -- HOFs ************************************************************
    -- *****************************************************************
    P_Func_ann            -> [[ func, idf, idf, (+:)[P_Expr], P_Expr  ]    -- <<=== jk: Summarizes Func_ann, Func, Func_out_data
                             ]

    -- *****************************************************************
    -- Definitions *****************************************************
    -- *****************************************************************
    P_Def_ann             -> [[ def, idf                            ]
                             ]

    -- *****************************************************************
    -- IO- annotations *************************************************
    -- *****************************************************************
    P_IO_ann              -> [[ ioWord, (*:)[P_Var]                     ]
                             -- ,[ ioTerm, (*:)[Zip]            ]           -- <<=== jk: Zip???
                             ]

    -- *****************************************************************
    -- Math expressions ************************************************
    -- *****************************************************************

    -- P_Rel_Expr            -> [[ P_Expr, relOp, P_Expr                          ]
    --                          ]
    P_Bln_Expr            -> [[ negOp, P_Bln_Expr                              ]        -- <<=== jk REPLACES ALL boolean rules
                             -- ,[ P_Rel_Expr, blnOp, P_Bln_Expr                  ]        -- <<=== jk TODO: combine with math-expr  -- LEFT-rcursive
                             ,[ lBrack, P_Bln_Expr, rBrack                     ]        --          MAKE: boolean exprs into Math exprs
                             ,[ P_Var                                          ]        --           ADD: typing
                             ]
    P_Expr                -> [[ P_Term, addOp, P_Expr                          ]       -- <<=== jk: Originally separated for +, -
                             ,[ P_Term                                         ]
                             ]
    P_Term                -> [[ P_Factor, mulOp, P_Term                        ]       -- <<=== jk: Originally separated for *, /, etc
                             ,[ P_Factor                                       ]
                             ]
    P_Factor              -> [[ lBrack, P_Expr, (*:)[comma,P_Expr], rBrack     ]
                             ,[ idf, lBrack, rBrack              ]
                             ,[ idf, lBrack, P_Expr, (*:)[comma,P_Expr], rBrack              ]
                             ,[ lSqBrack, rSqBrack                             ]
                             ,[ lSqBrack, P_Expr, (*:)[comma,P_Expr], rSqBrack ]
                             ,[ lSqBrack, P_Expr, dotdot, P_Expr, rSqBrack     ]               -- <<=== jk: only with "+1"
                             ,[ P_Number                                       ]
                             ,[ P_Var                                          ]
                             ]
    P_Var0                -> [[ idf                                            ]
                             ,[ lBrack, P_Var, rBrack                          ]
                             ,[ lBrack, P_Var, (*:)[comma,P_Var], rBrack       ]
                             ]
    P_Var                 -> [[ P_Var0, (*:)[[[dot,idf]<>[lSqBrack, P_Expr, rSqBrack]]
                                                       <>[lSqBrack, P_Expr, (+:)[comma,P_Expr], rSqBrack ]] ]   -- <<=== jk ADDED: [i][j], etc (also 0x, i.e. just idf)
                             ,[ refOp, P_Var                                   ]          -- <<=== REPLACES C_array
                             ,[ derefOp, P_Var                                 ]          -- "*", "&"
                             ]
    P_Number              -> [[ nmbr                                           ]
                             ,[ float                                          ]
                             ]

    -- *****************************************************************
    -- Guards **********************************************************
    -- *****************************************************************
    P_Guard_ann           -> [[ guard, P_Guard_data                            ]
                             ]
    P_Guard_data          -> [[ lBrack, P_Guard_list, rBrack                   ]
                             ]
    P_Guard_list          -> [[ P_Guard_param, (*:)[semicolon,P_Guard_param]     ]          -- <<=== jk: reformulated in E-BNF
                             ]
    P_Guard_param         -> [[ P_Guard_cond_list, colon, P_Guard_stmnt_list     ]
                             ]
    P_Guard_cond_list     -> [[ P_Bln_Expr, (*:)[comma, P_Bln_Expr]              ]          -- <<=== jk: reformulated in E-BNF
                             ]
    P_Guard_stmnt_list    -> [[ P_Guard_stmnt, (*:)[comma,P_Guard_stmnt]         ]          -- <<=== jk: reformulated in E-BNF
                             ]
    P_Guard_stmnt         -> [[ idf                                          ]
                             ]

    -- *****************************************************************
    -- Catching incompleteness *****************************************
    -- *****************************************************************
    _                     -> traceShow ("polcaGrammar: rule missing for non-terminal: " ++ show nt)
                             [[]]       -- for debug purposes: when there is no rule for a non-terminal


{- ===========================================================================================================================
-- ===========================================================================================================================
-- Leave these out for now

    -- *****************************************************************
    -- Algebraic structures ********************************************
    -- *****************************************************************
    Math_ann              -> [[ Ring_ann                                     ]
                             -- ,[ ...                                       ]
                             ]
    Ring_ann              -> [[ ring, lBrack, Add_op,      comma                        -- <<=== Add_op etc: TERMinals
                                            , Add_neutral, comma                        --              Should be CONCRETE operations.
                                            , Add_inverse, comma
                                            , Mult_op,     comma
                                            , Mult_neutral
                                    , rBrack
                              , lSqBrack, Data_type, rSqBrack                ]        -- <<=== Rule missing for Data_type
                             ]

    -- *****************************************************************
    -- Types ***********************************************************
    -- *****************************************************************
    Varinfo_ann           -> [[ varinfo, Type_size, Total_size               ]        -- for annotating declarations.
                             ,[ varinfo, Type_size                           ]        --        jk: yeah, to do everything double.
                             ]                                                        --        jk: forget about these for now
    Type_size             -> [[ Var                                        ]
                             ,[ C_expr                                       ]
                             ]
    Total_size            -> [[ Var                                        ]
                             ,[ C_expr                                       ]
                             ]

    -- *****************************************************************
    -- Memory operations ***********************************************
    -- *****************************************************************
    Mem_ann               -> [[ alloc,   Mem_parameter, Mem_parameter, Mem_parameter                     ]  -- summarizes ALL mem-rules
                             ,[ calloc,  Mem_parameter, Mem_parameter, Mem_parameter                     ]
                             ,[ realloc, Mem_parameter, Mem_parameter, Mem_parameter, Mem_parameter      ]
                             ,[ copy,    Mem_parameter, Mem_parameter, Mem_parameter, Mem_parameter      ]
                             -- ,[ Mem_free                                  ]         -- <<=== Rule missing
                             ]
    Mem_parameter         -> [[ C_location                                   ]
                             ,[ idf                                 ]
                             ,[ nmbr                                         ]
                             ]

    -- *****************************************************************
    -- platforms *******************************************************
    -- *****************************************************************
    Kernel_ann            -> [[ kernel, Platform_list        ]
                             ]
    Platform_list         -> [[ (+:)[Platform]                    ]     -- <<=== jk: reformulated in E-BNF
                             ]
    Platform              -> [ -- [ ""                             capture with Opt; what does "" stand for?
                              [ maxj                        ]
                             ,[ opencl                      ]
                             ,[ avx                         ]
                             ,[ x86_64                      ]
                             ,[ arm_v8                      ]
                           ]
    -- =================================================================
    Adapt_ann             -> [[ adapt, Adapt_platform_list   ]          -- <<=== jk: difference with "normal" Platform?
                             ]
    Adapt_platform_list   -> [[ (+:)[Adapt_platform]              ]     -- <<=== jk: reformulated in E-BNF
                             ]
    Adapt_platform        -> [[ maxj                        ]
                             ,[ opencl                      ]
                             ,[ mpi                         ]
                             ,[ openmp                      ]
                             ]

    -- *****************************************************************
    -- HardwareIO ******************************************************
    -- *****************************************************************
    HardwareIO_ann        -> [[ io                                           ]  -- <<=== jk: This one doesn't do much
                             ]

    -- *****************************************************************
    -- Polca_var_id ****************************************************
    -- *****************************************************************
    Polca_var_id          -> [[ idf                         ]           -- <<=== jk: only idf for Polca_var_id too limited
                             ]                                          --           Because: also used as func in HOFs
                                                                        --           -- jk later: for annotations that's ok,
                                                                        --           --    lambda abstractions can be derived.

    -- *****************************************************************
    -- various function stuff -- unused ********************************
    -- *****************************************************************
    Func_parameter        -> [[ C_location                          ]
                             ,[ idf                                 ]
                             ,[ nmbr                                ]
                             ]

    Func_in_data          -> [[ Func_parameter                      ]
                             ,[ lBrack, Func_ann, rBrack            ]
                             ,[ lBrack, Func_helper, rBrack         ]
                             ]

    Func_helper           -> [[ splitEveryFnc, Func_parameter, Func_in_data, Func_parameter   ]
                             ,[ zipFnc,        lBrack, (+:)[C_location], rBrack               ]
                             ,[ transposeFnc,  lBrack, C_location, rBrack                     ]
                             ]
    C_location            -> [[ Var                                        ]
                             ,[ refOp, Var                                 ]          -- "*", Var. NOTE: also *a[i]
                             ,[ derefOp, Var                               ]          -- "&", Var; "&" not recognized by ghci?
                             ]

    -- *****************************************************************
    -- various unused **************************************************
    -- *****************************************************************
    Smath                 -> [[ smath, lBrack, Expr, rBrack             ]
                             ,[ smath, lBrack, Expr, rBrack, Var ]
                             ]
    Expr_list             -> [[ (+:)[Expr]                              ]       -- <<=== jk: reformulated in E-BNF
                             ]                                                       -- <<=== jk: Nowhere used?
-}
