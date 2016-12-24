# Polca_Haskell

This repository contains an initial version of the Haskell code that performs actions concerning transformations of code. These actions are grouped in several subdirectories, described below. In order to execute the code in these subdirectories, the following installations are necessary:

- install ghci (see haskell.org/downloads)
- install a package for ghci: cabal install FPPrac.Trees
- install clash (see clash-lang.org)

Description of the content of the subdirectories:

- AnnC2HS: transformation from Annotated C code to equivalent and executable Haskell code.
In the file Main.hs examples of annotated C code are given (mostly variants of the N-Body problem), and some functions to analyse these examples. Execute as follows:
Run: ghci Main
and then evaluate, e.g.,
str2PTree 24
hs_code 24

- nBody: the result for the nBody problem of the above transformation.
The files NBody_hs_i.hs contain the generated and executable Haskell code from the corresponding annotated C code in Main.hs in the subdirectory AnnC2HS.
For example, evaluating
xyCrds 100
gives the first 100 iterations of the N-Body problem.

- CLaSH: contains the result of transforming the Haskell code into code that can be translated by CLaSH into VHDL or Verilog. The resulting VHDL code is given in the subdirectory vhdl.

- H2H2C (still *under construction*): this subdirectory contains a preliminary attempt to automate transformations of higher order Haskell functions, and to generate C code by exploiting the repetitive nature of higher order functions in order to get better performance than realized by standard Haskell implementations. This is not part of Polca, but is added for completeness sake concerning the view of programming independently of a platform.
