import Parser
import LexM
import Pretty
import PpIDLSyn

import Control.Monad
import System.IO
import System.Environment
import System.FilePath

main = do
    [f,g]          <- getArgs
    [src_f, src_g] <-
      forM [f,g] $ \a -> do
        src    <- readFile a
        Left x <- runLexM [] f src parseIDL
        return x

    if (src_f == src_g)
       then print "The input IDL is structurally identical"
       else do putStrLn $ showIDL (ppIDL f src_f)
               putStrLn $ showIDL (ppIDL g src_g)

{-

$ runhaskell Parse.hs math.idl math.idl

True
module Math {
[pure]
long abs([in] long x);;
[pure]
long labs([in] long x);;
[pure]
double fabs([in] double x);;
[pure]
double ceil([in] double x);;
[pure]
double floor([in] double x);;
[pure]
double fmod([in] double x,
            [in] double y);;
[pure]
double exp([in] double x);;
[pure]
double log([in] double x);;
[pure]
double log10([in] double x);;
[pure]
double frexp([in] double x,
             [in, out, ref] long *nptr);;
[pure]
double ldexp([in] double x,
             [in] long n);;
[pure]
double modf([in] double x,
            [in, out, ref] double *nptr);;
[pure]
double pow([in] double x,
           [in] double y);;
[pure]
double sqrt([in] double x);;
[pure]
long rand([in] void );;
void srand([in] long seed);;
[pure]
double cos([in] double x);;
[pure]
double sin([in] double x);;
[pure]
double tan([in] double x);;
[pure]
double acos([in] double x);;
[pure]
double asin([in] double x);;
[pure]
double atan([in] double x);;
[pure]
double atan2([in] double x,
             [in] double y);;
[pure]
double cosh([in] double x);;
[pure]
double sinh([in] double x);;
[pure]
double tanh([in] double x);;
};

        -}
