#ifndef __P_2_RCFUN_R_GLOBALENV7
#define __P_2_RCFUN_R_GLOBALENV7
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <nimble/NimArr.h>
#include <Rinternals.h>
#include <nimble/accessorClasses.h>
#include <nimble/nimDists.h>
#include <nimble/nimOptim.h>
#include <nimble/nimIntegrate.h>
#include <nimble/nimbleCppAD.h>
#include <nimble/nimDerivs_dists.h>

NimArr<2, double>  rcFun_R_GlobalEnv7 ( NimArr<2, double> & ARG1_X_, NimArr<1, double> & ARG2_y_ );

extern "C" SEXP  CALL_rcFun_R_GlobalEnv7 ( SEXP S_ARG1_X_, SEXP S_ARG2_y_ );
#endif
