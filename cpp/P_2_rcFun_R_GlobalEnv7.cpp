#ifndef __P_2_RCFUN_R_GLOBALENV7_CPP
#define __P_2_RCFUN_R_GLOBALENV7_CPP
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <math.h>
#include <Rmath.h>
#include <nimble/EigenTypedefs.h>
#include <nimble/Utils.h>
#include <nimble/accessorClasses.h>
#include <iostream>
#include <nimble/RcppUtils.h>
#include "P_2_rcFun_R_GlobalEnv7.h"

NimArr<2, double>  rcFun_R_GlobalEnv7 ( NimArr<2, double> & ARG1_X_, NimArr<1, double> & ARG2_y_ )  {
NimArr<2, double> ans;
Map<MatrixXd> Eig_ans(0,0,0);
EigenMapStrd Eig_ARG1_X_Interm_5(0,0,0, EigStrDyn(0, 0));
EigenMapStrd Eig_ARG1_X_Interm_6(0,0,0, EigStrDyn(0, 0));
EigenMapStrd Eig_ARG1_X_Interm_7(0,0,0, EigStrDyn(0, 0));
EigenMapStrd Eig_ARG2_y_Interm_8(0,0,0, EigStrDyn(0, 0));
if(ARG1_X_.dim()[0] != ARG2_y_.dim()[0]) {
 nimStop("Run-time size error: expected dim(ARG1_X_)[1] == dim(ARG2_y_)[1]");
}
ans.setSize(ARG1_X_.dim()[1], 1, 0, 0);
new (&Eig_ans) Map< MatrixXd >(ans.getPtr(),ARG1_X_.dim()[1],1);
new (&Eig_ARG1_X_Interm_5) EigenMapStrd(ARG1_X_.getPtr() + static_cast<int>(ARG1_X_.getOffset() + static_cast<int>(0)),ARG1_X_.dim()[0],ARG1_X_.dim()[1],EigStrDyn(ARG1_X_.strides()[1], ARG1_X_.strides()[0]));
new (&Eig_ARG1_X_Interm_6) EigenMapStrd(ARG1_X_.getPtr() + static_cast<int>(ARG1_X_.getOffset() + static_cast<int>(0)),ARG1_X_.dim()[0],ARG1_X_.dim()[1],EigStrDyn(ARG1_X_.strides()[1], ARG1_X_.strides()[0]));
new (&Eig_ARG1_X_Interm_7) EigenMapStrd(ARG1_X_.getPtr() + static_cast<int>(ARG1_X_.getOffset() + static_cast<int>(0)),ARG1_X_.dim()[0],ARG1_X_.dim()[1],EigStrDyn(ARG1_X_.strides()[1], ARG1_X_.strides()[0]));
new (&Eig_ARG2_y_Interm_8) EigenMapStrd(ARG2_y_.getPtr() + static_cast<int>(ARG2_y_.getOffset() + static_cast<int>(0)),ARG2_y_.dim()[0],1,EigStrDyn(0, ARG2_y_.strides()[0]));
Eig_ans = ((Eig_ARG1_X_Interm_5).transpose() * Eig_ARG1_X_Interm_6).inverse() * ((Eig_ARG1_X_Interm_7).transpose() * (Eig_ARG2_y_Interm_8));
return(ans);
}

SEXP  CALL_rcFun_R_GlobalEnv7 ( SEXP S_ARG1_X_, SEXP S_ARG2_y_ )  {
NimArr<2, double> ARG1_X_;
NimArr<1, double> ARG2_y_;
SEXP S_returnValue_1234;
NimArr<2, double> LHSvar_1234;
SEXP S_returnValue_LIST_1234;
SEXP_2_NimArr<2>(S_ARG1_X_, ARG1_X_);
SEXP_2_NimArr<1>(S_ARG2_y_, ARG2_y_);
GetRNGstate();
LHSvar_1234 = rcFun_R_GlobalEnv7(ARG1_X_, ARG2_y_);
PutRNGstate();
PROTECT(S_returnValue_LIST_1234 = Rf_allocVector(VECSXP, 3));
PROTECT(S_returnValue_1234 = NimArr_2_SEXP<2>(LHSvar_1234));
PROTECT(S_ARG1_X_ = NimArr_2_SEXP<2>(ARG1_X_));
PROTECT(S_ARG2_y_ = NimArr_2_SEXP<1>(ARG2_y_));
SET_VECTOR_ELT(S_returnValue_LIST_1234, 0, S_ARG1_X_);
SET_VECTOR_ELT(S_returnValue_LIST_1234, 1, S_ARG2_y_);
SET_VECTOR_ELT(S_returnValue_LIST_1234, 2, S_returnValue_1234);
UNPROTECT(4);
return(S_returnValue_LIST_1234);
}
#endif
