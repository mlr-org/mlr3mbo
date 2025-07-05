#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP c_eps_indicator(SEXP, SEXP);
extern SEXP c_sms_indicator(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP c_mu_sigma2_per_node_per_tree(SEXP, SEXP);
extern SEXP c_simple_var(SEXP, SEXP);
extern SEXP c_ltv_var(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"c_eps_indicator", (DL_FUNC) &c_eps_indicator, 2},
    {"c_sms_indicator", (DL_FUNC) &c_sms_indicator, 5},
    {"c_mu_sigma2_per_node_per_tree", (DL_FUNC) &c_mu_sigma2_per_node_per_tree, 2},
    {"c_simple_var", (DL_FUNC) &c_simple_var, 2},
    {"c_ltv_var", (DL_FUNC) &c_ltv_var, 2},
    {NULL, NULL, 0}
};

void R_init_mlr3mbo(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
