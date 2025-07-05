#include <R.h>
#include <Rinternals.h>
#include <math.h>

// Helper: check if value is in array
int in_int_array(int value, int *arr, int n) {
    for (int i = 0; i < n; i++)
        if (arr[i] == value) return 1;
    return 0;
}

// Helper: compute mean and variance
void mean_var(double *x, int n, double *mean, double *var) {
    if (n == 0) { *mean = NA_REAL; *var = NA_REAL; return; }
    double m = 0.0, M2 = 0.0;
    for (int k = 0; k < n; ++k) {
        double delta = x[k] - m;
        m += delta / (k + 1);
        M2 += delta * (x[k] - m);
    }
    *mean = m;
    double variance = (n > 1) ? (M2 / (n - 1)) : 0.0;
    *var = variance;
}

SEXP c_mu_sigma2_per_node_per_tree(SEXP obs_node_table, SEXP y) {
    // obs_node_table: integer matrix n_obs x n_trees, column-major
    // y: numeric vector, length n_obs
    if (!isInteger(obs_node_table) || !isReal(y))
        error("obs_node_table must be integer matrix, y must be numeric vector");
    SEXP dim = getAttrib(obs_node_table, R_DimSymbol);
    if (isNull(dim) || LENGTH(dim) != 2)
        error("obs_node_table must be a matrix");
    int n_obs = INTEGER(dim)[0], n_trees = INTEGER(dim)[1];

    int *table = INTEGER(obs_node_table);
    double *yy = REAL(y);

    SEXP result = PROTECT(allocVector(VECSXP, n_trees)); // list per tree

    for (int t = 0; t < n_trees; t++) {
        // 1. Find unique node ids for this tree
        int *nodes = (int *) R_alloc(n_obs, sizeof(int));
        int n_unique = 0;
        for (int i = 0; i < n_obs; i++) {
            int node = table[t * n_obs + i];
            if (!in_int_array(node, nodes, n_unique)) {
                nodes[n_unique++] = node;
            }
        }

        // 2. Prepare output for this tree: named list of c(mu, sigma2) for each node
        SEXP node_list = PROTECT(allocVector(VECSXP, n_unique));
        SEXP node_names = PROTECT(allocVector(STRSXP, n_unique));
        for (int k = 0; k < n_unique; k++) {
            int node = nodes[k];
            // Collect y values at this node
            double *y_tmp = (double *) R_alloc(n_obs, sizeof(double));
            int n_y_tmp = 0;
            for (int i = 0; i < n_obs; i++) {
                if (table[t * n_obs + i] == node) {
                    y_tmp[n_y_tmp++] = yy[i];
                }
            }
            double mu, sigma2;
            mean_var(y_tmp, n_y_tmp, &mu, &sigma2);

            SEXP mu_sigma2 = PROTECT(allocVector(REALSXP, 2));
            REAL(mu_sigma2)[0] = mu;
            REAL(mu_sigma2)[1] = sigma2;
            SET_VECTOR_ELT(node_list, k, mu_sigma2);
            UNPROTECT(1); // mu_sigma2

            // Name is the number of the node -- assume that we don't get more
            // than 99,999,999 nodes.
            char buf[8];
            snprintf(buf, 8, "%d", node);
            SET_STRING_ELT(node_names, k, mkChar(buf));
        }
        setAttrib(node_list, R_NamesSymbol, node_names);

        SET_VECTOR_ELT(result, t, node_list);
        UNPROTECT(2); // node_list, node_names
    }

    UNPROTECT(1); // result
    return result;
}

SEXP c_simple_var(SEXP predictions, SEXP mu_list) {
    if (!isInteger(predictions) || !isMatrix(predictions)) {
        error("predictions must be an integer matrix");
    }
    if (!isNewList(mu_list)) {
        error("mu_list must be a list");
    }

    SEXP dims = getAttrib(predictions, R_DimSymbol);
    int n_obs = INTEGER(dims)[0];
    int n_trees = INTEGER(dims)[1];
    int *pred = INTEGER(predictions);

    SEXP response = PROTECT(allocVector(REALSXP, n_obs));
    SEXP se = PROTECT(allocVector(REALSXP, n_obs));
    double *resp = REAL(response);
    double *serr = REAL(se);

    for (int i = 0; i < n_obs; ++i) {
        double mean = 0.0, M2 = 0.0;
        int count = 0;

        for (int j = 0; j < n_trees; ++j) {
            int node = pred[i + j * n_obs]; // column-major
            SEXP tree_list = VECTOR_ELT(mu_list, j);
            SEXP names = getAttrib(tree_list, R_NamesSymbol);

            SEXP mu_sigma2 = R_NilValue;
            char nodename[32];
            snprintf(nodename, 32, "%d", node);

            int n_nodes = length(tree_list);
            for (int k = 0; k < n_nodes; ++k) {
                if (strcmp(CHAR(STRING_ELT(names, k)), nodename) == 0) {
                    mu_sigma2 = VECTOR_ELT(tree_list, k);
                    break;
                }
            }
            if (mu_sigma2 == R_NilValue || length(mu_sigma2) < 1) {
                error("Node %d not found in mu_list for tree %d", node, j + 1);
            }
            double mu = REAL(mu_sigma2)[0];
            count++;
            double delta = mu - mean;
            mean += delta / count;
            M2 += delta * (mu - mean);
        }
        resp[i] = mean;
        double var = (count > 1) ? (M2 / (count - 1)) : 0.0;
        serr[i] = sqrt(var > 0.0 ? var : 0.0);
    }

    SEXP res = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(res, 0, response);
    SET_VECTOR_ELT(res, 1, se);

    SEXP names = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(names, 0, mkChar("response"));
    SET_STRING_ELT(names, 1, mkChar("se"));
    setAttrib(res, R_NamesSymbol, names);

    UNPROTECT(4);
    return res;
}

SEXP c_tvl_var(SEXP predictions, SEXP mu_list) {
    // Tree-based variance computation with overflow protection.
    // Uses Kahan summation for numerical stability when accumulating
    // large values of (mu^2 + sigma2) across many trees, preventing
    // potential overflow that could occur with simple accumulation.
    if (!isInteger(predictions) || !isMatrix(predictions)) {
        error("predictions must be an integer matrix");
    }
    if (!isNewList(mu_list)) {
        error("mu_list must be a list");
    }

    SEXP dims = getAttrib(predictions, R_DimSymbol);
    int n_obs = INTEGER(dims)[0];
    int n_trees = INTEGER(dims)[1];
    int *pred = INTEGER(predictions);

    SEXP response = PROTECT(allocVector(REALSXP, n_obs));
    SEXP se = PROTECT(allocVector(REALSXP, n_obs));
    double *resp = REAL(response);
    double *serr = REAL(se);

    double eps = DBL_EPSILON; // .Machine$double.eps in C

    for (int i = 0; i < n_obs; ++i) {
        double mean_mu = 0.0, mean_musq_plus_sigma2 = 0.0;
        // Welford's algorithm for mean_mu and mean(musq+sigma2):
        double welford_mean = 0.0;
        // Use Kahan summation for numerical stability and overflow prevention
        double sum_musq_plus_sigma2 = 0.0;
        double kahan_c = 0.0;  // Kahan summation compensation term

        for (int j = 0; j < n_trees; ++j) {
            int node = pred[i + j * n_obs]; // column-major
            SEXP tree_list = VECTOR_ELT(mu_list, j);
            SEXP names = getAttrib(tree_list, R_NamesSymbol);

            SEXP mu_sigma2 = R_NilValue;
            char nodename[32];
            snprintf(nodename, 32, "%d", node);

            int n_nodes = length(tree_list);
            for (int k = 0; k < n_nodes; ++k) {
                if (strcmp(CHAR(STRING_ELT(names, k)), nodename) == 0) {
                    mu_sigma2 = VECTOR_ELT(tree_list, k);
                    break;
                }
            }
            if (mu_sigma2 == R_NilValue || length(mu_sigma2) < 2) {
                error("Node %d not found or is malformed in mu_list for tree %d", node, j + 1);
            }
            double mu = REAL(mu_sigma2)[0];
            double sigma2 = REAL(mu_sigma2)[1];

            // Welford's algorithm for mean of mu
            double delta = mu - welford_mean;
            welford_mean += delta / (j + 1);

            // Kahan summation for mean(mu^2 + sigma2) to prevent overflow
            double y = mu * mu + sigma2 - kahan_c;
            double t = sum_musq_plus_sigma2 + y;
            kahan_c = (t - sum_musq_plus_sigma2) - y;
            sum_musq_plus_sigma2 = t;
        }
        mean_mu = welford_mean;
        mean_musq_plus_sigma2 = sum_musq_plus_sigma2 / n_trees;
        resp[i] = mean_mu;
        double se2 = mean_musq_plus_sigma2 - mean_mu * mean_mu;
        serr[i] = (se2 > 0.0) ? sqrt(se2) : 0.0;
        if (ISNAN(serr[i]) || serr[i] < eps) {
            serr[i] = 1e-8;
        }
    }

    SEXP res = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(res, 0, response);
    SET_VECTOR_ELT(res, 1, se);

    SEXP names = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(names, 0, mkChar("response"));
    SET_STRING_ELT(names, 1, mkChar("se"));
    setAttrib(res, R_NamesSymbol, names);

    UNPROTECT(4);
    return res;
}
