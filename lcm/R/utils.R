g <- glue::glue

G <- function(var, g, level) {
    as.numeric(var == level) / g #b(g)
}

K_p <- function(G, l, u) {
    if (l > u) {
        return(rep(1, nrow(G)))
    }

    out <- apply(as.matrix(G[, g("lcm_Gp_A{l:u}")]), 1, prod)
    out
}

K_s <- function(G, l, u) {
    if (l > u) {
        return(rep(1, nrow(G)))
    }

    out <- apply(as.matrix(G[, g("lcm_Gs_A{l:u}")]), 1, prod)
    out
}

H <- function(G, l, u) {
    if (l > u) {
        return(rep(1, nrow(G)))
    }

    out <- apply(as.matrix(G[, g("lcm_G_M{l:u}")]), 1, prod)
    out
}

Sum <- function(x) Reduce(`+`, x)

b <- function(x) {
    pmax(x, .01, na.rm = TRUE)
}
