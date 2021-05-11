source("static_const_values.R") # .sc prefix
source("common.R") # .cmm prefix

single_module.availability <- cmm.isolated_module_availability.fn(sc.mttf, sc.mttr_c)
dt <- cmm.dt_compute.fn(sc.dt.fraction, sc.mttr_c)
u_c <- 1/sc.mttr_c
u_p <- 1/sc.mttr_p

plot_title <- "Ex.13 Availability & Reliability"
cvg_idx <- 1
states.arr <- c("2 OK", "1 OK", "Failure")
plot_file <- 'plots/ex13_simulation.pdf'

transition_availability.list = vector('list', length(sc.coverage))
transition_reliability.list = vector('list', length(sc.coverage))
for(coverage in sc.coverage){

# @TODO
# --> Matrix engine
#   --> It builds the matrix regarding some state transition rules
#   --> Kinda hard to implement... Should consider something less time consuming

# "HARD CODED SHIT" ALERT!!! Still no dynamic way to build it... feels bad bro

  mtx_transition_availability <- matrix(c(
    1 - 2 * sc.lambda * dt,                # 2 OK -> 2 OK      1 1
    2 * sc.lambda * coverage * dt,         # 2 OK -> 1 OK      1 2
    2 * sc.lambda * (1 - coverage) * dt,   # 2 OK -> Failure   1 3

    u_c * dt,                              # 1 OK -> 2 OK      2 1
    1 - sc.lambda * dt - u_c * dt,         # 1 OK -> 1 OK      2 2
    sc.lambda * dt,                        # 1 OK -> Failure   2 3

    u_c * dt,                              # Failure -> 2 OK   3 1
    0,                                     # Failure -> 1 OK   3 2
    1 - u_c * dt                           # Failure -> Fail   3 3
    ),
    nrow = 3,
    byrow = T,
    dimnames = list(states.arr, states.arr))
    reliability_mtx <- mtx_transition_availability

    transition_availability.list[[cvg_idx]] <- mtx_transition_availability

    reliability_mtx[3,1] <- 0   # Reliability model doesn't accept you to go back from Failure state to Operational state
    reliability_mtx[3,3] <- 1
    transition_reliability.list[[cvg_idx]] <-reliability_mtx
    cvg_idx <- cvg_idx + 1
}
