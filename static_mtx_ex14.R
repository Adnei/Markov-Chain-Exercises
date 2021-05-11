source("static_const_values.R") # .sc prefix
source("common.R") # .cmm prefix

single_module.availability <- cmm.isolated_module_availability.fn(sc.mttf, sc.mttr_c)
dt <- cmm.dt_compute.fn(sc.dt.fraction, sc.mttr_c)
u_c <- 1/sc.mttr_c
u_p <- 1/sc.mttr_p

plot_title <- "Ex.14 Availability & Reliability"
cvg_idx <- 1
states.arr <- c("M1M2", "M2M3", "M1M3", "Failure")
plot_file <- 'plots/ex14_simulation.pdf'



transition_availability.list = vector('list', length(sc.coverage))
transition_reliability.list = vector('list', length(sc.coverage))
for(coverage in sc.coverage){
# This question has two coverage values. We calculate C2 based on C1. C2 = C1 - 0.05
C2 <- coverage - 0.05
# @TODO
# --> Matrix engine
#   --> It builds the matrix regarding some state transition rules
#   --> Kinda hard to implement... Should consider something less time consuming

# "HARD CODED SHIT" ALERT!!! Still no dynamic way to build it... feels bad bro

  mtx_transition_availability <- matrix(c(
    1 - 2 * sc.lambda * dt,                 # m1m2 -> m1m2  1 1
    sc.lambda * coverage * dt,              # m1m2 -> m2m3  1 2
    sc.lambda * C2 * dt,                    # m1m2 -> m1m3  1 3
    sc.lambda * (1-coverage) * dt +
      sc.lambda * (1-C2) * dt,              # m1m2 -> Fail  1 4

    u_c * dt,                               # m2m3 -> m1m2  2 1
    1 - 2 * sc.lambda * dt - u_c * dt,      # m2m3 -> m2m3  2 2
    0,                                      # m2m3 -> m2m3  2 3
    2 * sc.lambda * dt,                     # m2m3 -> Fail  2 4

    u_c * dt,                               # m1m3 -> m1m2  3 1
    0,                                      # m1m3 -> m2m3  3 2
    1 - 2 * sc.lambda * dt - u_c * dt,      # m1m3 -> m1m3  3 3
    2 * sc.lambda * dt,                     # m1m3 -> Fail  3 4

    u_c * dt,                               # Fail -> m1m2  4 1
    0,                                      # Fail -> m2m3  4 2
    0,                                      # Fail -> m1m3  4 3
    1 - u_c * dt                            # Fail -> Fail  4 4
    ),
    nrow = 4,
    byrow = T,
    dimnames = list(states.arr, states.arr))
    reliability_mtx <- mtx_transition_availability

    transition_availability.list[[cvg_idx]] <- mtx_transition_availability

    reliability_mtx[4,1] <- 0   # Reliability model doesn't accept you to go back from Failure state to Operational state
    reliability_mtx[4,4] <- 1
    transition_reliability.list[[cvg_idx]] <-reliability_mtx
    cvg_idx <- cvg_idx + 1
}
