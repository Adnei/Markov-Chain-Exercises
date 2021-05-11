source("static_const_values.R") # .sc prefix
source("common.R") # .cmm prefix

single_module.availability <- cmm.isolated_module_availability.fn(sc.mttf, sc.mttr_c)
dt <- cmm.dt_compute.fn(sc.dt.fraction, sc.mttr_c)
u_c <- 1/sc.mttr_c
u_p <- 1/sc.mttr_p

plot_title <- "Ex.15 Availability & Reliability"
cvg_idx <- 1
states.arr <- c("Op1", "Op2", "OpC", "OpNC", "Failure")
plot_file <- 'plots/ex15_simulation.pdf'

transition_availability.list = vector('list', length(sc.coverage))
transition_reliability.list = vector('list', length(sc.coverage))
for(coverage in sc.coverage){

# @TODO
# --> Matrix engine
#   --> It builds the matrix regarding some state transition rules
#   --> Kinda hard to implement... Should consider something less time consuming

# "HARD CODED SHIT" ALERT!!! Still no dynamic way to build it... feels bad bro

  mtx_transition_availability <- matrix(c(
    1 - 3 * sc.lambda * dt,                 # Op1 -> Op1   1 1
    2 * sc.lambda * dt,                     # Op1 -> Op2   1 2
    sc.lambda * coverage * dt,              # Op1 -> OpC   1 3
    sc.lambda * (1 - coverage) * dt,        # Op1 -> OpNC  1 4
    0,                                      # Op1 -> Fail  1 5 # Never Fails directly

    u_c * dt,                               # Op2 -> Op1   2 1
    1 - sc.lambda * dt - u_c * dt,          # Op2 -> Op2   2 2
    0,                                      # Op2 -> OpC   2 3
    0,                                      # Op2 -> OpNC  2 4
    sc.lambda * dt,                         # Op2 -> Fail  2 5

    u_c * dt,                               # OpC -> Op1   3 1
    0,                                      # OpC -> Op2   3 2
    1 - 2 * sc.lambda * dt - u_c * dt,      # OpC -> OpC   3 3
    0,                                      # OpC -> OpNC  3 4
    2 * sc.lambda * dt,                     # OpC -> Fail  3 5

    u_p * dt,                               # OpNC -> Op1  4 1
    0,                                      # OpNC -> Op2  4 2
    0,                                      # OpNC -> OpC  4 3
    1 - 2 * sc.lambda * dt - u_p * dt,      # OpNC -> OpNC 4 4
    2 * sc.lambda * dt,                     # OpNC -> Fail 4 5

    u_c * dt,                               # Fail -> Op1  5 1
    0,                                      # Fail -> Op2  5 2
    0,                                      # Fail -> OpC  5 3
    0,                                      # Fail -> OpNC 5 4
    1 - u_c * dt                            # Fail -> Fail 5 5
    ),
    nrow = 5,
    byrow = T,
    dimnames = list(states.arr, states.arr))
    reliability_mtx <- mtx_transition_availability

    transition_availability.list[[cvg_idx]] <- mtx_transition_availability

    reliability_mtx[5,1] <- 0   # Reliability model doesn't accept you to go back from Failure state to Operational state
    reliability_mtx[5,5] <- 1
    transition_reliability.list[[cvg_idx]] <-reliability_mtx
    cvg_idx <- cvg_idx + 1
}
