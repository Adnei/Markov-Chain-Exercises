source("static_const_values.R") # .sc prefix
source("common.R") # .cmm prefix

single_module.availability <- cmm.isolated_module_availability.fn(sc.mttf, sc.mttr_c)
dt <- cmm.dt_compute.fn(sc.dt.fraction, sc.mttr_c)
u_c <- 1/sc.mttr_c
u_p <- 1/sc.mttr_p

plot_title <- "Ex.12 Availability & Reliability"
cvg_idx <- 1
states.arr <- c("Op", "Covered", "Not covered", "Failure")
plot_file <- 'plots/ex12_simulation.pdf'

transition_availability.list = vector('list', length(sc.coverage))
transition_reliability.list = vector('list', length(sc.coverage))
for(coverage in sc.coverage){

# @TODO
# --> Matrix engine
#   --> It builds the matrix regarding some state transition rules
#   --> Kinda hard to implement... Should consider something less time consuming

# "HARD CODED SHIT" ALERT!!! Still no dynamic way to build it... feels bad bro

  mtx_transition_availability <- matrix(c( 1 - 2 * sc.lambda * dt, # Op -> Op   1 1
    2 * coverage * sc.lambda * dt,               # Op -> Covered   1 2
    sc.lambda * (1 - coverage) * dt,             # Op -> NC        1 3
    sc.lambda * (1 - coverage) * dt,             # Op - Failure    1 4

    u_c * dt,                      # Covered -> Op                 2 1
    1 - sc.lambda * dt - u_c * dt, # Covered -> Covered            2 2
    0,                             # Covered -> NC                 2 3
    sc.lambda * dt,                # Covered -> Failure            2 4

    u_p * dt,                      # NC -> Op                      3 1
    0,                             # NC -> Covered                 3 2
    1 - sc.lambda * dt - u_p * dt, # NC -> NC                      3 3
    sc.lambda * dt,                # NC -> Failure                 3 4

    u_c * dt,                      # Failure -> Op                 4 1
    0,                             # Failure -> Covered            4 2
    0,                             # Failure -> NC                 4 3
    1 - u_c * dt),                 # Failure -> Failure            4 4
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
