library(dplyr)
if (!require("markovchain")) install.packages("markovchain")
if (!require("diagram")) install.packages("diagram")
library(markovchain, diagram)

# source("static_mtx_ex12.R")
# source("static_mtx_ex13.R")
# source("static_mtx_ex14.R")
source("static_mtx_ex15.R")

SCALE <- 1000

pdf(plot_file)

plot(c(0:1), c(0:1), ylim=c(0,1), xlim = c(0,100), type = "n",
    main = plot_title,
    xlab = "Time (h) 1:1000 scale", ylab = "Probability")


color_test <- c("red", "blue", "green", "orange", "black")

transitions.list <- list(transition_availability.list, transition_reliability.list)

for(idx in c(1:2)){
  current_transition_mtx.list <- transitions.list[[idx]]
  cvg_idx <- 1
  for(coverage in sc.coverage){
    N <- 0
    prob.arr <- c()
    mk_chain <- new("markovchain", states = states.arr, byrow = T,
       transitionMatrix = current_transition_mtx.list[[cvg_idx]], name = "System Probability")

    steady_op <- steadyStates(mk_chain)[1]

    while(TRUE){
      nth_op <- (mk_chain^N)[1,1]
      prob.arr <- c(prob.arr, nth_op)
      if (idx == 1){
        if((nth_op - steady_op) < 10*exp(-25)) break
      }else{
        if((nth_op - steady_op) < 10*exp(-8)) break
      }

      N <- N + 1
    }

    # Tricky way to get the line till the infinity
    x_axis <- c(0:N/SCALE, 9999)
    y_axis <- c(prob.arr, prob.arr[length(prob.arr)])

    lines(x_axis, y_axis, ylim=c(0,1), type = "l", col = color_test[cvg_idx])

    cvg_idx <- cvg_idx + 1
  }
}

dev.off()
