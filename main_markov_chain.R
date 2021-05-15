source('static_mtx_ex15.R')
#source('static_mtx_ex14.R')
#source('static_mtx_ex13.R')
#source('static_mtx_ex12.R')
#source("static_mtx_ex12.R")
# source("static_mtx_ex13.R")
# source("static_mtx_ex14.R")
# source("static_mtx_ex15.R")

if (!require("dplyr")) install.packages("dplyr")
if (!require("markovchain")) install.packages("markovchain")
if (!require("diagram")) install.packages("diagram")
if (!require("Bolstad2")) install.packages("Bolstad2")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("cowplot")) install.packages("cowplot")

SCALE <- 1000
PLOT_BASE_SIZE <- 26
PLOT_DEFAULT_LINE_SIZE <- 2
PLOT_TINY_LINE_SIZE <- 0.6
############################################################################
#data.frame columns
full_N.arr <- c()
full_prob.arr <- c()
full_type.arr <- c()
full_coverage.arr <- c()
# Measure is a speacial column
#   --> If it is an availability problem, then we measure the "asymptotic availability"
#   --> If it is a Reliability problem, then we measure the MTTF
full_measure.arr <- c()
############################################################################

transitions.list <- list(transition_availability.list, transition_reliability.list)

for(idx in c(1:2)){
  current_transition_mtx.list <- transitions.list[[idx]]
  cvg_idx <- 1
  type <- if(idx == 1) "Availability" else "Reliability"
  for(coverage in sc.coverage){
    group_idx <- coverage + idx
    N <- 0
    prob.arr <- c()
    mk_chain <- new("markovchain", states = states.arr, byrow = T,
       transitionMatrix = current_transition_mtx.list[[cvg_idx]], name = "System Probability")
    steady_op <- steadyStates(mk_chain)[1]

    while(TRUE){
      nth_op <- (mk_chain^N)[1,1]
      prob.arr <- c(prob.arr, nth_op)

      if (type == "Availability"){
        if((nth_op - steady_op) < 10*exp(-25)) break
      }else{
        if((nth_op - steady_op) < 10*exp(-8)) break
      }

      N <- N + 1
    }
    type.arr <- rep(type, N+1)
    coverage.arr <- rep(coverage, N+1)
    measure.arr <- rep(min(prob.arr), N+1) #asymptotic availability
    if(type == "Reliability"){
      #Calculating the area under the curve by Simpson's method
      #   --> It is used in orderd to get the MTTF
      simp_auc <- sintegral(c(0:N), prob.arr)
      glimpse(simp_auc) #Just printing some info about the area under the curve calculated by the simpson's method
      measure.arr <- rep(simp_auc$int, N+1) # simp_auc$int --> MTTF
    }
    ############################################################################
    #Data Frame binds
    #I decided to split the data.frame into arrays in order to avoid rbind
    full_N.arr <- c(full_N.arr, c(0:N))
    full_prob.arr <- c(full_prob.arr, prob.arr)
    full_type.arr <- c(full_type.arr, type.arr)
    full_coverage.arr <- c(full_coverage.arr, coverage.arr)
    full_measure.arr <- c(full_measure.arr, measure.arr)
    ############################################################################
    cvg_idx <- cvg_idx + 1
  }
}

mkv_data.df <- data.frame(Time = full_N.arr,
  Probability = full_prob.arr,
  Type = full_type.arr,
  Coverage = full_coverage.arr,
  Measure = full_measure.arr,
  stringsAsFactors = F
)

reliability.df <- mkv_data.df[mkv_data.df$Type == "Reliability", ]
availability.df <- mkv_data.df[mkv_data.df$Type == "Availability", ]
rel_mttf.df <- data.frame(MTTF = unique(reliability.df$Measure),
    Coverage = unique(reliability.df$Coverage))
avai_asymptotic.df <- data.frame(Availability = unique(availability.df$Measure),
    Coverage = unique(availability.df$Coverage))

reliability_plot <- ggplot(reliability.df) +
    geom_line(aes(x = Time/SCALE,
      y = Probability,
      group = Coverage,
      colour = Coverage
    ),
    size = PLOT_DEFAULT_LINE_SIZE
  )  +
  geom_vline(mapping = aes(
      xintercept = MTTF/SCALE,
      group = Coverage,
      linetype = paste(paste("C:", Coverage),paste("MTTF:", round(MTTF/SCALE,2)))
    ),
    data = rel_mttf.df,
    show.legend=TRUE,
    color = "red",
    size = PLOT_TINY_LINE_SIZE
  ) +
  scale_linetype_manual(name="MTTF", values = c(1:5)) +
  xlab(paste("Time (h). Scale 1:",SCALE)) +
  ylab("Reliability") +
  ggtitle(plot_title) +
  theme_minimal() +
  theme_bw(base_size=PLOT_BASE_SIZE) +
  ylim(0,1)

availability_plot <- ggplot(availability.df) +
    geom_line(aes(x = Time/SCALE,
      y = Probability,
      group = Coverage,
      colour = Coverage
    ),
    size = PLOT_DEFAULT_LINE_SIZE
  ) +
  geom_hline(mapping = aes(
      yintercept = Availability,
      group = Coverage,
      linetype = paste(paste("C:", Coverage),paste("A:", round(Availability,2)))
    ),
    data = avai_asymptotic.df,
    show.legend=TRUE,
    color = "red",
    size = PLOT_TINY_LINE_SIZE
  ) +
  scale_linetype_manual(name="Asymptotic Availability", values = c(1:5)) +
  xlab(paste("Time (h). Scale 1:",SCALE)) +
  ylab("Availability") +
  ggtitle(plot_title) +
  theme_minimal() +
  theme_bw(base_size=PLOT_BASE_SIZE) +
  ylim(0,1)

rel_avai.plots <- plot_grid(reliability_plot, availability_plot, labels = "AUTO")
file_device <- paste(plot_file, "png", sep=".")
ggsave(filename=file_device, plot=rel_avai.plots, device="png", width=31.25, height = 21.875, dpi = 400)
