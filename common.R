################################################################################
#                                cmm prefix                                    #
################################################################################

cmm.dt_compute.fn <- function(proportion, mttr){
  proportion * mttr
}

cmm.isolated_module_availability.fn <- function(mttf, mttr){
  mttf/(mttf + mttr)
}
