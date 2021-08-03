# init.R
#
# R code to install packages if not already installed
#

my_packages = c('shiny', 'plyr', 'dplyr', 'forcats', 'leaflet', 'ukpolice', 'opencage', 'highcharter', 'e1071', 'zooplaR', 'Metrics', 'stringr', 'scales', 'lattice', 'shinydashboard', 'shinythemes', 'shinyWidgets' )

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
