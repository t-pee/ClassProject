# Warranty Claim Scratchpad/sandbox
# TMP 10/27/23

library(tidyverse)

# functions first

################################################################################
ReadWarrantyData <- function(fn) {
  X <- read_csv(fn, col_types = list(
    'MachineAge' = col_double(), # machine age in calendar days
    'hourReading' = col_double(), # hour meter reading at time of claim
    'HrPerDay' = col_double(), # hours machine use per day of age
    'SN' = col_character(), # machine serial number
    'ModelNum' = col_character(), # model number
    'Model' = col_factor(c(
      "A", "B", "C", "D", "E", "F", 
      "G", "H", "I", "J", "K", "L")),
    'CustType' = col_factor(c(
      "1", "2", "3", "4", "5", "6", "7", "8")),
    'InvertedAge' = col_factor((levels = c("x", "")))
    ))
  
  numRowsLoaded <- nrow(X)
  print(paste("Loaded", numRowsLoaded, "rows from", fn))
  return(X)
}

################################################################################
DecimateData <- function(X, fraction) {
  rowsInit <- nrow(X)
  rowsRemain <- round(rowsInit * fraction, -2)
  Xd <- slice_sample(X, n=rowsRemain)
  print(paste("Sliced data down from", rowsInit, "to", rowsRemain, "points."))
  return(Xd)
}

################################################################################
CreateHrVsAgeLinPlot <- function(d, lx, ly) {
  # Create linear scale plot
  
  scPlot <- ggplot(data = d)
  
  scPlot <- scPlot + 
    geom_point(mapping = aes(
    x = MachineAge, 
    y = hourReading, 
    color = CustType,
    stroke = 0.1,
    alpha = 0.5
    )
  )

  # Add line segments to show hours/week usage
  ls168 <- geom_segment(x = 0.1, y = 0.1, aes(yend = ly, xend = ly/24 * 168/168)) # 168 hr/wk
  ls80  <- geom_segment(x = 0.1, y = 0.1, aes(yend = ly, xend = ly/24 * 168/ 80)) # 80 hr/wk
  ls40  <- geom_segment(x = 0.1, y = 0.1, aes(yend = ly, xend = ly/24 * 168/ 40)) # 40 hr/wk
  ls20  <- geom_segment(x = 0.1, y = 0.1, aes(yend = ly, xend = ly/24 * 168/ 20)) # 20 hr/wk
  ls10  <- geom_segment(x = 0.1, y = 0.1, aes(yend = lx / 7*10, xend = lx)) # 10 hr/wk
  ls5   <- geom_segment(x = 0.1, y = 0.1, aes(yend = lx / 7* 5, xend = lx)) # 5 hr/wk
  ls2   <- geom_segment(x = 0.1, y = 0.1, aes(yend = lx / 7* 2, xend = lx)) # 2 hr/wk
  ls1   <- geom_segment(x = 0.1, y = 0.1, aes(yend = lx / 7* 1, xend = lx)) # 1 hr/wk
  
  scPlot <- scPlot + ls168 + ls80 + ls40 + ls20 + ls10 + ls5 + ls2 + ls1
  
  scPlot <- scPlot + labs(
    x = "Machine Hours @ Report (hr)",
    y = "Machine Age Since Installation (days)",
    title = "Machine Hours vs. Age",
    subtitle = "Data from 9/28/16 to 9/27/18"
  )

  scPlot <- scPlot + coord_cartesian(
    xlim = c(10, lx), 
    ylim = c(1, ly)
  )

  print(scPlot)
  print("Plotted data.")
  return(scPlot)
}

################################################################################
### Start of main code
################################################################################

# Parameters to tweak
file2load <- "./Data/HoursVsAge.csv"
pts2Plot <- 0.05  # fraction of points to plot (0.0 to 1.0)
limY <- 2000  # upper graph limit for hr/wk lines
limX <- 1000  # upper graph limit for hr/wk lines

hva <- ReadWarrantyData(file2load)
plotData <- select(DecimateData(hva, pts2Plot), MachineAge, hourReading, CustType)
CreateHrVsAgeLinPlot(plotData, limX, limY)
