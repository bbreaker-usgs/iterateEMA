Run PeakfqSA in 1-year Intervals
================

Overview
--------

This script was developed to evaluate trends in sequential runs of the Expected Moments Algorithm (EMA) on chunks of peak-flow data to evaluate changes in the 2-percent, 1-percent, and 0.2-percent annual exceedance probability (AEP) as peak-flow values are added over time.

``` r
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dataRetrieval, dplyr, lmomco, DT, ggplot2, smwrBase, scales, gtable, grid, gridExtra)

setwd("C:/Users/bbreaker/Documents/PeakfqSA")

site <- "07069500"

pkFile <- readNWISpeak(site, convertType = FALSE)

pkFile[1,3] <- "1915-08-01"

pkFile$peak_dt <- as.Date(pkFile$peak_dt, format = "%Y-%m-%d")

pkFile[1,6] <- "H"

pkPath <- "C:/Users/bbreaker/Documents/PeakfqSA"

emaComp <- iterateEMA(pkFile = pkFile, pkPath = pkPath, beginYrH = 1915, beginYr = 1937, endYr = 2014, threshold = 125000, 
                      skewOpt = "WEIGHTED", genSkew = -0.17, skewSD = 0.348)

#DT::datatable(emaComp, rownames = FALSE, options = list(scrollX='400px'))

pkFile$peak_va <- as.numeric(pkFile$peak_va)

pkFile$wYear <- as.numeric(as.character(waterYear(pkFile$peak_dt)))

p1 <- ggplot() +
  geom_point(data = pkFile, aes(x = wYear, y = peak_va), 
             alpha = 1, fill = "red", pch = 21, size = 3) +
  geom_point(data = emaComp, aes(x = endYear, y = Q500),
             alpha = 1, fill = "grey", pch = 25, size = 3) +
  geom_line(data = emaComp, aes(x = endYear, y = upperCI500), linetype = "dotted") +
  geom_line(data = emaComp, aes(x = endYear, y = lowerCI500), linetype = "dotted") +
  scale_y_continuous(labels = comma) +
  annotation_logticks(sides = "rl", size = 0.1) +
  labs(x = "Year", y = expression(atop("Annual peak streamflow,", paste("in ft"^"3","/s")))) +
  theme_USGS()

p2 <- ggplot() +
  geom_point(data = pkFile, aes(x = wYear, y = peak_va), 
             alpha = 1, fill = "red", pch = 21, size = 3) +
  geom_point(data = emaComp, aes(x = endYear, y = Q100),
             alpha = 1, fill = "white", pch = 23, size = 3) +
  geom_line(data = emaComp, aes(x = endYear, y = upperCI100), linetype = "longdash") +
  geom_line(data = emaComp, aes(x = endYear, y = lowerCI100), linetype = "longdash") +
  scale_y_continuous(labels = comma) +
  annotation_logticks(sides = "rl", size = 0.1) +
  labs(x = "Year", y = expression(atop("Annual peak streamflow,", paste("in ft"^"3","/s")))) +
  theme_USGS()

p3 <- ggplot() +
  geom_point(data = pkFile, aes(x = wYear, y = peak_va), 
             alpha = 1, fill = "red", pch = 21, size = 3) +
  geom_point(data = emaComp, aes(x = endYear, y = Q50),
             alpha = 1, fill = "black", pch = 22, size = 3) +
  geom_line(data = emaComp, aes(x = endYear, y = upperCI50), linetype = "dashed") +
  geom_line(data = emaComp, aes(x = endYear, y = lowerCI50), linetype = "dashed") +
  scale_y_continuous(labels = comma) +
  annotation_logticks(sides = "rl", size = 0.1) +
  labs(x = "Year", y = expression(atop("Annual peak streamflow,", paste("in ft"^"3","/s")))) +
  theme_USGS()

drawTicks3(p1, p2, p3)
```

![](iterateEMAforGH_files/figure-markdown_github/unnamed-chunk-1-1.png)
