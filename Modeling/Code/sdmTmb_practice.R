library(dplyr)
library(ggplot2)
library(sdmTMB)

#install.packages("pak")
pak::pak("pbs-assess/sdmTMB", dependencies = TRUE)
head(pcod)

#Depth was centered and scaled by its standard deviation so that coefficient sizes weren’t too big or small.
glimpse(pcod)

mesh <- make_mesh(pcod, c("X", "Y"), cutoff = 10)
plot(mesh)

m <- sdmTMB(
  data = pcod,
  formula = present ~ depth_scaled + depth_scaled2,
  mesh = mesh, # can be omitted for a non-spatial model
  family = binomial(link = "logit"),
  spatial = "off"
)
m

m1 <- sdmTMB(
  data = pcod,
  formula = present ~ depth_scaled + depth_scaled2,
  mesh = mesh, # can be omitted for a non-spatial model
  family = binomial(link = "logit"),
  spatial = "on"
)
m1

m2 <- sdmTMB(
  data = pcod,
  formula = present ~ depth_scaled + depth_scaled2,
  mesh = mesh,
  family = binomial(link = "logit"),
  spatial = "on",
  time = "year",
  spatiotemporal = "IID"
)
m2

m3 <- sdmTMB(
  data = pcod,
  formula = density ~ poly(log(depth), 2),
  mesh = mesh,
  family = tweedie(link = "log"),
  spatial = "on",
  time = "year",
  spatiotemporal = "IID"
)
m3