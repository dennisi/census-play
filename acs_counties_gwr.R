library(GISTools) 
library(spdep)

# plot neighbors (Queen's case topology)
counties.nb <- poly2nb(acs_merged)
counties.labs <- poly.labels(acs_merged)
plot(acs_merged, col = "grey")
plot(counties.nb, coordinates(counties.labs), col = 'red', add = TRUE)

# Note: acs_merged is in lat/long degrees, not in feet!

# # 50 miles: 3.9 avg links; 75mi: 8.4; 100mi: 15.8
# counties.nb2 <- dnearneigh(poly.labels(acs_merged), 0, 75, longlat = TRUE) 
# plot(acs_merged, col = 'grey')
# plot(counties.nb2, coordinates(counties.labs), col = 'red', add = TRUE)

counties.lw <- nb2listw(counties.nb)
attach(data.frame(acs_merged))
moran.test(Undergraduate, counties.lw)
# => reject the hypothesis that I=0


# ERROR: Error in identical(spgeom1@proj4string, spgeom2@proj4string) : 
#        trying to get slot "proj4string" from an object of a basic class ("numeric") with no slots
# How many breaches of peace in each census block?
# How many undergraduate students in each county?
# density <- poly.counts(Undergraduate, acs_merged) /
#     poly.areas(acs_merged)
# 
# moran.test(density,counties.lw)

# Simulation-Based Tests (Monte Carlo)
moran.mc(Undergraduate, counties.lw, nsim=10000)

##
## Regression Models with Spatial Autoregression (SAR)
## 

# # filter out unoccupied blocks
# acs_merged <- acs_merged[Undergraduate > 0,] 
# detach(data.frame(acs_merged))
# attach(data.frame(acs_merged2))

# # calculate annual rates per 1,000 homes
# forced.rate <- 2000 * poly.counts(burgres.f, blocks2) / OCCUPIED
# notforced.rate <- 2000 * poly.counts(burgres.n, blocks2) / OCCUPIED
# 
# model1 <- lm(forced.rate ~ notforced.rate)
# summary(model1)

# blocks2.nb <- poly2nb(blocks2) 
# blocks2.lw <- nb2listw(blocks2.nb)
# model2 <- spautolm(forced.rate ~ notforced.rate, listw = blocks2.lw)
# summary(model2)
# # Conclusion: since p-value is so high (0.31), no need to allow for spatial dependency of the error term!


##
## Modifiable Areal Unit Problem:
##  * Will the same conclusions hold for tracts as did for blocks?

# plot(blocks, border = 'red')
# plot(tracts, lwd = 2, add = TRUE)
# detach(data.frame(blocks2))
# attach(data.frame(tracts))
# 
# forced.rate.t <- 2000 * poly.counts(burgres.f, tracts) / OCCUPIED
# notforced.rate.t <- 2000 * poly.counts(burgres.n, tracts) / OCCUPIED
# 
# model1.t <- lm(forced.rate.t ~ notforced.rate.t)
# summary(model1.t)


##
## Zone-Free Approach (using KDE)
##
            # Data for New Haven to use in example
            # data(newhaven)
            # How many breaches of peace in each census block?
            n.breach = poly.counts(breach,blocks)
            # Compute densities and map them
            choropleth(blocks, n.breach / blocks$AREA )
            choropleth(acs_merged, acs_merged$Hawaiian)
            kde.points(acs_merged$Hawaiian, n = 10)
            
## NOTE: kde.points() first argument needs to be a SpatialPoints object!!
acs_dens <- kde.points(acs_merged, n = 10) # 1MB; n = 1000 => 26MB; n = 1500 => 60MB

# masker <- poly.outer(acs_dens, tracts, extend = 100)
acs_dens_shades <- auto.shading(acs_dens$kde, n = 9, cols = brewer.pal(9, "Greens"), cutter = rangeCuts)
level.plot(acs_dens, shades = acs_dens_shades)
# add.masking(masker)
plot(acs_merged, add = TRUE)

## NOTE: kde.points() first argument needs to be a SpatialPoints object!!
acs_dens <- kde.points(acs_ug_ne, n = 10) # 1MB; n = 1000 => 26MB; n = 1500 => 60MB

# masker <- poly.outer(acs_dens, tracts, extend = 100)
acs_dens_shades <- auto.shading(acs_dens$kde, n = 9, cols = brewer.pal(9, "Greens"), cutter = rangeCuts)
level.plot(acs_dens, shades = acs_dens_shades)
# add.masking(masker)
plot(acs_ug_ne, add = TRUE)
