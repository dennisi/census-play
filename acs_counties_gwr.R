library(GISTools) 
library(spdep)

# plot neighbors (Queen's case topology)
counties.nb <- poly2nb(acs_merged)
counties.labs <- poly.labels(acs_merged)
plot(acs_merged, col = "grey")
plot(counties.nb, coordinates(counties.labs), col = 'red', add = TRUE)

# # WARNING: EVERYONE's a neighbor?!
# counties.nb2 <- dnearneigh(poly.labels(acs_merged), 0, miles2ft(1.2))
# plot(acs_merged, col = 'grey')
# plot(counties.nb2, coordinates(counties.labs), col = 'red', add = TRUE)

counties.lw <- nb2listw(counties.nb)
attach(data.frame(acs_merged))
moran.test(Undergraduate, counties.lw)
# => reject the hypothesis that I=0

# NEXT TODO: What to change 'breach' to?
density <- poly.counts(breach, acs_merged) /
    ft2miles(ft2miles(poly.areas(acs_merged)))
moran.test(density,counties.lw)
