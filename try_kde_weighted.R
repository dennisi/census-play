library(scales)
# Kernel Densities from point object
kde2d.weighted <- function (x, y, w, h, n = 25, lims = c(range(x), range(y))) {
    nx <- length(x)
    if (length(y) != nx) 
        stop("data vectors must be the same length")
    if (length(w) != nx & length(w) != 1)
        stop("weight vectors must be 1 or length of data")
    gx <- seq(lims[1], lims[2], length = n) # gridpoints x
    gy <- seq(lims[3], lims[4], length = n) # gridpoints y
    if (missing(h)) 
        h <- c(bandwidth.nrd(x), bandwidth.nrd(y));
    if (missing(w)) 
        w <- numeric(nx)+1;
    h <- h/4
    ax <- outer(gx, x, "-")/h[1] # distance of each point to each grid point in x-direction
    ay <- outer(gy, y, "-")/h[2] # distance of each point to each grid point in y-direction
    z <- (matrix(rep(w,n), nrow=n, ncol=nx, byrow=TRUE)*matrix(dnorm(ax), n, nx)) %*% t(matrix(dnorm(ay), n, nx))/(sum(w) * h[1] * h[2]) # z is the density
    return(list(x = gx, y = gy, z = z))
}

kde.points.weighted <- function(pts, w, h, hscale=1, n=200, lims=NULL) {
    xy = coordinates(pts)
    p4s = CRS(proj4string(pts))
    if (missing(h)) h = c(bandwidth.nrd(xy[,1]),bandwidth.nrd(xy[,2])) * hscale
    if (is.null(lims)) 
    { lims=c(range(xy[,1]),range(xy[,2])) }
    else 
    { lims = t(bbox(lims))}
    kd = kde2d.weighted(xy[,1],xy[,2], w, h,n,lims)
    temp = SpatialPoints(expand.grid(kd$x,kd$y))
    temp = SpatialPixelsDataFrame(temp,data.frame(kde=array(kd$z,length(kd$z))))
    proj4string(temp) = p4s
    temp}

##########################################################################################
kde.w.plot.dai <- function(acs_data, acs_w_metric, hscale = 1, n = 200, palname = "Greens", mtitle = "") {
    # acs_w_metric <- "Total.Population"
    xlim <- acs_data@bbox[c(1,3)] 
    ylim <- acs_data@bbox[c(2,4)]
    acs_dens_w <- kde.points.weighted(acs_data, 
                                      w = acs_data@data[[acs_w_metric]] # Weights
                                      ,# / (acs_data@data$ALAND + acs_data@data$AWATER), # per sq.mile
                                       # / sum(acs_data@data$Total.Population),          # per capita
                                      hscale = hscale,
                                      n = n,
                                      lims = acs_data)
    masker <- poly.outer(acs_dens_w, acs_data)
#     acs_dens_shades <- auto.shading(acs_dens_w$kde, n=9, cols = alpha(brewer.pal(9, palname),0.9), cutter = rangeCuts)
#     plot(acs_data) 
#     level.plot(acs_dens_w, shades = acs_dens_shades, add = TRUE)
#     add.masking(masker)
    # plot(poly.labels(acs_data), add = TRUE)
    acs_dens_shades <- auto.shading(acs_dens_w$kde, n=9, cols = brewer.pal(9, palname), cutter = rangeCuts)
    level.plot(acs_dens_w, shades = acs_dens_shades)
    add.masking(masker)
    plot(acs_data, border=alpha("gray", 0.4), xlim = xlim, ylim = ylim, add = TRUE)
    # plot(poly.labels(acs_data), add = TRUE)
    choro.legend(acs_dens_w@bbox[1,1], mean(acs_dens_w@bbox[2,]), acs_dens_shades, cex=0.5, 
                 title = paste0("KDE for ", acs_w_metric), xjust = 0, bg = 'white')
    title(paste(acs_w_metric, mtitle))
}
##########################################################################################

windows(width = 10, height = 7, pointsize = 14)

kde.w.plot.dai(acs_merged, "Total.Population", palname = "Greens")
kde.w.plot.dai(acs_merged, "Total.Population", palname = "Greens", hscale = 0.5)
kde.w.plot.dai(acs_merged, "Undergraduate", palname = "Blues")
kde.w.plot.dai(acs_merged, "Undergraduate", palname = "Blues", hscale = 0.2)

acs_ne_counties <- acs_merged[acs_merged$STATEFP == '31',]
acs_ne_counties@data['Total.Population.log'] <- log(acs_ne_counties[["Total.Population"]])
acs_ne_counties@data['Undergraduate.log'] <- log(acs_ne_counties[["Undergraduate"]])
kde.w.plot.dai(acs_ne_counties, "Total.Population", palname = "Greens")
kde.w.plot.dai(acs_ne_counties, "Total.Population", palname = "Greens", hscale = 0.05)
kde.w.plot.dai(acs_ne_counties, "Undergraduate", palname = "Blues")

# exclude Douglas County (contains Omaha) COUNTYFP = 055, GEOID=31055
acs_ne_counties_nomaha <- acs_ne_counties[ acs_ne_counties$Total.Population < 500000, ]
kde.w.plot.dai(acs_ne_counties_nomaha, "Total.Population", palname = "Greens", hscale = 0.2, 
               mtitle = "Douglas County (Omaha) Excluded, h.scale=0.2")
kde.w.plot.dai(acs_ne_counties_nomaha, "Undergraduate", palname = "Blues", hscale = 0.2, 
               mtitle = "Douglas County (Omaha) Excluded, h.scale=0.2")
kde.w.plot.dai(acs_ne_counties_nomaha, "Undergraduate.log", palname = "Blues", hscale = 0.4, 
               mtitle = "Douglas County (Omaha) Excluded, h.scale=0.4")

par(oma=c(6,2,0,0) )
par(mar=c(0,0,0,0))

kde.w.plot.dai(acs_ne_counties_nomaha, "Total.Population", palname = "Greens", hscale = 0.2, 
               mtitle = "mar=c(0,0,0,0)")

par(mar=c(4,5,2,5))
kde.w.plot.dai(acs_ne_counties_nomaha, "Total.Population", palname = "Greens", hscale = 0.2, 
               mtitle = "mar=c(4,5,2,5)")

par(mar=c(1,1,1,1))
par(oma=c(1,1,1,1) )
kde.w.plot.dai(acs_ne_counties_nomaha, "Total.Population", palname = "Greens", #hscale = 0.2, 
               mtitle = "mar=c(1,1,1,1), oma=c(1,1,1,1)")

# SCC Service Area counties:
scc_FPs <- c(
    '025', # Cass
    '059', # Fillmore
    '067', # Gage
    '095', # Jefferson
    '097', # Johnson
    '109', # Lancaster
    '127', # Nemaha
    '131', # Otoe
    '133', # Pawnee
    '147', # Richardson
    '151', # Saline
    '155', # Saunders
    '159', # Seward
    '169', # Thayer
    '185'  # York
)
acs_ssc_counties <- acs_ne_counties[ acs_ne_counties$COUNTYFP %in% scc_FPs, ]

kde.w.plot.dai(acs_ssc_counties, "Total.Population", palname = "Greens")
kde.w.plot.dai(acs_ssc_counties, "Total.Population", palname = "Greens", hscale = 0.2)
kde.w.plot.dai(acs_ssc_counties, "Undergraduate", palname = "Blues")

    
    
    
    
    
acs_dens_w <- kde.points.weighted(acs_ug_ne, 
                                  #acs_ug_ne@data$Undergraduate, # Weights
                                  acs_ug_ne@data$Undergraduate / sum(acs_ug_ne@data$Undergraduate),
                                  n = 200)

masker <- poly.outer(acs_dens_w, acs_ug_ne, extend = 100)
acs_dens_shades <- auto.shading(acs_dens_w$kde, n = 9, cols = brewer.pal(9, "Greens"), cutter = rangeCuts)
level.plot(acs_dens_w, shades = acs_dens_shades)
# add.masking(masker)
plot(acs_ug_ne, add = TRUE)

plot(acs_dens_w$kde, acs_dens_w@grid.index)
plot(acs_dens$kde)

auto.shading(acs_dens_w$kde, n = 9, cols = brewer.pal(9, "Greens"), cutter = rangeCuts)
auto.shading(acs_dens$kde, n = 9, cols = brewer.pal(9, "Greens"), cutter = rangeCuts)

order(acs_dens$kde)



acs_dens_w <- kde.points.weighted(acs_merged, 
                                  #acs_ug_ne@data$Undergraduate, # Weights
                                  # acs_merged@data$Undergraduate / sum(acs_merged@data$Undergraduate),
                                  sum(log(acs_merged@data$Undergraduate) / sum(log(acs_merged@data$Undergraduate))),
                                  n = 400)

masker <- poly.outer(acs_dens_w, acs_merged)
acs_dens_shades <- auto.shading(acs_dens_w$kde, n = 9, cols = brewer.pal(9, "Greens"), cutter = rangeCuts)
level.plot(acs_dens_w, shades = acs_dens_shades)
add.masking(masker)
plot(acs_merged, add = TRUE)


###########################################################################################
## Nebraska Only
acs_merged_ne <- acs_merged[acs_merged$STATEFP=='31',]


acs_dens_w <- kde.points.weighted(acs_merged_ne, 
                                  #acs_ug_ne@data$Undergraduate, # Weights
                                  w = acs_merged_ne@data$Total.Population, # / sum(acs_merged_ne@data$Total.Population),
                                  n = 400)
level.plot(acs_dens_w)
plot(acs_merged_ne, add = TRUE)
# plot(poly.labels(acs_merged_ne), add = TRUE)
##########################################################################################


###########################################################################################
windows(width = 10, height = 7, pointsize = 14)

metric <- "Undergraduate"
acs_dens_w <- kde.points.weighted(acs_merged, 
                                  #acs_ug_ne@data$Undergraduate, # Weights
                                  w = acs_merged@data[[metric]] / sum(acs_merged@data[[metric]])
                                  # w = acs_merged@data[[metric]] #acs_merged@data$High.School
                                  ,# / (acs_merged@data$ALAND + acs_merged@data$AWATER), # / sum(acs_merged@data$Total.Population),
                                  h = c(1.0832833, 0.5996318) * 3,
                                  n = 400)
masker <- poly.outer(acs_dens_w, acs_merged, extend = 0)
acs_dens_shades <- auto.shading(acs_dens_w$kde, n=9, cols = brewer.pal(9, "Greens"), cutter = rangeCuts)
level.plot(acs_dens_w, shades = acs_dens_shades)
add.masking(masker)
plot(acs_merged, add = TRUE)
# plot(poly.labels(acs_merged), add = TRUE)
choro.legend(acs_dens_w@bbox[1,1], mean(acs_dens_w@bbox[2,]), acs_dens_shades, cex=0.5, title = paste0("KDE for ", metric),
             xjust = 0, bg = 'white')
title(metric)
##########################################################################################

# masker <- poly.outer(acs_dens_w, acs_merged_ne, extend = 1)
#acs_dens_shades <- auto.shading(acs_dens_w$kde, n = 5, cols = brewer.pal(5, "Greens"), cutter = rangeCuts)
#level.plot(acs_dens_w, shades = acs_dens_shades)
# add.masking(masker)

# level.plot(gwr.res2, index = 'Advance', shades = gwr.shades)
level.plot(acs_dens_w, index = 'kde')
acs_dens_w

#plot(acs_merged_ne@data$, add = TRUE)


choropleth(acs_merged_ne, log(acs_merged_ne$Undergraduate))
cbind(acs_merged_ne@data$Geo.Name, acs_merged_ne$Undergraduate, log(acs_merged_ne$Undergraduate))
acs_dens_shades

#        rep(w, 25)
sum(log(acs_merged@data$Undergraduate) / sum(log(acs_merged@data$Undergraduate)))
log(acs_merged@data$Undergraduate) / sum(log(acs_merged@data$Undergraduate))
acs_merged@data$Undergraduate / sum(acs_merged@data$Undergraduate)
log(acs_merged@data$Undergraduate) / sum(log(acs_merged@data$Undergraduate))
acs_merged@data$Undergraduate / sum(acs_merged@data$Undergraduate)
length (
    acs_merged@data$Undergraduate / sum(acs_merged@data$Undergraduate)
)
