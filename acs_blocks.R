bg_Lancaster = block_groups(state='NE', county = '109', cb = TRUE) # 30MB

acs_blockgr_shapes <- rbind_tigris(bg_Lancaster)
acs_block_groups <-
    geo.make(
        state = 'NE', county = 'Lancaster', tract = '*', block.group = '*', check = TRUE
    )

tl = acs.lookup(table.name = "B01003")
cbind(tl@results$variable.code, 
      tl@results$variable.name)

acs_bg_pop <- acs.fetch(geo=acs_block_groups, 
                        table.number="B01003", col.names = c("total"), 
                        endyear = 2013, span = 5, dataset = 'acs')

# convert to a data.frame for merging
acs_bg_pop_df <-
    data.frame(
        paste0(
            str_pad(acs_bg_pop@geography$state, 2,  "left", pad = "0"),
            str_pad(acs_bg_pop@geography$county, 3, "left", pad = "0"),
            str_pad(acs_bg_pop@geography$tract, 6,  "left", pad = "0"),
            acs_bg_pop@geography$blockgroup
        ),
        acs_bg_pop@estimate[,"total"],
        stringsAsFactors = FALSE
    )

rownames(acs_bg_pop_df)<-1:nrow(acs_bg_pop_df)
names(acs_bg_pop_df)<-c("GEOID", "total")

acs_bg_merged<- geo_join(acs_blockgr_shapes, acs_bg_pop_df, "GEOID", "GEOID")
# there are some tracts with no land that we should exclude
acs_bg_merged <- acs_bg_merged[acs_bg_merged$ALAND>0,]


# Make KDE Weighted Plots
kde.w.plot.dai(acs_bg_merged, "total", palname = "Greens")
kde.w.plot.dai(acs_bg_merged, "total", palname = "Greens", hscale = 0.5)
