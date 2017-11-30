pal1 <- c("#a97975", #bottom right (bottom-left third)
          "#de9fff", #pregnancy
          "#ff5f8b", #bottom right (bottom-right third)
          "#019a51", #blood forming organs
          "#E55252", #circulatory
          "#99B4F9", #digestive
          "#CEFEFD", #genitourinary
          "#EE8484", #musculoskeleta
          "#17957e", #nervrous CEFEFD
          "#FFCCA2", #respiratory FFCCA2 FFD8B7
          "#FEF5CE", #skin cutaneous
          "#A5D6FA", #endocrine
          "#EAFAA5", #infectious diseases
          "#DDCCF3", #injury
          "#FFFFFF", #mental disorders
          "#B9B9B9", #bottom right (top third)
          "#F6EBBE") #symptoms
#grid::grid.raster(pal1, interpolate = FALSE) #check color palette
png(filename="ICD-9_tree_2005.png",width=1300, height=850,unit="px")
treemap(df.ed05,
        index=c("Group","Subgroup","Classification"), #
        vSize="Freq",
        type="index",
        #vColor="Group.n",
        title="Emergency Department Visits in 2005 based on NHAMCS",
        overlap.labels = 0.5,
        palette=pal1,
        border.col = c("#101010","#292929","#333333"),
        fontsize.title = 40,
        fontsize.labels = c(30, 24, 16),
        lowerbound.cex.labels=.4,
        fontcolor.labels = c("#000000","#292929","#333333"),
        fontface.labels = c(2,4,1),
        fontfamily.labels = c("sans"),
        inflate.labels=F,
        align.labels = c("center","center"),
        bg.labels=230 # 0 and 255 that determines the transparency
)
dev.off() #3c0120