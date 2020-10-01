
JPGQUALITY=50
PDFRESOLUTION=200
WIDTH=1000
HORIZONTALPADDING=10
VERTICALPADDING=10

# Fig 1
montage figuresraw/mapindic_alpha_2015.png figuresraw/mapindic_moran_2015.png -tile 1x2 -geometry +0+"$VERTICALPADDING" -resize "$WIDTH"x figures/Fig1.png

# Fig 2
montage figuresraw/mapindic_cluster.png figuresraw/distributions_cluster-year.png -tile 1x2 -geometry +0+"$VERTICALPADDING" -resize "$WIDTH"x figures/Fig2.png

# Fig 3
montage figuresraw/CorrelatedPerco_size200_gradient002_corr04_centers3_nonBinary.png figuresraw/Expmixture_size200_centers10_maxRadiusRate0-2_hierarchy1_seed-2489151330471457438.png figuresraw/Gravity_size200_g03_gravity2-5_hierarchy0-5_centers3_pop20000_seed7990907742188865088.png figuresraw/ReactionDiffusion_size200_alpha0-8_beta0-2_diffSteps1_growthRate100_population5000_seed-2225875956205852410.png -tile 2x2 -geometry +"$HORIZONTALPADDING"+"$VERTICALPADDING" -resize "$WIDTH"x -border 2 -bordercolor Black figures/Fig3.png

# Fig 4
cp figuresraw/scatter_all.png figures/Fig4.png

# Fig 5
cp figuresraw/overlap.png figures/Fig5.png
