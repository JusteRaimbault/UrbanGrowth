
JPGQUALITY=50
PDFRESOLUTION=200
WIDTH=2000
HORIZONTALPADDING=10
VERTICALPADDING=10

# Fig 1
montage figuresraw/mapindic_alpha_2015.png figuresraw/mapindic_moran_2015.png -tile 1x2 -geometry +0+"$VERTICALPADDING" -resize "$WIDTH"x figures/Fig1.png

# Fig 2
montage figuresraw/mapindic_cluster.png figuresraw/distributions_cluster-year.png -tile 1x2 -geometry +0+"$VERTICALPADDING" -resize "$WIDTH"x figures/Fig2.png
