
JPGQUALITY=50
PDFRESOLUTION=300
WIDTH=2000
HORIZONTALPADDING=10
VERTICALPADDING=10


# Fig 1
convert -density $PDFRESOLUTION -background white -alpha remove figuresraw/multiscale_morph.pdf -resize "$WIDTH"x -quality $JPGQUALITY figures/Fig1.jpg

# Fig 2
convert figuresraw/allindics_hist.png -resize "$WIDTH"x -quality $JPGQUALITY figures/Fig2.jpg

# Fig 3
convert figuresraw/deltaHierarchymacroPopulations-mesoTimeSteps_colorMacroInteractionDecay_facetmesoMacroDecayUpdateMax-macroMesoBetaUpdateMax.png -resize "$WIDTH"x -quality $JPGQUALITY figures/Fig3.jpg

# Fig 4
convert figuresraw/onefactor_allindics_mesoMacroDecayUpdateMax_errorbars.png -resize "$WIDTH"x -quality $JPGQUALITY figures/Fig4.jpg

# Fig 5
convert figuresraw/deltamesoMorans-macroMesoAlphaUpdateMax_colormacroMesoBetaUpdateMax_facetmesoMacroCongestionCost-mesoMacroDecayUpdateMax_mesoBeta0_11.png -resize "$WIDTH"x -quality $JPGQUALITY figures/Fig5.jpg

# Fig 6
convert figuresraw/deltamesoSlopes-macroMesoAlphaUpdateMax_colormacroMesoBetaUpdateMax_facetmesoMacroCongestionCost-mesoMacroDecayUpdateMax_mesoBeta0_11.png -resize "$WIDTH"x -quality $JPGQUALITY figures/Fig6.jpg

# Fig 7
convert figuresraw/pareto_macroHierarchy-mesoHierarchy.png -resize "$WIDTH"x -quality $JPGQUALITY figures/Fig7.jpg

