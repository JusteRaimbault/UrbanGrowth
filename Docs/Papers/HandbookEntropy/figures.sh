
JPGQUALITY=50
PDFRESOLUTION=200
WIDTH=1500
HEIGHT=1000
HORIZONTALPADDING=10
VERTICALPADDING=10

convert figuresraw/innovation_leg.png -gravity center -extent 3500x figuresraw/tmpleg.png
montage figuresraw/innovation_1.png figuresraw/innovation_2.png figuresraw/innovation_3.png figuresraw/innovation_4.png -tile 2x2 -geometry +"$HORIZONTALPADDING"+"$VERTICALPADDING" -resize "$WIDTH"x -border 2 -bordercolor Black figuresraw/tmp3.png
montage figuresraw/tmp3.png figuresraw/tmpleg.png -tile 1x2 -geometry +"$HORIZONTALPADDING"+"$VERTICALPADDING" -resize "$WIDTH"x figures/Fig3.jpg
rm figuresraw/tmp3.png figuresraw/tmpleg.png
