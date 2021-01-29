
JPGQUALITY=50
PDFRESOLUTION=200
WIDTH=1500
HEIGHT=1000
HORIZONTALPADDING=10
VERTICALPADDING=10

convert figuresraw/coevol_leg.png -gravity center -extent 3500x figuresraw/tmpleg.png
montage figuresraw/coevol_1.png figuresraw/coevol_2.png figuresraw/coevol_3.png figuresraw/coevol_4.png -tile 2x2 -geometry +"$HORIZONTALPADDING"+"$VERTICALPADDING" -resize "$WIDTH"x -border 2 -bordercolor Black figuresraw/tmp1.png
montage figuresraw/tmp1.png figuresraw/tmpleg.png -tile 1x2 -geometry +"$HORIZONTALPADDING"+"$VERTICALPADDING" -resize "$WIDTH"x figures/Fig1tmp.jpg
convert figures/Fig1tmp.jpg -set colorspace Gray -separate -average Fig1.jpg
rm figuresraw/tmp1.png figuresraw/tmpleg.png figures/Fig1tmp.jpg

convert figuresraw/innovation_leg.png -gravity center -extent 3500x figuresraw/tmpleg.png
montage figuresraw/innovation_1.png figuresraw/innovation_2.png figuresraw/innovation_3.png figuresraw/innovation_4.png -tile 2x2 -geometry +"$HORIZONTALPADDING"+"$VERTICALPADDING" -resize "$WIDTH"x -border 2 -bordercolor Black figuresraw/tmp2.png
montage figuresraw/tmp2.png figuresraw/tmpleg.png -tile 1x2 -geometry +"$HORIZONTALPADDING"+"$VERTICALPADDING" -resize "$WIDTH"x figures/Fig2tmp.jpg
convert figures/Fig2tmp.jpg -set colorspace Gray -separate -average Fig2.jpg
rm figuresraw/tmp2.png figuresraw/tmpleg.png figures/Fig2tmp.jpg
