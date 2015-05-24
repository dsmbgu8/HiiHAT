# File: README-6d-synthetic.txt 
# Created Sept. 2004, Erzse'bet Mere'nyi (erzsebet@rice.edu)
# Last Modified by B. Bue (bbue@rice.edu), July 13, 2011

This directory contains:

6d_synthetic.wvl   standard "wavelength" file for all 6-d synthetic data cubes
6d_synthetic.ctab  standard color table with 30 colors, to be used with class labels

8class_noisy/ Directory of 8 class dataset containing 4 large and 4 relatively small or rare classes
11class_noisy/  8class/layout1 data set with 3 rare classes added, noisy 

These data directories contain the data cube(s), reference (representation of spatial layout) and class map images, and truth labels.The same files are also provided in ENVI format where applicable (i.e., image cubes and other image files). Additionally, each directory contains a description (.dscr) file which gives details on each file in that directory.


===== Description of data files =====

***** Image data *****

Find particulars in the respective subdirectories, in *.dscr files.
Here, general properties are listed.

----- Image cubes -----

<filename>.viff

6-band image file (binary data). 128 rows by 128 columns by 6 bands, 1
signed short integer (16-bit) per data item (= one 16-bit integer per
pixel per band) for noiseless data, 1 float per item for noisy
data. This is indicated in the corresponding *.dscr files.  This file
is a raw raster file, with a fixed-length 1024-byte header preceding
it.  (The header can simply be skipped when reading the file.)  The
data follow a Band Sequential (BSQ) file layout, i.e., in the file,
the first image band is laid out first, starting with row one, then
row 2, and so on. Then comes the 2nd band, and so on.  There is no
additional data in the file (no end-of-line markers or other
delimiters.)  The <*>.viff files are Khoros .VIFF files, so if you had Khoros
software, it would understand these multi-band image files.  However,
you can read / import them with no problem according to the above
format description.

----- Reference image data -----

<filename>.ref 

Reference image, can be anything that shows the spatial structure of
the input image cube. For example, in the case of the 6d synthetic
image cubes this is usually the "magnitude" image (gives the vector
norm at each pixel, which simulates the albedo variations in a natural
remote sensing apsctral image). The purpose of any .ref image is to
help navigate the spatial features.  A <>.ref image is usually gray
scale, i.e., has 1 band. It has a fixed 1024-byte header that can be
skipped when reading it in. It is also a Khoros .VIFF file.

<filename>.map 

Truth labels, color coded according to 6d_synthetic30.ctab.
Khoros .VIFF file with 1024-byte header, unsigned byte.

***** Meta data for image cubes *****

6d_synthetic.wvl

"Wavelength" file for image. Specifies the spectral positions of each
image band. In this case, "wavelength" is just a band number.

6d_synthetic30.ctab

Text (ascii) file containing RGB color codes for class labels.  You
only need this for visualization such as for a class map, or to
visualize sample data locations in the image using the color codes
of the classes.  This is our standard color scheme for all
6d-synthetic image cubes. The # symbol delineates a comment line. Each
line contains <letter> Red_value Green-Value Blue_vale The character *
is no exception but we use the corresponding color to indicate
unclassified pixels in our class maps (hence the asterisk, not another
letter).  If a given image cube does not contain certain classes the
corresponding labels and color codes will not get utilized in our
software, so we always use the same color scheme (the same
6d_synthetic30.ctab) for these 6d images.


<filename>.incl

Text (ascii) file containing labels for any spatial areas of an image.
We call this an "include" file and use, as a standard file extension,
<>.incl, to indicate the nature of the file. It is defining a spatial
mask for pixels to be included in a given processing, and at the same
time it can also assign labels to the pixels.

Labeled areas are defined by "include area" and "exclude area"
commands as shown below. The four numbers (Xul Yul Xlr Ylr) are the
coordinates of the upper left and lower right corners of the area to
be included or excluded from sample selection. Units are in
image pixels, 1-indexed.  Pixel (1,1) is the upper left corner of the
image.  The letter after the coordinates gives the label for the selected area. Areas for
classes can be defined in any order. The # symbol marks a comment line.

Examples: 
# 
# include area 190 142 194 144 A 
#
-- 
the above line designates the rectangle ((190,142);(194,144)) as (part
of) the pixels in class A (assigns label A to all pixels in this
rectangle).

#
# exclude area 192 142 192 142 A 
#
-- 
the above line removes the rectangle ((192,142);(192,142)) from the 
pixels labeled as class A.

----- ENVI format image data -----

<filename>.img 

ENVI format image containing exactly the same data as the
corresponding <filename>.viff data file 

<filename>.img.hdr 

ENVI format image header for the corresponding <filename>.img image file.

<filename>.map.img  

ENVI classification image (truth labels) for the corresponding <filename>.viff data
file. Contains a single band ENVI classification image corresponding
to the classes defined in <filename>-all.incl. Colors are defined
according to the 6d_synthetic30.ctab color table.

<filename>.map.img.hdr 

ENVI format image header for the corresponding <filename>.map.img
classification image.

----- Copyright  -----

Copyright 2001-2011 Rice University
