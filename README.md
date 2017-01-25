---
title: "README"
author: "Niall Goulding"
date: "16 January 2017"
output: html_document
---

#DCMS Map Maker

##Intro

<<<<<<< HEAD
This package is used to make maps for DCMS statistical publications. It combines input data in a standard format with an Ordnance Survey boundary line shapefile distributed under the Open Government License. It outputs a graphics file of the map.
=======
This package is used to make maps for DCMS statistical publications. It combines input data in a standard format with an Ordnance Survey boundary line shapefile distributed under the Open Government License.
>>>>>>> a4ca2ad335fbab9ea83abf80535953e63bf86479

##Installation

~~~~
install.packages("devtools")
library(devtools)
install_github("DCMSstats/dcmsmaps")
~~~~

##Usage

~~~~
library(dcmsmaps)

<<<<<<< HEAD
dcmsmaps(csvfile = "~/Documents/mydata.csv", outfile = "~/Documents/mymap", mincol = "#3CB43C", maxcol = "#5B7DC8", england = FALSE, labels = TRUE, scale = TRUE, pound = FALSE)
~~~~

* set csvfile to equal the name of your input csv file in the standard format (remember to use double slashes, e.g. `\\`, if you're on Windows)
* set outfile to be the name of your output map in WMF format (EPS format on Mac). Don't worry about the file extension (e.g. .wmf), this is added for you.
=======
dcmsmaps(csvfile = "~/Documents/mydata.csv", outfile = "~/Documents/mymap.eps", mincol = "#3CB43C", maxcol = "#5B7DC8", england = FALSE, labels = TRUE, scale = TRUE, pound = FALSE)
~~~~

* set csvfile to equal the name of your input csv file in the standard format (remember to use double slashes, e.g. `\\`, if you're on Windows)
* set outfile to be the name of your output map in EPS format
>>>>>>> a4ca2ad335fbab9ea83abf80535953e63bf86479
* `mincol` is the hexidecimal colour to set the lower end of your map's scale
* `maxcol` is the hexidecimal colour to set the upper end of your map's scale
* set `england` to `TRUE` if your map is only for England, `FALSE` for the whole UK
* set `englandwales` to `TRUE` if your map is only for England and Wales, `FALSE` for the whole UK
* set `labels` to `FALSE` to turn map labels off
* set `scale` to `FALSE` to turn the colour scale legend off
* set `pound` to `TRUE` if you want the data displayed on your map to be prefixed with a £ sign, e.g., if you're plotting GVA per region.

### Input data

The code is flexible enough to work with population like figures, £thousands and precise GBP values.

Remember to follow the standard format for your input data CSV file. An example file is included in this package. It should be formatted as follows:

| NAME                     | mapdata |
|--------------------------|---------|
| East Midlands            | 70.7    |
| East of England          | 71.0    |
| London                   | 77.5    |
| North East               | 69.2    |
| North West               | 70.0    |
| South East               | 72.8    |
| South West               | 71.6    |
| West Midlands            | 70.2    |
| Yorkshire and the Humber | 70.0    |
| Northern Ireland         | 69.3    |
| Scotland                 | 70.4    |
| Wales                    | 69.8    |

If the names of the UK regions and devoled administrations, and the column headings, are misspelt or otherwise different to this the map will not be created properly.

### Using the output in Microsoft Office

<<<<<<< HEAD
This package produces an WMF (or EPS) file, which is a graphics file that doesn't suffer from pixelation or compression issues when you save your Document or Presentation to PDF. To use it in PowerPoint etc. select Insert > Picture > mymap.eps. Then save it to PDF and it'll look nice no matter how much you zoom in!
=======
This package produces an EPS file, which is a graphics file that doesn't suffer from pixelation or compression issues when you save your Document or Presentation to PDF. To use it in PowerPoint etc. select Insert > Picture > mymap.eps. Then save it to PDF and it'll look nice no matter how much you zoom in!
>>>>>>> a4ca2ad335fbab9ea83abf80535953e63bf86479






