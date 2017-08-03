<<<<<<< HEAD
# echogram 0.1.1
* The default option 'channel = 1" has been changed to 'NULL' in read.echogram() and bottom.hac() functions. When 'channel' is missing, the minimum channel is selected, allowing to read HAC files from Furuno echosounders where 'channel = 0'.
* Due to inconsistent results observed when sampling pixels in echograms, the functions echogram() and sample.echogram() were reviewed and corrected. The changes are:
In echogram(), the default value for 'xref' is now the ping number, which solves the issue with sample.echogram() returning incorrect values when selecting pixels from the echogram. Also, the labels in the vertical axis (depths) are now correct. The echogram's main title is now by default the acoustic frequency. Finally, image.scale() code included in echogram() has been updated to imageScale() function from sinkr package by Marc Taylor (see ?echogram).
The sample.echogram() function now opens a new graphics device (with dev.new). Sampled pixels are marked immediatly after selection. The code of the internal function reshape.echogram() now takes into account the ping number as reference. 
=======
# echogram 0.1.1
* The default option 'channel = 1" has been changed to 'NULL' in read.echogram() and bottom.hac() functions. When 'channel' is missing, the minimum channel is selected, allowing to read HAC files from Furuno echosounders where 'channel = 0'.
* Due to inconsistent results observed when sampling pixels in echograms, the functions echogram() and sample.echogram() were reviewed and corrected. The changes are:
In echogram(), the default value for 'xref' is now the ping number, which solves the issue with sample.echogram() returning incorrect values when selecting pixels from the echogram. Also, the labels in the vertical axis (depths) are now correct. The echogram's main title is now by default the acoustic frequency. Finally, image.scale() code included in echogram() has been updated to imageScale() function from sinkr package by Marc Taylor (see ?echogram).
The sample.echogram() function now opens a new graphics device (with dev.new). Sampled pixels are marked immediatly after selection. The code of the internal function reshape.echogram() now takes into account the ping number as reference, and therefore the ping number is included in the results. 
>>>>>>> 738d0270bff33d4e20b510519064c3827157e677
