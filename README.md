# Hyperspectral Image Interactive Holistic Analysis Toolkit (HiiHAT)
http://hyperspectral.jpl.nasa.gov/

OVERVIEW
________________________________________________________

Hyperspectral imagery has provided dramatic new insight into the geology and atmosphere of other planets. However, understanding these images can be quite challenging since scientists can only visualize a small number of bands. The Hyperspectral Image Interactive Holistic Analysis Toolkit (Hii-Hat) is an intelligent assistant to help analysts efficiently browse, summarize, and search hyperspectral images. The software consists of a plugin to the IDL/ENVI environment. The algorithms are designed for the special challenges of high dimensional terrestrial and planetary science datasets:

  - High noise levels: Many of the most interesting planetary science 
    questions involve spectral features at the limits of detectability. 
    We emphasize robust strategies capable of detecting subtle spectral 
    features with high levels of noise.
  - Uncertain constituents: In many hyperspectral imaging settings, we have 
    very few ground truth samples from the surface. We address this by 
    "unsupervised" analysis that looks for patterns in the observed data
    itself, and optionally by incorporating domain knowledge in the form of 
    spectral libraries.
  - Fast turnaround time: Tactical observation planning may require fast 
    decisions, so the analysis procedures aim to be as automated as possible.
    The software toolkit includes automatic procedures for image  
    summarization, and techniques to search images for key spectral features.


SYSTEM REQUIREMENTS 
________________________________________________________

  - ITT ENVI+IDL 4.3+ (http://ittvis.com) 
  - Optional: CRISM Analysis Tool (CAT): required for pre-processing CRISM 
    image formats. Available at: 
       http://pds-geosciences.wustl.edu/missions/mro/crism.htm 
  - Tested on Red Hat Enterprise Linux Client release 5.6 (Tikanga) x86_64 and Mac OSX 10.4+ Intel

INSTALLATION 
________________________________________________________

1. Unzip the outer archive directory, and place it into the save_add directory of your local ENVI installation.

2. Launch ENVI.  You should see a new "Hii-HAT" directory appear with several
   new menu options.  

3. You're done! Read the included user guide (the 'doc' subdirectory) for 
   information on specific menu options.


CONTACTS
________________________________________________________

Brian D. Bue (brian.d.bue@jpl.nasa.gov)
Jet Propulsion Laboratory, California Institute of Technology
4800 Oak Grove Dr. M/S 306-463
Pasadena, CA 91109 USA

David R. Thompson (david.r.thompson@jpl.nasa.gov)
Jet Propulsion Laboratory, California Institute of Technology
4800 Oak Grove Dr. M/S 306-463
Pasadena, CA 91109 USA

Lukas Mandrake (lukas.mandrake@jpl.nasa.gov)
Jet Propulsion Laboratory, California Institute of Technology
4800 Oak Grove Dr. M/S 306-463
Pasadena, CA 91109 USA


COPYRIGHT
________________________________________________________

Copyright 2009-2011, by the California Institute of Technology. ALL RIGHTS
RESERVED. United States Government Sponsorship acknowledged. Any commercial
use must be negotiated with the Office of Technology Transfer at the
California Institute of Technology.

This software may be subject to U.S. export control laws and regulations.  By
accepting this document, the user agrees to comply with all applicable U.S.
export laws and regulations.  User has the responsibility to obtain export
licenses, or other export authority as may be required before exporting such
information to foreign countries or providing access to foreign persons.


________________________________________________________
Created: 24 September 2009 by David R. Thompson (david.r.thompson@jpl.nasa.gov)
Updated: 14 September 2011 by Brian D. Bue (brian.d.bue@jpl.nasa.gov)

