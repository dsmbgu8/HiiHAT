;+
; Median filter image
;
; :Categories:
;   util 
;
; :Params:
;   img, in, required, type="fltarr(ns,nl,nb)"
;    image to median filter
;   median_filter_width, in, required, type=fix
;    width in bands of median filter to use 
;
; :Returns:
;   median filtered image
; 
; :Examples:
;   The following code will median filter the bands of the the input image I
;   with a width 3 filter
;
;   hiihat_median_filter_image, I, 3
;
; :Author: Lukas Mandrake and Brian Bue
; :History:
;   2009 (LM): code initially written in hiihat_preprocess.pro
;
;   Dec 05, 2010 (BDB): routine created from proprocessing code
;
; :Copyright:
;  Copyright 2010, by the California Institute of Technology. ALL RIGHTS
;  RESERVED. United States Government Sponsorship acknowledged. Any commercial
;  use must be negotiated with the Office of Technology Transfer at the
;  California Institute of Technology.
; 
;  This software may be subject to U.S. export control laws and regulations. By
;  accepting this document, the user agrees to comply with all applicable U.S.
;  export laws and regulations.  User has the responsibility to obtain export
;  licenses, or other export authority as may be required before exporting such
;  information to foreign countries or providing access to foreign persons.
;-
pro hiihat_median_filter_image, img, median_filter_width
  title='hiihat_median_filter_image'  
  debug =  hiihat_get_config_parm('debug')
  if debug then print, "Entering "+title

  img_size = size(img)
  n_samples = img_size[1]
  n_lines = img_size[2]
  n_bands = img_size[3]

  envi_report_init, "Median Filtering", base=base, /INTERRUPT, title=title 
  envi_report_inc, base, n_bands-1

  if median_filter_width le 0 then goto, cleanup

  mod_step = round(n_bands/10)

  img_filtered = fltarr(n_samples, n_lines, n_bands)
  for j=0,n_bands-1 do begin  
     if j mod mod_step eq 0 then begin        
        envi_report_stat, base, j, n_bands-1, cancel=cancel   
        if cancel then goto, cleanup     
     endif
     ;; determine safe boundaries near edges
     min_band = max([j - median_filter_width, 0        ])
     max_band = min([j + median_filter_width, n_bands-1])
     
     ;;print,j, n_bands-1
     band_range = lindgen(max_band-min_band+1)+min_band
     img_filtered[*,*,j] = median(img[*,*,band_range], dimension=3)
  endfor 
  img = img_filtered
cleanup:
  envi_report_init, base=base, /finish 
  if debug then print, "Exiting "+title
end
