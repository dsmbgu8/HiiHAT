;+
; Perform low-pass image filtering on the given image
;
; :Categories: util 
;
; :Params:
;   image, in, required, type="fltarr(ns,nl,nb)"
;    image to filter
;   filt_size, in, required, type=fix
;    size of filter
;
;
; :Returns:
;   filtered copy of image
; 
; :Examples:
;   The following code filter bands with a kernel of size 5
;
;   hiihat_lowpass_image_filter, I, 5
;
; :Author: Lukas Mandrake and Brian Bue
;
; :History:
;   2009 (LM): code initially written in hiihat_preprocess.pro
;
;   Dec 05, 2010 (BDB): routine created from proprocessing code
;
; :Copyright:
;  Copyright 2009, by the California Institute of Technology. ALL RIGHTS
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
pro hiihat_lowpass_image_filter, image, filt_size
  title='hiihat_lowpass_image_filter'
  debug = hiihat_get_config_parm('debug')
  if debug then print, "Entering "+title

  if filt_size le 0 then goto, cleanup

  n_samples = (size(image))[1]
  n_lines = (size(image))[2]
  n_bands = (size(image))[3]

  envi_report_init, "Lowpass Filtering", base=base, /INTERRUPT, title=title 
  envi_report_inc, base, n_bands-1

  kernelSize = [filt_size, filt_size]
  kernel = replicate((1./(kernelSize[0]*kernelSize[1])), $
                     kernelSize[0], kernelSize[1])
  for j=0,(n_bands-1) do begin
     envi_report_stat, base, j, n_bands-1, cancel=cancel   
     if cancel then goto, cleanup     
     image[*,*,j] = convol(float(image[*,*,j]), kernel, $
                           /center, /edge_truncate)
  endfor

cleanup:
  if debug then print, "Exiting "+title
end
