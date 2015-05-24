;+
; Normalize an input image by the L1, L2, or L Inf norm
;
; :Categories:
;   util 
;
; :Params:
;   image, in, required, type="fltarr(ns,nl,nb)"
;    image to normalize
;   norm_type, in, required, type=string
;    normalization function to use 
;
; :Returns:
;   normalized copy of image
; 
; :Examples:
;   The following code will normalize bands of the the input image I by the L2 norm
;
;   normalize_image, I, 'Euclidean (L 2)'
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
pro hiihat_normalize_image, image, norm_type
  title='hiihat_normalize_image'  
  debug =  hiihat_get_config_parm('debug')
  if debug then print, "Entering "+title

  ;; FIXME: this can probably be vectorized easily...
  n_samples = (size(image))[1]
  n_lines = (size(image))[2]

  envi_report_init, "Normalizing Image", base=base, /INTERRUPT, title=title 
  envi_report_inc, base, n_samples-1

  if norm_type eq "None" then goto, cleanup


  mod_step = round(n_samples/25)

  for x=0,n_samples-1 do begin
     if x mod mod_step eq 0 then begin        
        envi_report_stat, base, x, n_samples-1, cancel=cancel   
        if cancel then goto, cleanup     
     endif
     for y=0,n_lines-1 do begin
        image[x,y,*] = hiihat_normalize_array(image[x,y,*],norm_type)
     endfor
  endfor

cleanup:
  envi_report_init, base=base, /finish 
  if debug then print, "Exiting "+title
end

