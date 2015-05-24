;+
; Divide image by mean or mean column spectrum
;
; :Categories: util 
;
; :Params:
;   image, in, required, type="fltarr(ns,nl,nb)"
;    image to filter
;   div_type, in, required, type=string
;    type of division to perform, "None", "Spatial Mean", "Spectral
;    Mean", or "Spectrum"
;
; :Returns:
;   filtered copy of image
; 
; :Examples:
;   The following code divides the mean spectrum out of image I
;
;   hiihat_divide_mean, I, "Spectral Mean"
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
pro hiihat_divide_mean, image, div_type, denom_spectrum=denom_spectrum
  title='hiihat_divide_mean'
  debug =  hiihat_get_config_parm('debug')
  if debug then print, "Entering "+title

  if div_type eq "None" then goto, cleanup

  n_samples = (size(image))[1]
  n_lines = (size(image))[2]
  n_bands = (size(image))[3]

  envi_report_init, "Mean Division", base=base, /INTERRUPT, title=title 
  envi_report_inc, base, n_bands-1

  mod_step = round(n_bands/10)

  ;; divide by mean spectrum
  ;; but you've got to do better than this...
  ;; you can't let negative values into your averaging, unfortunately,
  ;; making the entire calculation take a ton more time
  if div_type eq "Spatial Mean" then begin
     for j=0,(n_bands-1) do begin
        band_sum   = 0.0
        band_count = 0.0
        if j mod mod_step eq 0 then begin        
           envi_report_stat, base, j, n_bands-1, cancel=cancel   
           if cancel then goto, cleanup     
        endif
        for x=0,n_samples-1 do begin
           for y=0,n_lines-1 do begin
              if image[x, y, j] gt 0.0 then begin
                 band_sum = band_sum + image[x,y,j]
                 band_count = band_count + 1.0
              endif
           endfor
        endfor
        image[*,*,j] = image[*,*,j]/band_sum * band_count
     endfor
  endif else if div_type eq "Spectral Mean" then begin
     ;; divide by mean spectrum FOR EACH COLUMN
     ;; but you've got to do better than this...
     ;; you can't let negative values into your averaging, unfortunately,
     ;; making the entire calculation take a ton more time
     ;; column dependence is to remove striping noise effects
     for j=0,(n_bands-1) do begin
        if j mod mod_step eq 0 then begin        
           envi_report_stat, base, j, n_bands-1, cancel=cancel   
           if cancel then goto, cleanup     
        endif
        for x=0,n_samples-1 do begin
           ;; print, x, j
           band_sum   = 0.0
           band_count = 0.0
           for y=0,n_lines-1 do begin
              if image[x, y, j] gt 0.0 then begin
                 band_sum = band_sum + image[x,y,j]
                 band_count = band_count + 1.0
              endif
           endfor
           image[x,*,j] = image[x,*,j]/band_sum * band_count
        endfor        
     endfor
  endif else if div_type eq "Spectrum" then begin
    ;; divide the image by the given spectrum
    if keyword_set(denom_spectrum) then begin
      for j=0,(n_bands-1) do begin
        image[*,*,j] = image[*,*,j]/denom_spectrum[j]
      endfor
    endif else begin
        print, "Denominator spectrum is not specified."
    endelse
  endif else begin 
     print, "Unknown div_type: ", div_type     
  endelse
  cleanup:
  envi_report_init, base=base, /finish 
  if debug then print, "Exiting "+title
end
