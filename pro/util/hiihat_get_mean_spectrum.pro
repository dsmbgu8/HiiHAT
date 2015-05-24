;+ 
; Calculates the mean spectrum from a set of pixels, detecting bad
; (infinite) and zeroed pixels
;
; :Categories:
;  util
;
; :Author: David Ray Thompson and Brian D. Bue
;
; :Params:
;  pixels: in, required, type="fltarr(n,nb)"
;    set of n input pixels of dimension nb 
; :Keywords:
;  mean_spectrum: out, required, type="fltarr(nb)"
;    output mean spectrum
;  bad_pixels: out, optional, type=fix
;    number of bad pixels 
;  zero_pixels: out, optional, type=fix
;    number of zeroed pixels
;
; :Copyright:
;  Copyright 2010, by the California Institute of Technology. ALL RIGHTS
;  RESERVED. United States Government Sponsorship acknowledged. Any commercial
;  use must be negotiated with the Office of Technology Transfer at the
;  California Institute of Technology.
; 
;  This software may be subject to U.S. export control laws and regulations.  By
;  accepting this document, the user agrees to comply with all applicable U.S.
;  export laws and regulations.  User has the responsibility to obtain export
;  licenses, or other export authority as may be required before exporting such
;  information to foreign countries or providing access to foreign persons.
;-
pro hiihat_get_mean_spectrum, pixels, mean_spectrum=mean_spectrum, $
                              bad_pixels=bad_pixels, zero_pixels=zero_pixels

  ;; Set compiler and debug options
  compile_opt strictarr
  debug = hiihat_get_config_parm('debug')
  verbose = hiihat_get_config_parm('verbose')
  robust = hiihat_get_config_parm('robust_means')
  
  title='hiihat_get_mean_spectrum'
  ;;if debug then print, "Entering "+title
  finite_max = hiihat_get_config_parm('finite_max')
  if finite_max eq 0 then finite_max = 99999
  
  robust_min = 0
  robust_max = finite_max            

  pix_sz = size(pixels)
  n_pix = pix_sz[1]
  n_b = pix_sz[2]

  bad_pixels = 0
  zero_pixels = 0
  ;; robust mean
  if robust then begin            
     ;; infinities? or zeros? zero-out the mean_spectrum
     bad_pixels = ulong(total(total((finite(pixels) and (pixels lt robust_max)),2) lt n_b))
     zero_pixels = ulong(total(total((pixels eq 0),2) eq n_b))

     if (bad_pixels gt 0) then begin
        mean_spectrum = fltarr(n_b)
     endif else begin
        if (n_pix gt 1) then begin
           mean_spectrum = total(pixels, 1)/float(n_pix)
        endif else begin
           mean_spectrum = reform(pixels)
        endelse
     endelse                 
  endif else begin          
     if (n_pix gt 1) then begin
        mean_spectrum = total(pixels, 1)/float(n_pix)
     endif else begin
        mean_spectrum = reform(pixels)
     endelse            
  endelse

cleanup:
    ;;if debug then print, "Exiting "+title
end
