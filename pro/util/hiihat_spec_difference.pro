;+
; returns a floating point value representing distance between two spectra
; :Categories: segmentation
; :Params:
;  adata: in, required, type="fltarr(nb)"
;   first input spectrum
;  bdata: in, required, type="fltarr(nb)"
;   second input spectrum
;  dist_metric: in, required, type=string
;   distance metric between adata and bdata
; 
; :Keywords:
;  reject_zeros: in, optional, type=boolean
;   reject zero or infinite spectra
;  M: in, optional, type="fltarr(nb,nb)" 
;   tranformation matrix for Mahalanobis distance function, if unspecified,
;   reverts to euclidean distance
;
; :History:
;   2009 (DRT): Initial implementation
;
;   Dec 31, 2010 (BDB): docstr added
;  
; :Author: David Ray Thompson and Brian D. Bue
;
; :Copyright:
;  Copyright 2009, by the California Institute of Technology. ALL RIGHTS
;  RESERVED. United States Government Sponsorship acknowledged. Any commercial
;  use must be negotiated with the Office of Technology Transfer at the
;  California Institute of Technology.
; 
;  This software may be subject to U.S. export control laws and regulations.  By
;  accepting this document, the user agrees to comply with all applicable U.S.
;  export laws and regulations.  User has the responsibility to obtain export
;  licenses, or other export authority as may be required before exporting such
;  information to foreign countries or providing access to foreign persons.
;
;-
function hiihat_spec_difference, adata, bdata, dist_metric, $
                                 reject_zeros=reject_zeros, M=M
  SMALLEST = 1e-40
  HUGE     = 1e40
  ;; We often don't want to ever merge with any entirely zero spectra (bad
  ;; data) or infinity values, but we do want those strange values to 
  ;; cluster together
  
  if keyword_set(reject_zeros) then begin
     tadata = total(adata)
     tbdata = total(bdata)
     if tadata lt SMALLEST and tbdata gt 0.0      then return, HUGE
     if tadata gt 0.0      and tbdata lt SMALLEST then return, HUGE
     if tadata lt SMALLEST and tbdata lt SMALLEST then return, 0.0
     iadata = n_elements(adata) - total(finite(adata))
     ibdata = n_elements(bdata) - total(finite(bdata))
     if iadata gt 0        and ibdata eq 0        then return, HUGE
     if iadata eq 0        and ibdata gt 0        then return, HUGE
     if iadata gt 0        and ibdata gt 0        then return, 0.0
  endif

  if dist_metric eq 'mahalanobis' and not keyword_set(M) then begin
     print, 'Mahalanobis metric requires nb x nb matrix "M." Reverting to Euclidean metric.'
     dist_metric = 'euclidean'
  endif

  case dist_metric of
     ;; Spectral Angle version (we generally don't use this anymore)
     'sad': return, acos(total(adata*bdata)/sqrt(total(adata*adata)*total(bdata*bdata)))
     ;; euclidean distance
     'euclidean': return, sqrt(total((adata-bdata)^2))
     ;; squared absolute difference version (not normalized)
     'euclidean_sq': return, total((adata-bdata)^2)
     ;; euclidean distance of L1-normalized spectra
     'euclidean_n': return, sqrt(total((adata/total(adata) - $
                                        bdata/total(adata))^2))
     'mahalanobis': begin 
        return, sqrt(transpose(adata-bdata) # M # transpose(adata-bdata))
     end
  endcase
end
