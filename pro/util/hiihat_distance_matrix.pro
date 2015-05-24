;+
; Calculate a distance matrix between a set of spectra
;
; :Categories:
;   util 
;
; :Params:
;   dist_metric, in, required, type=string
;     distance function to use 
;   spectraA, in, required, type="fltarr(nA,nb)"
;     list of nA spectra to compare
;   spectraB, in, optiona, type="fltarr(nB,nb)"
;     if specified, calculate nA x nB distance matrix between spectraA and
;     spectraB
;
; :Returns:
;   nA x nA or nA x nB distance matrix
; 
; :Examples:
;   The following code will calculate the nA x nB distance matrix between sA and sB
;
;   dmtxAB = hiihat_distance_matrix('euclidean', sA, sB)
;
; :Author: Brian D. Bue
; :History:
;  Jan 11, 2011 (BDB): initially written
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
function hiihat_distance_matrix, dist_metric, spectraA, spectraB
  nA = (size(spectraA))[1]
  if n_elements(spectraB) ne 0 then begin
     if (size(spectraA))[2] ne (size(spectraB))[2] then begin
        print, size(spectraA), size(spectraB)
        print, 'Error: comparing spectra of different dimensionality'
        return, 0
     endif
     nB = (size(spectraB))[1]
  endif else begin 
     nB = nA
     spectraB = spectraA     
  endelse

  dmtxAB = fltarr(nA,nB)
  for i=0, nA-1 do begin
     for j=0, nB-1 do begin
        dmtxAB[i,j] = hiihat_spec_difference(spectraA[i,*],spectraB[j,*],dist_metric)
     end
  end
  
  return, dmtxAB
end
