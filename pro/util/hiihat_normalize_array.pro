;+
; Normalize an input array by the L1, L2, or L Inf norm
;
; :Categories:
;   util 
;
; :Params:
;   array, in, required, type="fltarr(n,1)"
;     array to normalize
;   norm_type, in, required, type=string
;     normalization function to use 
;
; :Returns:
;   normalized copy of array
; 
; :Examples:
;   The following code will normalize the input array x by the L2 norm
;
;   normed_x = hiihat_normalize_array(x, 'Euclidean (L 2)')
;
; :Author: David Ray Thompson and Brian D. Bue
; :History:
;  2009 (DRT): initially written in hiihat_preprocess.pro
;  Dec 05, 2010 (BDB): moved into separate routine
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
function hiihat_normalize_array, array, norm_type

    ;handle wrong data size entered for array
;    b=size(array)
;    if b[0] lt 0 or b[0] gt 1 then begin
;        print, "Error... norm can only handle a vector"
;        exit
;    endif

    ; handle no norm requested
    if norm_type eq 'None'            then return, array
    
    ; handle empty array
    if total(array) eq 0             then return, array

    if norm_type eq 'Area (L 1)'      then return, array/float(abs(total(array      )))
    if norm_type eq 'Euclidean (L 2)' then return, array/sqrt (total(array*array))
    if norm_type eq 'Peak (L inf)'    then return, array/float(max(array))

end
