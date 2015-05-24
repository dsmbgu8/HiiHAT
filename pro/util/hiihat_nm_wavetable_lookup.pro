;+
;  Seeks the closest nm wavelength in a given wavelength vector and returns
;  its band number
;
; :Categories:
;   util
;
; :Author:  David Ray Thompson & Lukas Mandrake
;
; :Params:
;  target_nm: in, required, type=fix
;    target value in nanometers
;  wvt: in, required, type="fltarr(nb,1)"
;    wavelength table in nm
; 
; :History:
;  2009 (DRT, LM): initially implemented
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
function hiihat_nm_wavetable_lookup, target_nm, wvt
     distance = min(abs(wvt - target_nm),min_sub)
     return, min_sub
end


