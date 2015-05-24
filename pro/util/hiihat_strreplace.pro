;+
; Replaces first instance of one string inside another, works on
; arrays as well
; :Categories: util
; :Author: Lukas Mandrake
; :Params:
;  instrings: in, required, type=string
;   input string
;  str2find: in, required, type=string
;   string to locate within instrings
;  str2sub: in, required, type=string
;   string to substitute for str2find
;
; :Keywords:
;  ignore_case: in, optional, type=boolean
;   ignore case in string search
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
;-
function hiihat_strreplace, instrings, str2find, str2sub, ignore_case=ignore_case

;;   Check integrity of input parameter

  NP        = N_PARAMS()
  if (NP ne 3) then message,'Must be called with 3 parameters, '+$
                            'instrings, found, replaced'

  newstrings = instrings

  sz        = SIZE(newstrings)
  ns        = n_elements(sz)
  if (sz(ns-2) ne 7) then message,'Parameter must be of string type.'

  found      = STRING(str2find)
  
  ;; do the find on the lowered string if ignore_case is enabled
  if keyword_set(ignore_case) then begin 
     newstrings = strlowcase(newstrings)
     found = strlowcase(str2find)
  endif

  pos       = STRPOS(newstrings,found)
  here      = WHERE(pos ne -1, nreplace)

  if (nreplace eq 0) then return,instrings

  replaced=STRING(str2sub)
  Flen      = strlen(found)
  for i=0,nreplace-1 do begin
     j         = here(i)
     prefix    = STRMID(instrings(j),0,pos(j))
     suffix    = STRMID(instrings(j),pos(j)+Flen,$
                        strlen(instrings(j))-(pos(j)+Flen))
     newstrings(j) = prefix + replaced + suffix
  endfor

  return,newstrings
end
