;+
; Assert that a given statement is true, otherwise clear the system of all files, ROI's
;
; :Categories:
;   util 
;
; :Params:
;   statement: in, required, type=boolean
;    boolean function to assert
;   text: in, required, type=string
;    error message if assertion fails
;
; :Keywords:
;   title: in, optional, type=string 
;    title for dialog message box
;
; :Returns:
;   returns all functions if assertion fails
; 
; :Author: Lukas Mandrake
;
; :History:
;  2009 (LM): initially written
;
;  Dec 05, 2010 (BDB): added docstr
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
pro hiihat_assert, statement, text, title=title
  if not statement then begin
     error_msg = "ASSERT FAILURE: "+text
     if keyword_set(title) then begin ;; if title set, use a dialogbox
        ok = dialog_message(error_msg, title=title) 
     endif else print,error_msg
     retall
     stop
  endif
end
