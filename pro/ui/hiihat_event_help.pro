;+ 
; Event callback for learn metric menu option.
;
; :Author: David Thompson, Brian Bue
;
; :Categories: 
;  ui
;
; :Params:
;  ev: in, required, type=event
;   input event
;
; :Copyright:
;  Copyright 2011, by the California Institute of Technology. ALL RIGHTS
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
pro hiihat_event_help, ev
    ;; Set compiler and debug options
    compile_opt strictarr
    debug = hiihat_get_config_parm('debug')
    verbose = hiihat_get_config_parm('verbose')
    gui_status = hiihat_get_config_parm('gui_status')

    title='hiihat_event_help'
    if debug then print, "Entering "+title

    sep = path_sep()
    hiihat_path = hiihat_get_config_parm('hiihat_path')
    hiihat_doc_path = strtrim(hiihat_path+"doc"+sep+"Hii-HAT_1.1_userguide.pdf",2)
    online_help,book=hihat_doc_path,/full_path
       
cleanup:
    if debug then print, "Exiting "+title
end
   

