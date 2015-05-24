;+ 
; Event callback for reload config file menu option.
;
; :Categories: ui
;
; :Author: Brian Bue
;
; :Params:
;  ev: in, required, type=event
;    input event
;
; :Copyright:
;
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
pro hiihat_event_config_reload, ev
  title="hiihat_event_config_reload"
  debug = hiihat_get_config_parm('debug')
  verbose = hiihat_get_config_parm('verbose')
  if debug then print, "Entering "+title
  cfg=hiihat_parse_config_file(verbose=verbose)
  defsysv, "!hiihat_config", cfg
  if debug then print, "Exiting "+title
end 
