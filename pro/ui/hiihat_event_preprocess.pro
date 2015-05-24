;+ 
; Event callback for preprocessing menu option.
;
; :Author: David Ray Thompson
;
; :Categories: 
;  ui
;
; :Params:
;  ev: in, required, type=event
;   input event
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
pro hiihat_event_preprocess, ev
    compile_opt strictarr
    verbose =  hiihat_get_config_parm('verbose')
    debug =  hiihat_get_config_parm('debug')
    gui_status =  hiihat_get_config_parm('gui_status')
    title='hiihat_event_preprocess'

    if debug then print, "Entering "+title

    ; load an image (code from the envi user guide)
    envi_select, fid=in_file_id, title="Select an input image", dims=use_dims, /NO_SPEC
    if (in_file_id eq -1) then goto, cleanup
    envi_file_query, in_file_id, fname=in_fname, descrip=descrip

    if keyword_set(descrip) then begin
       ;; check if file has already been flagged as preprocessed 
       if strmatch(descrip, '*hiihat_pp*',/fold_case) then begin
          dialog = 'Image has already been preprocessed, continue?'
          ok = dialog_message(dialog, title=title, /cancel)
          if (strupcase(ok) eq 'CANCEL') then begin
             goto, cleanup
          endif
       endif 
    endif

    hiihat_preprocess_ask_params, $
       image_type = image_type,$
       filter_negative = filter_negative, $
       median_filter_width = median_filter_width, $
       norm_type = norm_type, $ 
       div_type = div_type, $ 
       lowpass_filter = lowpass_filter, $
       out_filename = out_filename, $
       base_filename = in_fname, $
       in_memory = in_memory, $
       preprocess_cancel = preprocess_cancel

    if preprocess_cancel then begin 
       dialog='Preprocessing cancelled'
       if not gui_status then print,dialog else ok=dialog_message(dialog,title=title)
       goto, cleanup
    endif

    hiihat_preprocess, in_file_id, $
                       out_filename=out_filename, $
                       image_type = image_type, $
                       filter_negative = filter_negative, $
                       median_filter_width = median_filter_width, $
                       norm_type = norm_type, $
                       div_type = div_type, $
                       lowpass_filter = lowpass_filter, $
                       use_dims = use_dims, $
                       in_memory=in_memory,$
                       verbose = verbose
                       
cleanup:
    if debug then print, "Exiting "+title
end

