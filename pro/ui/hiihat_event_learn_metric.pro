;+ 
; Event callback for learn metric menu option.
;
; :Author: Brian Bue
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
pro hiihat_event_learn_metric, ev
    ;; Set compiler and debug options
    compile_opt strictarr
    debug = hiihat_get_config_parm('debug')
    verbose = hiihat_get_config_parm('verbose')
    gui_status = hiihat_get_config_parm('gui_status')

    title='hiihat_event_learn_metric'
    if debug then print, "Entering "+title

    ;; load an image (code from the envi user guide)
    envi_select, fid=in_file_id, title="Select an input image", $
                 dims = use_dims, pos=pos
    if (in_file_id eq -1) then begin
       dialog='Error opening file '
       if not gui_status then print, dialog else ok=dialog_message(dialog,title=title)                                      
       goto, cleanup
    endif

    envi_file_query, in_file_id, fname=img_fname

    hiihat_learn_metric_ask_params, reg_parms=reg_parms, max_samples=max_samples, $
                                    in_memory=in_memory, out_filename=out_filename, $
                                    out_matrix_filename=out_matrix_filename, $
                                    base_filename=img_fname, matrix_in_memory=matrix_in_memory, $
                                    learn_metric_cancel=learn_metric_cancel

   if learn_metric_cancel then begin
       dialog='Metric learning cancelled'
       if not gui_status then print,dialog else ok=dialog_message(dialog,title=title)
       goto, cleanup
    endif  

    if verbose then print, "Learning metric"
    hiihat_learn_metric, in_file_id, reg_parms=reg_parms, max_samples=max_samples, $
                         out_filename=out_filename, in_memory=in_memory, $
                         out_matrix_filename=out_matrix_filename, matrix_in_memory=matrix_in_memory, $
                         matrix_r_fid=matrix_r_fid, use_dims=use_dims, $
                         pos=pos, verbose=verbose
       
cleanup:
    if debug then print, "Exiting "+title
end
   

