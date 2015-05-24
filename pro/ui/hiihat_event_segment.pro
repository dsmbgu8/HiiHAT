;+ 
; Event callback for image segmentation menu option.
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
pro hiihat_event_segment, ev
    ;; Set compiler and debug options
    compile_opt strictarr
    debug = hiihat_get_config_parm('debug')
    verbose = hiihat_get_config_parm('verbose')
    gui_status = hiihat_get_config_parm('gui_status')

    title='hiihat_event_segment'
    if debug then print, "Entering "+title

    ;; load an image (code from the envi user guide)
    envi_select, fid=in_file_id, title="Select an input image", $
                 dims = use_dims, /NO_SPEC
    if (in_file_id eq -1) then goto, cleanup
    envi_file_query, in_file_id, fname=img_fname

    ;; query segmentation parameters
    hiihat_segment_ask_params, c=c, min_size=min_size, dist_metric=dist_metric, $                             
                               in_memory=in_memory, out_filename=out_filename, $
                               base_filename=img_fname, segment_cancel=segment_cancel
    
    if segment_cancel then begin
       dialog='Segmentation cancelled'
       if not gui_status then print,dialog else ok=dialog_message(dialog,title=title)
       goto, cleanup
    endif

    ;; do the segmentation
    if verbose then print, "Computing segmentation"
    hiihat_segment_felzenszwalb, in_file_id, c, min_size, dist_metric, M=M, $
      out_filename=out_filename, r_fid=segments_fid, use_dims=use_dims, $
      verbose=verbose, in_memory=in_memory, /permute_segments

cleanup:
    if debug then print, "Exiting "+title
end
   

