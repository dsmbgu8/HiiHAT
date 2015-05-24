;+ 
; Event callback for neutral region detection function
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
pro hiihat_event_find_neutral, ev 
    ;Set compiler and debug options
    compile_opt strictarr
    debug = hiihat_get_config_parm('debug')
    verbose = hiihat_get_config_parm('verbose') 
    gui_status = hiihat_get_config_parm('gui_status') 

    title='hiihat_event_find_neutral'
    if debug then print, "Entering "+title

    ;; load an image
    envi_select, fid=img_fid, $
        title="Select an input image (spatial subset permitted)",$
        dims = use_dims
    if (img_fid eq -1) then goto, cleanup
    
    ; select a segmentation image 
    envi_select, fid=seg_fid, title=$
            "Select a segmentation, or 'cancel' to recompute"

    ; do a new segmentation, if required
    seg_in_memory = 0
    if (seg_fid eq -1) then begin
        if verbose then print,"Performing segmentation"
        hiihat_segment_ask_params, c=c, min_size=min_size, $
          dist_metric=dist_metric, $
          segment_cancel=segment_cancel

        if segment_cancel then begin
           dialog = 'Segmentation cancelled'
           if not gui_status then print, dialog else ok = dialog_message(dialog, title=title)
           goto, cleanup
       endif

       hiihat_segment_felzenszwalb, img_fid, c, min_size, dist_metric, $
         r_fid=seg_fid, use_dims = use_dims, /in_memory
       
        seg_in_memory = 1
    endif 

    ; select an output file
    dialog="Select an output file for the neutral spectrum"
    out_filename = envi_pickfile(title=dialog, output=1)
    write_status = hiihat_check_file(out_filename,write=1)
    if write_status eq 0 then begin
       goto, cleanup
    endif else if write_status eq 2 then begin
       ok = dialog_message("File exists, overwrite?", title=title, /cancel)
       if (strupcase(ok) eq 'CANCEL') then return
    endif

    ; select an output image file
    dialog="Select an output file for the ratioed image"
    ratioed_out_filename = envi_pickfile(title=dialog, output=1)
    write_status = hiihat_check_file(ratioed_out_filename,write=1)
    if write_status eq 0 then begin
       goto, cleanup
    endif else if write_status eq 2 then begin
       ok = dialog_message("File exists, overwrite?", title=title, /cancel)
       if (strupcase(ok) eq 'CANCEL') then return
    endif

    ; call the neutral region routine
    hiihat_get_neutral_region, img_fid=img_fid, seg_fid=seg_fid, $
            roi_out_id = roi_out_id, spectrum_out_filename=out_filename,$
            ratioed_out_filename=ratioed_out_filename,$
            coalesce_threshold=coalesce_threshold, use_dims = use_dims,$
            verbose=verbose

cleanup:
    ; clean up
    if (seg_in_memory) then envi_file_mng, id=seg_fid, /remove
    if debug then print, "Exiting "+title
end
  

