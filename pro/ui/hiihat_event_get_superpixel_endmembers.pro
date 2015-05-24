;+ 
; Event callback for superpixel endmember detection menu option.
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
pro hiihat_event_get_superpixel_endmembers, ev 

    ;Set compiler and debug options
    compile_opt idl2
    debug = hiihat_get_config_parm('debug')
    verbose = hiihat_get_config_parm('verbose')
    gui_status = hiihat_get_config_parm('gui_status')

    title='hiihat_event_get_superpixel_endmembers'
    if debug then print, 'Entering '+title

    ; load an image
    envi_select, fid=img_fid, title="Select an input image", dims = use_dims, $
      /NO_SPEC
    if (img_fid eq -1) then begin
       if verbose then print, 'Invalid input image'
       goto, cleanup
    endif
    envi_file_query, img_fid, fname=img_fname

    ;; select a segmentation image 
    dialog="Select a segmentation, or 'cancel' to recompute"
    envi_select, fid=seg_fid, title=dialog, /NO_DIMS, /NO_SPEC

    hiihat_endmember_ask_params, coalesce_threshold=coalesce_threshold, base_filename=img_fname, $
                                 n_endmembers=n_endmembers, method=method, $;seg_fid=seg_fid, $
                                 out_filename=out_filename, roi_out_filename=roi_out_filename, $
                                 endmember_cancel=endmember_cancel
    if endmember_cancel then begin
       if verbose then print, 'Endmember detection cancelled'
       goto, cleanup
    endif

    ;; do a new segmentation, if required
    if (seg_fid eq -1) then begin ;; in-memory
        if verbose then print,"Performing segmentation"
        hiihat_segment_ask_params, c=c, min_size=min_size, out_filename=seg_out_filename, $
                                   dist_metric=dist_metric, in_memory=seg_in_memory, $
                                   base_filename=img_fname, segment_cancel=segment_cancel
        if segment_cancel then begin
           ;;ok = dialog_message('Segmentation cancelled', title=title)
           goto, cleanup
        endif

        hiihat_segment_felzenszwalb, img_fid, c, min_size, dist_metric, permute_segments=1, $
                                     r_fid=seg_fid, use_dims = use_dims, verbose=verbose, $
                                     in_memory=seg_in_memory, out_filename=seg_out_filename
                                     
    endif else begin
        envi_file_query, img_fid, nb = nb , nl = nl
        envi_file_query, seg_fid, nb = nb_seg, nl = nl_seg
        hiihat_assert, (nb ne nb_seg) or (nl ne nl_seg), "Image and segmentation file geometry must match" 
        seg_in_memory = 0
    endelse

    use_nfindr = (method eq "NFINDR")
    if roi_out_filename ne "" then begin 
        ;; call the endmember routine and write ROIs
       hiihat_get_superpixel_endmembers, img_fid, seg_fid=seg_fid, $
              n_endmembers = n_endmembers, out_name_roi = roi_out_filename, $
              out_name_sli =out_filename, coalesce_threshold=coalesce_threshold, $
              use_dims = use_dims, verbose=verbose, use_nfindr=use_nfindr

    endif else begin      
       ;; get endmembers without writing ROIs
       hiihat_get_superpixel_endmembers, img_fid, seg_fid=seg_fid, n_endmembers = n_endmembers, $
              out_name_sli =out_filename, coalesce_threshold=coalesce_threshold, $
              use_dims = use_dims, verbose=verbose, use_nfindr=use_nfindr
        
    endelse

    if verbose then print,"Endmember ROI's and spectra are now available"
cleanup:
    ; clean up
    ;;if seg_in_memory then envi_file_mng, id=seg_fid, /remove
    if debug then print,'Exiting '+title
end
  
