;+
; Event callback for mean image calculation menu option.
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
pro hiihat_event_mean_image, ev
    ;; Set compiler and debug options
    compile_opt strictarr
    debug = hiihat_get_config_parm('debug')
    verbose = hiihat_get_config_parm('verbose')

    title='hiihat_event_mean_image'
    if debug then print, "Entering "+title

    ;; load the image (code from the envi user guide)
    envi_select, fid=in_file_id, title="Select an input image", dims = use_dims, $
      /NO_SPEC
    if (in_file_id eq -1) then return

    ;; load the segmentation image (code from the envi user guide)
    envi_select, fid=seg_file_id, title="Select an input segmentation", dims = use_dims, $
      /NO_SPEC
    if (seg_file_id eq -1) then return

    ; select an output file
    in_memory = 0
    dialog = "Select an output file, or 'cancel' to compute in memory"
    out_filename = envi_pickfile(title=dialog, output=1)
    write_status = hiihat_check_file(out_filename,write=1)
    if write_status eq 0 then begin
       errmsg = "Invalid output file, computing in memory"
       in_memory=1
    endif else if write_status eq 2 then begin
       ok = dialog_message("File exists, overwrite?", title=title, /cancel)
       if (strupcase(ok) eq 'CANCEL') then return
    endif

    ;; calculate the mean image
    if verbose then print, "Calculating mean image"
    if in_memory then begin       
       hiihat_segment_spectra, in_file_id, seg_file_id, spectra=spectra, $
                               r_fid = r_fid, verbose=verbose, mean_fid=mean_fid, $
                               return_image=1
    endif else begin       
       hiihat_segment_spectra, in_file_id, seg_file_id, spectra=spectra, $
                               r_fid = r_fid, verbose=verbose, mean_fid=mean_fid, $
                               mean_image_name=out_filename, return_image=1
    endelse

    if debug then print, "Exiting "+title
end
   

