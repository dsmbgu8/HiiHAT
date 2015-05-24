;+ 
; Event callback for autoclass menu option. 
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
pro hiihat_event_autoclass, ev     
    compile_opt strictarr

    title='hiihat_event_autoclass'

    ;; default parameters
    verbose = hiihat_get_config_parm('verbose')
    debug = hiihat_get_config_parm('debug')
    gui_status = hiihat_get_config_parm('gui_status')
    robust_means = hiihat_get_config_parm('robust_means')

    if debug then print, "Entering "+title

    ;; load an image (code from the envi user guide)
    envi_select, fid=unclipped_fid, title="Select an input image", $
                 dims = use_dims, /NO_SPEC
    if (unclipped_fid eq -1) then begin
       dialog = 'Invalid input image'
       if not gui_status then print, dialog else ok=dialog_message(dialog, title=title)                 
       goto, cleanup
    endif
    envi_file_query, unclipped_fid, wl = unclipped_wl, bnames = unclipped_bnames, $
                     dims=unclipped_dims, wavelength_units = wavelength_units, $
                     fwhm=unclipped_fwhm, fname=fname

    if keyword_set(use_dims) then unclipped_dims = use_dims

    ;; get preprocessing params
    hiihat_preprocess_ask_params, image_type = image_type, base_filename=fname, $
                                  median_filter_width = median_filter_width, filter_negative=filter_negative, $
                                  norm_type = norm_type, div_type = div_type, $
                                  in_memory=preprocess_in_memory, out_filename=pp_out_filename, $
                                  preprocess_cancel = preprocess_cancel
    if preprocess_cancel then begin
       dialog='Preprocessing cancelled: no preprocessing will be perfomed'
       if not gui_status then print, dialog else ok=dialog_message(dialog, title=title)
    endif

    if not keyword_set(pp_out_filename) then pp_out_filename = fname

    ; query segmentation paramteters
    hiihat_segment_ask_params, c=c, min_size=min_size, dist_metric=dist_metric, $
                               base_filename=pp_out_filename, out_filename=seg_filename, $
                               in_memory=seg_in_memory, segment_cancel=segment_cancel
    if segment_cancel then begin
       dialog = 'Segmentation cancelled'
       if not gui_status then print, dialog else ok=dialog_message(dialog, title=title)
       goto, cleanup
    endif

    ;; query endmember extraction parameters
    hiihat_endmember_ask_params, coalesce_threshold=coalesce_threshold, $
                                 base_filename=pp_out_filename, n_endmembers=n_endmembers, $
                                 out_filename=endm_filename, roi_out_filename=roi_out_filename, $
                                 method=endm_method, endmember_cancel=endmember_cancel
    if endmember_cancel then begin
       dialog='Endmember detection cancelled'
       if not gui_status then print, dialog else ok=dialog_message(dialog, title=title)    
       goto, cleanup
    endif

    ;; choose classification parms
    hiihat_classify_ask_params, class_method=class_method, method=method, base_filename=pp_out_filename, $
                                in_memory=class_in_memory, out_filename=class_out_filename, $
                                rule_in_memory=rule_in_memory, rule_out_filename=rule_out_filename, $
                                classify_cancel=classify_cancel                                                                                               
    if classify_cancel then begin
       dialog='Classification cancelled'
       if not gui_status then print, dialog else ok=dialog_message(dialog, title=title)    
       goto, cleanup
    endif

    if not preprocess_cancel then begin
       if verbose then print, "Preprocessing"       
       hiihat_preprocess, unclipped_fid, image_type = image_type, filter_negative=filter_negative, $
                          r_fid = img_fid, norm_type=norm_type, div_type=div_type, $
                          median_filter_width = median_filter_width, use_dims=unclipped_dims, $
                          in_memory=preprocess_in_memory, out_filename=pp_out_filename
       envi_file_query, img_fid, dims=img_dims, fwhm=img_fwhm, wl=img_wl, $
                        bnames=img_bnames
    endif else begin
       dialog="No preprocessing performed, using raw input image"
       if not gui_status then print, dialog else ok=dialog_message(dialog, title=title)    
       img_fid = unclipped_fid
       img_dims = unclipped_dims
       img_fwhm = unclipped_fwhm
       img_wl = unclipped_wl
       img_bnames = unclipped_bnames
    endelse


    ;; check if there is a transform available for this image type
    envi_file_query, img_fid, descrip=descrip
    image_pp_type=(stregex(descrip,'.*hiihat_pp: ([^\.]+)\.*', $
                        /EXTRACT, /FOLD_CASE, /SUBEXPR))[1]
    if image_pp_type eq '' then image_type = 'Unknown'
    hiihat_transform_ask_params, image_pp_type, trans_filename=trans_filename, $
                                 out_filename=trans_out_filename, in_memory=trans_in_memory, $
                                 max_rank=max_rank, base_filename=pp_out_filename, $ 
                                 verbose=verbose, transform_cancel=transform_cancel    

    if not transform_cancel then begin
       envi_open_file, trans_filename, r_fid=matrix_fid 
       if (matrix_fid eq -1) then begin
          dialog='Error opening transform file '
          if not gui_status then print, dialog else ok=dialog_message(dialog,title=title)                                      
          goto, cleanup
       endif

       if verbose then print, "Transforming image"  
       hiihat_apply_metric, img_fid, matrix_fid, trans_out_filename, r_fid = trans_r_fid,$
                            use_dims=use_dims, in_memory=trans_in_memory, verbose=verbose,$
                            rank=max_rank
    endif


    if verbose then print, "Segmenting"
    hiihat_segment_felzenszwalb, img_fid, c, min_size, dist_metric, $
                               r_fid=seg_fid, out_filename=seg_filename, $
                               in_memory=seg_in_memory

    if verbose then print, "Getting endmembers using method ", endm_method
    use_nfindr = (endm_method eq 'NFINDR')
    hiihat_get_superpixel_endmembers, img_fid, seg_fid=seg_fid, $
                                      n_endmembers=n_endmembers, $ ;number of endmembers to request
                                      out_name_roi=roi_out_filename, $ ;roi endmember file outpath
                                      out_name_sli=endm_filename, $ ; name of the sli file to generate for endmember spectra                                      
                                      ;out_name_txt=endm_filename , $ ; the name of the txt wavelength file for endmember spectra
                                      r_fid=endmember_fid, $ ; return fid for the endmember spectra
                                      abund_r_fid=abund_r_fid, $ ; return fid for abundance image
                                      coalesce_threshold=coalesce_threshold, $ ;
                                      ;use_dims=img_dims, $ ; subsection of the image to process
                                      use_nfindr=use_nfindr, $ ; Use the NFINDR algorithm (custom HIIHAT implementation)
                                      ;ignore_segmentation=ignore_segmentation, $ ; Use entire image unsegmented (for comparison)
                                      ;seed=seed, $ ; if used, the random seed for the NFINDR algorithm (optional)
                                      ;in_memory=in_memory, $
                                      verbose=verbose, $
                                      gui_status=gui_status

    if not keyword_set(endmember_fid) then begin
       dialog='Endmember detection failed'
       if not gui_status then print, dialog else ok=dialog_message(dialog, title=title)    
       goto, cleanup
    endif    

    ;; get the data back
    envi_file_query, endmember_fid, ns = ns, dims = endmember_dims
    endmembers = envi_get_data(fid=endmember_fid, dims=endmember_dims, pos=0)

    hiihat_generate_class_colors, n_endmembers, class_names=class_names, $
                                  colors=colors

    pos = indgen(ns) ;; ns is the number of bands in the spectral library
    ; do the classification
    if verbose then print, "Classifying using method ",class_method
    envi_doit, 'CLASS_DOIT', fid=img_fid, mean=endmembers,$
        class_names = class_names, r_fid=r_fid, dims=img_dims, $
        method=method, out_name = class_out_filename, in_memory=class_in_memory, $
        pos=pos, lookup=colors, rule_in_memory=rule_in_memory, $
        rule_fid=rule_fid, rule_out_name=rule_out_filename

cleanup:
    if debug then print, "Exiting "+title
end
  

