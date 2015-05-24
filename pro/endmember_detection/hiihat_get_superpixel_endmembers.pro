;+ 
; Perform endmember detection using either the SMACC or N-FINDR algorithm.
;
; :Categories: endmember_detection
;
; :Author: David Ray Thompson
;
; :Params:
;  img_fid: in, required, type=fix
;    file id of input image
;
; :Keywords:
;  seg_fid: in, required, type=fix
;    file id of input image segmentation map. required if ignore_segmentation is false
;  n_endmembers: in, required, type=fix
;    number of endmembers to extract
;  out_name_roi: in, optional, type=string
;    output file for saving ROIs
;  out_name_sli: in, optional, type=string
;    output file for saving endmember spectral library
;  out_name_txt: in, optional, type=string
;    output file for saving spectral library (txt format)
;  r_fid: out, optional, type=fix
;    file id of output endmember spectral library
;  abund_r_fid: out, optional, type=fix
;    file id of output abundance image (SMACC only)
;  coalesce_threshold: in, optional, type=float
;    SMACC coalesce threshold
;  use_dims: in, optional, type=size
;    user-specified dimensions to process
;  use_nfindr: in, optional, type=boolean
;    use the N-FINDR endmember extraction algorithm instead of SMACC
;  ignore_segmentation: in, optional, type=boolean
;    segment image pixels rather than image superpixels (as defined by the
;    segmentation map)
;  seed: in, optional, type=float
;    random seed for N-FINDR endmember extraction
;  verbose: in, optional, type="boolean"
;    enable verbose status messages
;  in_memory: in, optional, type=boolean
;    compute endmembers in memory instead of writing to file
;  gui_status: in, optional, type=boolean
;    output gui-based status updates for non-essential output
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
pro hiihat_get_superpixel_endmembers, img_fid, seg_fid=seg_fid, $
                                      n_endmembers=n_endmembers, $ ;number of endmembers to request
                                      out_name_roi=out_name_roi, $ ;roi endmember file outpath
                                      out_name_sli=out_name_sli, $ ; name of the sli file to generate for endmember spectra                                      
                                      out_name_txt=out_name_txt , $ ; the name of the txt wavelength file for endmember spectra
                                      r_fid=endmember_fid, $ ; return fid for the endmember spectra
                                      abund_r_fid=abund_r_fid, $ ; return fid for abundance image
                                      coalesce_threshold=coalesce_threshold, $ ;
                                      use_dims=use_dims, $ ; subsection of the image to process
                                      use_nfindr=use_nfindr, $ ; Use the NFINDR algorithm (custom HIIHAT implementation)
                                      ignore_segmentation=ignore_segmentation, $ ; Use entire image unsegmented (for comparison)
                                      seed=seed, $ ; if used, the random seed for the NFINDR algorithm (optional)
                                      in_memory=in_memory, $
                                      verbose=verbose, $
                                      gui_status=gui_status

  compile_opt IDL2              ; strictarr

  debug = hiihat_get_config_parm('debug')  
  backup_rois = hiihat_get_config_parm('backup_rois')
  check_endmember_redundancy = hiihat_get_config_parm('check_endmember_redundancy')

  title='hiihat_get_superpixel_endmembers'
  if debug then print, "Entering "+title 

  if not keyword_set(verbose) then verbose=0 
  if not keyword_set(gui_status) then gui_status=0
  if not keyword_set(in_memory) then in_memory=0
  if not keyword_set(ignore_segmentation) then ignore_segmentation=0
  if not keyword_set(use_nfindr) then use_nfindr=0
  if not keyword_set(check_endmember_redundancy) then check_endmember_redundancy=0

  dialog='Must specify a segmentation file unless ignore_segmentation=1'
  hiihat_assert, ignore_segmentation or keyword_set(seg_fid), dialog, title=title
                 


  if verbose then print, 'ignore_segmentation:', ignore_segmentation


  ;; Get wavelength units from original file
  envi_file_query, img_fid, wl=wl, wavelength_units=wavelength_units, $
                   bnames=bnames, nb=nb, ns=ns, nl=nl, bbl=bbl, $ 
                   dims=dims, data_type=data_type, fwhm=fwhm

  envi_file_query, seg_fid, nb=nb_seg, nl=nl_seg

  hiihat_assert, (nb ne nb_seg) or (nl ne nl_seg), $
                 "Image and segmentation file geometry must match", $
                 title=title

  if not keyword_set(use_dims) then use_dims=dims

  if backup_rois then begin
     ;; save the current (non-endmember detected) rois to disk
     ;; before wiping them, and restore them at the end of this subroutine
     hiihat_save_rois, verbose=verbose
  endif

  envi_delete_rois, /all        

  if ignore_segmentation then begin
     ;; Handle the ignore segmentation case, simpler.
     temp_fid=img_fid
     dims=use_dims
     n_bands=nb
     ns_endmembers=ns
     nl_endmembers=nl
     nfindr_reject_zeros=0 ;; all endmembers are algorithmically "true" endmembers
  endif else begin
     if verbose then print,"Fetching spectra for segments"
     ;; get representative spectra from each segment
     hiihat_segment_spectra, img_fid, seg_fid, spectra=spectra, $
                             use_dims=use_dims, verbose=verbose
     ;; , $ mean_image_name='0testmeanspectra.img', r_fid=meanimage_fid, /return_image

     n_segments = (size(spectra))[1]

     ;; remove zero spectra 
     if verbose then print, "Checking for zeroed spectra"        
     nonzero_idx = where(total((spectra eq 0),2) ne nb)
     if n_elements(nonzero_idx) gt 0 then begin
        spectra = spectra[nonzero_idx,*]
        n_segments = (size(spectra))[1]
     endif
     if verbose then print, n_segments-n_elements(nonzero_idx), $
                               " zeroed spectra found"

     ;; remove redundant spectra
     if check_endmember_redundancy then begin
        if verbose then print, "Checking for redundant spectra"        
        envi_report_init, "Checking for redundant spectra (click cancel to skip)", base=base, /INTERRUPT, title=title 
        envi_report_inc, base, n_segments-1
        mod_step = 100

        red_thr = 1e-10
        keep_mask = intarr(n_segments)+1
        for i=0,n_segments-1 do begin
           if i mod mod_step eq 0 then begin
              envi_report_stat, base, i, n_segments-1, cancel=cancel   
              if cancel then goto, skip_redun_check
           endif
           for j=i+1,n_segments-1 do begin
              dij = total((spectra[i,*]-spectra[j,*])^2)
              if dij lt red_thr then begin 
                 if verbose then print, "Redundant spectra found at indices:", [i,j]
                 keep_mask[j] = 0
              end
           endfor
        endfor
        if verbose then print, ulong(n_segments-total(keep_mask)), $
                               " redundant spectra found"   

        if total(keep_mask) ne n_segments then begin        
           spectra = spectra[where(keep_mask eq 1),*]
           n_segments = (size(spectra))[1]
        endif

skip_redun_check:
        envi_report_init, base=base, /finish 
     endif

     idl_tmpdir = getenv("IDL_TMPDIR")
     temp_end_img_file=idl_tmpdir+'superpixel_endmembers_tmp.img'

     if verbose then print,"Writing temporary superpixel segment spectra"
     ;; Write temporary file
     n_segments=(size(spectra))[1]
     n_bands=(size(spectra))[2]
     if verbose then print,"Original num segment: ",n_segments, " num_band: ",n_bands

     ;; We are forced to make a redundant temporary file
     ;; Only the first row is filled with the
     ;; mean spectra of each segment, while
     ;; the second is simply zeros. These are
     ;; not included in the NFINDR
     ;; calculation, but the MNF process
     ;; within NFINDR cannot process a N x 1
     ;; image file, sadly.

     redun=2
     spec=fltarr(n_segments,redun,n_bands)
     spec[*,0,*] = spectra
     spectra = spec
     
     dims=[-1,0,n_segments-1,0,redun-1] ; nl=1

     envi_write_envi_file, spectra, $
                           bnames=bnames, $
                           data_type=data_type, $
                           dims=dims, $
                           nb=n_bands, $
                           nl=redun, $ 
                           ns=n_segments, $
                           out_name=tmp_end_img_file, $
                           r_fid=temp_fid, $
                           wavelength_units=wavelength_units, $
                           wl=wl, fwhm=fwhm, $
                           bbl=bbl, $
                           /in_memory

     ns_endmembers=n_segments
     nl_endmembers=redun
     nfindr_reject_zeros=1 ;; otherwise we'll have a false zeroed endmember
  endelse

  if use_nfindr then begin
     if verbose then print,"Using NFINDR"
     
     hiihat_nfindr, $
        temp_fid, n_endmembers, $ 
        use_dims       = dims, $
        out_name_sli   = out_name_sli, $
        out_name_txt   = out_name_txt, $
        roi_outfile    = out_name_roi  , $
        r_spectra_fid  = endmember_fid, $
        r_endmember_ids= endmember_ids, $
        abund_r_fid    = abund_r_fid, $
        seed           = seed, $
        reject_zeros   = nfindr_reject_zeros, $
        verbose        = verbose, $
        in_memory      = in_memory, $
        gui_status     = gui_status

     ;; Remap the endmember IDs to
     ;; n_segments, as we were forced to mape
     ;; a redundant map n_segments x 2 for
     ;; MNF to work
     ;;for i=0, n_endmembers-1 do begin
     ;;   endmember_ids[i]=endmember_ids[i] mod n_segments
     ;;endfor
     
  endif else begin                 
     if verbose then print,"Using SMACC"
     
     envi_doit, 'ENVI_SMACC_DOIT', $
                fid         = temp_fid, $
                pos         = indgen(n_bands), $
                dims        = dims, $
                n_endmembers= n_endmembers, $
                r_fid       = endmember_fid, $
                out_name    = out_name_sli, $ ; unique options below
                roi_name    = out_name_roi, $
                in_memory   = in_memory, $
                abund_r_fid = abund_r_fid, $
                abund_in_memory = in_memory, $
                coalesce    = coalesce_threshold
  endelse

  ;; if the endmember_fid parameter is not set, or the out_name_roi file was not
  ;; written then endmember detection failed
  if not keyword_set(endmember_fid) or $ 
     not file_test(out_name_roi,/read) then begin
     dialog='Endmember detection failed.'     
     if not gui_status then print, dialog else ok=dialog_message(dialog, title=title)
     goto, cleanup
  endif

  ;; convert ROI pixels into superpixel-sized ROIS
  envi_delete_rois, /all 
  envi_restore_rois, out_name_roi  
  roi_ids=envi_get_roi_ids()      

  n_rois = n_elements(roi_ids)
  if n_rois eq 0 then begin
     dialog='Empty ROI list, no endmembers detected'     
     if not gui_status then print, dialog else ok=dialog_message(dialog, title=title)
     goto, cleanup      
  endif

  if n_rois ne n_endmembers then begin
     dialog='Incorrect number ('+strtrim(n_rois,2)+') of detected endmembers'     
     if not gui_status then print, dialog else ok=dialog_message(dialog, title=title)
     goto, cleanup      
  endif

  if not ignore_segmentation then begin
     ;; will hold IDs of new ROIs 
     ;; (that will be in image coords)
     new_roi_ids=intarr(n_endmembers) 

     ;; load superpixels
     envi_file_query, seg_fid, nb=seg_nb, ns=seg_ns, nl=seg_nl, dims=seg_dims
     segments=envi_get_data(fid=seg_fid, dims=seg_dims, pos=[0]) 
     n_segments=max(segments)+1 ; assume consecutate labels, start w/zero 
     
     ;; translate SMACC rois (from synthetic temp image) back to real-image
     ;; coordinates using the superpixel segments
     if debug then begin
        print, "nseg: ", n_segments 
        print, "nend: ", n_endmembers
        print, "nrois:", (size(roi_ids))[2]
     endif

     ;; if the user selected a subset of the image, we need to offset
     ;; the ROI positions accordingly, these will both be zero if the
     ;; entire image is used
     samp_offset = use_dims[1]
     line_offset = use_dims[3]

     for i=0, n_endmembers-1 do begin  
        segment_index=long(envi_get_roi(roi_ids[i]))
        segment_index=segment_index mod n_segments ; this undoes the redundant nl=2 in the temp image required by MNF
        pts=where(segments eq segment_index[0])
        ypts=line_offset+floor( pts / ns)
        xpts=samp_offset+(pts mod ns)
        roi_name='Class '+strtrim(string(i+1),2)
        roi_color = i+1
        if roi_color eq 1 then roi_color = 49
        new_roi_ids[i]=envi_create_roi(color=roi_color, ns=ns, nl=nl, name=roi_name)
        envi_define_roi, new_roi_ids[i], /point, xpts=xpts, ypts=ypts            
     endfor 
     roi_ids=new_roi_ids
  endif
  
 
  if verbose then print, "Writing to ROI file ",out_name_roi
  envi_save_rois, out_name_roi, roi_ids

  ;; write out txt's
  if keyword_set(out_name_txt) then begin
     envi_file_query, endmember_fid, ns=nb, wl=wl, dims=dims
     data=envi_get_data(fid=endmember_fid, dims=dims, pos=0)

     if not keyword_set(out_name_txt) then begin 
        out_name_txt = out_name_sli+'.txt'       
     endif else if out_name_txt eq out_name_sli then begin
        dialog='Output library filename and output text filename must not be equal.'
        if not gui_status then print, dialog else ok=dialog_message(dialog, title=title)     
        goto, cleanup
     endif

     hiihat_write_spectrum, spectrum=data, nb=nb, wl=wl, fwhm=fwhm, $
                            text_filename=out_name_txt
  endif

cleanup:
  if temp_fid ne img_fid then begin
     ;; temp_fid points to a temp file which we want to dereference
     envi_file_mng, id=temp_fid, /remove 
     ;; get rid of our redundant temp image file
     file_delete, temp_end_img_file, /quiet
  endif

  if backup_rois then hiihat_load_rois, verbose=verbose
  if debug then print, "Exiting "+title 
end


