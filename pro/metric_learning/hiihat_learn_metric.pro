;+ 
; Given an image and a set of ROIs defining a set of classes,
; transforms the image via LDA. The transformed image is maximally linearly
; separable with respect to the given classes.
;
; :Author: Brian Bue
;
; :Categories: 
;  metric_learning
;
; :Params:
;  img_fid: in, required, type=fid
;    File id of image with a set of ROIs defining classes for metric learning
;
; :Keywords:
;  max_samples: in, required, type=int
;    maximum number of samples per class
;  reg_parms: in, required, type=struct
;    regularization parameters
;  use_dims: in, optional, type=size
;    spatial subset of input image
;  pos: in, optional, type=intarr(nb)
;    subset of spectral bands 
;  r_fid: in, optional, type=fix
;    file id of returned image
;  in_memory: in, optional, type=boolean
;    store transformed image in memory
;  matrix_r_fid: in, optional, type=fix
;    file id of matrix image
;  matrix_in_memory: in, optional, type=boolean
;    store transformation matrix in memory
;  out_filename: in, optional, type=string
;    name of output file to save transformed image
;  out_matrix_filename: in, optional, type=string
;    name of output file to save lda matrix image
;  verbose: in, optional, type=boolean
;    print verbose output to console
;
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
pro hiihat_learn_metric, img_fid, max_samples=max_samples, reg_parms=reg_parms, $
                         use_dims=use_dims, pos=pos, r_fid=r_fid, $
                         in_memory=in_memory, out_filename=out_filename, $
                         out_matrix_filename=out_matrix_filename, matrix_in_memory=matrix_in_memory, $
                         matrix_r_fid=matrix_r_fid, verbose=verbose                       

  ;; Set compiler and debug options
  compile_opt strictarr

  debug = hiihat_get_config_parm('debug')
  finite_max = hiihat_get_config_parm('finite_max')
  
  mod_step = 5 ;; step for printing output 

  title='hiihat_learn_metric'
  if debug then print, "Entering "+title

  if not keyword_set(max_samples) then max_samples = finite_max
  
  ;;envi_restore_rois, roi_filename
  roi_ids=envi_get_roi_ids()      
  n_classes = n_elements(roi_ids)

  if n_classes eq 0 then begin
     print, 'No ROIs defined for this image'
     goto, cleanup
  endif

  ;; load the image, correct for use_dims being smaller than whole image
  envi_file_query, img_fid, dims=img_dims, nb = img_nb, bnames = bnames,$
                   ns = img_ns, nl = img_nl, wl = img_wl, fwhm = fwhm, $
                   wavelength_units=wavelength_units, data_type=data_type

  if not keyword_set(pos) then pos = lindgen(img_nb)
  img_nb = n_elements(pos)

  if keyword_set (use_dims) then begin 
     img_ns = use_dims[2]-use_dims[1]+1
     img_nl = use_dims[4]-use_dims[3]+1
     if verbose then print, "Reset img_ns, img_nl to",img_ns, img_nl
  endif else use_dims = img_dims

  ;; parse the image to transform
  envi_report_init, "Loading image data", base=base, /INTERRUPT, title=title 
  envi_report_inc, base, img_nb-1
  lda_img = fltarr(img_ns*img_nl, img_nb)
  for i=0,img_nb-1 do begin
     if i mod mod_step eq 0 then begin
        envi_report_stat, base, i, img_nb-1, cancel=cancel   
        if cancel then goto, cleanup
     endif
     band = envi_get_data(fid=img_fid, dims=use_dims, pos=pos[i])
     lda_img[*,i] = reform(band,img_ns*img_nl)
  endfor   
  envi_report_init, base=base, /finish 

  samp_offset = use_dims[1]
  line_offset = use_dims[3]

  ;; parse the class data from the ROIs
  envi_report_init, "Loading ROI data", base=base, /INTERRUPT, title=title 
  envi_report_inc, base, n_classes-1
  for i=0, n_classes-1 do begin  
     
     envi_report_stat, base, i, n_classes-1, cancel=cancel   
     if cancel then goto, cleanup     

     roi_idx=envi_get_roi(roi_ids[i],roi_name=roi_name,roi_color=roi_color)
     if not keyword_set(roi_name) then begin
        print, "No ROIs available for this image"
        goto, cleanup
     endif
     n_roi_pixels = n_elements(roi_idx)

     ;; ignore single pixel roi
     if n_roi_pixels le 1 then begin
        print, "Less than 2 pixels in ROI index: ", i, " skipping"       
        continue
     endif

     pts = array_indices([img_ns,img_nl],roi_idx,/dimensions)
     
     ;; if we spatially subset the image, we need to update the roi positions
     col = pts[0,*]+samp_offset
     row = pts[1,*]+line_offset

     n_sel_pixels = min([n_roi_pixels,max_samples])
     if verbose then $
        print, "Sampling "+strtrim(n_sel_pixels,2)+" of "+strtrim(n_roi_pixels,2)+" pixels from ROI: "+roi_name

     ;; randomly permute pixels
     sel_idx = (sort(randomu(seed, n_roi_pixels+1,/long)))[0:n_sel_pixels-1]
     n_roi_pixels = n_sel_pixels

     roi_pixels = transpose(lda_img[col[sel_idx]+(row[sel_idx]*img_ns),*])
             
     roi_labvec=lonarr(n_roi_pixels)+(i+1)
     
     if n_elements(roi_data) eq 0 then roi_data = roi_pixels else $
        roi_data = [[roi_data],[roi_pixels]]

     if n_elements(roi_labels) eq 0 then roi_labels = roi_labvec else $
        roi_labels = [roi_labels,roi_labvec]
  end
  roi_data = transpose(roi_data)

  envi_report_init, base=base, /finish 
  
  ;; get the LDA transformation matrix via xvalidation
  A = hiihat_mdmc_xvalidate(roi_data, roi_labels, reg_parms, $
                            domains=domains, rank=rank, $
                            verbose=verbose)

  ;; apply transformation matrix
  lda_img = hiihat_apply_transform(lda_img, A)
  lda_img = reform(lda_img, img_ns, img_nl, rank, /overwrite)

  double_type = 5
  ;; output the resulting image for segmentation / endmember detection
  envi_write_envi_file, lda_img, out_name=out_filename, $
                        in_memory=in_memory, $                        
                        data_type = double_type, $
                        dims = use_dims, $
                        nb = out_rank, $
                        nl = img_nl, $
                        ns = img_ns, $
                        r_fid = r_fid


  A_dims = [-1L,0,img_nb,0,rank]
  ;; write the output matrix to file
  envi_write_envi_file, A, out_name=out_matrix_filename, $
                        in_memory=matrix_in_memory, $                        
                        data_type = double_type, $
                        dims = A_dims, $
                        nb = 1, $
                        nl = rank, $
                        ns = img_nb, $
                        r_fid = matrix_r_fid


cleanup:
    if debug then print, "Exiting "+title
end
