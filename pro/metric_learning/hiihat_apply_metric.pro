;+ 
; Given an image and a precomputed LDA matrix transforms the image. 
; The transformed image is maximally linearly separable with respect 
; to the classes used to compute the LDA matrix.
;
; :Author: Brian Bue
;
; :Categories: 
;  metric_learning
;
; :Params:
;  img_fid: in, required, type=fid
;    file id of image with a set of ROIs defining classes for metric learning
;  matrix_fid: in, required, type=fix
;    file id of intput matrix
;  out_filename: in, optional, type=string
;    name of output file to save transformed image
;
; :Keywords:
;  use_dims: in, optional, type=size
;    spatial subset of input image
;  pos: in, optional, type=intarr(nb)
;    subset of spectral bands 
;  r_fid: in, optional, type=fix
;    file id of returned image
;  in_memory: in, optional, type=boolean
;    store transformed image in memory
;  rank: in, optional, type=fix
;    rank of output transform 
;  verbose: in, optional, type=boolean
;    print verbose output to console
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
pro hiihat_apply_metric, img_fid, matrix_fid, out_filename, r_fid = r_fid,$
        pos=pos, use_dims=use_dims, in_memory=in_memory, verbose=verbose,$
        rank=rank

  ;; Set compiler and debug options
  compile_opt strictarr

  debug = hiihat_get_config_parm('debug')
  finite_max = hiihat_get_config_parm('finite_max')
  
  mod_step = 5 ;; step for printing output 

  title='hiihat_apply_metric'
  if debug then print, "Entering "+title

  ;; ;; ...A may have lots of dimensions, so for a stopgap I'll 
  ;; ;; reduce this arbitrarily to lighten memory requirements
  if not keyword_set(rank) then rank = 5

  ;; load the transform file
  envi_file_query, matrix_fid, dims=trans_dims
  A = envi_get_data(fid=matrix_fid, dims=trans_dims, pos=0) 

  ;; load the image, correct for pos (bands), use_dims being smaller than whole image
  if not keyword_set(pos) then begin
     envi_file_query, img_fid, nb=img_nb
     pos = lindgen(img_nb)
  endif
  img_nb = n_elements(pos)

  if (size(A))[1] ne img_nb then begin
     dialog='Image and transform dimensionality mismatch'
     ok=dialog_message(dialog,title=title)                                      
     goto, cleanup
  end    
  
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

  ;; apply the transform
  out_rank = (size(A))[2]
  lda_img = hiihat_apply_transform(lda_img, A)
  lda_img = reform(lda_img, img_ns, img_nl, out_rank, /overwrite)

  ;; output the resulting image for segmentation / endmember detection
  double_type = 5
  envi_write_envi_file, lda_img, out_name=out_filename, $
                        in_memory=in_memory, $                        
                        data_type = double_type, $
                        dims = use_dims, $
                        nb = out_rank, $
                        nl = img_nl, $
                        ns = img_ns, $
                        r_fid = r_fid

cleanup:
    if debug then print, "Exiting "+title
end
