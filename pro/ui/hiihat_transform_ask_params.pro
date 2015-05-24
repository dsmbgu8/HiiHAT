;+ 
; Select a pre-computed transformation matrix for application to a
; known image format
;
; :Categories:
;  ui
;
; :Params: 
;  image_type: in, required, type=string
;    Image type to transform, from hiihat_pp description provided in hdr file
;
; :Keywords:
;  trans_filename: out, required, type=string
;    transfomation matrix filename
;  in_memory: out, optional, type=boolean
;    store transformed image in memory
;  base_filename: in, optional, type=string
;    base file name to assign to output image
;  out_filename: out, optional, type=string
;    name of file in which to save transformed image
;  max_rank: in, optional, type=fix
;    maximum rank of output transform (default = 5)
;  base_filename: in, optional, type=string
;    prefix to append to output filenames if available
;  transform_cancel: in, required, type=boolean
;    user selected cancel
;
; :Author: Brian D. Bue
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
pro hiihat_transform_ask_params, image_type, trans_filename=trans_filename, $
                                 out_filename=out_filename, in_memory=in_memory, $
                                 max_rank=max_rank, base_filename=base_filename, $
                                 verbose=verbose, transform_cancel=transform_cancel

  title='hiihat_transform_ask_params'  

  debug =  hiihat_get_config_parm('debug')
  if debug then print, "Entering "+title
  gui_status = hiihat_get_config_parm('gui_status')

  transform_cancel = 0

  if not keyword_set(base_filename) then begin 
     def_filename='hiihat_transform_preprocessed'
  endif else begin
     def_filename=hiihat_strreplace(base_filename,'.img','',ignore_case=1)+'_lda'
  endelse   

  ;; identify file prefix of transformation matrix  
  case image_type of 
     'CRISM FRT (CAT-processed)': begin
        file_prefix = 'crism_frt_cat'
     end
     'CRISM MRDR (CAT-processed)': begin
        file_prefix = 'crism_mrdr_cat'
     end
     'CRISM MSW (CAT-processed)': begin
        file_prefix = 'crism_msw_cat'
     end
     'CRISM MSP (CAT-processed)': begin
        file_prefix = 'crism_msp_cat'
     end
     'M3 L1B RDN (raw)': begin
        file_prefix = 'm3_l1b_rdn'
     end
     'USGS AVIRIS reflectance': begin
        file_prefix = 'aviris_rfl'
     end
     'EO-1 Hyperion 10-band': begin
        file_prefix = 'eo1_10b'
     end
     'EO-1 Hyperion 12-band': begin
        file_prefix = 'eo1_12b'
     end
     'Generic': begin
        file_prefix = 'gen' 
     end
     'Unknown': begin
        file_prefix = '*' ;; user chose to continue despite lack of hiihat_pp flag
     end
     else: begin
        dialog='Unrecognized image type'
        if not gui_status then print, dialog else ok=dialog_message(dialog,title=title)                                      
        transform_cancel = 1
        goto, cleanup
     end
  endcase

  ;; list all the files in the transforms directory with the proper prefix
  hiihat_path = hiihat_get_config_parm('hiihat_path')
  sep = path_sep()
  data_path = hiihat_path+sep+'data'+sep+'transforms'+sep

  ;; list all the files (without the .hdr suffix)
  trans_files = file_basename(file_search(data_path+file_prefix+'*'),'.hdr')
  
  trans_files = trans_files[uniq(trans_files,sort(trans_files))]
  if trans_files[0] eq '' then begin
     if verbose then print, 'No transformations for image type: "'+image_type+'."'
     transform_cancel=1
     goto, cleanup
  endif

  max_rank = 5 ;; default max rank parameter

  ;; choose the transformation file
  base = widget_auto_base(title='Transformation Parameters')  
  nil = widget_pmenu(base, prompt='Existing transformations:', $
                     list=trans_files, uvalue='trans_index',default=0,/auto)
  nil = widget_param(base, dt=3, field=3, floor=0., default=string(max_rank), $
                     prompt='Maximum Rank', uvalue='max_rank', /auto)  
  nil = widget_outfm(base, prompt="Output image file", uvalue='outf', $
                     default=def_filename+'.img', /auto)
  result = auto_wid_mng(base)  
  
  if (result.accept eq 0) then begin
     transform_cancel = 1
     goto, cleanup  
  endif

  max_rank = float(result.max_rank)


  ;; get transform file
  trans_filename = data_path+trans_files[result.trans_index]
  read_status = hiihat_check_file(trans_filename,read=1)
  if read_status eq 0 then begin
     dialog = "Unable to read input file "+trans_filename
     if not gui_status then print,dialog else ok=dialog_message(dialog,title=title)
     transform_cancel=1
     goto, cleanup
  endif

  ;; select an output file
  in_memory = result.outf.in_memory
  if not in_memory then begin
     out_filename = result.outf.name 
     write_status = hiihat_check_file(out_filename,write=1)
     if write_status eq 0 then begin
        dialog = "Invalid output file, computing in memory"
        if not gui_status then print,dialog else ok=dialog_message(dialog,title=title)
        in_memory=1
     endif else if write_status eq 2 then begin
        ok = dialog_message('File "'+out_filename+'" exists, overwrite?', $
                            title=title, /cancel)
        if (strupcase(ok) eq 'CANCEL') then begin
           transform_cancel=1
           goto, cleanup
        endif
     endif
  endif


cleanup:
  if debug then print, "Exiting "+title
end
