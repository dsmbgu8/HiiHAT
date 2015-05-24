;+ 
; GUI wrapper for querying parameters specific to hiihat_preprocess function
;
; :Author: David Ray Thompson
; :Categories: ui
; :Keywords:
;  image_type: out, optional, type=string
;    name of known image type, or "Generic" for an unspecified image type 
;  median_filter_width: out, optional, type=fix
;    width of median filter, 0 for no filtering
;  norm_type: out, optional, type=string
;    type of normalization to perform, "None" for no normalization
;  div_type: out, optional, type=string
;    divide out "Mean" or "MeanColumn", "None" for no division
;  lowpass_filter: out, optional, type=fix
;    width of lowpass filter kernel, 0 for no filtering
;  out_filename: out, required, type=string
;    file name of output preprocessed image
;  in_memory: out, required, type=boolean
;    output preprocessed image in memory
;  base_filename: in, optional, type=string
;    base name of image file to segment, if available
;  preprocess_cancel: out, optional, type=bolean
;    set to 1 if user cancels preprocessing operations
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
pro hiihat_preprocess_ask_params, image_type = image_type, $
                                  filter_negative = filter_negative, $
                                  median_filter_width = median_filter_width, $
                                  norm_type = norm_type, $
                                  div_type = div_type, $
                                  lowpass_filter = lowpass_filter, $
                                  out_filename = out_filename, $
                                  base_filename=base_filename, $
                                  in_memory = in_memory, $
                                  preprocess_cancel = preprocess_cancel

    ;Set compiler and debug options
    compile_opt strictarr
    debug = hiihat_get_config_parm('debug')
    gui_status =  hiihat_get_config_parm('gui_status')
    title='hiihat_preprocess_ask_params'
    if debug then print, "Entering "+title

    if not keyword_set(base_filename) then begin 
       def_filename='hiihat_preprocess'
    endif else begin
       def_filename=hiihat_strreplace(base_filename,'.img','',ignore_case=1)+'_pp'
    endelse


    ;; bail out only if the user chooses to do so
    preprocess_cancel = 0

    image_types = ['Generic', $
                  'CRISM FRT (CAT-processed)',$
                   'CRISM MRDR (CAT-processed)',$
                   'CRISM MSW (CAT-processed)',$
                   'CRISM MSP (CAT-processed)',$
                   'EO-1 ALI 10-band',$
                   'EO-1 Hyperion 12-band',$
                   'M3 L1B RDN (raw)',$
                   'USGS AVIRIS reflectance']
    
    norm_types = ['None',$
                  'Area (L 1)',$
                  'Euclidean (L 2)',$
                  'Peak (L inf)']

    div_types = ['None', 'Spatial Mean', 'Spectral Mean']

    base = widget_auto_base(title='Preprocessing Parameters') 
    nil = widget_sslider(base, title='Spectral Median Filter', min=0, max=5, $  
              value=2, uvalue='median_filter', /auto)  
    nil = widget_sslider(base, title='Spatial Lowpass Filter', min=0, max=5, $  
              value=0, uvalue='lowpass_filter', /auto)  
    nil = widget_pmenu(base, prompt='Image Type', $
                       list=image_types, uvalue='image_type',default=0,/auto)
    nil = widget_pmenu(base, prompt='Normalization', $
                       list=norm_types , uvalue='norm_type', default=0,/auto)
    nil = widget_pmenu(base, prompt='Division Type', $
                       list=div_types , uvalue='div_type', default=0,/auto)
    nil = widget_menu(base, list=['Filter Negative Values'], $
                       uvalue='filter_negative',default_array=[1], /auto) 
    nil = widget_outfm(base, prompt="Outfile", uvalue='outf', $
                       default=def_filename+'.img', /auto)

    result = auto_wid_mng(base)  

    if (result.accept eq 0) then begin
       preprocess_cancel = 1
       goto, cleanup  
    endif

    image_type = image_types[result.image_type]
    median_filter_width = long(result.median_filter)
    norm_type  = norm_types [result.norm_type]
    div_type  = div_types [result.div_type]
    lowpass_filter = long(result.lowpass_filter)
    filter_negative = long(result.filter_negative[0])
    in_memory = result.outf.in_memory

    ; select an output file
    if not in_memory then begin
       out_filename = result.outf.name ;;envi_pickfile(title=dialog, output=1)
       write_status = hiihat_check_file(out_filename,write=1)
       if write_status eq 0 then begin
          dialog='Invalid output file, computing in memory'
          if not gui_status then print, dialog else  ok=dialog_message(dialog, title=title)              
          in_memory=1
       endif else  if write_status eq 2 then begin
          ok = dialog_message('File "'+out_filename+'" exists, overwrite?', $
                              title=title, /cancel)
          if (strupcase(ok) eq 'CANCEL') then begin
             preprocess_cancel=1
             goto, cleanup
          endif
       endif
    endif

cleanup:
    if debug then print, "Exiting "+title
end

