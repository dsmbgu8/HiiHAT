;+
; Routine to query user for metric learning parameters.
;
; :Author: Brian D. Bue
; :Categories: ui
; :Keywords:
;
;  reg_parms: out, required, type=struct
;    set of regularization parameters determined by reg_tune menu parameter
;  in_memory: out, optional, type=boolean
;    store metric learning image transform in memory
;  base_filename: in, optional, type=string
;    base file name to assign to output image
;  out_filename: out, optional, type=string
;    name of file in which to save transformed image
;  max_samples: in, required, type=fix
;    maximum number of samples per class
;  learn_metric_cancel: out, optional, type=boolean
;    flag to cancel metric learning operation
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
pro hiihat_learn_metric_ask_params, reg_parms=reg_parms, in_memory=in_memory, $
                                    base_filename=base_filename, out_filename=out_filename,  $
                                    out_matrix_filename=out_matrix_filename, matrix_in_memory=matrix_in_memory, $
                                    max_samples=max_samples, learn_metric_cancel=learn_metric_cancel

    ;;Set compiler and debug options
    compile_opt strictarr
    debug = hiihat_get_config_parm('debug')
    gui_status = hiihat_get_config_parm('gui_status')

    title = 'hiihat_learn_metric_ask_params'
    if debug then print, "Entering "+title

    ;; don't cancel unless user bails out
    learn_metric_cancel = 0

    max_samples = 100    
    reg_tune = 50

    if not keyword_set(base_filename) then begin 
       def_filename='hiihat_learn_metric'
    endif else begin
       def_filename=hiihat_strreplace(base_filename,'.img','',ignore_case=1)+'_lda'
    endelse

    base = widget_auto_base(title='Metric Learning Parameters') 
    nil = widget_param(base, dt=3, field=3, floor=1, default=string(max_samples), $
            prompt='Samples per class', uvalue='max_samples', /auto)  
    nil = widget_sslider(base, dt=3, floor=0, ceil=100, min=0, max=100, value=reg_tune, $
            title='Regularization Amount (%)', uvalue='reg_tune', field=0, /auto)  
    nil = widget_outfm(base, prompt="Output image file", uvalue='outf', $
                       default=def_filename+'.img', /auto)
    nil = widget_outfm(base, prompt="Matrix output file", uvalue='outf_mat', $
                       default=def_filename+'_matrix.img', /auto)
    

    result = auto_wid_mng(base) 

    if (result.accept eq 0) then begin 
       learn_metric_cancel = 1
       goto, cleanup
    endif  

    ; select an output file
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
             learn_metric_cancel=1
             goto, cleanup
          endif
       endif
    endif

    ; select an output file for the output matrix
    matrix_in_memory = result.outf_mat.in_memory
    if not matrix_in_memory then begin
       dialog = "Select an output file, or 'cancel' to compute in memory"
       out_matrix_filename = result.outf_mat.name 
       write_status = hiihat_check_file(out_matrix_filename,write=1)
       if write_status eq 0 then begin
          dialog = "Invalid output file, computing in memory"
          if not gui_status then print,dialog else ok=dialog_message(dialog,title=title)
          matrix_in_memory=1
       endif else if write_status eq 2 then begin
          ok = dialog_message('File "'+out_matrix_filename+'" exists, overwrite?', $
                              title=title, /cancel)
          if (strupcase(ok) eq 'CANCEL') then begin
             learn_metric_cancel=1
             goto, cleanup
          endif
       endif
    endif



    max_samples = fix(result.max_samples)
    reg_tune = float(result.reg_tune)/100.0
    reg_type = 'constant' 
    lambda_min = 0.0
    lambda_max = 1.0
    fold_min = 2
    fold_max = 10
    delta_min = 0.01
    delta_max = 0.25

    num_folds = fold_min+fix((fold_max-fold_min)*reg_tune)
    lambda_delta = delta_min+((delta_max-delta_min)*(1.0-reg_tune))

    ;lambda_delta = float(result.lambda_delta)
    ;num_folds = fix(result.num_folds)
    
    reg_parms = {reg_type:reg_type, lambda:lambda_min, lambda_min:lambda_min,$
                 lambda_max:lambda_max, lambda_delta:lambda_delta, num_folds:num_folds}

cleanup:
    if debug then print, "Exiting "+title    
end
   

