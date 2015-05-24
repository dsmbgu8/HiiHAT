;+
; Routine to query user for image segmentation parameters.
;
; :Author: Brian D Bue
; :Categories: ui
; :Keywords:
;  class_method: out, required, type=string
;    string value for classification method name
;  method: out, required, type=fix
;    ENVI value of classification method for CLASS_DOIT function
;  out_filename: out, optional, type=string
;    output file name for classification map
;  in_memory: out, optional, type=boolean
;    compute classification in memory if out_filename not specified/valid
;  base_filename: in, optional, type=string
;    base name of image file to segment, if available
;  classify_cancel: out, optional, type=boolean
;    flag to cancel classification operation
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
pro hiihat_classify_ask_params, class_method=class_method, method=method, $
                                in_memory=in_memory, out_filename=out_filename, $
                                rule_in_memory=rule_in_memory, rule_out_filename=rule_out_filename, $
                                base_filename=base_filename, classify_cancel=classify_cancel

    ;;Set compiler and debug options
    compile_opt strictarr
    debug = hiihat_get_config_parm('debug')
    gui_status = hiihat_get_config_parm('gui_status')

    title = 'hiihat_classify_ask_params'
    if debug then print, "Entering "+title

    if not keyword_set(base_filename) then begin 
       def_filename='hiihat_class'
    endif else begin
       def_filename=hiihat_strreplace(base_filename,'.img','',ignore_case=1)+'_class'
    endelse

    ;; don't cancel unless user bails out
    classify_cancel = 0
    class_method = 'SAM' ;; use SAM by default
    methods = ['Parallelepiped','Minimum distance','Maximum likelihood',$
               'SAM', 'Mahalanobis', 'Binary Encoding', 'SID']
    method_idx = [0,1,2,3,5,6,8] ;; ENVI indices for methods 
    base = widget_auto_base(title='Classification Parameters')  
    nil = widget_pmenu(base, prompt='Method', $
            list=methods, uvalue='method',default=3,/auto)
    nil = widget_outfm(base, prompt="Classification output file", $
                      uvalue='outf', default=def_filename+'.img', /auto)
    nil = widget_outfm(base, prompt="Rule output file", $
                      uvalue='rulef', default=def_filename+'_rule.img', /auto)

    result = auto_wid_mng(base) 
    if (result.accept eq 0) then begin
       classify_cancel=1
       goto, cleanup
    endif

    class_method = methods[result.method]
    method = method_idx[result.method]
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
             classify_cancel=1
             goto, cleanup
          endif
       endif
    endif

    rule_in_memory = result.rulef.in_memory
    if not rule_in_memory then begin       
       rule_out_filename = result.rulef.name 
       write_status = hiihat_check_file(rule_out_filename,write=1)
       if write_status eq 0 then begin
          dialog = "Invalid rule output file, computing in memory"
          if not gui_status then print,dialog else ok=dialog_message(dialog,title=title)
          rule_in_memory=1
       endif else if write_status eq 2 then begin
          ok = dialog_message('File "'+rule_out_filename+'" exists, overwrite?', $
                              title=title, /cancel)
          if (strupcase(ok) eq 'CANCEL') then begin
             classify_cancel=1
             goto, cleanup
          endif
       endif
    endif

cleanup:
    if debug then print, "Exiting "+title    
end
   

