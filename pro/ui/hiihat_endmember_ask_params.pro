;+
; Query user for endmember detection parameters
;
; :Categories:
;   ui 
;
; :Keywords:
;   coalesce_threshold: in, optional, type=fix
;     SMACC only, maximum Spectral Angle Mapper (SAM) 
;     threshold above which endmember spectra are considered unique 
;     (angle/1000), in range [0,1000], values outside range set to nearest bound
;   n_endmembers: in, required, type=fix
;     number of endmembers in range [1,30]
;   method: in, required, type=string
;     detection method, one of 'NFINDR' or 'SMACC'
;  out_filename: out, required, type=string
;    file name of output spectral librar
;  in_memory: out, optional, type=boolean
;    store endmember rois and library in memory
;  roi_out_filename: out, required, type=string
;    file name of output roi file
;  base_filename: in, optional, type=string
;    base name of image file to segment, if available
;   endmember_cancel: out, required, type=boolean
;     true if user cancels endmember detection
;
; :Examples:
;   hiihat_endmember_ask_params, coalesce_threshold=50, 
;                              n_endmembers=10, method='NFINDR', 
;                              endmember_cancel=endmember_cancel
; 
; :History:
;   2009 (DRT): Initial implementation.
;
;   Dec 14, 2010 (BDB): Added docstr.
;
; :Author: David Ray Thompson
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
pro hiihat_endmember_ask_params, coalesce_threshold=coalesce_threshold, $ ;seg_fid=seg_fid, $
                                 n_endmembers = n_endmembers, method = method, $
                                 out_filename = out_filename, base_filename=base_filename, $
                                 roi_out_filename=roi_out_filename, in_memory=in_memory, $
                                 endmember_cancel=endmember_cancel
    compile_opt strictarr

    debug = hiihat_get_config_parm('debug')
    gui_status = hiihat_get_config_parm('gui_status')

    title='hiihat_endmember_ask_params'
    if debug then print, "Entering "+title

    if not keyword_set(base_filename) then begin 
       def_filename='hiihat_endm'
    endif else begin
       def_filename=hiihat_strreplace(base_filename,'.img','',ignore_case=1)+'_endm'
    endelse

    n_endmembers = 10
    coalesce_threshold = 0.0

    ;; don't cancel unless user bails out
    endmember_cancel=0

    methods=['SMACC','NFINDR']

    ; dialog to get SMACC parameters (code from ENVI documentation)
    base = widget_auto_base(title='Endmember Parameters')  
    ;nil = widget_outf(base, prompt="Segmentation file (optional)", $
    ;                  uvalue='seg_filename', /auto)
    nil = widget_sslider(base, title='Number of endmembers', min=1, max=30, $  
                         value=10, uvalue='n_endmembers', /auto)  
    nil = widget_pmenu(base, prompt='Method', list=methods, uvalue = 'method', /auto )
    nil = widget_param(base, prompt='SMACC Coalescence Threshold (Angle / 1000)', $
                       floor=0, ceil=1000, dt=3, default='0', uvalue='coalesce_threshold', /auto)
    nil = widget_outf(base, prompt="Endmember Outfile (required)", $
                      uvalue='out_filename', default=def_filename+'.sli', /auto)
    nil = widget_outf(base, prompt="ROI Outfile (required)", $
                      uvalue='roi_out_filename', default=def_filename+'.roi', /auto)

    result = auto_wid_mng(base)  
    if result.accept eq 0 then begin
       endmember_cancel=1
       goto, cleanup
    endif

    ;; open segmentation file if available
    ;envi_open_file, result.seg_filename, r_fid=seg_fid  

    n_endmembers = result.n_endmembers
    coalesce_threshold = float(result.coalesce_threshold) / 1000.0
    method = methods[result.method]  
    out_filename = result.out_filename
    write_status = hiihat_check_file(out_filename,write=1)
    if write_status eq 0 then begin
       dialog='Endmember detection cancelled: Cannot write to outfile: '+out_filename
       if not gui_status then print,dialog else ok=dialog_message(dialog,title=title)
       endmember_cancel=1
       goto, cleanup
    endif else if write_status eq 2 then begin
       dialog='File "'+out_filename+'" exists, overwrite?'
       ok = dialog_message(dialog, title=title, /cancel)
       if (strupcase(ok) eq 'CANCEL') then begin 
          endmember_cancel=1
          goto, cleanup
       endif
    endif

    roi_out_filename = result.roi_out_filename
    write_status = hiihat_check_file(roi_out_filename,write=1)
    if write_status eq 0 then begin
       dialog='Endmember detection cancelled: Cannot write to ROI outfile: '+roi_out_filename
       if not gui_status then print,dialog else ok=dialog_message(dialog,title=title)
       endmember_cancel=1
       goto, cleanup
    endif else if write_status eq 2 then begin
       dialog='File "'+roi_out_filename+'" exists, overwrite?'
       ok = dialog_message(dialog, title=title, /cancel)
       if (strupcase(ok) eq 'CANCEL') then begin 
          endmember_cancel=1
          goto, cleanup
       endif
    endif    

cleanup:
    if debug then print, "Exiting "+title
end
