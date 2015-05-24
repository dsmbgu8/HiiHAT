;+
; Routine to query user for image segmentation parameters.
;
; :Author: David Ray Thompson
; :Categories: ui
; :Keywords:
;  c: out, required, type=fix
;    threshold for segment merging
;  min_size: out, required, type=float
;    minimum size for each segment
;  dist_metric: out, required, type=string
;    distance metric for pixel-based comparisons
;  in_memory: out, optional, type=boolean
;    store segmentation result in memory
;  out_filename: out, optional, type=string
;    output filename
;  base_filename: in, optional, type=string
;    base name of image file to segment, if available
;  segment_cancel: out, optional, type=boolean
;    flag to cancel segmentation operation
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
pro hiihat_segment_ask_params, c=c, min_size=min_size, dist_metric=dist_metric, $                             
                               in_memory=in_memory, base_filename=base_filename, $
                               out_filename=out_filename, segment_cancel=segment_cancel
    ;;Set compiler and debug options
    compile_opt strictarr
    debug = hiihat_get_config_parm('debug')
    gui_status = hiihat_get_config_parm('gui_status')

    title = 'hiihat_segment_ask_params'
    if debug then print, "Entering "+title

    if not keyword_set(base_filename) then begin 
       def_filename='hiihat_segmentation'
    endif else begin
       def_filename=hiihat_strreplace(base_filename,'.img','',ignore_case=1)+'_seg'
    endelse

    ;; don't cancel unless user bails out
    segment_cancel = 0

    min_size = 20
    c = 0.001
    dist_metric = 'euclidean_sq'
    metrics = ['sad', 'euclidean', 'euclidean_sq', 'euclidean_n']

    base = widget_auto_base(title='Segmentation Parameters')  
    nil = widget_param(base, dt=3, field=3, floor=0., default=string(min_size), $
            prompt='Minimum Size', uvalue='min_size', /auto)  
    nil = widget_param(base, dt=4, field=3, floor=0., default=string(c), $
            prompt="C Threshold", uvalue='c', /auto)
    nil = widget_pmenu(base, prompt='Spectral Distance Metric', $
            list=metrics, uvalue='dist_metric',default=2,/auto)
    nil = widget_outfm(base, prompt="Outfile", uvalue='outf', $
                       default=def_filename+'.img', /auto)

    result = auto_wid_mng(base) 

    if (result.accept eq 0) then begin 
       segment_cancel = 1
       goto, cleanup
    endif  

    min_size = float(result.min_size)
    c = float(result.c)
    dist_metric = metrics[result.dist_metric]
                                         
    ; select an output file
    in_memory = result.outf.in_memory
    if not in_memory then begin
       dialog = "Select an output file, or 'cancel' to compute in memory"
       out_filename = result.outf.name ;; envi_pickfile(title=dialog, output=1)
       write_status = hiihat_check_file(out_filename,write=1)
       if write_status eq 0 then begin
          dialog = "Invalid output file, computing in memory"
          if not gui_status then print,dialog else ok=dialog_message(dialog,title=title)
          in_memory=1
       endif else if write_status eq 2 then begin
          ok = dialog_message('File "'+out_filename+'" exists, overwrite?', $
                              title=title, /cancel)
          if (strupcase(ok) eq 'CANCEL') then begin
             segment_cancel=1
             goto, cleanup
          endif
       endif
    endif

cleanup:
    if debug then print, "Exiting "+title    
end
   

