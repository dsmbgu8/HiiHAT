;+
; Rebuilds all .pro files in the hiihat root directory. Assumes that every file has a pro/function definition in the file
; that matches the file name (i.e., "procedure_name.pro" contains a function
; named "procedure_name"). Returns all functions on exit. 
;
; :Categories: build
;
; :Author: Lukas Mandrake and Brian D. Bue
;
; :History:
;
;   2009 (Lukas Mandrake): Initially implemented
;   Dec 21, 2010 (BDB): Added docstr, recursive directory parsing
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
pro make_hiihat
  hiihat_path = hiihat_get_config_parm('hiihat_path')
  hiihat_pro  = file_search(hiihat_path, '*.pro')
  
  for i=0, n_elements(hiihat_pro)-1 do begin
     proc_name = file_basename(hiihat_pro[i],'.pro')
     ;; do not attempt to recursively compile this file
     if proc_name eq 'make_hiihat' or proc_name eq 'mh' then continue
          
     resolve_routine, proc_name, /compile_full_file, /either
  end

  retall 
end
