;+
; Runs all of the HiiHAT tests in the HiiHAT pro/testing
; directory. All testcases are assumed to start with the prefix
; "test_hiihat." Rebuilds the testcase if keyword argument build=1
;
; :Categories:
;   testing
;
; :History:
;   Dec 10, 2010 (BDB): Initial implementation
;  
; :Keywords:
;  build: in, optional, type=boolean
;    Rebuild all unit tests in testing directory before running them.
;
; :Examples:
;   To run all of the hiihat tests (and rebuild them before we run
;   them), we do:
;
;   hiihat_run_tests, build=1
;
; :Author: Brian D. Bue
;
; :Copyright:
;  Copyright 2010, by the California Institute of Technology. ALL RIGHTS
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
pro hiihat_run_tests, build=build
  if not keyword_set(build) then build = 1 ;; rebuild by default
  hh_path = hiihat_get_config_parm('hiihat_path')

  sep = path_sep()
  test_path = hh_path+'pro'+sep+'testing'+sep
  test_files  = file_search(test_path, 'test_hiihat*.pro')

  print, "Loading tests from "+test_path
  for i=0, n_elements(test_files)-1 do begin
     proc_name = file_basename(test_files[i],'.pro')  
     test_name = hiihat_strreplace(proc_name, "__define", "")
     if not keyword_set(test_pro) then begin 
        test_pro = [test_name] 
     endif else begin 
        test_pro = [test_pro, test_name]
     endelse

     if build then $
        resolve_routine, proc_name, /compile_full_file, /either
  endfor  

  print, "Running tests: ", test_pro

  mgunit, test_pro
end 
