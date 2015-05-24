;+
; Runs unit tests. 
; 
; @param tests {in}{optional}{type=strarr} array of test suites and/or test cases
; @keyword log_file {in}{optional}{type=string} name of file to send output to; if 
;          not present sends output to the output log
;-
pro mgunit, tests, log_file=logFile
  compile_opt strictarr

  testRunner = obj_new('MGtestCliRunner', log_file=logFile)
  
  if (n_elements(tests) gt 0) then begin
    testsuite = obj_new('MGtestSuite', test_runner=testRunner)
    testsuite->add, tests
    testsuite->run
    obj_destroy, testsuite
  endif

  obj_destroy, testRunner
end
