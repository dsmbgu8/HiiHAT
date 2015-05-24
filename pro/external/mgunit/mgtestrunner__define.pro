;+
; Report a test suite has begun.
; 
; @param testsuite {in}{required}{type=string} name of test suite
; @keyword ntestcases {in}{required}{type=integer} number of test suites/cases 
;          contained by the test suite
; @keyword ntests {in}{required}{type=integer} number of tests contained in the 
;          hierarchy below this test suite
; @keyword level {in}{required}{type=level} level of test suite
;-
pro mgtestrunner::reportTestSuiteStart, testsuite, $
                                        ntestcases=ntestcases, $
                                        ntests=ntests, $
                                        level=level
  compile_opt strictarr
end


;+
; Report the results of a test suite.
;
; @keyword npass {in}{required}{type=integer} number of passing tests contained 
;          in the hierarchy below the test suite
; @keyword nfail {in}{required}{type=integer} number of failing tests contained 
;          in the hierarchy below the test suite
; @keyword level {in}{required}{type=integer} level of test suite
;-
pro mgtestrunner::reportTestSuiteResult, npass=npass, nfail=nfail, level=level
  compile_opt strictarr

end


;+
; Report a test case has begun.
; 
; @param testcase {in}{required}{type=string} name of test case
; @keyword ntests {in}{required}{type=integer} number of tests contained in this 
;          test case
; @keyword level {in}{required}{type=level} level of test case
;-
pro mgtestrunner::reportTestCaseStart, testcase, ntests=ntests, level=level
  compile_opt strictarr

end


;+
; Report the results of a test case.
;
; @keyword npass {in}{required}{type=integer} number of passing tests
; @keyword nfail {in}{required}{type=integer} number of failing tests
; @keyword level {in}{required}{type=integer} level of test case
;-
pro mgtestrunner::reportTestCaseResult, npass=npass, nfail=nfail, level=level
  compile_opt strictarr

end


;+
; Report the start of single test.
; 
; @param testname {in}{required}{type=string} name of test
; @keyword level {in}{required}{type=integer} level of test case
;-
pro mgtestrunner::reportTestStart, testname, level=level
  compile_opt strictarr

end


;+
; Report the result of a single test.
; 
; @param msg {in}{required}{type=string} message to display when test fails
; @keyword passed {in}{required}{type=boolean} whether the test passed
;-
pro mgtestrunner::reportTestResult, msg, passed=passed
  compile_opt strictarr

end


;+
; Free resources.
;-
pro mgtestrunner::cleanup
  compile_opt strictarr

end


;+
; Initialize the test runner.
;
; @returns 1 for success, 0 for failure
;-
function mgtestrunner::init
  compile_opt strictarr
  
  return, 1B
end


;+
; Define member variables.
;
; @file_comments Results for tests, test cases, and test suites are reported to
;                the test runner. Each subclass of MGtestRunner displays them in
;                some way MGtestRunner itself is abstract and shouldn't be
;                instantiated.  
;
; @field dummy needed because IDL requires at least one field
;-
pro mgtestrunner__define
  compile_opt strictarr

  define = { MGtestRunner, dummy : 0L }
end
