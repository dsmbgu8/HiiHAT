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
pro mgtestclirunner::reportTestSuiteStart, testsuite, $
                                           ntestcases=ntestcases, $
                                           ntests=ntests, $
                                           level=level
  compile_opt strictarr

  indent = level eq 0 ? '' : string(bytarr(level * self.indent) + self.space)
  printf, self.logLun, $
          indent + 'Starting test suite ' + testsuite $
          + ' (' $
          + strtrim(ntestcases, 2) + ' test case' + (ntestcases eq 1 ? '' : 's') $
          + ', ' $
          + strtrim(ntests, 2) + ' test' + (ntests eq 1 ? '' : 's') $
          + ')'
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
pro mgtestclirunner::reportTestSuiteResult, npass=npass, nfail=nfail, level=level
  compile_opt strictarr

  indent = string(bytarr((level + 1L) * self.indent) + self.space)
  printf, self.logLun, $
          indent + 'Results: ' $
          + strtrim(npass, 2) + ' / ' + strtrim(npass + nfail, 2) $
          + ' tests passed'
end


;+
; Report a test case has begun.
; 
; @param testcase {in}{required}{type=string} name of test case
; @keyword ntests {in}{required}{type=integer} number of tests contained in this
;          test case
; @keyword level {in}{required}{type=level} level of test case
;-
pro mgtestclirunner::reportTestCaseStart, testcase, ntests=ntests, level=level
  compile_opt strictarr

  indent = string(bytarr(level * self.indent) + self.space)
  printf, self.logLun, $
          indent + 'Starting ' + testcase $
          + ' (' + strtrim(ntests, 2) + ' test' + (ntests eq 1 ? '' : 's') + ')' 
end


;+
; Report the results of a test case.
;
; @keyword npass {in}{required}{type=integer} number of passing tests
; @keyword nfail {in}{required}{type=integer} number of failing tests
; @keyword level {in}{required}{type=integer} level of test case
;-
pro mgtestclirunner::reportTestCaseResult, npass=npass, nfail=nfail, level=level
  compile_opt strictarr

  indent = string(bytarr((level + 1L) * self.indent) + self.space)
  printf, self.logLun, $
          indent + 'Results: ' $
          + strtrim(npass, 2) + ' / ' + strtrim(npass + nfail, 2) $
          + ' tests passed'
end


;+
; Report the start of single test.
; 
; @param testname {in}{required}{type=string} name of test
; @keyword level {in}{required}{type=integer} level of test case
;-
pro mgtestclirunner::reportTestStart, testname, level=level
  compile_opt strictarr

  indent = string(bytarr((level + 1L) * self.indent) + self.space)
  printf, self.logLun, indent + testname + ': ', format='(A, $)'
end


;+
; Report the result of a single test.
; 
; @param msg {in}{required}{type=string} message to display when test fails
; @keyword passed {in}{required}{type=boolean} whether the test passed
;-
pro mgtestclirunner::reportTestResult, msg, passed=passed
  compile_opt strictarr

  printf, self.logLun, (passed ? 'passed' : 'failed "' + msg + '"')
end


;+
; Free resources.
;-
pro mgtestclirunner::cleanup
  compile_opt strictarr

  if (self.logLun gt 0) then free_lun, self.logLun
  self->mgtestrunner::cleanup
  !quiet = 0
end


;+
; Initialize the test runner.
;
; @returns 1 for success, 0 for failure
; @keyword log_file {in}{optional}{type=string} if present, output is sent to
;          that file, otherwise output is sent to stdout
;-
function mgtestclirunner::init, log_file=logFile
  compile_opt strictarr

  if (~self->mgtestrunner::init()) then return, 0B

  if (n_elements(logFile) gt 0) then begin
    openw, logLun, logFile, /get_lun
    self.logLun = logLun
  endif else begin
    self.logLun = -1L
  endelse

  self.indent = 3L
  self.space = (byte(' '))[0]

  !quiet = 1
  return, 1B
end


;+
; Define member variables.
;
; @file_comments Results for tests, test cases, and test suites are reported to
;                the test runner. The MGtestCliRunner displays the results in
;                the output log or in a log file.
;
; @field logLun the logical unit number to send output to (-1L by default)
; @field indent number of spaces a single indent should be
; @field space byte value of the space character
;-
pro mgtestclirunner__define
  compile_opt strictarr

  define = { MGtestCliRunner, inherits MGtestRunner, $
             logLun : 0L, $
             indent : 0L, $
             space : 0B $
           }
end
