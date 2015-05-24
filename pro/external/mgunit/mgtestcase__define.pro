;+
; This is a safe place to actually run a single test. Any errors that occur are
; assumed to be from the test and recorded as a failure for it.
;
; @returns boolean
; @param testname {in}{required}{type=string} name of method
; @keyword message {out}{optional}{type=string} error message if test failed
;-
function mgtestcase::runTest, testname, message=msg
  compile_opt strictarr, logical_predicate

  error = 0L
  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    msg = !error_state.msg
    return, 0L   ; fail
  endif

  result = call_method(testname, self)
  if (~result) then msg = !error_state.msg
  return, keyword_set(result)
end


;+
; Run the tests for this class (i.e. methods with names that start with "test").
;-
pro mgtestcase::run
  compile_opt strictarr, logical_predicate

  self.testRunner->reportTestCaseStart, obj_class(self), ntests=self.ntests, $
                                        level=self.level

  ; run each test
  for t = 0L, self.ntests - 1L do begin
    self.testRunner->reportTestStart, (*self.testnames)[t], level=self.level
    result = self->runTest((*self.testnames)[t], message=msg) 
    if (result) then ++self.npass else ++self.nfail

    ; remove prefix from msg if present
    prefix = obj_class(self) + '::' + (*self.testnames)[t] + ': '
    if (n_elements(msg) gt 0 && strpos(msg, prefix) eq 0) then begin
      prefixLength = strlen(prefix)
      msg = strmid(msg, prefixLength)
    endif

    ; remove ASSERT from msg if present 
    prefix = 'ASSERT: '
    if (n_elements(msg) gt 0 && strpos(msg, prefix) eq 0) then begin
      prefixLength = strlen(prefix)
      msg = strmid(msg, prefixLength)
    endif
    
    ; construct the log message for the test
    logMsg = result $
             ? '' $ 
             : (n_elements(msg) eq 0 $
                ? '' $
                : msg)
    
    self.testRunner->reportTestResult, logMsg, passed=result
  endfor

  self.testRunner->reportTestCaseResult, npass=self.npass, nfail=self.nfail, $
                                         level=self.level
end


;+
; Find the name and number of tests (i.e. methods with names that start with 
; "test").
;-
pro mgtestcase::findTestnames
  compile_opt strictarr

  ; find tests: any method with name test*
  help, /routines, output=routines
  functionsPos = where(strmatch(routines, 'Compiled Functions:'), count)
  routines = routines[functionsPos:*]  
  result = stregex(routines, '^' + obj_class(self) + '::(test[^ ]*).*', $
                   /extract, /subexpr, /fold_case)
  testnames = reform(result[1, *])

  ; find names that matched
  ind = where(testnames ne '', ntests)
  if (ntests gt 0) then begin
    testnames = testnames[ind]
  endif

  ; record results
  self.ntests = ntests
  *self.testnames = testnames
end


;+
; Get properties of the object.
; 
; @keyword npass {out}{optional}{type=integer} number of passing tests
; @keyword nfail {out}{optional}{type=integer} number of failing tests
; @keyword ntests {out}{optional}{type=integer} number of tests
; @keyword testnames {out}{optional}{type=strarr} array of method names which 
;          begin with "test"
;-
pro mgtestcase::getProperty, npass=npass, nfail=nfail, ntests=ntests, $
                             testnames=testnames
  compile_opt strictarr
  
  npass = self.npass
  nfail = self.nfail
  ntests = self.ntests
  if (arg_present(testnames)) then testnames = *self.testnames
end


;+
; Test suites can contain other test suites or test cases. The level is the
; number of layers down from the top most test suite (level 0).
;
; @param level {in}{required}{type=integer} new level of object
;-
pro mgtestcase::setLevel, level
  compile_opt strictarr
  
  self.level = level
end


;+
; Free resources.
;-
pro mgtestcase::cleanup
  compile_opt strictarr

  ptr_free, self.testnames
end


;+
; Intialize test case.
;
; @returns 1 for succcess, 0 for failure
; @keyword test_runner {in}{required}{type=object} subclass of MGtestRunner
;-
function mgtestcase::init, test_runner=testRunner
  compile_opt strictarr

  self.testRunner = testRunner

  self.testnames = ptr_new(/allocate_heap)
  self->findTestnames

  self.level = 0L
  self.npass = 0L
  self.nfail = 0L

  return, 1B
end


;+
; Define member variables.
;
; @file_comments Subclass MGtestCase to actually write tests. Any function
;                method whose name starts with "test" will be considered a
;                test. Tests are executed and results are reported to the test
;                runner object.
;
; @field testRunner subclass of MGtestRunner
; @field testnames pointer to string array of method names that start with "test"
; @field level number of layers down from the top-containing suite
; @field ntests total number of tests
; @field npass number of passing tests
; @field nfail number of failing tests
;-
pro mgtestcase__define
  compile_opt strictarr

  define = { MGtestCase, $
             testRunner : obj_new(), $
             testnames : ptr_new(), $
             level : 0L, $
             ntests : 0L, $
             npass : 0L, $
             nfail : 0L $
             }             
end
