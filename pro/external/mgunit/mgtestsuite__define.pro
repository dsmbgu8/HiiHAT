;+
; Run the contained test suites or test cases.
;-
pro mgtestsuite::run
  compile_opt strictarr

  self->getProperty, name=name, ntestcases=ntestcases, ntests=ntests
  self.testRunner->reportTestSuiteStart, name, $
                                         ntestcases=ntestcases, $
                                         ntests=ntests, $
                                         level=self.level

  ntestcases = self.testcases->count()
  for t = 0L, ntestcases - 1L do begin
    otestcase = self.testcases->get(position=t)
    otestcase->run
    
    ; accumulate results
    otestcase->getProperty, npass=npass, nfail=nfail
    self.npass += npass
    self.nfail += nfail
  endfor

  self.testRunner->reportTestSuiteResult, npass=self.npass, nfail=self.nfail, $
                                          level=self.level
end


;+
; Add a scalar or array of test suites or test cases.
;
; @param tests {in}{required}{type=strarr} classnames of test suites or test
;        cases 
;-
pro mgtestsuite::add, tests
  compile_opt strictarr
  
  for t = 0L, n_elements(tests) - 1L do begin
    otestcase = obj_new(tests[t], test_runner=self.testRunner)
    otestcase->setLevel, self.level + 1L
    self.testcases->add, otestcase
  endfor
end


;+
; Get properties of the object.
; 
; @keyword name {out}{optional}{type=string} name of the object
; @keyword npass {out}{optional}{type=integer} number of passing tests contained
;          in the hierarchy below this object
; @keyword nfail {out}{optional}{type=integer} number of failing tests contained
;          in the hierarchy below this object
; @keyword ntestcases {out}{optional}{type=integer} number of directly contained
;          test suites or test cases
; @keyword ntests {out}{optional}{type=integer} number of tests contained in the
;          hierarchy below this object
;-
pro mgtestsuite::getProperty, name=name, npass=npass, nfail=nfail, $
                              ntestcases=ntestcases, ntests=ntests
  compile_opt strictarr

  name = self.name
  npass = self.npass
  nfail = self.nfail
  if (arg_present(ntestcases)) then ntestcases = self.testcases->count()

  if (arg_present(ntests)) then begin
    ntests = 0L
    for t = 0L, self.testcases->count() - 1L do begin
      otestcase = self.testcases->get(position=t)
      otestcase->getProperty, ntests=nCaseTests
      ntests += nCaseTests
    endfor
  endif

end


;+
; Test suites can contain other test suites or test cases. The level is the
; number of layers down from the top most test suite (level 0).
;
; @param level {in}{required}{type=integer} new level of object
;-
pro mgtestsuite::setLevel, level
  compile_opt strictarr

  self.level = level
  for t = 0L, self.testcases->count() - 1L do begin
    testcase = self.testcases->get(position=t)
    testcase->setLevel, level + 1
  endfor
end


;+
; Free resources.
;-
pro mgtestsuite::cleanup
  compile_opt strictarr

  obj_destroy, self.testcases
end


;+
; Initialize test suite.
;
; @returns 1 for success, 0 for failure
; @keyword name {in}{optional}{type=string}{default=classname} name of the object
; @keyword test_runner {in}{required}{type=object} subclass of MGtestRunner
;-
function mgtestsuite::init, name=name, test_runner=testRunner
  compile_opt strictarr

  self.name = n_elements(name) eq 0 ? obj_class(self) : name
  self.level = 0L 

  self.testRunner = testRunner


  self.testcases = obj_new('IDL_Container')
  
  self.npass = 0L
  self.nfail = 0L

  return, 1B
end


;+
; Define member variables.
; 
; @file_comments Test suites are containers for test cases. Either subclass
;                MGtestSuite and add test suites/test cases in its init method
;                or create a MGtestSuite and use the add method to add test
;                suites/cases.
; 
; @field name name of the object
; @field level number of layers below the top-most containing test suite
; @field testcases IDL_Container holding test suites or test cases
; @field testRunner subclass of MGtestRunner
; @field npass number of passing tests contained in the hierarchy below this 
;        test suite
; @field nfail number of failing tests contained in the hierarchy below this
;        test suite
;-
pro mgtestsuite__define
  compile_opt strictarr

  define = { MGtestSuite, $
             name : '', $
             level : 0L, $
             testcases : obj_new(), $
             testRunner : obj_new(), $
             npass : 0L, $
             nfail : 0L $
           }
end
