a = new Array();

a[1] = new Array("./assert.html", "assert.pro", "", "    Raises an error if the given condition is not met        param condition  in required type boolean  condition to assert    param msg  in optional type string default Assertion failed  message to          throw if condition is not met   pro assert  condition  msg   compile_opt strictarr  logical_predicate  hidden   on_error  2    if  condition  then message  n_elements msg  eq 0    Assertion failed  : msg end");
a[2] = new Array("./error_is_fail.html", "error_is_fail.pro", "", "");
a[3] = new Array("./error_is_pass.html", "error_is_pass.pro", "", "");
a[4] = new Array("./mgtestcase__define.html", "mgtestcase__define.pro", "", "    This is a safe place to actually run a single test  Any errors that occur are   assumed to be from the test and recorded as a failure for it       returns boolean    param testname  in required type string  name of method    keyword message  out optional type string  error message if test failed   function mgtestcase::runTest  testname  message msg   compile_opt strictarr  logical_predicate    error   0L   catch  error   if  error ne 0L  then begin     catch   cancel     msg    error_state msg     return  0L     fail   endif    result   call_method testname  self    if  result  then msg    error_state msg   return  keyword_set result  end       Run the tests for this class  i e  methods with names that start with  test    pro mgtestcase::run   compile_opt strictarr  logical_predicate    self testRunner reportTestCaseStart  obj_class self  ntests self ntests                                            level self level      run each test   for t   0L  self ntests   1L do begin     self testRunner reportTestStart   self testnames t  level self level     result   self runTest self testnames t  message msg       if  result  then  self npass else  self nfail        remove prefix from msg if present     prefix   obj_class self     ::     self testnames t     :       if  n_elements msg  gt 0   strpos msg  prefix  eq 0  then begin       prefixLength   strlen prefix        msg   strmid msg  prefixLength      endif        remove ASSERT from msg if present      prefix    ASSERT:       if  n_elements msg  gt 0   strpos msg  prefix  eq 0  then begin       prefixLength   strlen prefix        msg   strmid msg  prefixLength      endif            construct the log message for the test     logMsg   result                                    :  n_elements msg  eq 0                                         : msg           self testRunner reportTestResult  logMsg  passed result   endfor    self testRunner reportTestCaseResult  npass self npass  nfail self nfail                                             level self level end       Find the name and number of tests  i e  methods with names that start with     test    pro mgtestcase::findTestnames   compile_opt strictarr      find tests: any method with name test    help   routines  output routines   functionsPos   where strmatch routines   Compiled Functions:  count    routines   routines functionsPos:      result   stregex routines      obj_class self     :: test                          extract   subexpr   fold_case    testnames   reform result 1         find names that matched   ind   where testnames ne   ntests    if  ntests gt 0  then begin     testnames   testnames ind    endif      record results   self ntests   ntests    self testnames   testnames end       Get properties of the object        keyword npass  out optional type integer  number of passing tests    keyword nfail  out optional type integer  number of failing tests    keyword ntests  out optional type integer  number of tests    keyword testnames  out optional type strarr  array of method names which             begin with  test    pro mgtestcase::getProperty  npass npass  nfail nfail  ntests ntests                                 testnames testnames   compile_opt strictarr      npass   self npass   nfail   self nfail   ntests   self ntests   if  arg_present testnames  then testnames    self testnames end       Test suites can contain other test suites or test cases  The level is the   number of layers down from the top most test suite  level 0       param level  in required type integer  new level of object   pro mgtestcase::setLevel  level   compile_opt strictarr      self level   level end       Free resources    pro mgtestcase::cleanup   compile_opt strictarr    ptr_free  self testnames end       Intialize test case       returns 1 for succcess  0 for failure    keyword test_runner  in required type object  subclass of MGtestRunner   function mgtestcase::init  test_runner testRunner   compile_opt strictarr    self testRunner   testRunner    self testnames   ptr_new allocate_heap    self findTestnames    self level   0L   self npass   0L   self nfail   0L    return  1B end       Define member variables       file_comments Subclass MGtestCase to actually write tests  Any function                  method whose name starts with  test  will be considered a                  test  Tests are executed and results are reported to the test                  runner object       field testRunner subclass of MGtestRunner    field testnames pointer to string array of method names that start with  test     field level number of layers down from the top containing suite    field ntests total number of tests    field npass number of passing tests    field nfail number of failing tests   pro mgtestcase__define   compile_opt strictarr    define     MGtestCase                 testRunner : obj_new                 testnames : ptr_new                 level : 0L                 ntests : 0L                 npass : 0L                 nfail : 0L                               end");
a[5] = new Array("./mgtestclirunner__define.html", "mgtestclirunner__define.pro", "", "    Report a test suite has begun        param testsuite  in required type string  name of test suite    keyword ntestcases  in required type integer  number of test suites cases             contained by the test suite    keyword ntests  in required type integer  number of tests contained in the             hierarchy below this test suite    keyword level  in required type level  level of test suite   pro mgtestclirunner::reportTestSuiteStart  testsuite                                               ntestcases ntestcases                                               ntests ntests                                               level level   compile_opt strictarr    indent   level eq 0     : string bytarr level   self indent    self space    printf  self logLun              indent    Starting test suite     testsuite                                 strtrim ntestcases  2      test case     ntestcases eq 1     :  s                                  strtrim ntests  2      test     ntests eq 1     :  s                  end       Report the results of a test suite       keyword npass  in required type integer  number of passing tests contained             in the hierarchy below the test suite    keyword nfail  in required type integer  number of failing tests contained             in the hierarchy below the test suite    keyword level  in required type integer  level of test suite   pro mgtestclirunner::reportTestSuiteResult  npass npass  nfail nfail  level level   compile_opt strictarr    indent   string bytarr level   1L    self indent    self space    printf  self logLun              indent    Results:                 strtrim npass  2            strtrim npass   nfail  2                  tests passed  end       Report a test case has begun        param testcase  in required type string  name of test case    keyword ntests  in required type integer  number of tests contained in this            test case    keyword level  in required type level  level of test case   pro mgtestclirunner::reportTestCaseStart  testcase  ntests ntests  level level   compile_opt strictarr    indent   string bytarr level   self indent    self space    printf  self logLun              indent    Starting     testcase                     strtrim ntests  2      test     ntests eq 1     :  s       end       Report the results of a test case       keyword npass  in required type integer  number of passing tests    keyword nfail  in required type integer  number of failing tests    keyword level  in required type integer  level of test case   pro mgtestclirunner::reportTestCaseResult  npass npass  nfail nfail  level level   compile_opt strictarr    indent   string bytarr level   1L    self indent    self space    printf  self logLun              indent    Results:                 strtrim npass  2            strtrim npass   nfail  2                  tests passed  end       Report the start of single test        param testname  in required type string  name of test    keyword level  in required type integer  level of test case   pro mgtestclirunner::reportTestStart  testname  level level   compile_opt strictarr    indent   string bytarr level   1L    self indent    self space    printf  self logLun  indent   testname    :   format A    end       Report the result of a single test        param msg  in required type string  message to display when test fails    keyword passed  in required type boolean  whether the test passed   pro mgtestclirunner::reportTestResult  msg  passed passed   compile_opt strictarr    printf  self logLun   passed    passed  :  failed     msg     end       Free resources    pro mgtestclirunner::cleanup   compile_opt strictarr    if  self logLun gt 0  then free_lun  self logLun   self mgtestrunner::cleanup end       Initialize the test runner       returns 1 for success  0 for failure    keyword log_file  in optional type string  if present  output is sent to            that file  otherwise output is sent to stdout   function mgtestclirunner::init  log_file logFile   compile_opt strictarr    if  self mgtestrunner::init  then return  0B    if  n_elements logFile  gt 0  then begin     openw  logLun  logFile   get_lun     self logLun   logLun   endif else begin     self logLun    1L   endelse    self indent   3L   self space    byte   0     return  1B end       Define member variables       file_comments Results for tests  test cases  and test suites are reported to                  the test runner  The MGtestCliRunner displays the results in                  the output log or in a log file       field logLun the logical unit number to send output to  1L by default     field indent number of spaces a single indent should be    field space byte value of the space character   pro mgtestclirunner__define   compile_opt strictarr    define     MGtestCliRunner  inherits MGtestRunner                 logLun : 0L                 indent : 0L                 space : 0B                end");
a[6] = new Array("./mgtestrunner__define.html", "mgtestrunner__define.pro", "", "    Report a test suite has begun        param testsuite  in required type string  name of test suite    keyword ntestcases  in required type integer  number of test suites cases             contained by the test suite    keyword ntests  in required type integer  number of tests contained in the             hierarchy below this test suite    keyword level  in required type level  level of test suite   pro mgtestrunner::reportTestSuiteStart  testsuite                                            ntestcases ntestcases                                            ntests ntests                                            level level   compile_opt strictarr end       Report the results of a test suite       keyword npass  in required type integer  number of passing tests contained             in the hierarchy below the test suite    keyword nfail  in required type integer  number of failing tests contained             in the hierarchy below the test suite    keyword level  in required type integer  level of test suite   pro mgtestrunner::reportTestSuiteResult  npass npass  nfail nfail  level level   compile_opt strictarr  end       Report a test case has begun        param testcase  in required type string  name of test case    keyword ntests  in required type integer  number of tests contained in this             test case    keyword level  in required type level  level of test case   pro mgtestrunner::reportTestCaseStart  testcase  ntests ntests  level level   compile_opt strictarr  end       Report the results of a test case       keyword npass  in required type integer  number of passing tests    keyword nfail  in required type integer  number of failing tests    keyword level  in required type integer  level of test case   pro mgtestrunner::reportTestCaseResult  npass npass  nfail nfail  level level   compile_opt strictarr  end       Report the start of single test        param testname  in required type string  name of test    keyword level  in required type integer  level of test case   pro mgtestrunner::reportTestStart  testname  level level   compile_opt strictarr  end       Report the result of a single test        param msg  in required type string  message to display when test fails    keyword passed  in required type boolean  whether the test passed   pro mgtestrunner::reportTestResult  msg  passed passed   compile_opt strictarr  end       Free resources    pro mgtestrunner::cleanup   compile_opt strictarr  end       Initialize the test runner       returns 1 for success  0 for failure   function mgtestrunner::init   compile_opt strictarr      return  1B end       Define member variables       file_comments Results for tests  test cases  and test suites are reported to                  the test runner  Each subclass of MGtestRunner displays them in                  some way MGtestRunner itself is abstract and shouldn t be                  instantiated         field dummy needed because IDL requires at least one field   pro mgtestrunner__define   compile_opt strictarr    define     MGtestRunner  dummy : 0L   end");
a[7] = new Array("./mgtestsuite__define.html", "mgtestsuite__define.pro", "", "    Run the contained test suites or test cases    pro mgtestsuite::run   compile_opt strictarr    self getProperty  name name  ntestcases ntestcases  ntests ntests   self testRunner reportTestSuiteStart  name                                             ntestcases ntestcases                                             ntests ntests                                             level self level    ntestcases   self testcases count    for t   0L  ntestcases   1L do begin     otestcase   self testcases get position t      otestcase run            accumulate results     otestcase getProperty  npass npass  nfail nfail     self npass   npass     self nfail   nfail   endfor    self testRunner reportTestSuiteResult  npass self npass  nfail self nfail                                              level self level end       Add a scalar or array of test suites or test cases       param tests  in required type strarr  classnames of test suites or test          cases    pro mgtestsuite::add  tests   compile_opt strictarr      for t   0L  n_elements tests    1L do begin     otestcase   obj_new tests t  test_runner self testRunner      otestcase setLevel  self level   1L     self testcases add  otestcase   endfor end       Get properties of the object        keyword name  out optional type string  name of the object    keyword npass  out optional type integer  number of passing tests contained            in the hierarchy below this object    keyword nfail  out optional type integer  number of failing tests contained            in the hierarchy below this object    keyword ntestcases  out optional type integer  number of directly contained            test suites or test cases    keyword ntests  out optional type integer  number of tests contained in the            hierarchy below this object   pro mgtestsuite::getProperty  name name  npass npass  nfail nfail                                  ntestcases ntestcases  ntests ntests   compile_opt strictarr    name   self name   npass   self npass   nfail   self nfail   if  arg_present ntestcases  then ntestcases   self testcases count     if  arg_present ntests  then begin     ntests   0L     for t   0L  self testcases count    1L do begin       otestcase   self testcases get position t        otestcase getProperty  ntests nCaseTests       ntests   nCaseTests     endfor   endif  end       Test suites can contain other test suites or test cases  The level is the   number of layers down from the top most test suite  level 0       param level  in required type integer  new level of object   pro mgtestsuite::setLevel  level   compile_opt strictarr    self level   level   for t   0L  self testcases count    1L do begin     testcase   self testcases get position t      testcase setLevel  level   1   endfor end       Free resources    pro mgtestsuite::cleanup   compile_opt strictarr    obj_destroy  self testcases end       Initialize test suite       returns 1 for success  0 for failure    keyword name  in optional type string default classname  name of the object    keyword test_runner  in required type object  subclass of MGtestRunner   function mgtestsuite::init  name name  test_runner testRunner   compile_opt strictarr    self name   n_elements name  eq 0   obj_class self  : name   self level   0L     self testRunner   testRunner     self testcases   obj_new IDL_Container       self npass   0L   self nfail   0L    return  1B end       Define member variables        file_comments Test suites are containers for test cases  Either subclass                  MGtestSuite and add test suites test cases in its init method                  or create a MGtestSuite and use the add method to add test                  suites cases        field name name of the object    field level number of layers below the top most containing test suite    field testcases IDL_Container holding test suites or test cases    field testRunner subclass of MGtestRunner    field npass number of passing tests contained in the hierarchy below this           test suite    field nfail number of failing tests contained in the hierarchy below this          test suite   pro mgtestsuite__define   compile_opt strictarr    define     MGtestSuite                 name :                  level : 0L                 testcases : obj_new                 testRunner : obj_new                 npass : 0L                 nfail : 0L                end");
a[8] = new Array("./mgunit.html", "mgunit.pro", "", "    Runs unit tests         param tests  in optional type strarr  array of test suites and or test cases    keyword log_file  in optional type string  name of file to send output to  if             not present sends output to the output log   pro mgunit  tests  log_file logFile   compile_opt strictarr    testRunner   obj_new MGtestCliRunner  log_file logFile       if  n_elements tests  gt 0  then begin     testsuite   obj_new MGtestSuite  test_runner testRunner      testsuite add  tests     testsuite run     obj_destroy  testsuite   endif    obj_destroy  testRunner end");


var URL         = 0;
var FILENAME    = 1;
var DESCRIPTION = 2;
var CONTENTS    = 3;
var MATCH_TYPE  = 4;
var N_MATCHES   = 5;
var SCORE       = 6;
var i = 7;
var MATCHES     = 8;

var html;

var searchString;
var wildcard;
var invalidSearchString;
var origSearchString;
var styles = "<link rel=\"stylesheet\" type=\"text/css\" href=\"search.css\" />"

FILENAME_SCORE_ORDER    = 0;
URL_SCORE_ORDER         = 1;
DESCRIPTION_SCORE_ORDER = 2;
CONTENT_SCORE_ORDER     = 3;

SCORE_PER_TYPE = 15000;

var sortResultsByType = true;
var omitDescriptions = false;
var addMatchSummary = true;

var searchTitles = true;
var searchDescriptions = true;
var searchContent = true;

var footer = "";

function isAlnumAmp(ch) {
  if ((ch >= "a" && ch <= "z") || (ch == "&") ||(ch >= "A" && ch <= "Z") || (ch >= "0" && ch <="9")) {
    return true;
  } else {
    return false;
  }
}

function searchElement(fileNumber, matchType, upperSearchString) {
  var element = a[fileNumber][matchType].toUpperCase();
  var w, x, y;
  var z = 0;

  a[fileNumber][N_MATCHES] = 0;
  w = element.indexOf(upperSearchString);
  while (w >= 0){
    z = z + w + 1;
    if ((wildcard == -2) || (wildcard == -5)) {
      x = false;
    } else {
      if (w == 0) {
	x = false;
      } else {
	x = isAlnumAmp(element.charAt(w - 1));
      }
    }
    if ((wildcard == -3) || (wildcard == -5)) {
      y = false;
    } else {
      if (element.length - w == upperSearchString.length) {
	y = false;
      } else {
	y = isAlnumAmp(element.charAt(w + upperSearchString.length));
      }
    }
    if (!x && !y) {
      a[fileNumber][MATCHES + a[fileNumber][N_MATCHES]] = z - 1;
      a[fileNumber][N_MATCHES]++;
          }
    element = element.substring(w + 1, element.length);
    while (isAlnumAmp(element.charAt(0)) && element.length > 0) {
      element = element.substring(1, element.length);
      z++;
    }
    w = element.indexOf(upperSearchString);
  }
}

function searchFile(fileNumber, upperSearchString) {
  var matchIndex = -1, matchType;

  a[fileNumber][MATCH_TYPE] = -1;

  while (++matchIndex <= CONTENTS && a[fileNumber][MATCH_TYPE] == -1) {
    matchType = matchIndex == 0 ? FILENAME : (matchIndex == 1 ? DESCRIPTION : (matchIndex == 2 ? URL : CONTENTS));
    if ((matchType == FILENAME || matchType == URL) && !searchTitles) {
      continue;
    }

    if (matchType == DESCRIPTION && !searchDescriptions) {
      continue;
    }

    if (matchType == CONTENTS && !searchContent) {
      continue;
    }

    searchElement(fileNumber, matchType, upperSearchString);
    if (a[fileNumber][N_MATCHES] > 0) {
      a[fileNumber][MATCH_TYPE] = matchType;
    }
  }
}

function sortResults() {
  var fileNumber, t, tempScore, E;

  for (fileNumber = 1; fileNumber < a.length; fileNumber++) {
    a[fileNumber][i] = fileNumber;
  }

  if (sortResultsByType) {
    for (fileNumber = 1; fileNumber < a.length; fileNumber++) {
      if (a[fileNumber][MATCH_TYPE] == FILENAME) {
	a[fileNumber][SCORE] = (4 - FILENAME_SCORE_ORDER) * SCORE_PER_TYPE;
      } else if (a[fileNumber][MATCH_TYPE] == DESCRIPTION) {
	a[fileNumber][SCORE] = (4 - DESCRIPTION_SCORE_ORDER) * SCORE_PER_TYPE;
      } else if (a[fileNumber][MATCH_TYPE] == URL) {
	a[fileNumber][SCORE] = (4 - URL_SCORE_ORDER) * SCORE_PER_TYPE;
      } else {
	a[fileNumber][SCORE] = (4 - CONTENT_SCORE_ORDER) * SCORE_PER_TYPE + a[fileNumber][N_MATCHES];
      }
    }
    for (fileNumber = 2; fileNumber < a.length; fileNumber++) {
      tempScore = a[fileNumber][SCORE];
      E = a[fileNumber][i];
      for (t = fileNumber; t > 1 && tempScore > a[t-1][SCORE]; t--) {
	a[t][SCORE] = a[t-1][SCORE];
	a[t][i] = a[t-1][i];
      }
      a[t][SCORE] = tempScore;
      a[t][i] = E;
    }
  }
}

function putMatchSummary(fileNumber) {
  var pluralSuffix = a[fileNumber][N_MATCHES]==1 ? "" : "es";

  html += "";
  html += "<span class=\"match\">";
  if (a[fileNumber][MATCH_TYPE] == FILENAME) {
    html += " - matched title";
  } else {
    if (a[fileNumber][MATCH_TYPE] == DESCRIPTION) {
      html += " - matched description";
    } else {
      if (a[fileNumber][MATCH_TYPE] == URL) {
	html += " - matched URL";
      } else {
	html += " - " + a[fileNumber][N_MATCHES] + " match" + pluralSuffix + "";
      }
    }
  }
  html += "</span>";
}

function putMatchDescription(fileNumber, curMatch) {
  var matchLocation = a[fileNumber][MATCHES + curMatch - 1];
  var matchStart = matchLocation < 35 ? 0 : matchLocation - 35;
  var matchEnd = (matchLocation + 35 > a[fileNumber][CONTENTS].length) ? a[fileNumber][CONTENTS].length : matchLocation + 35;

  var Q = false;
  while ((matchStart >= 0) && !Q) {
    if (isAlnumAmp(a[fileNumber][CONTENTS].charAt(matchStart))) {
      matchStart--;
    } else {
      Q = true;
    }
  }
  matchStart++;

  Q = false;
  while ((matchEnd > matchLocation) && !Q) {
    if (isAlnumAmp(a[fileNumber][CONTENTS].charAt(matchEnd))) {
      matchEnd--;
    } else {
      Q = true;
    }
  }

  html += "<br>\".. " + a[fileNumber][CONTENTS].substring(matchStart, matchLocation);
  html += "<span class=\"foundString\">" + a[fileNumber][CONTENTS].substring(matchLocation, matchLocation + searchString.length) + "</span>";
  html += a[fileNumber][CONTENTS].substring(matchLocation + searchString.length, matchEnd) + " ..\"";
}

function putAllMatchDescriptions(fileNumber) {
  if (omitDescriptions == false) {
    var curMatch = 1;
    while ((curMatch < 4) && (curMatch <= a[fileNumber][N_MATCHES])) {
      putMatchDescription(fileNumber, curMatch);
      curMatch++;
    }
  }
}

function putFoundString(v, R) {
  html += v.substring(0, R);
  html += "<span class=\"foundString\">" + v.substring(R, R + searchString.length) + "</span>";
  html += v.substring(R + searchString.length, v.length);
}

function putItem(fileNumber, itemNumber) {
  html += "<p>" + itemNumber + ". ";
  html += "<a href=\"" + a[fileNumber][URL] + "\" target=\"file_frame\">" + a[fileNumber][FILENAME] + "</a>";

  if (a[fileNumber][MATCH_TYPE] == CONTENTS) {
    putAllMatchDescriptions(fileNumber);
  } else {
    html += "";
  }

  if (a[fileNumber][MATCH_TYPE] == DESCRIPTION) {
    html += "<span class=\"description\">";
    html += "<br>Description: ";
    html += "</span>";
    html += "";
    putFoundString(a[fileNumber][DESCRIPTION], a[fileNumber][MATCHES]);
  } else {
    if( a[fileNumber][DESCRIPTION].length > 0 ) {
      html += "<span class=\"description\">";
      html += "<br>Description: ";
      html += "</span>";
      html += "" + a[fileNumber][DESCRIPTION];
    } else {
    }
  }
  html += "<br>";
  html += "<span class=\"file\">";
  if (a[fileNumber][MATCH_TYPE] == URL) {
    putFoundString(a[fileNumber][URL], a[fileNumber][MATCHES]);
  } else {
    html += a[fileNumber][URL];
  }
  html += "</span>";
  if (addMatchSummary) {
    putMatchSummary(fileNumber);
  }
  html += "<br>";
}

function putResults() {
  var itemNumber = 0;

  if (!(invalidSearchString)) {
    for (var fileNumber = 1; fileNumber < a.length; fileNumber++) {
      if (a[a[fileNumber][i]][N_MATCHES] > 0) {
	putItem(a[fileNumber][i], ++itemNumber);
      }
    }
  } else {
    if (wildcard == -4) {
      html += "<p><span class=\"error\">ERROR:</span>&nbsp;The wildcard chararcter (*) must be at the beginning or end of the text.</p>";
    }
  }
}

function putHeader() {
  html += "<html><head><title>Search results for \"" + origSearchString + "\"</title>";
  html += styles;
  html += "</head><body>";
  html += "<h1>Search Results</h1>";
  html += "<div id=\"container\">";
  html += "<p>You searched for <span class=\"foundString\">" + origSearchString +".</span></p>";
}

function putFooter() {
  var nMatches = 0;

  if (!(invalidSearchString)) {
    for (var fileNumber = 1; fileNumber < a.length; fileNumber++) {
      if (a[fileNumber][N_MATCHES] > 0) {
	nMatches++;
      }
    }
  }

  if (nMatches == 0) {
    html += "<p>No pages matched your search.</p>";
  } else {
    var plural = nMatches == 1 ? "" : "s";
    html += "<p id=\"tagline\">" + nMatches + " page" + plural + " listed.</p>";
  }
  html += footer;
  html += "</div></body></html>";
}

function launchBrowser() {
  var htmlCode = html;

  iu = open("", "Object", "resizable=yes,scrollbars=yes,toolbar=no,menubar=no,location=no,directories=no,width=475,height=600");

  if ((navigator.appName.indexOf("Microsoft")!=-1) && (navigator.appVersion.indexOf("3.0") != -1)) {
    alert("Click to see results");
  }

  iu.document.open();
  iu.document.write(htmlCode);
  iu.document.close();
}

function replaceSpecialChars(str) {
  var returnStr = "";

  for (var index = 0; index < str.length; index++) {
    if (str.charAt(index) == "<") {
      returnStr += "&lt;";
    } else if (str.charAt(index) == ">") {
      returnStr += "&gt;";
    } else if (str.charAt(index) == "\"") {
      returnStr += "&quot;";
    } else {
      returnStr += str.charAt(index);
    }
  }
  return(returnStr);
}

function checkSearchString() {
  wildcard = searchString.indexOf("*");
  if (wildcard == 0) {
    wildcard = -2;
    invalidSearchString = false;
  } else if (wildcard == searchString.length -1) {
    wildcard = -3;
    invalidSearchString = false;
  } else if (wildcard > 0 ) {
    wildcard = -4;
    invalidSearchString = true;
  } else {
    invalidSearchString = false;
  }

  if (searchString.indexOf("*") != searchString.lastIndexOf("*")) {
    if (wildcard == -2) {
      if (searchString.lastIndexOf("*") == searchString.length - 1) {
	wildcard = -5;
      } else {
	wildcard = -4;
	invalidSearchString = true;
      }
    }
  }

  if ((wildcard == -2) || (wildcard == -5)) {
    searchString = searchString.substring(1, searchString.length);
  }

  if ((wildcard == -3) || (wildcard == -5)) {
    searchString = searchString.substring(0, searchString.length - 1);
  }
}

function toggleOmitDescriptions() {
  omitDescriptions = !omitDescriptions;
}

function toggleSortResults() {
  sortResultsByType = !sortResultsByType;
}

function toggleMatchSummary() {
  addMatchSummary = !addMatchSummary;
}

function toggleSearchTitles() {
  searchTitles = !searchTitles;
}

function toggleSearchDescriptions() {
  searchDescriptions = !searchDescriptions;
}

function toggleSearchContent() {
  searchContent = !searchContent;
}

function startsearch() {
  var upperSearchString;
  searchString = document.formSearch.txtSearch.value;

  if ((searchString.length > 0) && (searchString != "*")) {
    html = "";
    origSearchString = searchString;
    searchString = replaceSpecialChars(searchString);
    checkSearchString();
    upperSearchString = searchString.toUpperCase();
    if (!(invalidSearchString)) {
      for (var fileNumber = 1; fileNumber < a.length; fileNumber++) {
	searchFile(fileNumber, upperSearchString);
      }
      sortResults();
    }
    putHeader();
    putResults();
    putFooter();
    launchBrowser();
  }
}
