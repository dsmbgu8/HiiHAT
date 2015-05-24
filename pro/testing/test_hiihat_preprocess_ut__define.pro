pro test_hiihat_preprocess_ut__define
;+
; Unit test entry point definition for preprocessing functions.
; Initializes MGTestCase structure for MGUnit unit test. 
;
; :Categories:
;   testing
;
; :History:
;   Jan 7, 2011 (BDB): Initial implementation
;  
; :Examples:
;   This unit test can be run with the following command:
;
;   mgunit, 'test_hiihat_preprocess_ut'
;
; :Author: Brian D. Bue
;
; :Copyright:
;  Copyright 2011, by the California Institute of Technology. ALL RIGHTS
;  RESERVED. United States Government Sponsorship acknowledged. Any commercial
;  use must be negotiated with the Office of Technology Transfer at the
;  California Institute of Technology.
; 
;  This software may be subject to U.S. export control laws and regulations.  By
;  accepting this document, the user agrees to comply with all applicable U.S.
;  export laws and regulations.  User has the responsibility to obtain export
;  licenses, or other export authority as may be required before exporting such
;  information to foreign countries or providing access to foreign persons.
;-  compile_opt strictarr
  
  define = { test_hiihat_preprocess_ut, $
             tmpdir:'', seed:0, verbose:0, debug:0, $
             inherits MGTestCase } 
end

function test_hiihat_preprocess_ut::test_0_init
;+
; Initialization test and parameters for preprocessing functions.
;
; :Categories:
;   testing
;
; :Description:
;   Initializes parameters used by the preprocessing unit tests and
;   verifies that the HiiHAT directory is
;   accessible. This test is run before any other preprocessing
;   unit tests are executed. 
;
; :History:
;   Jan 7, 2011 (BDB): Initial implementation
;  
; :Author: Brian D. Bue
;
; :Copyright:
;  Copyright 2011, by the California Institute of Technology. ALL RIGHTS
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
  hh_path = hiihat_get_config_parm('hiihat_path')
  assert, file_test(hh_path), "HiiHAT directory missing"
  self.verbose = 0
  self.debug = 0
  self.tmpdir = getenv("IDL_TMPDIR")
  self.seed = 1
  return, 1
end

function test_hiihat_preprocess_ut::test_median_filter
;+
; Tests median filtering function on a simple image.
;
; :Categories:
;   testing
;
; :Description:
;   Tests the median filtering function on a 10x10x4 image where all
;   of the elements of each band correspond to their (zero-indexed)
;   band number. The median filtered result should consist of only the
;   3 values [1,2,3].
;
; :History:
;   Jan 7, 2011 (BDB): Initial implementation
;  
; :Author: Brian D. Bue
;
; :Copyright:
;  Copyright 2011, by the California Institute of Technology. ALL RIGHTS
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
  img = fltarr(10,10,4)
  img[*,*,1] += 1
  img[*,*,2] += 2
  img[*,*,3] += 3
  hiihat_median_filter_image, img, 1
  uniq_res = img[uniq(img,sort(img))] ;; should be 1,2,3
  assert, n_elements(uniq_res) eq 3, 'Median filter produced unexpected output'
  assert, total(uniq_res eq [1,2,3]) eq 3, 'Median filter produced unexpected output'
  return, 1
end

function test_hiihat_preprocess_ut::test_normalizaton
;+
; Tests normalization function on a simple image using several norm types
;
; :Categories:
;   testing
;
; :Description:
;   Tests the normalization function on a 20x20x10 unitary image using
;   the L1, L2, and Linf norms. Fails if any of the norm values are
;   non-unique or do not match their analytical value.
;
; :History:
;   Jan 7, 2011 (BDB): Initial implementation
;  
; :Author: Brian D. Bue
;
; :Copyright:
;  Copyright 2011, by the California Institute of Technology. ALL RIGHTS
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
  img = fltarr(20,20,10)+1
  hiihat_normalize_image, img,'Area (L 1)'
  uniq_res = img[uniq(img,sort(img))]
  assert, n_elements(uniq_res) eq 1, "Non-unique output for unit image"
  assert, uniq_res[0] eq 0.1, "Incorrect L1 norm output"

  img = fltarr(20,20,10)+1
  hiihat_normalize_image, img,'Euclidean (L 2)'
  uniq_res = img[uniq(img,sort(img))]
  assert, n_elements(uniq_res) eq 1, "Non-unique output for unit image"
  assert, abs(uniq_res[0]-0.316228) lt 0.000001, "Incorrect L2 norm output"

  img = fltarr(20,20,10)+1
  hiihat_normalize_image, img,'Peak (L inf)'
  uniq_res = img[uniq(img,sort(img))]
  assert, n_elements(uniq_res) eq 1, "Non-unique output for unit image"
  assert, uniq_res[0] eq 1, "Incorrect L inf norm output"

  return, 1
end

function test_hiihat_preprocess_ut::test_meandiv
;+
; Tests mean division function on a simple image.
;
; :Categories:
;   testing
;
; :Description:
;   Tests the mean diviscion function on a 10x10x10 unitary
;   image. Fails if the Mean and MeanColumn divisions do not produce
;   the same output. 
;
; :History:
;   Jan 7, 2011 (BDB): Initial implementation
;  
; :Author: Brian D. Bue
;
; :Copyright:
;  Copyright 2011, by the California Institute of Technology. ALL RIGHTS
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
  img1 = fltarr(10,10,10)+1
  img2 = img1

  hiihat_divide_mean, img1, 'Spatial Mean'
  hiihat_divide_mean, img2, 'Spectral Mean'

  dialog='Spatial and Spectral Mean division produced unmatched output on uniform image'
  assert, total(img1-img2) eq 0, dialog
  return, 1
end

