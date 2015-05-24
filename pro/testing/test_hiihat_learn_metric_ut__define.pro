pro test_hiihat_learn_metric_ut__define
;+
; Unit test entry point definition for metric learning functions.
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
;   mgunit, 'test_hiihat_learn_metric_ut'
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
  
  define = { test_hiihat_learn_metric_ut, $
             tmpdir:'', seed:0, verbose:0, debug:0, $
             inherits MGTestCase } 
end

function test_hiihat_learn_metric_ut::test_0_init
;+
; Initialization test and parameters for metric learning functions.
;
; :Categories:
;   testing
;
; :Description:
;   Initializes parameters used by the learn_metricing unit tests and
;   verifies that the HiiHAT directory is
;   accessible. This test is run before any other learn_metricing
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

function test_hiihat_learn_metric_ut::test_split_data
;+
; Tests the split data function given a set of labels
;
; :Categories:
;   testing
;
; :Description:
;   Tests the split data function given a set of labels
;
; :History:
;   May 30, 2011 (BDB): Initial implementation
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
  labels = [intarr(30)+1, intarr(10)+2, intarr(2)+3, intarr(1)+4]

  hiihat_split_data, labels, 2, verbose=self.verbose, $
                     train_idx=fold_tr, test_idx=fold_te
  trlab = labels[fold_tr]
  telab = labels[fold_te]

  utr = trlab[uniq(trlab,sort(trlab))]
  ute = trlab[uniq(telab,sort(telab))]

  assert, total(utr eq [1,2,3,4]) eq 4, 'Incorrectly sampled training data'
  assert, total(ute eq [1,2,3]) eq 3, 'Incorrectly sampled testing data'

  return, 1
end


function test_hiihat_learn_metric_ut::test_classify_mindist
;+
; Tests the mindist classifier on three linearly separable gaussians
;
; :Categories:
;   testing
;
; :History:
;   May 30, 2011 (BDB): Initial implementation
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
  nper = 100

  mu1 = [0,0]
  mu2 = [0,6]
  mu3 = [6,6]

  class1 = randomn(seed,nper,2)
  class1[*,0] += mu1[0]
  class1[*,1] += mu1[1]
  
  class2 = randomn(seed,nper,2)
  class2[*,0] += mu2[0]
  class2[*,1] += mu2[1]

  class3 = randomn(seed,nper,2)
  class3[*,0] += mu3[0]
  class3[*,1] += mu3[1]

  labels = [intarr(nper)+1,intarr(nper)+2,intarr(nper)+3]
  data = [class1,class2,class3]



  hiihat_split_data, labels, 2, verbose=self.verbose, $
                     train_idx=fold_tr, test_idx=fold_te


  tr_dat = data[fold_tr,*]
  te_dat = data[fold_te,*]
  tr_lab = labels[fold_tr]
  te_lab = labels[fold_te]

  pred = hiihat_classify_mindist(tr_dat,tr_lab,te_dat)
  acc = float(total(pred eq te_lab))/n_elements(te_lab)

  if self.verbose then print, "MinDist classifier accuracy:", acc

  assert, acc gt 0.95, 'MinDist accuracy too low'

  return, 1
end

function test_hiihat_learn_metric_ut::test_metric_learning
;+
; Tests metric learning in separating two nonspherical gaussians
;
; :Categories:
;   testing
;
; :Description:
;   Tests the metric learning function in separating a pair of
;   nonspherical gaussians 
;
; :History:
;   May 27, 2011 (BDB): Initial implementation
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
  nper = 100

  mu1 = [0,0]
  mu2 = [0,3]
  A = [[.166,.5],[.5,2.142]]
  
  choldc, A, P, /double ;; cholesky decompositon

  for j=0, 1 do for i = j, 1 do A[i,j] = 0d ;; leave only lower triangle
  A[(lindgen(2)*3)] = P ;; update diagonal

  ;; generate classes with given covariance
  class1 = A ## randomn(seed,nper,2)
  class1[*,0] += mu1[0]
  class1[*,1] += mu1[1]
  
  class2 = A ## randomn(seed,nper,2)
  class2[*,0] += mu2[0]
  class2[*,1] += mu2[1]
  
  labels = [intarr(nper)+1,intarr(nper)+2]
  data = [class1,class2]

  reg_parms = {reg_type:'constant', lambda:0, lambda_min:0,$
               lambda_max:1, lambda_delta:0.3, num_folds:2}

  A = hiihat_mdmc_xvalidate(data, labels, reg_parms, rank=rank, $
                            verbose=self.verbose)

  hiihat_split_data, labels, 2, verbose=self.verbose, $
                     train_idx=fold_tr, test_idx=fold_te


  tr_dat = data[fold_tr,*]
  te_dat = data[fold_te,*]
  tr_dat_lda = reform(A ## tr_dat, n_elements(fold_tr), rank)
  te_dat_lda = reform(A ## te_dat, n_elements(fold_te), rank)
  tr_lab = labels[fold_tr]
  te_lab = labels[fold_te]

  pred = hiihat_classify_mindist(tr_dat,tr_lab,te_dat)
  acc_euc = float(total(pred eq te_lab))/n_elements(te_lab)

  pred = hiihat_classify_mindist(tr_dat_lda,tr_lab,te_dat_lda)
  acc_lda = float(total(pred eq te_lab))/n_elements(te_lab)

  if self.verbose then begin
     ;; euclidean accuracy should be near 80%
     ;; lda accuracy should be near 90%
     print, 'Euclidean accuracy', acc_euc
     print, 'LDA accuracy', acc_lda

     range = [-6,6]

     plot, class1[*,0],class1[*,1], xr=range,yr=range,color=255, ps=4
     oplot, class2[*,0],class2[*,1],color=100, ps=4
  endif


  assert, acc_euc gt 0.7, 'Euclidean accuracy too low'
  assert, acc_euc lt acc_lda, 'Euclidean accuracy > LDA accuracy'
  assert, acc_lda gt .9 , 'LDA accuracy too low'
  return, 1
end


