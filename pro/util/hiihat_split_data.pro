;+ 
; Utility function for splitting labeled data across multiple folds
; for cross-validation. If multiple domains are specified, points are
; sampled sequentially across each domain and indices are returned in
; order of the domains.
;
; :Author: Brian Bue
;
; :Categories: 
;  util
;
; :Params:
;  labels: in, required, type=intarr(n)
;    labels for each of the data points
;  num_folds: in, required, type=fix
;    number of folds for data splitting
;
; :Keywords:
;  domains: in, optional, type=intarr(n)
;    domains for each of the data points
;  verbose: in, optional, type=boolean
;    print verbose output to console
;  train_idx: out, required, type=intarr(ntr)
;    output training indices
;  test_idx: out, required, type=intarr(nte)
;    output testing indices
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
pro hiihat_split_data, labels, num_folds, domains=domains, verbose=verbose, $
                       train_idx=train_idx, test_idx=test_idx
  
  if not keyword_set(domains) then domains=intarr(n_elements(labels))+1
  
  ulab = labels[uniq(labels,sort(labels))]
  udom = domains[uniq(domains,sort(domains))]

  k = n_elements(ulab) ;; number of classes
  D = n_elements(udom) ;; number of domains

  ;; re-initialize train/test indices
  if keyword_set(train_idx) then temp=size(temporary(train_idx))
  if keyword_set(test_idx) then temp=size(temporary(test_idx))

  for di=0,D-1 do begin 
     for ki=0,k-1 do begin
        ;; get the number of samples in this domain/class
        labdom_idx = where(labels eq ulab[ki] and domains eq udom[di])
        num_labdom = n_elements(labdom_idx)
        if num_labdom eq 0 then continue
        inds = sort(randomu(seed,num_labdom)*num_labdom)
        ;; select at least 1 training sample, 
        num_tr = max([1,num_labdom-long(num_labdom/num_folds)])
        num_te = num_labdom-num_tr

        ;; get at least one test point if possible
        if num_te eq 0 and num_tr gt 1 then begin
           num_tr -= 1
           num_te = 1
        endif
        
        if verbose then $
           print, num_tr, ' train points', num_te, ' test points'

        tr_inds = labdom_idx[inds[0:num_tr-1]]
        if n_elements(train_idx) eq 0 then train_idx = tr_inds else $ 
           train_idx = [train_idx,tr_inds]                 

        if num_te gt 0 then begin
           te_inds = labdom_idx[inds[num_tr:(num_tr+num_te)-1]]           
           if n_elements(test_idx) eq 0 then test_idx = te_inds else $
              test_idx = [test_idx,te_inds]                    
        endif        
     endfor
  endfor
end
