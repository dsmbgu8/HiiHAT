;+ 
; Wrapper to learn optimal regularization parameter lambda \in [0,1]
; for LDA via cross validation. 
;
; :Author: Brian Bue
;
; :Categories: 
;  metric_learning
;
; :Params:
;  data: in, required, type="fltarr(n,nb)"
;    labeled data to determine transform 
;  labels: in, required, type="intarr(n)"
;    labels for each of the data points
;  reg_parms: in, required, type=struct
;    regularization parameters (lambda, num_folds, etc)
;
; :Keywords:
;  domains: in, optional, type=intarr(n)
;    domains of input samples
;  in_rank: in, optional, type=fix
;    rank of transform to calculate (default = k-1)
;  rank: out, required, type=fix
;    rank of calculated transform
;  verbose: in, optional, type=boolean
;    print verbose output to console
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
function hiihat_mdmc_xvalidate, data, labels, reg_parms, domains=domains,$
                                in_rank=in_rank, rank=rank, verbose=verbose

  debug = hiihat_get_config_parm('debug')
  title='hiihat_mdmc_xvalidate'
  if debug then print, "Entering "+title
  if not keyword_set(verbose) then verbose=0

  data_size = size(data)
  n = data_size[1] ;; number of samples
  nb = data_size[2] ;; number of bands

  ;; all classes from the same domain - traditional FDA
  if not keyword_set(domains) then domains = intarr(n)+1

  ;; get unique labels and domains
  ulab = labels[uniq(labels,sort(labels))]
  udom = domains[uniq(domains,sort(domains))]
  
  k = n_elements(ulab) ;; number of classes
  D = n_elements(udom) ;; number of domains

  if verbose then begin
     print, "Evaluating regularization parameters for "+strtrim(n,2)+' samples of dimensionality '+strtrim(nb,2)
     print, "Total classes: "+strtrim(k,2), " domains: "+strtrim(D,2)     
  endif

  num_folds = reg_parms.num_folds
  lambda_min = reg_parms.lambda_min
  lambda_max = reg_parms.lambda_max
  lambda_delta = reg_parms.lambda_delta

  num_lambda = ceil((lambda_max-lambda_min)/lambda_delta)  
  
  ;; default to euclidean
  A_best = identity(nb) 
  
  acc_lda = fltarr(num_lambda)
  acc_euc = 0.0
  envi_report_init, "Learning metric", base=base, /INTERRUPT, title=title 
  envi_report_inc, base, num_folds-1
  for fi=0,num_folds-1 do begin
     envi_report_stat, base, i, num_folds-1, cancel=cancel   
     if cancel then begin
        goto, cleanup
     endif

     if verbose then print, "Fold "+strtrim(fi,2)

     hiihat_split_data, labels, num_folds, domains=domains, verbose=verbose, $
                        train_idx=fold_tr, test_idx=fold_te
     
     tr_dat = data[fold_tr,*]
     te_dat = data[fold_te,*]
     tr_lab = labels[fold_tr]
     te_lab = labels[fold_te]
     tr_dom = domains[fold_tr]
     te_dom = domains[fold_te]

     ;; re-initialize fold_{tr,te} 
     temp=size(temporary(fold_tr))
     temp=size(temporary(fold_te))

     pred = hiihat_classify_mindist(tr_dat,tr_lab,te_dat)
     acc_euc += float(total(pred eq te_lab))/n_elements(te_lab)
     
     lambda = lambda_min
     for li=0,num_lambda-1 do begin
        ;; get the LDA transformation matrix
        reg_parms.lambda = lambda
        lambda_cur = lambda 
        lambda = min([1.0,lambda+lambda_delta]) 
        A = hiihat_mdmc_lda(tr_dat, tr_lab, domains=tr_dom, $
                            reg_parms=reg_parms, rank=rank, $
                            verbose=verbose, error=error)
        if error ne 0 then begin
           print, 'Error: Singular matrix with lambda=', strtrim(lambda_cur,2)
           continue
        endif
        
        ;; apply transformation matrix
        tr_datA = reform(A ## tr_dat, n_elements(tr_lab), rank)
        te_datA = reform(A ## te_dat, n_elements(te_lab), rank)
     
        pred = hiihat_classify_mindist(tr_datA,tr_lab,te_datA)
        lda_acc = float(total(pred eq te_lab))/n_elements(te_lab)
        acc_lda[li] += lda_acc

        if verbose then print, "lambda=", lambda_cur, " LDA acc=",lda_acc        

        ;; clear rank parameter for next iteration
        temp=size(temporary(rank))
     endfor
  endfor
  envi_report_init, base=base, /finish 

  acc_best = max(acc_lda,max_sub)/num_folds
  lambda_best = lambda_min+(lambda_delta*max_sub)

  if verbose then begin
     print, 'LDA best lambda=', lambda_best, ' acc=', acc_best
     print, "EUC acc=", acc_euc/num_folds
  endif
  
  reg_parms.lambda = lambda_best
  A_best = hiihat_mdmc_lda(data, labels, domains=domains, $
                           reg_parms=reg_parms, rank=rank, $
                           verbose=verbose, error=error)
cleanup:
  if debug then print, "Exiting "+title
  return, A_best
end
