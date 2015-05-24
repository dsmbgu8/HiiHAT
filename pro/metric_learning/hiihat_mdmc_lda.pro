;+ 
; Calculate a LDA-based transform for multi-class/multi-domain input data. 
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
;
; :Keywords:
;  domains: in, optional, type=intarr(n)
;    domains of input samples
;  reg_parms: in, optional, type=struct
;    regularization parameters
;  in_rank: in, optional, type=fix
;    rank of transform to calculate (default = k-1)
;  rank: out, optional, type=fix
;    rank of calculated transform
;  verbose: in, optional, type=boolean
;    print verbose output to console
;  error: out, optional, type=fix
;    error code if method fails (e.g., due to singular matrix)
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
function hiihat_mdmc_lda, data, labels, domains=domains, reg_parms=reg_parms,$
                          in_rank=in_rank, rank=rank, verbose=verbose, error=error

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
     print, "Calculating LDA transform for "+strtrim(n,2)+' samples of dimensionality '+strtrim(nb,2)
     print, "Total classes: "+strtrim(k,2), " domains: "+strtrim(D,2)     
  endif

  Mw = fltarr(nb,nb) ;; within class scatter matrix
  Mb = fltarr(nb,nb) ;; between class scatter matrix

  ;; within-class scater evaluated over all domains
  for i=0, k-1 do begin
     classdat = data[where(labels eq ulab[i]),*]
     Mw += correlate(transpose(classdat),/covariance)
  end
  Mw /= k

  ;; between-class scatter evaluated over independent domains
  for j=0, D-1 do begin
     dom_idx = where(domains eq udom[j])
     dom_dat = data[dom_idx,*]
     dom_avg = total(dom_dat, 1)/float(n_elements(dom_idx))
     for i=0, k-1 do begin
        labdom_idx = where(labels eq ulab[i] and domains eq udom[j])
        n_labdom = n_elements(labdom_idx)
        if n_labdom eq 0 then continue
        labdom_dat = data[labdom_idx,*]
        labdom_avg = total(labdom_dat, 1)/float(n_labdom)        
        labdom_std = labdom_avg-dom_avg
        Mb += n_labdom * matrix_multiply(labdom_std,labdom_std,/btranspose)
     end
  end
  Mb /= n

  ;; regularize!
  if keyword_set(reg_parms) then begin
     switch reg_parms.reg_type of
        'constant': begin
           lambda = reg_parms.lambda     
           if lambda lt 0 or lambda gt 1 then begin
              print, 'Error: Lambda outside of [0,1] range'
              A = identity(nb)
              goto, cleanup
           endif
           Mw = (1.0-lambda)*Mw + lambda*identity(nb)
        end
     endswitch
  endif else begin
     if verbose then print, 'No regularization parameters provided. Skipping.'
  endelse

  ;; solve Mb / Mw eigensystem (and catch singluar matrix errors)
  catch, error

  S = matrix_multiply(Mb,invert(Mw,/double))
  eval = hqr(elmhes(S,/double), /double) 
  evec = eigenvec(S,eval,/double)  
  
  if error ne 0 then begin
      print, 'MDMC-LDA error: ', !ERROR_STATE.MSG  
      ;; default to squared euclidean
      A = identity(nb)
      goto, cleanup
   endif

  ;; Eigenvectors are not generally unique.  
  ;; Multiply each eigenvector by a complex scaling  
  ;; factor to force the initial term to be real.    
  ;; This normalization ensures a unique solution.  
  for i=0,nb-1 do evec[*,i] *= abs(evec[0,i])/evec[0,i]  

  ;; project into dimension k-1 (or smaller, if desired) space
  if not keyword_set(in_rank) then rank = k-1 else rank = in_rank

  if rank gt nb then begin
     print, "Warning: more classes than available bands, reducing rank"
     rank = nb
  endif

  if rank gt k-1 then $
     print, "Warning: rank > #classes-1, output may be unstable"

  eval_sort = reverse(sort(eval))
  if verbose then begin
     print, "Rank:", rank
     print, "Eigenvals:", double(eval[eval_sort])
  endif

  if eval_sort[0] lt 0 then $
     print, 'Warning: largest eigenvalue < 0, degenerate transform'

  A = double(evec[*,eval_sort[0:rank-1]]) ;; top eigenvectors

  ;; reform if A is one dimensional
  if (size(A))[0] eq 1 then A = reform(A,(size(A))[1],1)

cleanup:
  rank = (size(A))[2] ;; get final output rank which may have changed
  return, A ;; mahalanobis M = AA^T
end
