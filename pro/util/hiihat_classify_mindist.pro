;+ 
; Minimum distance to class means classifier.  
;
; :Author: Brian Bue
;
; :Categories: 
;  util
;
; :Params:
;  train_data: in, required, type="fltarr(ntr,nb)"
;    labeled training samples
;  train_labels: in, required, type="intarr(ntr)"
;    labels for each of the training samples
;  test_data: in, required, type="fltarr(nte,nb)"
;    unlabeled test samples to classify
;
; :Keywords:
;  dist_metric: in, optional, type=string
;    name of distance metric to use in hiihat_distance_metric function
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
function hiihat_classify_mindist, train_data, train_labels, test_data, $
                                  dist_metric=dist_metric, verbose=verbose

  if not keyword_set(dist_metric) then dist_metric='euclidean_sq'
  if not keyword_set(verbose) then verbose=0

  ulab = train_labels[uniq(train_labels,sort(train_labels))]
  k = n_elements(ulab)
 
  train_size = size(train_data)
  ntr = train_size[1]    ;; number of training samples
  nb = train_size[2]   ;; number of bands
  nte = (size(test_data))[1]    ;; number of test samples

  ;; calculate training means
  for ui=0,k-1 do begin
     class_idx = where(train_labels eq ulab[ui])
     class_pixels = train_data[class_idx,*]     
     mean_spectrum = total(class_pixels, 1)/float(n_elements(class_idx))
     if n_elements(train_means) eq 0 then train_means = mean_spectrum else $
        train_means = [[train_means], [mean_spectrum]]
  endfor
  train_means = transpose(train_means)

  ;; reform if one dimensional
  if (size(train_means))[0] eq 1 then $
     train_means = reform(train_means,(size(train_means))[1],1)

  ;; classify test pixels according to their distance from the
  ;; training means  
  dmean = hiihat_distance_matrix(dist_metric,test_data,train_means)
  mins = min(dmean,dimension=2,min_sub)
  usub = (array_indices([nte,k], min_sub, /dimensions))[1,*]
  pred = ulab[usub]

  return, pred
end
