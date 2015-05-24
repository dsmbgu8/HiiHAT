;+
;  Calculate the overall and per-segment "purity" with respect to a given class
;  map. 
;
; :Categories:
;   util 
; :Params:
;  seg_map: in, required, type="intarr(ns,nl)"
;    segmentation map
;  class_map: in, required, type="intarr(ns,nl)"
;    labeled class map
;
; :Keywords:
;  ignore_classes: in, optional, type="intarr(n_classes)"
;    list of class values to ignore (e.g. 0 for background classes)
;
;  purity: in, optional, type=float
;    overall purity of seg_map with respect to class_map    
;  mixtures: in, optional, type="fltarr(n_segments)"
;    purity of individual segments with respect to class_map
;  verbose: in, optional, type=boolean
;    print verbose output to console
;
; :History:
;   Dec 31, 2011 (BDB):  Initial implementation
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
;
;-
pro hiihat_seg_purity, seg_map, class_map, ignore_classes=ignore_classes, purity=purity, $
                       mixtures=mixtures, img=img, distances=distances, verbose=verbose, A=A

  uniq_seg = seg_map[uniq(seg_map,sort(seg_map))]
  uniq_class = class_map[uniq(class_map,sort(class_map))]
  mixed_seg_count = 0 ;; number of mixed segments
  n_ignored = 0 ;; number of completely ignored segments

  n_ignore = n_elements(ignore_classes)
  n_segments = n_elements(uniq_seg)
  n_classes = n_elements(uniq_class)
  mixtures = fltarr(n_segments)+1.0 ; assume each segment pure 
  distances = fltarr(n_segments)-1.0 ; distances of pure segs to class means

  if keyword_set(img) then begin
     if keyword_set(A) then dist_metric = 'mahalanobis' else dist_metric='euclidean'
     nsnl = (size(img))[1]
     nb = (size(img))[2]
     class_means = fltarr(n_classes, nb)
     for i=0,n_classes-1 do begin
        if (where(uniq_class[i] eq ignore_classes))[0] ne -1 then continue

        class_idx = where(class_map eq uniq_class[i])
        class_pix = img[class_idx,*]
        n_pix = n_elements(class_idx)
        if verbose then print, "Calculating class mean for ", n_pix, "pixels"
        class_means[i,*] = total(class_pix,1)/float(n_pix)
     endfor
  endif

  ;; examine each segment to make sure it consists of only a single class
  for i=0, n_segments-1 do begin
     seg_lab = uniq_seg[i]
     seg_idx = where(seg_map eq seg_lab)
     seg_class_labs = class_map[seg_idx] ; class labels within current segment

     ;; filter out ignored class pixels
     for j=0, n_ignore-1 do begin
        ig_class = ignore_classes[j]
        keep_idx = where(seg_class_labs ne ig_class)
        if keep_idx[0] eq -1 then begin 
           ;;all elements ignored, undefine seg_class_labs
           temp=size(temporary(seg_class_labs)) 
        endif else begin
           seg_class_labs = seg_class_labs[keep_idx]
           seg_idx = seg_idx[keep_idx]
        endelse
     end

     ;; check if we ignored all pixels in this segment
     if n_elements(seg_class_labs) eq 0 then begin
        n_ignored += 1
        mixtures[i] = -1
        seg_uniq_classes = [0]
     endif else begin        
        ;; pure segments should consist of a single class
        seg_uniq_classes = seg_class_labs[uniq(seg_class_labs, $
                                               sort(seg_class_labs))]
        n_seg_classes = n_elements(seg_uniq_classes)
        if n_seg_classes ne 1 then begin
           mixtures[i] = float(n_seg_classes)/n_elements(seg_class_labs)
           mixed_seg_count += 1
        end
     endelse

     if mixtures[i] eq 1.0 and keyword_set(img) then begin
        ci = where(uniq_class eq seg_uniq_classes[0])
        seg_mean = total(img[seg_idx,*],1)/float(n_elements(seg_idx))
        distances[i] = hiihat_spec_difference(transpose(seg_mean),$
                                              transpose(class_means[ci,*]),$
                                              dist_metric,M=A)                                                     
     endif
                                                     

     if verbose then begin
        outstr = 'Segment '+string(seg_lab,format='(%"%d")')+' purity: '+string(mixtures[i],format='(%"%0.3f")')+' distance: '+string(distances[i],format='(%"%0.3f")')
        print, outstr, " classes: ", seg_uniq_classes
        print, "unique classes: ", n_elements(uniq_class)
     endif
  end
  
  purity = 1.0-(float(mixed_seg_count)/(n_segments-n_ignored))
  if verbose then print, "Segmentation purity:", purity
end 
