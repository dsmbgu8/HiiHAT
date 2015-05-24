;+ 
; Generates n_classes rgb colors for spectral libraries and ROIs
;
; :Categories:
;  util
;
; :Author: David Ray Thompson and Brian D. Bue
;
; :Params:
;  n_classes: in, required, type=fix
;    number of classes for which colors are to be generated
; :Keywords:
;  class_names: in, optional, type=string
;    names of each class
;  colors: out, required, type="fltarr(n_classes,3)"
;    generated color array
;  class_prefix: in, optional, type=string
;    string prefix to append to class names
;
; :Copyright:
;  Copyright 2010, by the California Institute of Technology. ALL RIGHTS
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
pro hiihat_generate_class_colors, n_classes, class_names=class_names, $
                                  colors=colors, class_prefix=class_prefix

  if not keyword_set(class_prefix) then class_prefix="Class"

  ;; make up names & colors for classification
  class_names = ["Unclassified"]
  colors = [[0,0,0]]
  for i=0,n_classes-1 do begin
     class_names = [[class_names], [class_prefix+" "+strtrim(string(i),2)]]
     colindex = hiihat_rainbow_index(float(i)/float(n_classes-1))
     b =  colindex             / 65536L
     g = (colindex - b*65536L) / 256L
     r =  colindex - b*65536L - g*256L
     colors = [[colors],[r,g,b]] ; $
     ;; colindex mod 256, 255-(i mod 5) * 50.0,$  
     ;; 255-(i mod 2) * 128.0,$
     ;; 255-(i mod 3) * 85.0]]
  endfor
end
