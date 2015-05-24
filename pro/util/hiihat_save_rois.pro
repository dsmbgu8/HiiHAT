;+ 
; Saves the current set of ROIs to IDL_TMPDIR/temp.roi
;
; :Categories: util
;
; :Author: Brian D. Bue
;
; :Keywords: 
;  roi_file: in, optional, type=string
;    path to roi file for saving current set of rois, if not specified,
;    defaults to "IDL_TMPDIR/temp.roi"
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
pro hiihat_save_rois, roi_file=roi_file, verbose=verbose
  if not keyword_set(verbose) then verbose=0
  if not keyword_set(roi_file) then begin 
     idl_tmpdir = getenv("IDL_TMPDIR")
     roi_file = idl_tmpdir+'temp.roi'
  endif

  existing_rois = envi_get_roi_ids()
  if verbose then print, "existing rois", existing_rois
  if existing_rois[0] ne -1 then begin    
     if verbose then print, "Saving existing rois to "+roi_file
     file_delete, roi_file, /quiet     
     envi_save_rois, roi_file, existing_rois 
  endif 
end

