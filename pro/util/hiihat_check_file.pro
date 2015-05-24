;+
; File checking function for hiihat ui. Parameter read or write should be set 
; depending on whether the selected file is an input or an output file. Title is the name
; given to the envi_pickfile dialog box and any error dialog boxes that will be generated if
; errmsg is not equal to \"\".
;
; :Categories:
;   util 
;
; :Keywords:
;  read: in, required, type=boolean
;   check read permissions
;  write: in, required, type=boolean
;   check write permissions
;
; :Returns:
;  0 if file is not accessible (or is a directory) (read/write)
;  1 if file is accessible (read) and does not already exist (write)
;  2 if file is accessible and already exists (write)
;
; :Examples: 
;  The following code will check to see if fname is readable.
;   fname=envi_pickfile()
;   readable=hiihat_check_file(fname,read=1)
;   if readable ne 0 then print, "File is readable"
; 
; :History:
;   Dec 12, 2010 (BDB): docstr added.
;
; :Author: Brian D. Bue
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
function hiihat_check_file, filename, read=read, write=write
    if not keyword_set(read) then read=0
    if not keyword_set(write) then write=0

    if filename eq "" then return, 0 

    finfo = file_info(filename)
    
    ;; we don't like directories here, reject them as valid files
    if finfo.EXISTS and finfo.DIRECTORY then return, 0

    if read then begin
       if (not finfo.EXISTS) or (finfo.EXISTS and not finfo.READ) then begin
          return, 0
       endif
    endif

    if write then begin      
       if finfo.EXISTS then begin 
          if finfo.WRITE then begin
             return, 2
             ;;ok = dialog_message("File exists, overwrite?", title=title, /cancel)
             ;;if not (strupcase(ok) eq 'CANCEL') then return, filename 
          endif
          return, 0 ;; user cancelled or we can't write to the existing file
       endif

       ;; still need to check if nonexistent file is writable
       openw, lun, filename, error=cannot_open, /append, /get_lun
       if (cannot_open ne 0) or (keyword_set(lun) eq 0) then begin 
          return, 0
       endif else begin 
          ;; sucessfully allocated a lun for the nonexistent file, free it          
          free_lun, lun 
          temp=size(temporary(lun)) ;; undefine lun
       endelse
    endif

    return, 1
end
