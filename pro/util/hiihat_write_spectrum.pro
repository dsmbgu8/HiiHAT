;+ 
; Write out a spectrum both as a text file and a SLI img file. spectrum
; should be [nb,instances]
;
; :Categories: util
; 
; :Author: Lukas Mandrake
;
; :Keywords:
;  spectrum: in, required, type="fltarr(nb, numspec)"
;    array of spectra to write
;  nb: in, required, type=fix
;    number of bands
;  sli_filename: in, required, type=string
;    name of output .sli library file 
;  label: in, optional, type=string
;    label for spectrum in output library
;  text_filename: in, optional, type=string
;    name of output .txt library file 
;  lib_label: in, optional, type=string
;    label for output library 
;  fwhm: in, optional, type="fltarr(nb)"
;    full-width-at-half-maximum values for library spectra, if available
;
; :Copyright:
;  Copyright 2009, by the California Institute of Technology. ALL RIGHTS
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
pro hiihat_write_spectrum, spectrum = spectrum, nb = nb, sli_filename = sli_filename, wl = wl, $
                           label = spectrum_label, text_filename = text_filename, r_fid = r_fid, $
                           lib_label = lib_label, fwhm=fwhm


  debug = hiihat_get_config_parm('debug')
  title='hiihat_write_spectrum'
  if debug then print, "Entering "+title 

  NL = string(10B)

  ds = size(spectrum,/dimensions)
  numspec = ds[1]

  if keyword_set(sli_filename) then begin
     dims = [-1,numspec-1,0,nb-1,0]
;        dims = [-1,0,0,nb-1,0]
     

     if not keyword_set(lib_label) then lib_label = 'Spectra'

     file_type = envi_file_type("ENVI Spectral Library")

     ;; FIXME: If user requested a return fid, then
     ;; leave the file open. Otherwise, keep
     ;; closed.
     envi_write_envi_file, spectrum, r_fid=r_fid, dims=dims, fwhm=fwhm, $
                           data_type=data_type, nb=1, ns=nb, nl=numspec, $
                           out_name = sli_filename, wl = wl, bnames = [lib_label], $ 
                           wavelength_units = wu, spec_names = spectrum_label, $ 
                           file_type = file_type
  endif


  if keyword_set(text_filename) then begin
     openw, out_lun, text_filename, /get_lun
     
     line = ''
     for i = 0,numspec-1 do begin
        line+=' Spectrum'+string(i,format='(I03)')
     endfor
     printf, out_lun, "#Wavelength(um)"+line
     for t = 0,nb-1 do begin

        ;; add however many spectra are within
        ;; the spectrum structure into a single
        ;; long line
        line = ''
        for i=0,numspec-1 do begin
           line += ' '+string(spectrum[t,i],format='(F0.5)')
        endfor

        line = string(wl[t],format='(F0.6)') + line + NL
        writeu, out_lun, line
     endfor
     
     free_lun, out_lun
  endif

  if debug then print, "Exiting "+title 

end
