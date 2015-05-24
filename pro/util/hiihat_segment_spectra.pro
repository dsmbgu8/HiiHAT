;+
; Calculates mean signatures for a given image using its corresponding
; segmentation image, optionally returns the "mean image" populated with
; the calculated segments. Returns the mean spectra of each superpixel segment. 
;  Robust option will tolerate infinities and other nonsense input, returning 
;  a zeroed out spectrum. If user specifies r_fid, then
;  mean_image_name and r_fid are used to generate a new full image
;  where each superpixel has its mean spectrum replacing the entire
;  segment area. If /return_image is not set, mean_image_name and r_fid
;  are ignored. 
;
; :Categories:
;   util
;
; :Params:
;   img_fid: in, required, type="fltarr(ns,nl,nb)"
;     fid of input image data
;   seg_fid: in, required, type="fltarr(ns,nl)"
;     fid of segmentation image
;
; :Keywords:
;  spectra: out, required, type="fltarr(nseg, nbands)"
;  use_dims: in, optional, type=dims
;    override default dimensions
;  r_fid: out, optional, type=fix
;    return file id for output segment mean spectra
;  mean_image_name: in, optional, type=string
;    output mean image filename, if unspecified, will compute mean image in memory
;  mean_fid: out, optional, type=fix
;    fid of output mean image
;  variances: out, optional, type="fltarr(nseg,nbands)"
;    band variances of segment pixels
;  verbose: in, optional, type="boolean"
;    enable verbose status messages
;  return_image: in, optional, type=boolean
;    return mean image
;
; :Pre:
;   If the "robust_means" parameter is set to 1 in the configuration file, this
;   procedure will zero out means with zero or infinite band values
; 
; :Examples:
;   The following code will return the segments for img_fid/seg_fid along 
;   with the mean image for the data (in memory).
;
;   hiihat_segment_spectra, img_fid, seg_fid, spectra=spectra, 
;                         r_fid=r_fid, return_image=1, verbose=1
; 
; :Author: David Ray Thompson (DRT)
;
; :History:
;   2009 (DRT): initially implemented
;
;   Dec 13, 2010 (BDB): added docstr
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
;
;-
pro hiihat_segment_spectra, img_fid, seg_fid, spectra=spectra, $
                            use_dims = use_dims, r_fid = r_fid, $
                            variances=variances, mean_fid=mean_fid, $
                            mean_image_name = mean_image_name, $
                            return_image=return_image, verbose = verbose

  compile_opt IDL2

  debug = hiihat_get_config_parm('debug')
  title='hiihat_segment_spectra'

  if debug then print, "Entering "+title


  if not keyword_set(return_image) then return_image = 0
  ;; exclude pixels with zero or infinity reflectance entries
  
  ;; load the image, correct for use_dims being smaller than whole image
  envi_file_query, img_fid, dims=img_dims, nb = img_nb, bnames = bnames,$
                   ns = img_ns, nl = img_nl, wl = img_wl, fwhm = fwhm , $
                   wavelength_units = wavelength_units, data_type = data_type

  if keyword_set (use_dims) then begin 
     img_ns = use_dims[2]-use_dims[1]+1
     img_nl = use_dims[4]-use_dims[3]+1
     if verbose then print, "Reset segmentation img_ns, img_nl to",img_ns, img_nl
  endif else use_dims = img_dims



  img = fltarr(img_ns*img_nl, img_nb)
  for i=0,img_nb-1 do begin
     band = envi_get_data(fid=img_fid, dims = use_dims, pos = i)
     img[*,i] = band
  endfor    

  ;img = hiihat_load_bands(fid=img_fid, dims = use_dims)

;    assert, min(finite(img)) eq 1, "HIIHAT_SEGMENT_SPECTRA image file contains non-finite elements"


  if verbose then print, "Loading segmentation"

  envi_file_query, seg_fid, nb=seg_nb, ns=seg_ns, nl=seg_nl, dims=seg_dims
  segments = envi_get_data(fid=seg_fid, dims=seg_dims, pos = [0]) 
  labels = segments[uniq(segments,sort(segments))]   ;; unique segment labels
  labels = labels[sort(labels)]                      ;; FIXME: may be redundant
  n_segments = (size(labels))[1]               ;; length of unique label vector
  spectra = fltarr(n_segments, img_nb)               ; representative spectra
  if verbose then print, n_segments," segments loaded"

  if keyword_set(variances) then variances = fltarr(n_segments,img_nb)

  if return_image then meanimage=fltarr(img_ns,img_nl,img_nb)

  if verbose then print, "Averaging spectra"

  ;; Create the progress bar.
  envi_report_init, "Calculating mean spectra", base=base, /INTERRUPT, title=title 
  envi_report_inc, base, 100

  for i=0, n_segments-1 do begin
     
     if i mod 100 eq 0 then begin
        envi_report_stat, base, (i/float(n_segments)*100), 100, cancel=cancel                                             
        if cancel then goto, cleanup
     endif


     indices = where(segments eq labels[i])
     n_pix = n_elements(indices)
     segment_pixels = img[indices,*]

     ;;if verbose then print, n_pix, ' pixels in segment ', i
     hiihat_get_mean_spectrum, segment_pixels, mean_spectrum=mean_spectrum, $
                               bad_pixels=bad_pixels, zero_pixels=zero_pixels
     
     if bad_pixels gt 0 then print, bad_pixels, " bad pixels found in segment ", i
     if zero_pixels gt 0 then print, zero_pixels, ' zero pixels found in segment ', i


     ;;print, 'ms size', size(mean_spectrum)
     ;;print, "spec size", size(spectra[i,*])
     spectra[i,*] = mean_spectrum

     if keyword_set(variances) then begin
        mean_diff = fltarr(img_nb)
        for i=0, n_pix-1 do mean_diff+=(segment_pixels[i,*]-mean_spectrum)^2
        variances[i,*] = mean_diff/float(n_pix)
     endif
     if return_image then begin
        for t = 0, n_pix-1 do begin
           x = indices[t] mod seg_ns
           y = floor(indices[t] / seg_ns)
           meanimage[x,y,*] = mean_spectrum
        endfor     
     endif
  endfor

  
  envi_report_stat, base, 100, 100, cancel=cancel                                             
  
  if return_image then begin
     if not keyword_set(mean_image_name) then begin ;; in memory
        if verbose then print,"Computing mean image in memory"
        envi_write_envi_file, meanimage, $
                              /in_memory, $
                              bnames = bnames, $
                              data_type = data_type, $
                              dims = use_dims, $
                              nb = img_nb, $
                              nl = seg_nl, $
                              ns = seg_ns, $
                              fwhm = fwhm, $
                              out_name=mean_image_name, $
                              r_fid = mean_fid, $
                              wavelength_units = wavelength_units, $
                              wl = wl




     endif else begin
        if verbose then print,"Writing mean image to ",mean_image_name
        envi_write_envi_file, meanimage, $
                              bnames = bnames, $
                              data_type = data_type, $
                              dims = use_dims, $
                              nb = img_nb, $
                              nl = seg_nl, $
                              ns = seg_ns, $
                              fwhm = fwhm, $
                              out_name=mean_image_name, $
                              r_fid = mean_fid, $
                              wavelength_units = wavelength_units, $
                              wl = wl

        ;;envi_file_mng, id = mean_fid, /remove
        ;;envi_open_file, mean_image_name, r_fid = mean_fid, /no_realize
     endelse
  endif


cleanup:
  envi_report_init, base=base, /finish 
  if debug then print,"Exiting "+title
end


