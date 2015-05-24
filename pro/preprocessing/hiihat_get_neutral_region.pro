;+ 
; Detects a neutral spectrum within the given image and creates a ratio
; image by dividing out the detected spectrum.
;
; :Categories:
;  preprocessing
;
; :Author: David Ray Thompson
;
; :Keywords:
;  img_fid: in, required, type=fix
;    file id for input image data
;  seg_fid: in, required, type=fix
;    file id for input image segmentation map
;  roi_out_id: out, optional, type=fix
;    file id of output roi file
;  spectrum_out_filename: out, optional, type=string
;    filename to save output spectrum
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
pro hiihat_get_neutral_region, img_fid=img_fid, seg_fid=seg_fid, $
                             roi_out_id = roi_out_id, $
                             spectrum_out_filename=spectrum_out_filename, $
                             ratioed_out_filename=ratioed_out_filename, $
                             r_fid=spectrum_fid, $
                             coalesce_threshold=coalesce_threshold, $
                             use_dims = use_dims, $
                             neutral_spectrum = neutral_spectrum, $
                             spectrum_best = spectrum, $
                             best_segment_id = best_segment_id,$
                             verbose = verbose


    ;Set compiler and debug options
    compile_opt IDL2
    debug = hiihat_get_config_parm('debug')
    if not keyword_set(verbose) then verbose = 0
    if not keyword_set(ratioed_out_filename) then ratioed_out_filename = ""

    title='hiihat_get_neutral_region'
    if debug then print, "Entering "+title

    ; Create the progress bar.
    envi_report_init, "Neutral region detection.", base=base, /INTERRUPT, title=title 
    envi_report_inc, base, 100
    

    hiihat_segment_spectra, img_fid, seg_fid, spectra=spectra, $
                            use_dims = use_dims, verbose=verbose

    ; find best fit polynomial
    n_segments = (size(spectra))[1]
    n_bands = (size(spectra))[2]
    
    ; build data matrix
    best_residual = 9e99
    best_spectrum = 0
    best_segment_num = -1
    min_noise_level = 1e-9

    ; get segment extent information for excluding large regions
    max_segment_size = 10000 ; a region bigger than we'd ever want

    seg_ns = use_dims[2]-use_dims[1]+1
    seg_nl = use_dims[4]-use_dims[3]+1
    seg_nb = 1
    seg_dims = use_dims
    ;envi_file_query, seg_fid, nb=seg_nb, ns=seg_ns, nl=seg_nl, dims=seg_dims
    segments = envi_get_data(fid=seg_fid, dims=seg_dims, pos = [0]) 

    ;; Search for pathological regions of
    ;; the spectra that should be ignored
    ;; in the fit calculation
    badbands = intarr(n_bands)
    for i=0,n_segments-1 do begin
        zero_ids = where(spectra[i,*] eq 0)
        if zero_ids[0] gt -1 then badbands[zero_ids]=1
        infinite_ids = where(finite(spectra[i,*]) eq 0)
        if infinite_ids[0] gt -1 then badbands[infinite_ids]=1
    endfor ; walk segments searching for pathological regions
    goodbands = 1 - badbands
    n_goodbands = total(goodbands)
    goodbands_idx = where(goodbands eq 1)
    print, "Bad bands found:",where(badbands eq 1)
 
    for i=0,n_segments-1 do begin
        if i mod 20 eq 0 then begin
              envi_report_stat, base, i/(n_segments-1), 100, cancel=cancel                                             
              if cancel then begin
                 goto, cleanup
              endif
        endif


        ; fit a low-degree polynomial to the spectrum (least squares)
        ; We do not penalize for pathological regions previously determined
        x = indgen(n_goodbands)
        A = [[x], [fltarr(n_goodbands)+1.0]]
        y = transpose(spectra[i,goodbands_idx]+1)
        y = y / total(y) ; L1-normalize
        pinv = invert(transpose(A) # A) # transpose(A) 
        b = pinv # y

        ; exclude really big regions
        indices = array_indices(segments, where(segments eq i))
        n_pix = n_elements(indices)
        if (n_pix gt max_segment_size) then continue

        ; get the residual
        residual = total(((A # b) - y)^2)
        if (residual lt min_noise_level) then continue

        if (residual lt best_residual) then begin
            best_residual = residual 
            best_spectrum = i
            best_segment_num = i
        endif
    endfor

    if keyword_set(best_segment_id) then best_segment_id = best_segment_num
    
    ;load in the appropriate wavelengths to show on the x axis
    envi_file_query, img_fid, wl = wavelengths

                                ;return the actual best neutral
                                ;spectrum, not the compressed
                                ;good-band-only version
    if keyword_set(neutral_spectrum) then begin
        neutral_spectrum = spectra[best_spectrum,*]
    endif

    envi_report_stat, base, 100, 100

    ; write spectrum
    if (spectrum_out_filename ne "") then begin
        dims = [-1,0,0,n_bands-1,0]
        envi_write_envi_file, spectra[best_spectrum,*], r_fid=temp_fid, dims=dims, $
          data_type=data_type, nb=1, ns=n_bands, nl=1, $
          out_name = spectrum_out_filename, wl=wavelengths, $ 
          wavelength_units=wu, spec_names = ['neutral spectrum'],$ 
          file_type = envi_file_type("ENVI Spectral Library"), /no_open
                                ;Do the dance to prevent the available
                                ;bands menu from popping up
        envi_file_mng, id = temp_fid, /remove
        envi_open_file, spectrum_out_filename, r_fid = temp_fid, /no_realize
    endif

    if (keyword_set(spectrum)) then spectrum = spectra[best_spectrum,*]

    ns_endmembers = n_bands
    nl_endmembers = 1

    ; get image dimensions 
    envi_file_query, img_fid, ns=ns, nl=nl, nb=nb, dims=dims

    ; load superpixels
    envi_file_query, seg_fid, nb=seg_nb, ns=seg_ns, nl=seg_nl, dims=seg_dims
    segments = envi_get_data(fid=seg_fid, dims=seg_dims, pos = [0]) 
    n_segments = max(segments)+1 ; assume consecutate labels, start w/zero 
        
    ; define the ROI, write to file
    new_roi_id = envi_create_roi(color=7, name='Neutral Spectrum', $
                                 ns=ns, nl=nl)
    r_roi_id = new_roi_id
    pts = where(segments eq best_spectrum)
    subsection_ns = use_dims[2] - use_dims[1] + 1 
    ypts = floor(pts /   subsection_ns) + use_dims[3]
    xpts =       pts mod subsection_ns  + use_dims[1]
    envi_define_roi, new_roi_id, /point, xpts=xpts, ypts=ypts
    roi_out_id = new_roi_id

    ; write ratioed image
    if (ratioed_out_filename ne "") then begin
        ratioed = fltarr(ns, nl, nb)
        for j=0,nb-1 do begin
           envi_report_stat, base, j, nb-1, cancel=cancel   
           if cancel then goto, cleanup     
           ratioed[*,*,j] = envi_get_data(fid=img_fid,dims=dims, pos=j)/$
               spectra[best_spectrum,j]
        endfor
        envi_write_envi_file, ratioed, r_fid=temp_fid, dims=dims, $
          data_type=data_type, nb=nb, ns=ns, nl=nl, $
          out_name = ratioed_out_filename, wl=wavelengths, $ 
          wavelength_units=wu, /no_open
        envi_open_file, ratioed_out_filename, r_fid = temp_fid, /no_realize
    endif

cleanup:
    envi_report_init, base=base, /finish ;; close statusbar
    if debug then print, "Exiting "+title
end
  

