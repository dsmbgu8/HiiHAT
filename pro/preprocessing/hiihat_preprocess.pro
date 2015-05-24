;+ 
; Perform preprocesing steps for a set of known image formats
;
; :Categories:
;  preprocessing
;
; :Params:
;  in_file_id: in, required, type=fix
;    fid of input file to preprocess
;
; :Keywords:
;  out_filename: in, required, type=string
;    output file name, required if in_memory=0
;  image_type: in, optional, type=string
;    name of known image type, or "Generic" for an unspecified image type 
;  median_filter_width: in, optional, type=fix
;    width of median filter, 0 for no filtering
;  norm_type: in, optional, type=string
;    type of normalization to perform, "None" for no normalization
;  div_type: in, optional, type=string
;    divide out "Mean", "Spectrum", "MeanColumn", or "None" for no division
;  lowpass_filter: in, optional, type=fix
;    width of lowpass filter kernel, 0 for no filtering
;  r_fid: in, optional, type=fix
;    return file id of preprocessed image
;  use_dims: in, optional, type="longarr"
;    user defined dimensions
;  in_memory: in, required, type=boolean
;    enable in-memory computation of image, required if out_fname unspecified
;  verbose: in, optional, type=boolean
;    enable verbose console output
;
; :Author: David Ray Thompson
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
pro hiihat_preprocess, in_file_id, $
                       out_filename=out_filename, $
                       image_type=image_type,$
                       filter_negative=filter_negative, $
                       median_filter_width=median_filter_width, $
                       norm_type = norm_type, $
                       div_type = div_type, $
                       lowpass_filter = lowpass_filter, $
                       r_fid=r_fid, $
                       in_memory=in_memory, $
                       use_dims=use_dims, $
                       verbose=verbose

                                
  compile_opt strictarr
  title='hiihat_preprocess'  

  debug =  hiihat_get_config_parm('debug')
  if debug then print, "Entering "+title

  finite_max = hiihat_get_config_parm('finite_max')
  gui_status = hiihat_get_config_parm('gui_status')
  if finite_max eq 0 then finite_max = 10000.0

  if not keyword_set(in_memory) then in_memory = 0
  if not keyword_set(convert_if) then convert_if = 0
  if not keyword_set(verbose) then verbose = 0

  if not in_memory and not keyword_set(out_filename)  then begin
     ok = dialog_message('Must define either /in_memory or out_name', $
                         title=title)
     goto, cleanup
  end

  envi_file_query, in_file_id, dims=in_file_dims, nb = n_bands, $
                   ns = n_samples, nl = n_lines, wl = in_file_wavelengths, $
                   bnames = in_file_bnames, fwhm = in_file_fwhm, $
                   BBL = BBL, data_ignore_value = data_ignore_value, descrip = descrip, $
                   file_type = file_type, sensor_type = sensor_type, $ 
                   wavelength_units = wavelength_units, fname=in_fname, $
                   interleave = interleave, data_type=data_type 

  if keyword_set(use_dims) then in_file_dims = use_dims

  ;; overwrite the samples and lines based on user requested region
  n_samples = in_file_dims[2]-in_file_dims[1]+1
  n_lines = in_file_dims[4]-in_file_dims[3]+1

  ;; no lowpass filtering by default
  if not keyword_set(lowpass_filter) then lowpass_filter = 0
  
  ;; Default wavetable and fwhm
  ;; if the fwhm vector is provided, we need to trim it with the same
  ;; bands as the wavetable

  wvt = in_file_wavelengths
  if n_elements(in_file_fwhm) ne 0 then fwhm = in_file_fwhm

  ;; identify subset of bands to use for this data product
  case image_type of 
     'CRISM FRT (CAT-processed)': begin
        ;; flag CAT dependencies
        funcpath = routine_filepath('mro_crism_lookupwv',/is_function)
        if funcpath eq "" then begin
           dialog='CAT toolbox required for this image type'
           ok=dialog_message(dialog,title=title)                              
           goto, cleanup
        endif

        ;; use bands for 1.060 through 2.60 microns
        ;; convert microns to nm
        wvt = in_file_wavelengths*1000.0
        if n_elements(in_file_fwhm) ne 0 then begin 
           fwhm = in_file_fwhm*1000.0
        endif
        R1067 = mro_crism_lookupwv(1067,wvt)
        R2600 = mro_crism_lookupwv(2600,wvt)
        selected_bands = indgen(R2600-R1067+1)+R1067
     end
     'CRISM MRDR (CAT-processed)': begin
        funcpath = routine_filepath('mro_crism_lookupwv',/is_function)
        if funcpath eq "" then begin
           dialog='CAT toolbox required for this image type'
           ok=dialog_message(dialog,title=title)                              
           goto, cleanup
        endif

        ;; use bands for 1.060 through 2.60 microns
        wvt = in_file_wavelengths*1000.0 ;; convert to nm
        if n_elements(in_file_fwhm) ne 0 then begin 
           fwhm = in_file_fwhm*1000 ;; convert to nm
        endif
        R1067 = mro_crism_lookupwv(1067,wvt)
        R2600 = mro_crism_lookupwv(2600,wvt)
        selected_bands = indgen(R2600-R1067+1)+R1067
     end
     'CRISM MSW (CAT-processed)': begin
        selected_bands = indgen(42)
     end
     'CRISM MSP (CAT-processed)': begin
        selected_bands = indgen(42)
     end
     'M3 L1B RDN (raw)': begin
        R750 = hiihat_nm_wavetable_lookup(750,wvt)
        R2000 = hiihat_nm_wavetable_lookup(2000,wvt)
        selected_bands = indgen(R2000-R750+1)+R750
        lowpass_filter = 3

        ;; look for a solar spectrum relative to our current location
        
        hiihat_path = hiihat_get_config_parm('hiihat_path')
        sep = path_sep()
        specname = hiihat_path+'data'+sep+'future'+sep+'m3_solar_global85.sli'
        if file_test(specname, /read) then begin
            envi_open_file, specname, /no_interactive_query, /no_realize, $
                r_fid = denom_fid
            envi_file_query, denom_fid, dims=denom_dims, $
                nb = denom_nb, ns = denom_ns, nl = denom_nl
            denom_spectrum = envi_get_data(fid=denom_fid, $
                dims=denom_dims, pos=0)
            denom_spectrum = denom_spectrum[selected_bands]/!dpi

            if denom_ns eq n_bands and denom_nl eq 1 then begin
                convert_if = 1 
            endif else begin
                print, 'Skipping I/F due to band mismatch: '+specname 
            endelse
        endif else begin
            print, 'Cannot read solar spectrum at '+specname
        endelse
     end
     'USGS AVIRIS reflectance': begin
        R1067 = hiihat_nm_wavetable_lookup(1067,wvt)
        R2600 = hiihat_nm_wavetable_lookup(2600,wvt)
        selected_bands = indgen(R2600-R1067+1)+R1067
     end
     'EO-1 ALI 10-band': begin
        selected_bands = indgen(10)
     end
     'EO-1 Hyperion 12-band': begin
        selected_bands = indgen(12)
     end
     'Generic': begin
        selected_bands = indgen(n_bands)
     end
     else: begin
        dialog='Unrecognized image type'
        if not gui_status then print, dialog else ok=dialog_message(dialog,title=title)                                      
        goto, cleanup
     end
  endcase

  ;; trim output variables accordingly
  out_wvl = wvt[selected_bands]
  if n_elements(in_file_fwhm) ne 0 then out_fwhm = fwhm[selected_bands]
  out_bnames = in_file_bnames[selected_bands]
  
  ;; Create the progress bar.
  envi_report_init, "Loading data", base=base, /INTERRUPT, title=title 
  envi_report_inc, base, n_bands-1
  
  img_filtered = fltarr(n_samples, n_lines, n_bands)
  for j=0,n_bands-1 do begin
     envi_report_stat, base, j, n_bands-1, cancel=cancel   
     if cancel then goto, cleanup     
     img_filtered[*,*,j] = envi_get_data(fid=in_file_id,dims=in_file_dims, pos=j)
  endfor
  envi_report_init, base=base, /finish 
  
  if verbose and filter_negative then begin
     print, "Filtering negative values"
     bad_idx = where(img_filtered lt 0)
     if bad_idx[0] ne -1 then begin 
        if verbose then print, "Setting ", n_elements(bad_idx), " negative elements to zero"        
        img_filtered[bad_idx] = 0.0
     endif
  endif

  if verbose and median_filter_width gt 0 then $
     print, "Median filter, width:", median_filter_width
  hiihat_median_filter_image, img_filtered, median_filter_width

  if verbose then print, "Subsetting"
  n_selected_bands = n_elements(selected_bands)
  img = img_filtered[*,*,selected_bands]
  bad_idx = where((1-finite(img)) or ((img gt finite_max)))
  if bad_idx[0] ne -1 then begin
     if verbose then print, "Setting ", n_elements(bad_idx), " very large elements to zero"        
     img[bad_idx] = 0.0
  endif 
  if verbose and div_type ne "None" then $
     print, "Dividing out ", div_type
  hiihat_divide_mean, img, div_type

  if convert_if ne 0 then begin
     if verbose then print, "Transforming to I/F "
     hiihat_divide_mean, img, 'Spectrum', denom_spectrum=denom_spectrum
  endif
  
  if verbose and lowpass_filter then $
     print, "Spatial low pass filtering, filter size: ", lowpass_filter
  hiihat_lowpass_image_filter, img, lowpass_filter  

  if verbose and norm_type ne "None" then $
     print, "Normalizing using "+norm_type
  hiihat_normalize_image, img, norm_type                       

  if not keyword_set(in_fname) then in_fname = 'Unspecified file location.'

  pp_str = ' hiihat_pp: '+image_type+'.'
  if keyword_set(descrip) then descrip=descrip+pp_str else $
     descrip=in_fname+pp_str

  if in_memory then begin
     if verbose then print , "Output to memory"
     envi_write_envi_file, img, data_type = data_type, $
                           ns= n_samples, nl = n_lines, nb = n_selected_bands, $
                           wl = out_wvl, fwhm = out_fwhm, interleave=0, $
                           r_fid = r_fid, bnames = out_bnames, BBL = BBL, $
                           /in_memory, info=info_str, descrip = descrip, $
                           file_type = file_type, sensor_type=sensor_type, $ ;ENVI_SETUP_HEAD ENVI_ENTER_DATA
                           wavelength_units = wavelength_units
  endif else begin
     if verbose then  print , "Output to "+out_filename
     envi_write_envi_file, img, data_type = data_type,  interleave=0, $
                           ns= n_samples, nl = n_lines, nb = n_selected_bands, $
                           wl = out_wvl, fwhm = out_fwhm, BBL = BBL, $
                           r_fid = r_fid, bnames = out_bnames, $
                           out_name=out_filename, descrip = descrip, $
                           file_type = file_type, sensor_type = sensor_type, $ ;ENVI_SETUP_HEAD ENVI_ENTER_DATA
                           wavelength_units = wavelength_units


     ;; can't write directly data_ignore_value and h_info
     envi_setup_head, fname=out_filename, data_type = data_type, $
                      ns= n_samples, nl = n_lines, nb = n_selected_bands, $
                      wl = out_wvl, fwhm = out_fwhm, BBL = BBL, $
                      bnames = out_bnames, interleave=0, $ ;; interleave 0 = BSQ
                      descrip = descrip,  file_type = file_type, $
                      sensor_type=sensor_type, $ 
                      wavelength_units = wavelength_units, $ 
                      data_ignore_value = data_ignore_value
                      

     ;; Need to write geo_points


  endelse

cleanup:
  envi_report_init, base=base, /finish 
  if debug then print, "Exiting "+title
end

