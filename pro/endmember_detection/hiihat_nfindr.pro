
function hypervolume, data, ids, keeparray = keeparray
;+
; Calculates the hyperdimensional volume analog by taking
; the determinant of an augmented endmember matrix. 
; keeparray is a way to pass the existing data structure around without
; having to redefine it. Speeds up the processing quite a bit.
; :Author: David Ray Thompson & Lukas Mandrake
; :Categories: endmember_detection
; :Params:
;  data: in, required, type="fltarr(nend,nb)"
;    endmember matrix
;  ids: in, required, type="intarr(nend)"
;    endmember ids
; :Keywords:
;  keeparray: in, optional, type="fltarr(nend,nb+1)"
;    augmented endmember matrix
;-

;   SMALLEST = 1.0e-40

  dims = size(data,/dimensions)
  nb = dims[1]

  dims = size(ids,/dimensions)
  nend = dims[0]

  ;; Create the endmember matrix augmented with 1's
  if not keyword_set(keeparray) then keeparray=fltarr(nend,nb+1)
  keeparray[*,0] = 1

;   numzero = 0
  for i=0,nend-1 do begin
;       if badvals[ids[i]] then numzero += 1 ; something was wrong with this spectra
     keeparray[i,1:nb] = data[ids[i],*]
  endfor
  ;;If any of the spectra were totally
  ;;zero or infinity, return a tiny
  ;;volume that will never win a
  ;;comparison. However, don't return
  ;;entirely zero so that if many
  ;;endmembers are zero, it can still get
  ;;rid of problemmatic members while
  ;;others remain zero

;   if numzero gt 0 then return, numzero * SMALLEST 

  return, abs(determ(keeparray)) ;/ float(factorial(nb))
end

;  roi_name is keyword added to generated roi endmember (not set, no
;  roi's generated)

;+ 
; Applies the NFINDR algorithm of Dr. E. Winter to discover
; endmembers by expanding a simplex to the largest possible
; volume. May not give a singular answer where no single pixel
; endmember exists (local maxima), so must be run multiple times with
; different random seeds.
;
; :Params:
;   fid_image: in, required, type=fix
;     fid of image file to process
;   n_endmembers: in, required, type=fix
;     number of endmembers to generate
;
; :Keywords:
;   use_dims: in, optional, type=dims
;     user-specified dimensions
;   out_name_sli: in, optional, type=string
;     output path for spectra and (optional) roi's
;   out_name_txt: in, optional, type=string 
;     output path for spectra and (optional) roi's (text format)                   
;   roi_outfile: in, optional, type=string
;     filename of roi file to generate (single pixel ROI's)
;   r_spectra_fid: in, optional, type=fix 
;     Return fid of the endmember spectra
;   mnf_r_fid: in, optional, type=fix
;     Return fid of MNF transformed data
;   abund_r_fid: in, optional, type=fix
;     return fid for the abundance image (currently unused)
;   r_endmember_ids: in, optional, type="intarr(n_endmembers)"
;     Return array of the endmember ids (linear indexes into the img array)
;   seed: in, optional, type=fix
;     random seed to use to continue proper random numbers
;   reject_zeros: in, optional, type=boolean
;     reject zero or uniform bands
;   in_memory: in, optional, type=boolean
;     store output in memory rather than in file
;   verbose: in, optional, type=boolean
;     verbose output to console
;
; :Author: David Ray Thompson & Lukas Mandrake
; :Categories: endmember_detection
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
pro hiihat_nfindr, fid_image, $   
                   n_endmembers, $ 
                   use_dims = use_dims, $ ; user-specified dimensions
                   out_name_sli = out_name_sli, $ ; output path for spectra and (optional) roi's
                   out_name_txt = out_name_txt, $ ; output path for spectra and (optional) roi's (text format)                   
                   roi_outfile   = roi_outfile, $ ; filename of roi file to generate (single pixel ROI's)
                   r_spectra_fid = r_spectra_fid, $ ; Return fid of the endmember spectra
                   mnf_r_fid = mnf_r_fid, $ ; Return fid of MNF transformed data
                   abund_r_fid = abund_r_fid, $ ; return fid for the abundance image (currently unused)
                   r_endmember_ids = r_endmember_ids, $ ; Return array of the endmember ids (linear indexes into the img array)
                   seed = seed, $ ;random seed to use to continue proper random numbers
                   reject_zeros = reject_zeros, $ ;reject zero or uniform bands
                   in_memory = in_memory, $ ; store output in memory rather than in file
                   verbose = verbose, $ ; verbose output
                   gui_status = gui_status ; display gui status messages instead of console errors
                   
  compile_opt IDL2

;    hiihat_clear_system

  ;; How many interations will NFINDR
  ;; restart with a new set of random
  ;; initial endmembers, out of which a
  ;; global max volume is eventually selected
  

  debug = hiihat_get_config_parm('debug')
  title='hiihat_nfindr'
  if debug then print, "Entering "+title

  MAX_ITERATIONS = hiihat_get_config_parm('nfindr_iterations')
  if MAX_ITERATIONS eq 0 then MAX_ITERATIONS=10

  if not keyword_set(reject_zeros) then reject_zeros = 0

  ;; Sanitize inputs
  hiihat_assert, fid_image gt -1, "Must provide a valid file id link to a hyperspectral image"
  hiihat_assert, n_endmembers gt 2, "Must request number of endmembers > 2"
  if keyword_set(r_spectra_fid) then begin 
     hiihat_assert, keyword_set(out_name_sli), "If you request a spectral library fid, you must specify an out_name"
  endif

  ;; Create the progress bar.
  envi_report_init, "N-FINDR endmember detection", base=base, /INTERRUPT, title=title 
  envi_report_inc, base, MAX_ITERATIONS

  ;; Obtain image information
  envi_file_query, fid_image, dims=dims, nb = nb, ns = ns, nl = nl, $
                   wl = wl, fwhm=fwhm, wavelength_units = wavelength_units, $
                   bnames = bnames, bbl = bbl, data_type=data_type

  ;; If no wl, make one:
  if wl[0] eq -1 then wl = indgen(nb)

  ;; If no band band list, then make one with all accepted
  if bbl[0] eq -1 then bbl = intarr(nb)+1

  ;; if no fwhm, undefine it
  if fwhm[0] eq -1 then temp = size(temporary(fwhm))


  ;; If the user didn't specify dimensions, use entire image
  if not keyword_set(use_dims) then use_dims = dims

  nsu = ulong(use_dims[2]-use_dims[1]+1)
  nlu = ulong(use_dims[4]-use_dims[3]+1)
  nbu = n_endmembers-1

  if debug then begin
     print, "nbu=", nbu, " nsu=", nsu, " nlu=", nlu
     print, "use_dims", use_dims
  endif
     
  ;;Reduce data dimension to requested number of endmembers minus 1
  if (verbose) then print, "Reducing data dimensionality via MNF"

  ;;and load in the original image data
  if verbose then print, "Figuring out bad x,y spectra"
  sums    = fltarr(nsu,nlu)
  dataimg = fltarr(nsu,nlu,nb)
  for i=0,nb-1 do begin
     plane = envi_get_data(fid=fid_image, dims=use_dims, pos=i)
     dataimg[*,*,i] = plane
     sums += plane
  endfor
  
  nbad_spatial=0
  badpix = intarr(nsu,nlu)  
  ;;Figure out which spectra are invalid
  bad_idx = where(sums eq 0.0 or finite(sums) lt 1)
  if bad_idx[0] ne -1 then badpix[bad_idx] = 1

  nbad_spatial = floor(total(badpix))
  if verbose then print, "Detected", nbad_spatial, "bad pixels"

  ;; At this stage, we must manually apply
  ;;a spectral subset to take into
  ;;consideration any bad bands in the
  ;;data. If you fail to do this, you
  ;;will get a "Too many iterations in
  ;;TRIQL" should any bands be degenerate
  ;;(zero or identical to other
  ;;bands). We also check manually for
  ;;any uniform bands ourselves and mark
  ;;them as bad before proceeding.
  
  ;; check for uniform bands
  for b = 0, nb-1 do begin
     mn = mean(dataimg[*,*,b])
     var = variance(dataimg[*,*,b])
     if debug then begin
        minv = min(dataimg[*,*,b])
        maxv = max(dataimg[*,*,b])
        print, "Band", b, " mean:", mn, " var:", var, " min: ", minv, " max:", maxv
     endif
     ;;if total(dataimg[*,*,b] - mn) eq 0.0 then begin
     if var eq 0 then begin 
        bbl[b]=0
        if verbose then print, "Flagged bad band at",b
     end
  end

  nb_good = floor(total(bbl))
  if nb_good eq 0 then begin
     dialog="No good bands for endmember detection"
     ;;ok = dialog_message(dialog, title=title)    
     print, dialog
     goto, cleanup
  endif

  npixels = nlu*nsu
  npix_good = npixels - nbad_spatial
  if npix_good eq 0 then begin
     dialog="No good pixels for endmember detection"
     ;;ok = dialog_message(dialog, title=title)    
     print, dialog
     goto, cleanup
  endif

  if debug then begin
     print, 'npixels', npixels
     print, 'nbad_spatial', nbad_spatial
     print, "nb ", nb
     print, "nb_good ", nb_good
  endif

  keep_idx = where(bbl eq 1)
  if nb_good lt nb then begin
     if verbose then print, "Spectrally subsetting ",nb_good, $
                            "of", nb, "bands"
     
     idl_tmpdir = getenv("IDL_TMPDIR")
     fname = idl_tmpdir+'tmp_mnf_no_bad_bands.img'

     if keyword_set(fwhm) then fwhm_out = fwhm[keep_idx]
     envi_write_envi_file, dataimg[*,*,keep_idx], $
                           out_name = fname, /in_memory, $
                           bnames = bnames[keep_idx], $
                           data_type=data_type, $
                           dims=use_dims, $
                           nb=nb_good, $
                           nl=nlu, $ 
                           ns=nsu, $
                           r_fid=temp_fid, $
                           fwhm=fwhm_out, $
                           wavelength_units=wavelength_units, $
                           wl=wl[keep_idx]
  end else begin ;; here, we don't have to do any spectral subsetting
     temp_fid = fid_image 
  endelse

  gbstr = string(nb_good,format='(%"%d")')
  if npix_good le nb_good then begin
     gpstr = string(npix_good,format='(%"%d")')
     dialog='N-FINDR cannot proceed: '
     dialog=dialog+'(# good pixels='+gpstr+')  <= (# good bands='+gbstr+')'     
     if not gui_status then print, dialog else ok=dialog_message(dialog, title=title)     
     r_spectra_fid = 0 
     abund_r_fid = 0
     goto, cleanup     
  endif

  if nb_good lt n_endmembers then begin   
     nestr = string(n_endmembers,format='(%"%d")')
     dialog='N-FINDR cannot proceed: '
     dialog=dialog+'(# good bands='+gbstr+')  < (# endmembers='+nestr+')'
     if not gui_status then print, dialog else ok=dialog_message(dialog, title=title)     
     r_spectra_fid = 0 
     abund_r_fid = 0
     goto, cleanup     
  endif

  if verbose then print, "Running MNF transform"
  envi_doit, 'mnf_doit'     , $  
             fid        = temp_fid   , $
             pos        = lindgen(nb_good), $
             dims       = use_dims   , $
             r_fid      = fid_mnf    , $
             sd_dims    = use_dims   , $
             ;;out_name   = mnffile, $  ;;FIXME: do we want this as a kwarg?
             out_nb     = n_endmembers-1, $
             sta_name   = '', $
             /in_memory, $
             /shift_diff, $ 
             /no_plot
  
  if fid_mnf eq -1 then begin
     dialog='Minimum Noise Fraction transform failed'
     ;;ok = dialog_message(dialog, title=title) 
     print, dialog
     goto, cleanup
  endif

  if (verbose) then print, "Loading MNF results into accessible spectra"
  envi_file_query, fid_mnf, dims = dims_mnf

  datamnf = fltarr(nsu, nlu, nbu)
  for i=0,nbu-1 do begin
     datamnf[*,*,i] = envi_get_data(fid=fid_mnf, dims=dims_mnf, pos=i)
  endfor
  
  ;; Map the data to a single index (not x & y) for simplicity
  ;; Also figure out if any of the spectra look bad and flag
  if verbose then print, "Reforming data into linear array ",npixels," long"
  b     = intarr(npixels    )   ;reform(badpix,npixels)
  dataonedimension = fltarr(npixels,nbu)
  for y=0L,nlu-1 do begin
     for x=0L,nsu-1 do begin
        dataonedimension[x+y*nsu,*] = datamnf[x,y,*]
        b               [x+y*nsu  ] = badpix[x,y]
     endfor
  endfor
  badpix = b  ;; replace x,y structure with single increasing index
  
  ;;Begin with our randomly initialized set of vertices, but accept no bad vals
  globalbestvol = 0.0
  for iter = 0, MAX_ITERATIONS-1 do begin

     if verbose then print, "Setting initial random vertices for iteration ",iter," of ",MAX_ITERATIONS-1
     while 1 do begin
        endmember_ids = floor(randomu(seed,n_endmembers)*npixels)
        if max(badpix[endmember_ids]) eq 1 then begin
           ;; if verbose then print, "Rejecting initial endmembers containing known bad pixels"
           continue
        endif
        break 
     endwhile
     ;; if verbose then print, "Selected initial members: ",endmember_ids

     ;; Now begin the exhaustive expansion of the set of vertices
     bestvol = hypervolume(dataonedimension,endmember_ids,keeparray=keeparray)
     bestids = endmember_ids
     bestids = bestids[sort(bestids)]
     if verbose then begin
        print, "Initial volume:",bestvol
        print, "Initial ids   :",bestids
     endif

     made_a_change = 1
     while made_a_change do begin
        made_a_change = 0
        for j=0,n_endmembers-1 do begin ; which endmember are we seeking at the moment
           
           envi_report_stat, base, iter, MAX_ITERATIONS, cancel=cancel                                             
           if cancel then begin
              goto, cleanup
           endif
           

           curids = bestids
           curvol = bestvol
           
           for i=0L,npixels-1 do begin ; which pixel are we considering at the moment
              if badpix[i] then continue ;skip any spatial pixels known to be bad
              curids[j]=i
              testvol = hypervolume(dataonedimension,curids, keeparray=keeparray)
              if testvol gt bestvol then begin
                 bestids = curids
                 bestvol = testvol
                 made_a_change = 1
              endif
           endfor               ; i test pixel
        endfor                  ; j which endmember
     endwhile

     ;; Test if this solution is better than the total global solution thus far
     ;; considering all iterations
     if bestvol gt globalbestvol then begin
        bestids = bestids[sort(bestids)]
        globalbestvol = bestvol
        globalbestids = bestids
        if verbose then print, "Better solution found with volume ",bestvol," and members ",bestids
     endif
     
  endfor ;; iterations for global maxima
  
  if verbose then print, "Forming spectral library"
  
  ;; Form spectral library and save (we must reform the spectra from the
  ;; original image)

  spectral_library = fltarr(nb, n_endmembers)
  label=strarr(n_endmembers)
  
  i=0
  n_end=0
  ;; if globalbestids has any duplicate entries, remove them
  if debug then print, "globalbestids=", globalbestids
  globalbestids = globalbestids[uniq(globalbestids,sort(globalbestids))]
  n_true_endmembers = (size(globalbestids))[1]
  while n_end lt n_true_endmembers do begin
     x =         globalbestids[i] mod ns
     y = floor ( globalbestids[i]  /  ns )

     if debug then begin
        print, n_end, x, y
        print, transpose(dataimg[x,y,*])
     endif

     ;; discard zeroed endmembers if desired
     if reject_zeros and total(dataimg[x,y,*]) eq 0 then goto, skip

     spectral_library[*,n_end] = dataimg[x,y,*]
     label[n_end]='Endmember '+string(n_end,format='(I02)')
     n_end+=1
skip:     
     i+=1
     ;; the following break will only occur if there are multiple
     ;; zero spectra and reject_zeros = 1, in which case we'll return 
     ;; zeroed spectra by default
     if i ge n_true_endmembers then break 
  endwhile
  
  if keyword_set(out_name_sli) then begin

     if not keyword_set(out_name_txt) then begin 
        out_name_txt = out_name_sli+'.txt'       
     endif else if out_name_txt eq out_name_sli then begin
        dialog='Output library filename and output text filename must not be equal.'
        if not gui_status then print, dialog else ok=dialog_message(dialog, title=title)     
        goto, cleanup
     endif     

     hiihat_write_spectrum, spectrum = spectral_library, nb = nb, wl = wl, fwhm=fwhm, $
                            sli_filename = out_name_sli, text_filename = out_name_txt, $
                            label = label, r_fid = r_fid, lib_label = 'Endmembers'
     r_spectra_fid   = r_fid
  endif
  
  r_endmember_ids = globalbestids
  abund_r_fid = 0 ;; FIXME calculate abundance image

  if verbose then print, "Creating reduced ROI set"
  envi_delete_rois, /all
  ;; Form single-pixel ROI's
  new_roi_ids = intarr(n_true_endmembers)
  for i = 0,n_true_endmembers-1 do begin
     y = floor( globalbestids[i]  /  ns)
     x =        globalbestids[i] mod ns
     new_roi_ids[i] = envi_create_roi(color=i+2, name=label[i], ns=ns, nl=nl)
     envi_define_roi, new_roi_ids[i], /point, xpts=[x], ypts=[y]
  endfor

  if keyword_set(roi_outfile) then begin
     if verbose then print, "Writing reduced ROI set to ",roi_outfile
     envi_save_rois, roi_outfile, new_roi_ids
  endif

cleanup:
  ;; Get rid of the nobadvalues image if it was needed
  if keyword_set(temp_fid) then begin
     if temp_fid ne fid_image then envi_file_mng, id = temp_fid, /remove        
  endif
  ;; close the status bar
  envi_report_init, base=base, /finish 
  if keyword_set(fid_mnf) then envi_file_mng, id = fid_mnf, /remove 
  if debug then print, "Exiting "+title
end

