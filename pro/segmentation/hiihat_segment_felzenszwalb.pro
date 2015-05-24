

function find_root, elements, a
;+
; find the root index associated with this node
; 
; :Categories: segmentation
;
; :Params:
;  elements: in, required, type="fix(N,3)"
;   an Nx3 array with each column giving [parent rank size]
;   note that the parent field is another index  that doesn't necessarily
;   indicate the direct parent, just some node higher in the tree.  By 
;   following these indices we'll eventually get to a's root
;  a: in, required, type=fix
;   the node index
;-
    ind = ulong(a)
    while (ind ne elements[ind,0]) do begin
        ind = elements[ind,0]
    endwhile
    elements[a,0] = ind ; optimize by updating direct parent of query node 
    return, ulong(ind)
end

pro join_tree, elements, a, b
;+
; join the (disjoint) trees with root nodes a and b
;
; :Params:
;  elements: in, required, type="lonarr(N,3)"
;   an Nx3 array with each column giving [parent rank size]
;  a: in, required, type=long
;   root node a
;  b: in, required, type=long
;   root node b
;  
; :Categories: segmentation
;-
    if (elements[a,1] gt elements[b,1]) then begin 
        elements[b,0] = a
        elements[a,2] = elements[a,2] + elements[b,2]
    endif else begin
        elements[a,0] = b
        elements[b,2] = elements[a,2] + elements[b,2]
        if (elements[a,1] eq elements[b,1]) then $
            elements[b,1] = elements[b,1] + ulong(1)
    endelse
end

;+
;  Segment a hyperspectral image using the 
;  "efficient graph-based segmentation" approach of Pedro Felzenszwalib et al.
;  We omit their smoothing step (presume that the data is already clean).
;
; :Categories:
;   segmentation 
;
; :Params:
;  in_file_fid: in, required, type="fltarr(ns,nl,nb)"
;    fid of input image to be segmented
;  c: in, required, type=float
;    threshold parameter, larger values prefer larger segment sizes
;  min_size: in, required, type=fix
;    minimum segment size in pixels
;  dist_metric: in, required, type=string
;    distance metric to use in segmentation (default euc^2)
;
; :Keywords:
;  verbose: in, optional, type=boolean
;    enable verbose console output
;  use_dims: in, optional, type="longarr"
;    user defined dimensions
;  gui_status: in, optional, type=boolean
;    enable gui_status
;  out_filename: in, required, type=string
;    output file name 
;  in_memory: in, optional, type=boolean
;    output to memory rather than file
;  r_fid: out, required, type=fid
;    fid of output segmentation (either file or memory)
;  permute_segments: in, optional, type=boolean
;    randomly permute segment ids 
;  M: in, optional, type="fltarr(nb,nb)"
;    square, symmmetric matrix to use in mahalanobis distance (ignored if
;    dist_metric is not "mahalanobis")
;
; :Examples:
;   hiihat_segment_felzenszwalb, in_file_fid, c, min_size, dist_metric,
;                               out_filename=out_filename, in_memory=in_memory, 
;                               r_fid=seg_fid, verbose=verbose, use_dims=use_dims,
;                               save_mean=save_mean, mean_image_name=mean_image_name, 
;                               gui_status=gui_status
; 
; :History:
;   2009 (DRT): Initial implementation
;
;   Dec 31, 2010 (BDB): docstr added
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
;
;-
pro hiihat_segment_felzenszwalb, in_file_fid, c, min_size, dist_metric, M=M, $
                                 out_filename=out_filename, in_memory=in_memory, $
                                 r_fid=seg_fid, verbose=verbose, $
                                 use_dims=use_dims, gui_status=gui_status, $                                 
                                 permute_segments=permute_segments

    debug = hiihat_get_config_parm('debug')
    title='hiihat_segment_felzenszwalb'
    if debug then print, "Entering "+title 

    if not keyword_set(verbose) then verbose = 0
    if not keyword_set(gui_status) then gui_status = 0
    if not keyword_set(in_memory) then in_memory = 0
    if not keyword_set(permute_segments) then permute_segments = 0
   
    mod_step = 100

    finite_max = hiihat_get_config_parm('finite_max')
    if finite_max eq 0 then finite_max = 10000.0 ;;!VALUES.F_INFINITY 

    hiihat_assert, in_memory or keyword_set(out_filename), $
            'Neither in_memory or out_filename defined', title=title

    hiihat_assert, c gt 0.0, 'C cannot be zero or negative', title=title
    hiihat_assert, min_size ge 1, 'min_size must be >= 1', title=title
  

    envi_file_query, in_file_fid, dims=dims, nb = nb, ns = nc, nl = nr
    
    if not keyword_set(use_dims) then use_dims = dims   
    
    if verbose then begin 
       print, "Checking redundant array size on disk: ", dims
       print, "Selected image has ",nb," bands ",nc," samples ",nr," lines" 
       print, "User wants ", use_dims
    endif

    ;; overwrite the samples and lines based on user requested region
    nc = use_dims[2]-use_dims[1]+1
    nr = use_dims[4]-use_dims[3]+1

    if verbose then print, "User subselected ",nc," samples ",nr," lines"
    
    ;; use_dims supplants the "dims" var from above, using only a subset if
    ;; the user desires.

    envi_report_init, "Loading image data", base=base, /INTERRUPT, title=title 
    envi_report_inc, base, nb-1
    data = fltarr(nc, nr, nb)
    for i=0,nb-1 do begin
       if i mod mod_step eq 0 then begin
          envi_report_stat, base, i, nb-1, cancel=cancel   
          if cancel then goto, cleanup
       endif
       data[*,*,i] = envi_get_data(fid=in_file_fid, dims=use_dims, pos=i)
    endfor
    envi_report_init, base=base, /finish 
        
    nels = ulong(nc)*ulong(nr)
    nedges = ulong(0) 
    edges    = lonarr(nels*4,2) ; [a, b] linear indices
    weights  = fltarr(nels*4)+finite_max  ; spectrum difference represented by the edge

    ; create a forest of disjoint root nodes, represented by the 'elements'
    ; array with columns corresponding to [parent, rank, size].
    ; For root nodes, the parent indicates the node's own index into the 
    ; elements array.  
    elements = [[lindgen(nels)],[lonarr(nels)],[1+lonarr(nels)]]
    thresholds = fltarr(nels) + (float(c)/1.0)
 
    ; Create the progress bar.
    envi_report_init, "Building edge graph", base=base, /INTERRUPT, title=title 
    envi_report_inc, base, nc-1

    ; build an 8-connected graph of edges
    if verbose then print, "Building edge graph"
    for i=ulong(0),nc-1 do begin
       if i mod mod_step eq 0 then begin
          envi_report_stat, base, i, nc-1, cancel=cancel                                             
          if cancel then goto, cleanup
       endif
              
       for j=ulong(0),nr-1 do begin            
            a = i+j*nc
        
            if (i lt nc-1) then begin
                b = ((i+1)+(j*nc))
                edges[nedges,0] = a
                edges[nedges,1] = b 
                weights[nedges] = hiihat_spec_difference(data[i,j,*], $
                        data[i+1,j,*], dist_metric, /reject_zeros, M=M)
                nedges += 1
            endif

            if (j lt nr-1) then begin
                b = (i+(j+1)*nc)
                edges[nedges,0] = a
                edges[nedges,1] = b 
                weights[nedges] = hiihat_spec_difference(data[i,j,*], $
                        data[i,j+1,*], dist_metric, /reject_zeros, M=M)
                nedges += 1
            endif
            
            if ((i lt nc-1) and (j lt nr-1)) then begin
                b = ((i+1)+(j+1)*nc)
                edges[nedges,0] = a
                edges[nedges,1] = b 
                weights[nedges] = hiihat_spec_difference(data[i,j,*], $
                        data[i+1,j+1,*], dist_metric, /reject_zeros, M=M)
                nedges += 1   
            endif
            
            if ((i gt 0) and (j lt nr-1)) then begin
                b = ((i-1)+(j+1)*nc)
                edges[nedges,0] = a
                edges[nedges,1] = b 
                weights[nedges] = hiihat_spec_difference(data[i,j,*], $
                        data[i-1,j+1,*], dist_metric, /reject_zeros, M=M)
                nedges += 1   
            endif

        endfor
    endfor ;; graph complete
    envi_report_init, base=base, /finish 


    ;; sort by weight
    if verbose then print, "Sorting by weight"
    order = sort(weights)
    edges = edges[order,*]
    weights = weights[order]
    
    n_join_normal = 0L
    ;; segment
    envi_report_init, "Segmenting image", base=base, /INTERRUPT, title=title 
    envi_report_inc, base, nedges-1

    if (verbose) then print, "Segmenting"
    for i=ulong(0), nedges-1 do begin
       if i mod mod_step eq 0 then begin
          envi_report_stat, base, i, nedges-1, cancel=cancel                                             
          if cancel then goto, cleanup
       endif

        a = find_root(elements, edges[i, 0])
        b = find_root(elements, edges[i, 1])

        if ((a ne b) and $
            (weights[i] le thresholds[a]) and $
            (weights[i] le thresholds[b])) then begin

            n_join_normal += 1
            join_tree, elements, a, b
            a = find_root(elements, a)
            thresholds[a] = weights[i] + (c / float(elements[a,2]))
        endif 
    endfor
    envi_report_init, base=base, /finish 


;; NOTE: There is a weirdness here. Minsize is somehow merging the "0"
;; object with various segments producing a rough, jagged segment
;; edge. Not sure why.
;; NOTE: Minsize is controlling almost all merging events at this time
;; due to our C choice, the noise characteristics of CRISM data, etc. 
    n_join_minsize = 0L
    ; remove small sets
    envi_report_init, "Cleaning up small regions", base=base, /INTERRUPT, title=title 
    envi_report_inc, base, nedges-1

    if verbose then print, "Cleaning up small regions"
    for i=ulong(0), nedges-1 do begin       
        if i mod mod_step eq 0 then begin
          envi_report_stat, base, i, nedges-1, cancel=cancel                                             
          if cancel then goto, cleanup
        endif

        a = find_root(elements, edges[i, 0])
        b = find_root(elements, edges[i, 1])

        if ((a ne b) and $
            ((elements[a,2] lt min_size ) or $
             (elements[b,2] lt min_size )))  then begin ;and $
;          weights[i] lt HUGE and finite(weights[i]) then begin
;            print, "Minsize join with weights ",weights[i]," thresholds ",thresholds[a]," ",thresholds[b]
            n_join_minsize += 1
            join_tree, elements, a, b
        endif
    endfor

    if verbose then print,"Normal joins ",n_join_normal,", minsize joins ",n_join_minsize

    ; write labels
    if verbose then print, "Writing output"
    segmentation = ulonarr(nc, nr)

    ; map elements onto consecutive segment labels
    nlabels = ulong(0) 
    label_map = lonarr((size(elements))[1])-long(1)
    
    if  n_elements(label_map) eq 0 then begin       
       ok = dialog_message('Empty label map in segmentation', $
                           title=title)
       goto, cleanup
    endif

    ;; populate segmentation map with labeled regions
    for j=0,nr-1 do begin
       for i=0,nc-1 do begin
            label = find_root(elements, ulong(i)+ulong(j)*ulong(nc))
            if (label_map[label] eq -1) then begin
                label_map[label] = nlabels
                nlabels += ulong(1)
            endif
            segmentation[i,j] = label_map[label]
        endfor
    endfor

    if nlabels eq 0 then begin
       dialog="Segment labeling error: no labels found"
       if gui_status then ok=dialog_message(dialog,title=title) else $
          print, dialog       
       goto, cleanup
    endif

    if verbose then print,"Derived ",nlabels," segments"
    ;; Now rearrange all the superpixel
    ;; labels randomly to avoid the smooth
    ;; cascading monotonic labels that make
    ;; it hard to distinguish superpixels by
    ;; eye by label
   
    if nlabels gt 1 then begin 
       if permute_segments then begin
          ;; randomly permute labels, 'seed' is a kwparam
          newlabels = long(sort(randomu(seed, max(label_map)+1)))
          segmentation = newlabels[segmentation]
       endif
    endif else begin
       ;; if we only have a single label, then warn the user 
       ok = dialog_message('Warning: singleton segmentation', $
                           title=title)
    endelse

    unsigned_long = 13 ;; datatype for ulong
    band_name = "Superpixels (c="+ strtrim(string(c),2)+", min="+$
            strtrim(string(min_size),2)+", metric="+dist_metric+")"

    if in_memory then begin
        envi_write_envi_file, segmentation, data_type = unsigned_long, $
            ns=nc, nl = nr, nb = 1, r_fid = seg_fid, $
            out_name="Superpixels", bnames = [band_name], $
            file_type=envi_file_type("ENVI Segmentation"),$
            /in_memory
    endif else begin
        ;;This trick is here to prevent the available bands from opening but
        ;;still return an fid. envi_write_envi_file will always pop up that menu without /no_open,
        ;;but with no_open the file is not accessible using the r_fid

        envi_write_envi_file, segmentation, data_type = unsigned_long, $
                              ns=nc, nl = nr, nb = 1, r_fid = seg_fid, $
                              out_name=out_filename, bnames = [band_name], $
                              file_type=envi_file_type("ENVI Segmentation"), $
                              /no_open

        envi_file_mng, id = seg_fid, /remove
        envi_open_file, out_filename, r_fid = seg_fid, /no_realize
    endelse

cleanup:
    ;; close statusbar
    envi_report_init, base=base, /finish 
    if debug then print,"Exiting "+title
end

