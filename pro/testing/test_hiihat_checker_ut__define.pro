pro test_hiihat_checker_ut__define
;+
; Unit test entry point definition for checkerboard image.
; Initializes MGTestCase structure for MGUnit unit test. 
;
; :Categories:
;   testing
;
; :History:
;   Dec 10, 2010 (BDB): Initial implementation
;  
; :Examples:
;   This unit test can be run with the following command:
;
;   mgunit, 'test_hiihat_checker_ut'
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
  compile_opt strictarr
  
  define = { test_hiihat_checker_ut, $
             datadir:'', tmpdir:'', seed:0, verbose:0, debug:0, sep:'',$
             c:0.0, dist_metric:'', coalesce_threshold:0, permute_segments:0, $
             nl:[0.0], varl:[0.0], kl:[0,0], sql:[0], $
             inherits MGTestCase } 
end

function test_hiihat_checker_ut::test_0_init
;+
; Initialization test file for checkerboard segmentation and endmember
; detection unit tests. 
;
; :Categories:
;   testing
;
; :Description:
;   Initializes parameters used by the checkerboard unit tests and
;   verifies that the directory containing the data is
;   accessible. This test is run before any other checkerboard
;   unit tests are executed. 
;
; :History:
;   Dec 10, 2010 (BDB): Initial implementation
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
  hh_path = hiihat_get_config_parm('hiihat_path')
  assert, file_test(hh_path), "HiiHAT directory missing"
  self.sep = path_sep()
  self.datadir = hh_path+'data'+self.sep+'checker'+self.sep
  assert, file_test(self.datadir), "Data directory missing"
  self.verbose = 1
  self.tmpdir = getenv("IDL_TMPDIR")
  self.seed = 1
  self.c = 1.0
  self.dist_metric = 'euclidean_sq'
  self.coalesce_threshold = 0 ;; SMACC coalesce threshold
  self.nl = [3] ;; number of lines in checker file
  self.varl = [5.0] ;; noise variance
  self.kl = [3,9] ;; number of checker classes
  self.sql = [20] ;; checker square size
  self.permute_segments = 0
  return, 1
end


function test_hiihat_checker_ut::test_segmentation
;+
; Tests segmentation functions on 3 and 9 class noisy checkerboard images.
;
; :Categories:
;   testing
;
; :Description:
;   Tests the Felzenswalb segmentation algorithm on 3 and 9 class
;   checkerboard noisy/noiseless images. Both cases should produce segmentations
;   equivalent to the given class map with the provided
;   parameters. The first test assures that with a min_size parameter
;   much smaller than the smallest class size (in pixels), the correct
;   segmentation is still produced with noiseless data. The second
;   test performs the same segmentation on noisy data, assuring that
;   all segments are "pure" with respect to the class map (a pure
;   segment consists of pixels from only one class). The final
;   test assures that the correct segmentation is produced when the
;   min_size is set to the minimum known class size. 
;
; :History:
;   Dec 10, 2010 (BDB): Initial implementation
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
  compile_opt strictarr

  for ni=0, n_elements(self.nl)-1 do begin
     nstr=string(self.nl[ni],format='(%"%d")')
     for si=0, n_elements(self.sql)-1 do begin
        sqstr=string(self.sql[si],format='(%"%d")')
        for ki=0, n_elements(self.kl)-1 do begin
           k=self.kl[ki]
           kstr=string(k,format='(%"%d")')
           for vi=0, n_elements(self.varl)-1 do begin
              var=self.varl[vi]
              varstr=string(var,format='(%"%0.3f")')   
        
              ;; assume failure for tests 
              test_join=0
              test_pure=0
              test_minsize=0

              ;; load the requisite test files
              img_base='checker'+nstr+'x'+nstr+'_sq'+sqstr+'_d3_k'+kstr
              img_path=self.datadir+img_base+'.img'
              img_npath=self.datadir+img_base+'_n'+varstr+'.img'
              img_classpath=self.datadir+img_base+'_class.img'

              envi_open_file, img_path, r_fid=fid 
              envi_open_file, img_npath, r_fid=fid_n 
              envi_open_file, img_classpath, r_fid=fid_class

              ;; get file dimensions 
              envi_file_query, fid, dims=dims, ns=ns, nl=nl, nb=nb

              ;; load the class map
              img_class=envi_get_data(fid=fid_class, dims=dims, pos=[0])

              ;; check simple joins on noiseless data
              ;; This should not merge any pixels from dissimilar classes and 
              ;; should produce segments of the same size as the defined classes
              min_size=2
              hiihat_segment_felzenszwalb, fid, self.c, min_size, self.dist_metric, $
                                           r_fid=seg_fid, verbose=self.verbose, /in_memory, $
                                           permute_segments=self.permute_segments

              ;; get the resulting segmentation map
              segments=envi_get_data(fid=seg_fid, dims=dims, pos=[0]) 

              ;; the segmentation should be the same as the class map
              test_join=(total(img_class-segments) eq 0)
              envi_file_mng, id=seg_fid, /remove 

              ;; now try on noisy data
              hiihat_segment_felzenszwalb, fid_n, self.c, min_size, self.dist_metric, $
                                           r_fid=seg_fid, verbose=self.verbose, /in_memory, $
                                           permute_segments=self.permute_segments

              ;; the segmentation should consist of only pure pixels
              hiihat_seg_purity, seg_fid, fid_class, purity=purity, verbose=self.verbose
              test_pure = purity eq 1.0

              envi_file_mng, id=seg_fid, /remove 

              ;; check minsize on noisy data. 
              ;; This forces the segmentation algorithm to join all regions 
              ;; up to the maximum size for each class, so each segment should 
              ;; consist of one class
              min_size=((ns*nl)/k-1) ;; known size for each class
              hiihat_segment_felzenszwalb, fid_n, self.c, min_size, $
                                           self.dist_metric, r_fid=seg_fid, $
                                           verbose=self.verbose, /in_memory, $
                                           permute_segments=self.permute_segments

              ;; get the resulting segmentation map
              segments=envi_get_data(fid=seg_fid, dims=dims, pos=[0])               

              ;; this should also be the same as the class map
              test_minsize = (total(img_class-segments) eq 0)

cleanup_segmentation_test:
              ;; clean up
              envi_file_mng, id=fid, /remove
              envi_file_mng, id=fid_n, /remove
              envi_file_mng, id=fid_class, /remove
              envi_file_mng, id=seg_fid, /remove


              assert, test_join, "Joins: segmentation does not match class map: "+img_base     
              assert, test_minsize, "Minsize: segmentation does not match class map: "+img_base     

           endfor
        endfor
     endfor
  endfor

  return, 1
end

function test_hiihat_checker_ut::test_endmember_detection
;+
; Tests endmember detection functions on 3 and 9 class noisy checkerboard images.
;
; :Categories:
;   testing
;
; :Description:
;   Tests both the SMACC and N-FINDR endmember detection algorithms on
;   the checkerboard images. Both algorithms use the known class map
;   as the superpixels, and attempt to retrieve the known number of
;   classes for each image. SMACC will succeed and the endmembers
;   should match the class means. N-FINDR should fail gracefully on
;   all inputs since it can only extract (2,num_bands-1] endmembers. 
;
; :History:
;   Dec 10, 2010 (BDB): Initial implementation
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
  compile_opt strictarr

  for ni=0, n_elements(self.nl)-1 do begin
     nstr=string(self.nl[ni],format='(%"%d")')
     for si=0, n_elements(self.sql)-1 do begin
        sqstr=string(self.sql[si],format='(%"%d")')
        for ki=0, n_elements(self.kl)-1 do begin
           k=self.kl[ki]
           kstr=string(k,format='(%"%d")')
           for vi=0, n_elements(self.varl)-1 do begin
              var=self.varl[vi]
              varstr=string(var,format='(%"%0.3f")')        

              ;; remove old rois
              envi_delete_rois, /all

              ;; assume test failure
              test_nfindr=0
              test_smacc=0
              test_smacc_endm=0

              ;; load the requisite test files
              img_base='checker'+nstr+'x'+nstr+'_sq'+sqstr+'_d3_k'+kstr
              img_path=self.datadir+img_base+'.img'
              img_npath=self.datadir+img_base+'_n'+varstr+'.img'
              img_classpath=self.datadir+img_base+'_class.img'

              out_name_roi=self.datadir+img_base+'_endm.roi'
              out_name_sli=self.datadir+img_base+'_endm.sli'
              out_name_slh=self.datadir+img_base+'_endm.sli.hdr'
              out_name_txt=self.datadir+img_base+'_endm.txt'
             
              ;; delete temporary files if they exist
              file_delete, out_name_sli, out_name_slh, /quiet
              file_delete, out_name_roi, out_name_txt, /quiet

              ;; open the image
              envi_open_file, img_path, r_fid=fid 
              envi_open_file, img_npath, r_fid=fid_n 
              envi_open_file, img_classpath, r_fid=fid_class

              envi_file_query, fid, dims=dims, ns=ns, nl=nl, nb=nb

              ;; assume "perfect" segmentation; use known class partitions
              img_class=envi_get_data(fid=fid_class, dims=dims, pos=[0])
             
              ;; first we wish to detect the class means with SMACC
              hiihat_segment_spectra, fid_n, fid_class, spectra=class_means, $
                                      verbose=self.verbose

              hiihat_get_superpixel_endmembers, fid_n, seg_fid=fid_class, $
                                                n_endmembers=k, $ ;number of endmembers to request
                                                out_name_roi=out_name_roi, $ ;roi endmember file outpath
                                                out_name_sli=out_name_sli, $ ; name of the sli file to generate for endmember spectra
                                                r_fid=endmember_fid_smacc, $ ; return fid for the endmember spectra
                                                abund_r_fid=smacc_abund_r_fid, $
                                                coalesce_threshold=coalesce_threshold, $ ;                                              
                                                use_nfindr=0, $ ; Do not Use the NFINDR algorithm (custom HIIHAT implementation)
                                                ignore_segmentation=0, $ ; Use entire image unsegmented (for comparison)                                              
                                                seed=self.seed, $ ; if used, the random seed for the NFINDR algorithm (optional)
                                                in_memory=1, $
                                                verbose=self.verbose

              test_smacc=keyword_set(endmember_fid_smacc)              
              if  test_smacc then begin ;; if this test succeeds, check endmembers
                 envi_file_query, endmember_fid_smacc, ns=ns, nb=nb, nl=nl, dims=dims
                 endmembers_smacc=transpose(envi_get_data(fid=endmember_fid_smacc, dims=dims, pos=0))
                 test_smacc_endm = total(endmembers_smacc[sort(endmembers_smacc)] - $
                                         class_means[sort(class_means)]) eq 0
              endif

              ;; try N-FINDR (and fail due to nb=3 and k>nb-1)
              hiihat_get_superpixel_endmembers, fid_n, seg_fid=fid_class, $
                                                n_endmembers=k, $ ;number of endmembers to request
                                                out_name_roi=out_name_roi, $ ;roi endmember file outpath
                                                out_name_sli=out_name_sli, $ ; name of the sli file to generate for endmember spectra
                                                r_fid=endmember_fid_nfindr, $ ; return fid for the endmember spectra
                                                abund_r_fid=nfindr_abund_r_fid, $
                                                coalesce_threshold=coalesce_threshold, $ ;                                              
                                                use_nfindr=1, $ ; Use the NFINDR algorithm (custom HIIHAT implementation)
                                                ignore_segmentation=0, $ ; Use entire image unsegmented (for comparison)                                              
                                                seed=self.seed, $ ; if used, the random seed for the NFINDR algorithm (optional)
                                                in_memory=1, $                                                
                                                verbose=self.verbose

              ;; expected failure - N-FINDR can only extract (2,nb-1]
              ;;                    endmembers, so this should fail
              ;;                    gracefully
              test_nfindr=(keyword_set(endmember_fid_nfindr) eq 0)

cleanup_endmember_test:
              ;; remove old rois
              envi_delete_rois, /all

              ;; get rid of old files
              envi_file_mng, id=fid, /remove
              envi_file_mng, id=fid_n, /remove
              envi_file_mng, id=fid_class, /remove
              if keyword_set(endmember_fid_smacc) then $
                 envi_file_mng, id=endmember_fid_smacc, /remove
              if keyword_set(endmember_fid_nfindr) then $
                 envi_file_mng, id=endmember_fid_nfindr, /remove
              if keyword_set(smacc_abund_r_fid) then $
                 envi_file_mng, id=smacc_abund_r_fid, /remove
              if keyword_set(nfindr_abund_r_fid) then $
                 envi_file_mng, id=nfindr_abund_r_fid, /remove
              
              assert, test_smacc, "SMACC endmember detection failed"
              assert, test_smacc_endm, "SMACC endmembers do not match class means"
              assert, test_nfindr, "N-FINDR endmember detection did not fail gracefully"

           endfor
        endfor
     endfor
  endfor             
 
  return, 1
 end


function test_hiihat_checker_ut::test_neutral_region
;+
; Tests neutral region on 3x3 noisy grid.
;
; :Categories:
;   testing
;
; :Description:
;   Tests the neutral region finder on a 3x3 macrogrid with artificial
;   noise. Determines whether the found neutral region detected is the
;   lower-left grid (correct answer) regardless of the neutral region
;   precise shape determined. 
;
; :History:
;   Feb 16, 2011 (LM): Initial implementation
;  
; :Author: Lukas Mandrake
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
  compile_opt strictarr

  ;; remove old rois
  envi_delete_rois, /all

  ;; assume failure for tests 
  test_segmentation            = 0
  test_neutral_region_location = 0
  test_neutral_region_size     = 0

  ;; load the requisite test files
  spectest=self.datadir+'neutral_spectrum_test.temp.sli'
  img_test=self.datadir+'checker3x3_sq20_d3_k9_n5.000.img'

  ;; remove old temp files
  file_delete, spectest, /quiet

  envi_open_file, img_test, r_fid=fid 
  envi_file_query, fid, dims=dims, ns=ns, nl=nl, nb=nb

  ;; We will now perform a "standard" segmentation on this
  ;; test data. The segments will grossly oversegment the
  ;; noisy grid image. 
  min_size=20
  c=0.01
  hiihat_segment_felzenszwalb, fid, c, min_size, $
    "euclidean_sq", r_fid=seg_fid, $
    verbose=self.verbose, /in_memory, $
    permute_segments=0

  ;Generate and test segmentation
  segments=envi_get_data(fid=seg_fid, dims=dims, pos=[0])
  test_segmentation = (max(segments) gt 2) and (min(segments) ge 0) 
  assert, test_segmentation           , "Neutral Region: segmentation was not generated: "            +img_test     

  ;Generate neutral region ROI
  hiihat_get_neutral_region, img_fid=fid, seg_fid=seg_fid, $
    roi_out_id = roi_out_id, spectrum_out_filename=spectest,$
    coalesce_threshold=coalesce_threshold, verbose=self.verbose

  ;Convert to x,y points
  roi_contents = envi_get_roi(roi_out_id)
  ypts = floor(roi_contents  /  ns)
  xpts =       roi_contents mod ns
  
  ;Test size of neutral region
  test_neutral_region_size = (size(xpts,/dimensions) gt 5) and (size(xpts,/dimensions) lt 100)
  assert, test_neutral_region_size    , "Neutral Region: neutral region was inappropriately sized: "  +img_test     

                                ;Test location of neutral region
                                ;All of ROI should be contained in the
                                ;lower-left square of the image
  test_neutral_region_location = (max(xpts) le 19) and (min(xpts) ge 0) and (max(ypts) le 59) and (min(ypts) ge 40)
  assert, test_neutral_region_location, "Neutral Region: neutral region was not in correct location: "+img_test     

cleanup_neutral_region_test:
  ;; clean up
  envi_open_file, spectest, r_fid=fidspec 
  envi_file_mng, id=    fid, /remove
  envi_file_mng, id=seg_fid, /delete, /remove
  envi_file_mng, id=fidspec, /delete, /remove
  envi_delete_rois, roi_out_id

  return, 1
end
