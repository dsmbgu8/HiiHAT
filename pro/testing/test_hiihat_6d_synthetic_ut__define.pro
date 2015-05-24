pro test_hiihat_6d_synthetic_ut__define
;+
; Unit test entry point definition for synthetic multispectral
; image. Initializes MGTestCase structure for MGUnit unit test. 
;
; :Categories:
;   testing
;
; :History:
;   Jan 3, 2011 (BDB): Initial implementation
;  
; :Examples:
;   This unit test can be run with the following command:
;
;   mgunit, 'test_hiihat_6d_synthetic_ut'
;
; :Author: Brian D. Bue
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
  
  define = { test_hiihat_6d_synthetic_ut, $
             datadir:'', tmpdir:'', verbose:0, debug:0, sep:'', $
             coalesce_threshold:0, permute_segments: 0, $
             inherits MGTestCase } 
end

function test_hiihat_6d_synthetic_ut::test_0_init
;+
; Initialization test and parameters for 6d synthetic dataset. 
;
; :Categories:
;   testing
;
; :Description:
;   Initializes parameters used by the 6d-synthetic unit tests and
;   verifies that the directory containing the data is
;   accessible. This test is run before any other 6-d synthetic
;   unit tests are executed. 
;
; :History:
;   Jan 3, 2011 (BDB): Initial implementation
;  
; :Author: Brian D. Bue
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

  hh_path = hiihat_get_config_parm('hiihat_path')
  assert, file_test(hh_path), "HiiHAT directory missing"
  self.verbose = 0
  self.debug = 0
  self.sep = path_sep()
  self.datadir = hh_path+'data'+self.sep+'6d_synthetic'+self.sep
  assert, file_test(self.datadir), "Data directory missing"
  self.tmpdir = getenv("IDL_TMPDIR")
  self.coalesce_threshold = 0
  self.permute_segments = 0 ;; do not permute segments
  envi_delete_rois, /all ;; get rid of any old rois

  return, 1
end


function test_hiihat_6d_synthetic_ut::test_segmentation
;+
; Tests segmentation functions on 11 class noisy 6d-synthetic data.
;
; :Categories:
;   testing
;
; :Description:
;   Tests the felzenszwalb segmentation algorithm on a 6-dimensional 
;   synthetic image. The synthetic image contains 11 noisy classes,
;   including one single pixel class, which are all separable with the
;   squared Euclidean metric. This function tests to make sure that
;   the segmentation algorithm produces 11 "pure" segments (a pure
;   segment consists of pixels from only one class). 
;
; :History:
;   Jan 3, 2011 (BDB): Initial implementation
;  
; :Author: Brian D. Bue
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

  dist_metric='euclidean'

  ;; assume test failure
  test_pure=0
  test_nseg=0

  ;; set up parameters for 11 class noisy data
  k = 11
  kstr = string(k,format='(%"%d")')
  nstr = "-n_m0v10"
  img_base=kstr+'class_noisy'+self.sep+''+kstr+'class-layout1'
  img_npath = self.datadir+img_base+nstr+'.img'
  img_classpath=self.datadir+img_base+'.map.img'

  ;; open noisy image and class map
  envi_open_file, img_npath, r_fid=fid_n 
  envi_open_file, img_classpath, r_fid=fid_class

  ;; get input file dimensions
  envi_file_query, fid_n, dims=dims, ns=ns, nl=nl, nb=nb

  ;; load class map data 
  img_class=envi_get_data(fid=fid_class, dims=dims, pos=[0])

  ;; get class means
  hiihat_segment_spectra, fid_n, fid_class, spectra=class_means, $
                          verbose=self.verbose, variances=variances

  ;; do segmentation, allowing for single pixel classes
  min_size=1
  c = 10
  hiihat_segment_felzenszwalb, fid_n, C, min_size, dist_metric, $
                               r_fid=fid_seg, verbose=self.verbose, /in_memory, $
                               permute_segments=self.permute_segments

  ;; get resulting image semgnets
  hiihat_segment_spectra, fid_n, fid_seg, spectra=img_segments, $
                          verbose=self.verbose

  ;; load segment image
  img_seg=envi_get_data(fid=fid_seg, dims=dims, pos=[0])

  ;; if the segmentation produced < # of classes, fail
  test_nseg = n_elements(uniq(img_seg)) gt k

  ;; all segments should be "pure" - consisting of only a single class
  hiihat_seg_purity, img_seg, img_class, purity=purity, verbose=self.verbose
  test_pure = purity eq 1.0

cleanup_segmentation_test:
  ;; clean up
  envi_file_mng, id=fid_n, /remove
  envi_file_mng, id=fid_class, /remove
  envi_file_mng, id=fid_seg, /remove

  ;; assert segmentation tests
  assert, test_nseg, "Segmentation produced less than #classes segments"+img_base     
  assert, test_pure, "Segmentation produced mixed segments"+img_base     
  
  return, 1
end

function test_hiihat_6d_synthetic_ut::test_nfindr
;+
; Tests the N-FINDR algorithm on noisy 8-class synthetic data. 
;
; :Categories:
;   testing
;
; :Description:
;   Test the hiihat_get_superpixel_endmembers function using the
;   N-FINDR algorithm using noisy 8-class synthetic data. First
;   attempts to detect the exact class means using the known class map
;   as the input to the superpixel endmember detection routine. These
;   endmembers should match the class means exactly. Next
;   attempts to detect superpixel endmembers from a new
;   oversegmentation of the image data. These endmembers should be
;   within the known noise variance of the class means. N-FINDR can
;   only extract num_bands-1 endmembers, but those endmembers should
;   each be representatives from a different class.
;
; :History:
;   Jan 3, 2011 (BDB): Initial implementation
;  
; :Author: Brian D. Bue
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

  dist_metric="euclidean"

  ;; assume test failure
  test_nfindr=0
  test_nfindr_seg=0
  test_nfindr_endm=0
  test_nfindr_endm_seg=0

  ;; remove old rois
  envi_delete_rois, /all  

  ;; load noisy 8-class synthetic data
  k = 8
  kstr = string(k,format='(%"%d")')
  nstr = "-n_m0v5"
  img_base=kstr+'class_noisy'+self.sep+''+kstr+'class-layout1'
  img_npath = self.datadir+img_base+nstr+'.img'
  img_classpath=self.datadir+img_base+'.map.img'

  ;; define output temporary file paths
  out_name_roi=self.datadir+img_base+'_endm.roi'
  out_name_sli=self.datadir+img_base+'_endm.sli'
  out_name_slh=self.datadir+img_base+'_endm.sli.hdr'
  out_name_txt=self.datadir+img_base+'_endm.txt'
  
  ;; delete temporary files if they exist
  file_delete, out_name_sli, out_name_slh, /quiet
  file_delete, out_name_roi, out_name_txt, /quiet

  ;; load noisy image and class map
  envi_open_file, img_npath, r_fid=fid_n 
  envi_open_file, img_classpath, r_fid=fid_class

  envi_file_query, fid_n, dims=dims, ns=ns, nl=nl, nb=nb

  ;; assume "perfect" segmentation; use known class partitions
  img_class=envi_get_data(fid=fid_class, dims=dims, pos=[0])

  ignore_seg = 1 ;; use the segmentation for superpixel endmember extraction

  ;; we wish to detect the class means
  hiihat_segment_spectra, fid_n, fid_class, spectra=class_means, $
                          verbose=self.verbose


  n_endmembers=5 ;; N-FINDR can only extract nb-1 endmembers

  ;; first extract endmembers matching the class means
  hiihat_get_superpixel_endmembers, fid_n, seg_fid=fid_class, $
                                    n_endmembers=n_endmembers, $ ;number of endmembers to request
                                    out_name_roi=out_name_roi, $ ;roi endmember file outpath
                                    out_name_sli=out_name_sli, $ ; name of the sli file to generate for endmember spectra
                                    out_name_txt=out_name_txt, $ ; name of the sli file to generate for endmember spectra
                                    r_fid=endmember_fid_nfindr, $ ; return fid for the endmember spectra
                                    abund_r_fid=nfindr_abund_r_fid, $
                                    coalesce_threshold=coalesce_threshold, $ ;                                              
                                    use_nfindr=1, $ ; Use the NFINDR algorithm (custom HIIHAT implementation)
                                    ignore_segmentation=0, $ ; Use entire image unsegmented (for comparison)                                              
                                    seed=seed, $ ; if used, the random seed for the NFINDR algorithm (optional)
                                    in_memory=1, $                                                
                                    verbose=self.verbose

  ;; make sure the output fid was set (this checks the above function
  ;; returned properly)
  test_nfindr=keyword_set(endmember_fid_nfindr)
  if not test_nfindr then goto, cleanup_test_nfindr

  ;; get the endmembers
  envi_file_query, endmember_fid_nfindr, ns=ns, nb=nb, nl=nl, dims=dims
  endmembers_nfindr=transpose(envi_get_data(fid=endmember_fid_nfindr, dims=dims, pos=0))

  ;; these should match the class means exactly
  dmtx = hiihat_distance_matrix(dist_metric, class_means, endmembers_nfindr)
  test_nfindr_endm = total(min(dmtx,dimension=1)) eq 0.0
  

  ;; now extract endmembers from a segmentation
  min_size=1
  C = 100
  hiihat_segment_felzenszwalb, fid_n, C, min_size, dist_metric, $
                               r_fid=fid_seg, verbose=self.verbose, /in_memory, $
                               permute_segments=self.permute_segments
 
  ;; remove endmember/abund fids if they exist
  if keyword_set(endmember_fid_nfindr) then $
    envi_file_mng, id=endmember_fid_nfindr, /remove
  if keyword_set(nfindr_abund_r_fid) then $
    envi_file_mng, id=nfindr_abund_r_fid, /remove

  hiihat_get_superpixel_endmembers, fid_n, seg_fid=fid_seg, $
                                    n_endmembers=n_endmembers, $ ;number of endmembers to request
                                    out_name_roi=out_name_roi, $ ;roi endmember file outpath
                                    out_name_sli=out_name_sli, $ ; name of the sli file to generate for endmember spectra
                                    out_name_txt=out_name_txt, $ ; name of the sli file to generate for endmember spectra
                                    r_fid=endmember_fid_nfindr, $ ; return fid for the endmember spectra
                                    abund_r_fid=nfindr_abund_r_fid, $
                                    coalesce_threshold=coalesce_threshold, $ ;                                              
                                    use_nfindr=1, $ ; Use the NFINDR algorithm (custom HIIHAT implementation)
                                    ignore_segmentation=0, $ ; Use entire image unsegmented (for comparison)                                              
                                    seed=seed, $ ; if used, the random seed for the NFINDR algorithm (optional)
                                    in_memory=1, $                                                
                                    verbose=self.verbose

  ;; make sure the output fid was set (this checks the above function
  ;; returned properly)
  test_nfindr_seg=keyword_set(endmember_fid_nfindr)
  if not test_nfindr_seg then goto, cleanup_test_nfindr

  ;; get the endmembers
  envi_file_query, endmember_fid_nfindr, ns=ns, nb=nb, nl=nl, dims=dims
  endmembers_nfindr=transpose(envi_get_data(fid=endmember_fid_nfindr, dims=dims, pos=0))

  ;; make sure they're close enough to the true class means
  ;; (assuming euclidean distance)
  scalef = 10000.0 ;; 6d-syn data scaled by 10k 
  dmtx = hiihat_distance_matrix(dist_metric, class_means / scalef, endmembers_nfindr / scalef)  
  test_nfindr_seg_endm = (total(min(dmtx,dimension=1)) le (n_endmembers*12.0))

  if self.verbose then begin
     print, "Class means"
     print, class_means
     
     print, "NFINDR Endmembers"
     print, endmembers_nfindr
     
     print, "Distance Matrix"
     print, dmtx
  endif

cleanup_test_nfindr:

  envi_delete_rois, /all

  envi_file_mng, id=fid_n, /remove
  envi_file_mng, id=fid_class, /remove
  envi_file_mng, id=fid_seg, /remove

  if keyword_set(endmember_fid_nfindr) then $
    envi_file_mng, id=endmember_fid_nfindr, /remove
  if keyword_set(nfindr_abund_r_fid) then $
    envi_file_mng, id=nfindr_abund_r_fid, /remove

  ;;delete temporary files if they exist
  file_delete, out_name_sli, out_name_slh, /quiet
  file_delete, out_name_roi, out_name_txt, /quiet
  
  ;; assert tests
  assert, test_nfindr, "N-FINDR endmember detection failed"
  assert, test_nfindr_seg, "N-FINDR superpixel endmember detection failed"
  assert, test_nfindr_endm, "N-FINDR endmembers do not match class means"
  assert, test_nfindr_seg_endm, "N-FINDR superpixel endmembers too far from class means"

  return, 1
 end


function test_hiihat_6d_synthetic_ut::test_smacc
;+
; Tests the SMACC algorithm on noisy 8-class synthetic data. 
;
; :Categories:
;   testing
;
; :Description:
;   Test the hiihat_get_superpixel_endmembers function using the
;   SMACC algorithm using noisy 8-class synthetic data. First
;   attempts to detect the exact class means using the known class map
;   as the input to the superpixel endmember detection routine. These
;   endmembers should match the class means exactly. Next
;   attempts to detect superpixel endmembers from a new
;   oversegmentation of the image data. These endmembers should be
;   within the known noise variance of the class means. SMACC can
;   extract an arbitrary number of endmembers, so we should be able to
;   retrieve all 8 class means and select pixel endmembers that are
;   near each of those 8 means. 
;
; :History:
;   Jan 3, 2011 (BDB): Initial implementation
;  
; :Author: Brian D. Bue
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

  dist_metric="euclidean"

  ;; assume test failure
  test_smacc = 0
  test_smacc_endm = 0
  test_smacc_seg = 0
  test_smacc_endm_seg = 0

  envi_delete_rois, /all  

  ;; load noisy 8-class synthetic data
  k = 8
  kstr = string(k,format='(%"%d")')
  nstr = "-n_m0v5"
  img_base=kstr+'class_noisy'+self.sep+''+kstr+'class-layout1'
  img_npath = self.datadir+img_base+nstr+'.img'
  img_classpath=self.datadir+img_base+'.map.img'

  ;; set up temp filenames
  out_name_roi=self.datadir+img_base+'_endm.roi'
  out_name_sli=self.datadir+img_base+'_endm.sli'
  out_name_slh=self.datadir+img_base+'_endm.sli.hdr'
  out_name_txt=self.datadir+img_base+'_endm.txt'
  
  ;; delete temporary files if they exist
  file_delete, out_name_sli, out_name_slh, /quiet
  file_delete, out_name_roi, out_name_txt, /quiet

  ;; open the noisy image and class map
  envi_open_file, img_npath, r_fid=fid_n 
  envi_open_file, img_classpath, r_fid=fid_class

  envi_file_query, fid_n, dims=dims, ns=ns, nl=nl, nb=nb

  ;; assume "perfect" segmentation; use known class partitions
  img_class=envi_get_data(fid=fid_class, dims=dims, pos=[0])

  
  ;; first, we wish to detect the class means
  hiihat_segment_spectra, fid_n, fid_class, spectra=class_means, $
                          verbose=self.verbose

  n_endmembers = 8 ;; SMACC can get all eight class endmembers
  hiihat_get_superpixel_endmembers, fid_n, seg_fid=fid_class, $
                                    n_endmembers=n_endmembers, $ ;number of endmembers to request
                                    out_name_roi=out_name_roi, $ ;roi endmember file outpath
                                    out_name_sli=out_name_sli, $ ; name of the sli file to generate for endmember spectra
                                    r_fid=endmember_fid_smacc, $ ; return fid for the endmember spectra
                                    abund_r_fid=smacc_abund_r_fid, $
                                    coalesce_threshold=coalesce_threshold, $ ;                                              
                                    use_nfindr=0, $ ; Do not Use the NFINDR algorithm (custom HIIHAT implementation)
                                    ignore_segmentation=0, $ ; Use entire image unsegmented (for comparison)                                              
                                    seed=seed, $ ; if used, the random seed for the NFINDR algorithm (optional)
                                    in_memory=1, $
                                    verbose=self.verbose

  ;; make sure that the endmember detection routine completed
  test_smacc=keyword_set(endmember_fid_smacc)
  if test_smacc then begin ;; if this test succeeds, check endmembers
     envi_file_query, endmember_fid_smacc, ns=ns, nb=nb, nl=nl, dims=dims
     endmembers_smacc=transpose(envi_get_data(fid=endmember_fid_smacc, dims=dims, pos=0))
     
     ;; these should equal the class means
     dmtx = hiihat_distance_matrix(dist_metric, class_means, endmembers_smacc)
     test_smacc_endm = (total(min(dmtx,dimension=1)) eq 0.0)

     if keyword_set(endmember_fid_smacc) then $
        envi_file_mng, id=endmember_fid_smacc, /remove
     if keyword_set(smacc_abund_r_fid) then $
        envi_file_mng, id=smacc_abund_r_fid, /remove

  endif

  ;; now do superpixel endmember detection
  min_size=1
  C = 1
  hiihat_segment_felzenszwalb, fid_n, C, min_size, dist_metric, $
                               r_fid=fid_seg, verbose=self.verbose, /in_memory, $
                               permute_segments=self.permute_segments

  hiihat_get_superpixel_endmembers, fid_n, seg_fid=fid_seg, $
                                    n_endmembers=n_endmembers, $ ;number of endmembers to request
                                    out_name_roi=out_name_roi, $ ;roi endmember file outpath
                                    out_name_sli=out_name_sli, $ ; name of the sli file to generate for endmember spectra
                                    r_fid=endmember_fid_smacc, $ ; return fid for the endmember spectra
                                    abund_r_fid=smacc_abund_r_fid, $
                                    coalesce_threshold=coalesce_threshold, $ ;                                              
                                    use_nfindr=0, $ ; Do not Use the NFINDR algorithm (custom HIIHAT implementation)
                                    ignore_segmentation=0, $ ; Use entire image unsegmented (for comparison)                                              
                                    seed=seed, $ ; if used, the random seed for the NFINDR algorithm (optional)
                                    in_memory=1, $
                                    verbose=self.verbose
 
  ;; make sure that the endmember detection routine completed
  test_smacc_seg=keyword_set(endmember_fid_smacc)
  if test_smacc_seg then begin
     ;; make sure they're close enough to the true class means
     ;; (assuming euclidean distance)
     scalef = 10000.0
     dmtx = hiihat_distance_matrix(dist_metric, class_means / scalef, endmembers_smacc / scalef)
     test_smacc_endm_seg = (total(min(dmtx,dimension=1)) le (n_endmembers*12.0))

     if self.verbose then begin
        print, "Class means"
        print, class_means
        
        print, "SMACC Endmembers"
        print, endmembers_smacc
        
        print, "Distance Matrix"
        print, dmtx
     endif
  endif

cleanup_test_smacc:
  ;; remove old rois
  envi_delete_rois, /all

  ;; close files
  envi_file_mng, id=fid_n, /remove
  envi_file_mng, id=fid_class, /remove
  envi_file_mng, id=fid_seg, /remove

  if keyword_set(endmember_fid_smacc) then $
     envi_file_mng, id=endmember_fid_smacc, /remove
  if keyword_set(smacc_abund_r_fid) then $
     envi_file_mng, id=smacc_abund_r_fid, /remove

  ;; delete temporary files if they exist
  file_delete, out_name_sli, out_name_slh, /quiet
  file_delete, out_name_roi, out_name_txt, /quiet

  assert, test_smacc, "SMACC endmember detection failed"
  assert, test_smacc_endm, "SMACC endmembers do not match class means"
  assert, test_smacc_seg, "SMACC superpixel endmember detection failed"
  assert, test_smacc_endm_seg, "SMACC superpixel endmembers too far from class means"

  return, 1
 end
