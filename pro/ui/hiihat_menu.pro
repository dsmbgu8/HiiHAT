;+ 
; Hii-HAT main menu interface. Based on code included in the CAT
; toolkit for CRISM, by Shannon Pelkey June 2006
;
; :Author: David R. Thompson
; :Categories: ui
;
; :Params: 
;   buttonInfo, in, required, type=buttoninfo
;    menu button ui parameter
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
pro hiihat_menu_define_buttons, buttonInfo
  menustr='HiiHAT v1.03'
  p = 0

  ENVI_DEFINE_MENU_BUTTON, buttonInfo, Value=menustr, $
    ref_value = 'File', /Sibling, position='after', /Menu

  ENVI_DEFINE_MENU_BUTTON, buttonInfo, Value='Preprocess Image', $
    ref_value=menustr, uvalue="HHPreprocess", position=p, $
    event_pro='hiihat_event_preprocess'
  p = p + 1

  ENVI_DEFINE_MENU_BUTTON, buttonInfo, Value='Superpixel Segmentation', $
    ref_value=menustr, uvalue="HHSegment",position=p, $
    event_pro='hiihat_event_segment'
  p = p+1
  ENVI_DEFINE_MENU_BUTTON, buttonInfo, Value='Superpixel Endmember Detection', $
    ref_value=menustr, uvalue="HHEndmembers",position=p, $
    event_pro='hiihat_event_get_superpixel_endmembers'
  p = p+1
  ENVI_DEFINE_MENU_BUTTON, buttonInfo, Value='Superpixel Neutral Region Detection', $
    ref_value=menustr, uvalue="HHNeutral", position=p, $
    event_pro='hiihat_event_find_neutral'
  p = p+1
  ENVI_DEFINE_MENU_BUTTON, buttonInfo, Value='Calculate Mean Image', $
    ref_value=menustr, uvalue="HHMean Image", position=p, $
    event_pro='hiihat_event_mean_image'

  ENVI_DEFINE_MENU_BUTTON, buttonInfo, Value='Summary Classification', $
    ref_value=menustr, uvalue="HHSummaryClass", position=p, $
    event_pro='hiihat_event_autoclass'
  p = p+1 

  ENVI_DEFINE_MENU_BUTTON, buttonInfo, Value='Learn Metric from ROIs', $
    ref_value=menustr, uvalue="HHLearnMetric",position=p, $
    event_pro='hiihat_event_learn_metric'
  p = p+1

  ENVI_DEFINE_MENU_BUTTON, buttonInfo, Value='Apply Learned Metric', $
    ref_value=menustr, uvalue="HHApplyMetric",position=p, $
    event_pro='hiihat_event_apply_metric'
  p = p+1


  ENVI_DEFINE_MENU_BUTTON, buttonInfo, Value='HiiHAT Configuration', $
                           ref_value=menustr, position=p, /Menu
  ENVI_DEFINE_MENU_BUTTON, buttonInfo, Value='Reload Configuration File', $
                           ref_value='HiiHAT Configuration', position=0, $
                           event_pro='hiihat_event_config_reload', $
                           uvalue="HHReload"
  ENVI_DEFINE_MENU_BUTTON, buttonInfo, Value='Save Current Configuration As Default', $
                           ref_value='HiiHAT Configuration', position=1, $
                           event_pro='hiihat_event_config_save', $
                           uvalue="HHSave"


  ENVI_DEFINE_MENU_BUTTON, buttonInfo, Value='HiiHAT User Guide', $     
    ref_value='Help', position=1,event_pro='hiihat_event_help',$
    uvalue='HHhelp',separator=1

end

pro hiihat_menu
;+
; This is a placeholder function neccessary because IDL requires each source
; file to contain a function by the same name as the file in order for the
; resolve_routine function (used to rebuild HiiHAT) to work properly.
;-
end
