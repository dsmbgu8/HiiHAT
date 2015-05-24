;+ 
; This script makes all of the idldoc-based documentation for all procedures in hiihat/pro. 
;
;  :Author: B. Bue
;
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
;
;_
pro make_hiihat_docs
  docs_title="HiiHAT"
  docs_subtitle="Developer Documentation for HiiHAT IDL functions."
  hiihat_path = hiihat_get_config_parm('hiihat_path')
  sep = path_sep()
  rootpath = hiihat_path+sep+'pro'+sep
  docspath = hiihat_path+sep+'doc'+sep+'idldoc'+sep
  idldoc, root=rootpath, output=docspath, format="rst", source_link=1, $
          title=docs_title, subtitle=docs_subtitle
end
