;+
; Returns the value of the requested configuration parameter. On the first call, initializes the 
; read-only, global system variable "!hiihat_config via the hiihat_parse_config_file function. 
; Returns the value of the requested parameter, or 0 if the parameter
; is not found in the configuration parameter structure 
;
; :Categories: initialization 
; :Author: Brian Bue
; :Params:
;  parm: in, required, type=string
;   name of parameter to retrieve (case insensitive string).
; :Keywords:
;  cfg_file: in, optional, type=string
;    path to config file to parse
;
; :Examples:
;   Returns the value of the configuration parameter "verbose"
;
;   verbose = hiihat_get_config_parm("verbose")
; 
; :History:
;   Dec 05, 2010 (BDB): Initially commited to svn.
;-
function hiihat_get_config_parm, parm, cfg_file=cfg_file
  ;; if we haven't parsed the config file/defaults, do it here

  ;; if our system parameter does not exist, initialize it
  defsysv, "!hiihat_config", exists=exists
  if not exists or keyword_set(cfg_file) then begin     
     cfg = hiihat_parse_config_file(cfg_file=cfg_file)
     defsysv, "!hiihat_config", cfg     
  endif
  
  parm_idx = where(tag_names(!hiihat_config) eq STRUPCASE(parm))
  if parm_idx eq -1 then begin
     print, "Configuration parameter ", parm, " is undefined."
     return, 0 
  endif

  return, !hiihat_config.(parm_idx)
end

