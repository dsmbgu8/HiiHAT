function hiihat_convert_string_typed, str
;+
; helper function to convert numeric strings to float, fix, or string
; :Params:
;  str: in, required, type=string
;   input string to parse
;-  
  floatval=stregex(str,"^([+-]?[0-9]+\.([0-9]+)?|\.[0-9]+)$", /extract) 
  if floatval then val=float(floatval) else begin
     fixval=stregex(str,"^([+-]?[0-9]+)$", /extract) 
     if fixval then val = fix(fixval) else val = str
  endelse
  return, val
end

;+
; Parses configuration parameters in the (global) hiihat.sav file and (user-defined)
; hiihat.cfg parameter file. If save_defaults is true, the default parameters in hiihat.sav
; are overwritten. Global parameters can be accessed via the hiihat_get_config_param 
; function.
;
; :Categories:
;   initialization 
;
; :Keywords:
;  save_defaults: in, optional, type=boolean
;   overwrite defaults saved in hiihat_defaults.sav file
;  cfg_file: in, optional, type=string
;   config file to parse (default: hiihat.cfg)
;  def_file: in, optional, type=string
;   .sav file to parse and optionally write (default: hiihat_defaults.sav)
;  defaults_only: in, optional, type=boolean
;    use default values only (don't parse cfg file)
;  verbose: in, optional, type=boolean
;    print verbose output to console
;
; :Examples:
;   The following code will parse the config file "input.cfg" 
;   and save the parsed values to the "defaults.sav" file:
; 
;   hiihat_parse_config_file, cfg_file='input.cfg', def_file='defaults.sav', $
;                           save_defaults=1
;  
; :History:
;   Dec 05, 2010 (BDB): Initially commited to svn.
;
;-
function hiihat_parse_config_file, cfg_file=cfg_file, def_file=def_file, $
                                   save_defaults=save_defaults, verbose=verbose, $
                                   defaults_only=defaults_only
  compile_opt strictarr

  title='hiihat_parse_config_file'
  
  ;; get the path to this function to get the hiihat path
  func_file_path=routine_filepath(title,/is_function)
  sep = path_sep()
  hiihat_path=strmid(func_file_path,0,strpos(func_file_path,'pro'+sep+'initialization'))

  ;; make sure we have the right one
  if not file_test(hiihat_path+sep+'pro'+sep) then begin
     print, "ERROR: HiiHAT directory not found"
     retall
  endif

  ;; set up default file paths within hiihat directory
  if not keyword_set(cfg_file) then begin
     cfg_file = hiihat_path+'hiihat.cfg'
  endif

  if not keyword_set(def_file) then begin
     def_file = hiihat_path+'hiihat_defaults.sav'
  endif

  if not keyword_set(save_defaults) then save_defaults=0

  ;; load saved default parameters, if available 
  if file_test(def_file) then begin
     restore, def_file ;; parses the old "cfg" struct
  endif else begin 
     cfg = {hiihat_path:hiihat_path, cfg_file:cfg_file, def_file:def_file} 
     save_defaults=1 ;; write a new default file
  endelse

  ;; if config file unavailable, skip to cleanup
  if file_test(cfg_file) and not keyword_set(defaults_only) then begin 
     openr, lun, cfg_file, /get_lun 
  endif else begin
     defaults_only=1
     goto, cleanup
  endelse

  ;; since we may need to override the typedefs in the old struct (which
  ;; idl does not allow), create a new struct here 
  ;; FIXME: this will allow one to arbitarily change typedefs of config parms

  record = ''  
  new_cfg = {hiihat_path:hiihat_path, cfg_file:cfg_file, def_file:def_file} 
  while (eof(lun) ne 1) do begin
     readf, lun, record
     record = strtrim(record)
     ;; skip comments
     if strmid(record,0,1) eq ';' then continue 

     ;; parse tag/value pair
     tagval = strsplit(record, '=', /extract)  
     tag = strupcase(strtrim(tagval[0]))

     ;; remove trailing comments and get typed value
     valstr = strtrim((strsplit(tagval[1],";", /extract))[0])         
     typed_value = hiihat_convert_string_typed(valstr)

     ;; append to config parms
     new_cfg = create_struct(new_cfg,tag,typed_value) 
  endwhile
  free_lun, lun

  ;; populate config parms with old defaults not parsed in user .cfg file
  cfg_names = tag_names(cfg)
  for i = 0,n_tags(cfg)-1 do begin
     cfg_tag = cfg_names[i]
     idx = where(tag_names(new_cfg) eq cfg_tag)
     if idx eq -1 then begin ;; cfg parm not given in hiihat.cfg file
        new_cfg = create_struct(new_cfg,cfg_tag,cfg.(i))
     endif 
  endfor

  ;; update global config variable accordingly
  cfg = new_cfg

  ;; if the paths are incorrect (e.g. from an old save file), 
  ;; update them before writing/returning
  if cfg.HIIHAT_PATH ne hiihat_path then begin     
     cfg.HIIHAT_PATH=hiihat_path
     cfg.CFG_FILE=cfg_file
     cfg.DEF_FILE=def_file
  endif

cleanup:
  ;; dump the newly parsed defaults if desired 
  if save_defaults then begin
     write_status = hiihat_check_file(def_file,write=1)
     if write_status eq 0 then begin 
        dialog='Cannot write defaults file: '+def_file
        ok=dialog_message(dialog,title=title)
     endif else begin
        save, filename = def_file, cfg
     endelse
  endif

  debug = cfg.DEBUG
  if not keyword_set(verbose) then verbose = cfg.VERBOSE 
  if debug then begin
     print, "Entering "+title 
     if keyword_set(defaults_only) then begin
        if verbose then begin
           print, 'Config file not found or defaults_only=1'
           print, "Loading defaults from: "+def_file
        endif
     endif else begin
        if verbose then begin 
           print, "Loading defaults from: "+def_file
           print, "Loading configuration from: "+cfg_file
        endif
     endelse

     if save_defaults and verbose then $
        print, "Saving defaults to: "+def_file

     if verbose then help, cfg, /struct
     print, 'Exiting '+title
  endif

  return, cfg
end


