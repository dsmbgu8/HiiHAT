function indgen_uts::init
  compile_opt strictarr

  if (~self->mgtestsuite::init()) then return, 0

  self->add, ['indgen_ut', 'findgen_ut']
  
  
  return, 1
end

pro indgen_uts__define
  compile_opt strictarr
  
  define = { indgen_uts, inherits MGTestSuite }
end
