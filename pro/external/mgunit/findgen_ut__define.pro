pro findgen_ut__define
  compile_opt strictarr
  
  define = { findgen_ut, inherits MGTestCase }
end

function findgen_ut::test_basic
  compile_opt strictarr
  
  a = findgen(5)
  assert, array_equal(a, [0.0, 1.0, 2.0, 3.0, 4.0]), 'Correct elements'

  return, 1
end

function findgen_ut::test_error
  compile_opt strictarr
    ;;@error_is_pass

    a = findgen('string')

    return, 0
 end


function findgen_ut::test_baderror
  compile_opt strictarr  
    ;;@error_is_fail

    a = findgen('another_string')

    return, 1
 end


