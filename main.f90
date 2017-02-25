program main

  use dense_matrix_test

  
  unit_test: block
    
    call test_dense_matrix_create(1,1)
    call test_dense_matrix_create(12,2)

  end block unit_test
  
end program main
