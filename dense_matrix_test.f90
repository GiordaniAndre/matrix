#include "scalar.fpp"
!=====================================================================!
! Module to test the dense matrix implementations
!=====================================================================!

module dense_matrix_test

  use, intrinsic :: iso_fortran_env, only : WP => REAL64
  use dense_matrix_interface
  
contains

  subroutine test_dense_matrix_create(nrows, ncols)

    type(integer)      :: nrows
    type(integer)      :: ncols
    type(dense_matrix) :: A
    type(scalar)       :: val

    ! Test the first constructor 
    create_and_add : block

      A = dense_matrix(nrows, ncols)

      if (nrows .ne. A % get_row_size()) then
         print *, "Number of row mismatch"
      end if

      if (ncols .ne. A % get_col_size()) then
         print *, "Number of col mismatch"
      end if

      do i = 1, nrows
         do j = 1, ncols

            ! Generate a random number
            call random_number(val)

            ! Insert the matrix entries into the block
            call A % add_entry(i, j, val)

            ! Get the matrix entries from the block
            if (val - A % get_entry(i,j) .gt. epsilon(1.0_WP) ) then
               print *, "DENSE MATRIX error", i, j, val, A % get_entry(i,j)
            end if

         end do
      end do

    end block create_and_add

    ! Test for constructor 2
    constructor2: block

      type(scalar) :: B(nrows,ncols)

      print *, "constructing dense matrix from existing matrix"
      
      A = dense_matrix(B)
      
    end block constructor2

  end subroutine test_dense_matrix_create

end module dense_matrix_test
