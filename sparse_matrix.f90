#include "scalar.fpp"
!=====================================================================!
! A sparse matrix type for sparse linear algebra
!
! Author: Komahan Boopathy (komahan@gatech.edu)
!=====================================================================!

module sparse_matrix_interface

  use iso_fortran_env, only : WP => REAL64
  use matrix_interface, only: matrix
  
  implicit none

  private
 
  public :: sparse_matrix
  
  ! Specialized matrix type for sparse storage
  type, abstract, extends(matrix) :: sparse_matrix
     
     type(integer) :: nnz

   contains

     procedure :: get_num_nonzeros
     procedure :: set_num_nonzeros
     
  end type sparse_matrix  

  contains
    
    !===================================================================!
    ! Sets the number of nonzeros in the sparse matrix
    !===================================================================!
    
    pure subroutine set_num_nonzeros(this, nnz)

      class(sparse_matrix), intent(inout) :: this
      type(integer), intent(in) :: nnz

      this % nnz = nnz

    end subroutine set_num_nonzeros
    
    !===================================================================!
    ! Returns the number of nonzero entries in the sparse matrix
    !===================================================================!
    
    pure type(integer) function get_num_nonzeros(this) result(nnz)
      
      class(sparse_matrix), intent(in) :: this

      nnz = this % nnz

    end function get_num_nonzeros

end module sparse_matrix_interface
