#include "scalar.fpp"
!=====================================================================!
! Module that defines a matrix type that contains dense storage of
! entries
!
! Author: Komahan Boopathy (komahan@gatech.edu)
!=====================================================================!

module dense_matrix_interface
  
  use iso_fortran_env, only : WP => REAL64
  use matrix_interface, only: matrix

  implicit none

  private

  public :: dense_matrix, destroy
  
  ! Specialized matrix type for dense storage
  type, extends(matrix) :: dense_matrix
     
     type(scalar), allocatable, dimension(:,:) :: vals
     
   contains

     ! Type-Bound Procedures
     procedure :: add_entry
     procedure :: get_entry

     ! Destructor
     final :: destroy
     
  end type dense_matrix
  
  ! Interfaces
  interface dense_matrix
     procedure construct_empty_matrix
     procedure construct_from_matrix
  end interface dense_matrix
   
contains

    
  !===================================================================!
  ! Initializes an instance of dense matrix
  !===================================================================!
  
  function construct_empty_matrix(row_size, col_size) result(this)

    integer            :: col_size
    integer            :: row_size
    type(dense_matrix) :: this

    ! Set matrix dimensions
    call this % set_row_size(row_size)
    call this % set_col_size(col_size) 

    ! Allocate space
    allocate(this % vals(this % get_row_size(), this % get_col_size()))

    ! Zero the entries
    this % vals = 0.0_WP
    
  end function construct_empty_matrix
    
  !===================================================================!
  ! Creates an instance of matrix from supplied matrix entries
  !===================================================================!
  
  pure type(dense_matrix) function construct_from_matrix(mat) result(this)

    type(scalar), intent(in) :: mat(:,:)

!!$    ! Bounds and values are copied
!!$    allocate(this % vals, source = mat) ! mold = mat copies just the bounds
!!$
!!$    ! Set matrix dimensions before return
!!$    call this % set_row_size( this % get_row_size() )
!!$    call this % set_col_size( this % get_col_size() )

    find_size: block

      type(integer) :: mat_shape(2)

      mat_shape = shape(mat)

      ! Set matrix dimensions before return
      call this % set_row_size( mat_shape(1) )
      call this % set_col_size( mat_shape(2) )
      
    end block find_size
        
    ! Allocate space
    allocate(this % vals(this % get_row_size(), this % get_col_size()))

    ! Zero the entries
    this % vals = mat

  end function construct_from_matrix

  !=================================================================!
  ! Destructor for the matrix
  !=================================================================!

  subroutine destroy(this)

    type(dense_matrix), intent(inout) :: this

    ! Free up allocated memory in heap
    if (allocated(this % vals)) then
       deallocate(this % vals)
    end if

    print *, "Destructing dense matrix!"

  end subroutine destroy
  
  !=================================================================!
  ! Adding an entry to a dense matrix
  !=================================================================!

  pure subroutine add_entry(this, row, col, val)

    class(dense_matrix), intent(inout) :: this
    type(integer), intent(in) :: row
    type(integer), intent(in) :: col
    type(scalar), intent(in)  :: val
    
    this % vals(row, col) = val

  end subroutine add_entry
  
  !=================================================================!
  ! Fetch the entry corresponding to the row and column
  !=================================================================!

  pure type(scalar) function get_entry(this, row, col) result(val)
    
    class(dense_matrix), intent(in) :: this
    type(integer), intent(in) :: row
    type(integer), intent(in) :: col

    val = this % vals(row, col)
    
  end function get_entry
  
end module dense_matrix_interface
