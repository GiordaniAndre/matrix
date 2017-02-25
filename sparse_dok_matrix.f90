#include "scalar.fpp"

!=====================================================================!
! A Dictionary of keys implementation of the sparse matrix type. This
! is currently a constant SHAPE implementation of the matrix.
!
! Author: Komahan Boopathy (komahan@gatech.edu)
!=====================================================================!

module sparse_matrix_dok

  use iso_fortran_env, only : WP => REAL64
  use sparse_matrix_interface, only : sparse_matrix
  
  implicit none
  
  private
  
  public :: dok_matrix
  
  ! Dictionary of keys sparse matrix
  type, extends(sparse_matrix) :: dok_matrix
     
     type(integer), allocatable :: keys(:,:)
     type(scalar) , allocatable :: vals(:)
     
   contains
     
     ! Implemented procedures
     procedure :: add_entry => add_sparse_entry
     procedure :: get_entry => get_sparse_entry

     ! Destructor
     final :: destroy     

  end type dok_matrix

  ! Interfaces to construct this matrix
  interface dok_matrix
     procedure construct_dok_from_empty
     procedure construct_dok_from_data
  end interface dok_matrix

  contains
    
    !=================================================================!
    ! Initializes an instance of sparse matrix
    !=================================================================!
    
    pure type(dok_matrix) function construct_dok_from_empty(nnz) &
         & result(this)

      type(integer), intent(in) :: nnz

      ! Set the number of values to store
      call this % set_num_nonzeros(nnz)

      ! Allocate space
      allocate(this % keys(2,this % get_num_nonzeros()))
      allocate(this % vals(this % get_num_nonzeros()))

      ! Zero the entries
      this % vals = 0.0_WP
      this % keys = -1

    end function construct_dok_from_empty

    !=================================================================!
    ! Initializes an instance of sparse matrix from supplied data
    !=================================================================!
    
    pure type(dok_matrix) function construct_dok_from_data(rows, cols, vals) &
         & result(this)

      type(integer), intent(in) :: rows(:)
      type(integer), intent(in) :: cols(:)
      type(scalar), intent(in)  :: vals(:)
      
!!$    ! Bounds and values are copied
!!$    allocate(this % vals, source = mat) ! mold = mat copies just the bounds
!!$
!!$    ! Set matrix dimensions before return
!!$    call this % set_row_size( this % get_row_size() )
!!$    call this % set_col_size( this % get_col_size() )
      
      associate( size_vals => size(vals), &
           size_rows => size(rows), &
           size_cols => size(cols) )
        
        if ( size_vals .eq. size_cols .and. &
             size_vals .eq. size_rows ) then
           
           ! Set matrix dimensions before return
           call this % set_num_nonzeros( size_vals )
           
        end if
        
      end associate
      
      ! Allocate space
      allocate(this % keys(2, this % get_num_nonzeros()))
      allocate(this % vals(this % get_num_nonzeros()))

      ! Copy the data and nonzero indices
      this % keys(1,:) = rows
      this % keys(2,:) = cols
      this % vals = vals

      ! Sort the keys, may be?
      
    end function construct_dok_from_data
    
    !=================================================================!
    ! Destructor for the matrix
    !=================================================================!
    
    subroutine destroy(this)

      type(dok_matrix), intent(inout) :: this
      
      ! Free up allocated memory in heap
      if (allocated(this % vals)) then
         deallocate(this % vals)
      end if
     
      if (allocated(this % keys)) then
         deallocate(this % keys)
      end if

      print *, "Destructing sparse matrix!"

    end subroutine destroy

    !=================================================================!
    ! Adding an entry to a sparse matrix
    !=================================================================!

    pure subroutine add_sparse_entry(this, row, col, val)
      
      class(dok_matrix), intent(inout) :: this
      type(integer), intent(in) :: row
      type(integer), intent(in) :: col
      type(scalar), intent(in)  :: val

      ! print *, 'Added values to sparse matrix!'

    end subroutine add_sparse_entry

    !=================================================================!
    ! Fetch the entry corresponding to the row and column
    !=================================================================!
    
    pure type(scalar) function get_sparse_entry(this, row, col) result(val)
      
      class(dok_matrix), intent(in) :: this
      type(integer), intent(in) :: row
      type(integer), intent(in) :: col
            
    end function get_sparse_entry
    
  end module sparse_matrix_dok
