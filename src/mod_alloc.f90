module mod_alloc
   implicit none

   private
   public :: alloc_arr, free_arr

contains

   subroutine alloc_arr(arr, arrsize)
      real, allocatable, intent(in out) :: arr(:)
      integer, intent(in) :: arrsize
      integer :: stat
      character(100) :: errmsg

      if (allocated(arr)) call free_arr(arr)

      allocate (arr(arrsize), stat=stat, errmsg=errmsg)
      if (stat > 0) error stop errmsg
   end subroutine alloc_arr

   subroutine free_arr(arr)
      real, allocatable, intent(in out) :: arr(:)
      integer :: stat
      character(100) :: errmsg

      if (.not. allocated(arr)) return

      deallocate (arr, stat=stat, errmsg=errmsg)
      if (stat > 0) error stop errmsg
   end subroutine free_arr

end module mod_alloc
