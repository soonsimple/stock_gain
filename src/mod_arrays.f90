module mod_arrays
   ! Utility functions that operate on arrays.

   implicit none

   private

   public :: average, reverse, std_dev, moving_average, moving_std_dev, crosspos, crossneg

contains

   pure real function average(x)
      ! Returns an average of x
      real, intent(in) :: x(:)
      average = sum(x)/size(x)
   end function average

   pure function reverse(x)
      ! Reverses the order of elements of x
      real, intent(in) :: x(:)
      real :: reverse(size(x))
      reverse = x(size(x):1:-1)
   end function reverse

   pure real function std_dev(x)
      ! Returns the standard deviation of x.
      real, intent(in) :: x(:)
      std_dev = sqrt(average((x - average(x))**2))
   end function std_dev

   pure function moving_average(x, w) result(sma)
      ! Returns the simple moving average of x with one-sided window w.
      real, intent(in) :: x(:)
      integer, intent(in) :: w
      real :: sma(size(x))
      integer :: i, ii

      do i = 1, size(x)
         ii = max(i - w, 1)
         sma(i) = average(x(ii:i))
      end do

   end function moving_average

   pure function moving_std_dev(x, w) result(stdw)
      ! Returns the moving standard deviation of x with one-sided window w.
      real, intent(in) :: x(:)
      integer, intent(in) :: w
      real :: stdw(size(x))
      integer :: i, ii

      do i = 1, size(x)
         ii = max(i - w, 1)
         stdw(i) = std_dev(x(ii:i))
      end do

   end function moving_std_dev

   pure function crosspos(x, w) result(res)
      real, intent(in) :: x(:)
      integer, intent(in) :: w
      integer, allocatable :: res(:)
      real, allocatable :: xavg(:)
      logical, allocatable :: greater(:), smaller(:)
      integer :: i

      res = [(i, i=2, size(x))]

      xavg = moving_average(x, w)

      greater = x > xavg
      smaller = x < xavg

      res = pack(res, greater(2:) .and. smaller(:size(x) - 1))

   end function crosspos

   pure function crossneg(x, w) result(res)
      real, intent(in) :: x(:)
      integer, intent(in) :: w
      integer, allocatable :: res(:)
      real, allocatable :: xavg(:)
      logical, allocatable :: greater(:), smaller(:)
      integer :: i

      res = [(i, i=2, size(x))]

      xavg = moving_average(x, w)

      greater = x > xavg
      smaller = x < xavg

      res = pack(res, smaller(2:) .and. greater(:size(x) - 1))

   end function crossneg

end module mod_arrays

