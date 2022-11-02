program stock_volatility

   use mod_io, only: read_stock, write_stock
   use mod_arrays, only: average, reverse, std_dev, &
                         moving_average, moving_std_dev

   implicit none

   character(len=4), allocatable :: symbols(:)
   character(len=:), allocatable :: time(:)
   real, allocatable :: open (:), high(:), low(:), close (:)
   real, allocatable :: adjclose(:), volume(:)
   real :: gain
   integer :: n, im

   symbols = ['AAPL', 'AMZN', 'CRAY', 'CSCO', 'HPQ ', &
              'IBM ', 'INTC', 'MSFT', 'NVDA', 'ORCL']

   do n = 1, size(symbols)
      ! print *, 'Working on '//symbols(n)
      call read_stock('data/'//trim(symbols(n))//'.csv', time, &
                      open, high, low, close, adjclose, volume)

      im = size(time)
      adjclose = reverse(adjclose)

      if (n == 1) then
         print *, time(im)//' through '//time(1)
         print *, 'Symbol, Average (USD), Volatility (USD), Relative Volatility (%)'
         print *, '----------------------------------------------------------------'
      end if

      print *, symbols(n), average(adjclose), std_dev(adjclose), &
         nint(std_dev(adjclose)/average(adjclose)*100)

      time = time(im:1:-1)

      call write_stock(trim(symbols(n))//'_volatility.txt', time, adjclose, &
                       moving_average(adjclose, 30), moving_std_dev(adjclose, 30))

   end do

end program stock_volatility

