!Author:SUNTAOTAO
!Date:20201218
!I got inspired by reading section14 and SolarCalcs.pdf.
!2.1Calculate the declination angle on a certain date..

module Declination_angle

implicit none

   !real, parameter :: pi = 3.1415926536
   !integer :: N
   
contains      

   subroutine Decl_angle(N,A)
	implicit none   
	integer, intent(in)  :: N
    real, intent(out)    :: A
	real 				 :: pi
	pi = 3.1415926536
	A=23.45*sin(((N+284.)*360/365)*pi/180) 
    print*, "A is", A          
   end subroutine Decl_angle 
   
end module Declination_angle
