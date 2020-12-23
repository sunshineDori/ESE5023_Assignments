!Author:SUNTAOTAO
!Date:20201218
!I got inspired by reading section14 and SolarCalcs.pdf.
!Calculate the apparent solar time (AST) in a certain location for a certain date and time.

module AST

implicit none

   real, parameter :: pi = 3.1415926536
   !integer 			  :: N
   
contains      

   subroutine A_solar_time(N,Long,LST,ASTcal)
   implicit none
   integer, intent(in)  :: N
   real, intent(in)  :: Long,LST
   integer, intent(out) :: ASTcal
   real  			 :: D,ET,LSTM
	D = (360*(N-81.)/365)*pi/180
	ET = 9.87*sin(2*D) - 7.53*cos(D) - 1.5*sin(D)
	LSTM = 15*(nint(Long/15))
	ASTcal = LST + 4*(LSTM - Long) + ET
    print*, "AST is ", ASTcal          
   end subroutine A_solar_time  
   
end module AST
