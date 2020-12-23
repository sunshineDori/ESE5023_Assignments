!Author:SUNTAOTAO
!Date:20201218
!I got inspired by reading section14 and SolarCalcs.pdf.
!https://www.it1352.com/2036813.html
!Print the SZA for Shenzhen (22.542883N, 114.062996E) 
!at 14:35 (Beijing time; UTC+8) on 2020-12-20.


program Main

use AST
use Declination_angle

implicit none

!real(4), parameter :: pi = 3.1415926536
real			   :: A, Long, LST, H, Lat, SZA
integer :: N, ASTcal


!2.1Calculate the declination angle on 12.20 of Shenzhen.
N=355
call Decl_angle(N,A)
write(*,*),"Angle is ",A

!2.2Calculate the AST in Shenzhen (22.542883N, 114.062996E) for 14:35 on 2020-12-20.
Long = 114.062996
!LST = 14:35,which change to minute is 14*60+35=875min
LST = 875
call A_solar_time(N,Long,LST,ASTcal)
!Change minute to Hour:minute
write(*,*),"AST is ",floor(ASTcal/60.),":",mod(ASTcal,60)

!2.3Print the SZA in a certain location for a certain date and time.
H = (ASTcal -720)/4
Lat = 22.542883
SZA = ACOS(cos(Lat*pi/180)*cos(A*pi/180)*cos(H*pi/180) +sin(Lat*pi/180)*sin(A*pi/180)) *180/pi
write(*,*),"SZA is ",SZA

end program Main