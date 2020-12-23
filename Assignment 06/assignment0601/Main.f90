!Author:SUNTAOTAO
!Date:20201218
!I got inspired by reading section14.
!LIYUAN explained to me that I need use transpose function for matrix calculation;
!And the data type of variables both in main and module should keep the same.
program Main

implicit none

integer  :: u, i
real(8)  :: M(3,4), N(3,3),Mtran(4,3),Ntran(3,3),MN(4,3),MNtran(3,4)

! File unit
u = 50
!Matrix size

! Open the M file
open(unit=u, file='M.dat', status='old')
! Read data line by line and pass the value to M
read(u,*) M
! Close the file
close(u)
! Display the values
Print*, 'Matrix M'
do i = 1,4
  write(*,*) M(:,i)
enddo

! Open the N file
open(unit=u, file='N.dat', status='old')
! Read data line by line and pass the value to M
read(u,*) N
! Close the file
close(u)
! Display the values
Print*, 'Matrix N'
do i = 1,3
  write(*,*) N(:,i)
enddo

Mtran = transpose(M)
Ntran = transpose(N)

call Matrix_multip(Mtran,Ntran,MN)

! Write the values to a new file, in a certain format
MNtran = transpose(MN)
open(unit=u, file='MN.dat', status='replace')
do i = 1,4
    write(u, '(f8.1,f8.1,f8.1)') MNtran(:,i)
enddo
close(u)

end program Main