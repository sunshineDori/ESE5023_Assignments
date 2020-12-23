!Author:SUNTAOTAO
!Date:20201218
!I got inspired by reading section14 and Assignment 01-2.

subroutine Matrix_multip(M,N,MN)
implicit none

        real(8),intent(in)::M(4,3),N(3,3)
        real(8),intent(out)::MN(4,3)
        real(8)::Mtemp
        INTEGER:: i,j,k
        ![a,k]=size(M)
        ![k,b]=size(N)
        do i=1,4
          do j=1,3
            Mtemp=0
            do k=1,3
                  Mtemp=Mtemp+M(i,k)*N(k,j)
            enddo
          MN(i,j)=Mtemp
          enddo
        enddo

		print*,'Matrix MN'
        do i=1,4
          print*,MN(i,:)
        enddo
end subroutine Matrix_multip