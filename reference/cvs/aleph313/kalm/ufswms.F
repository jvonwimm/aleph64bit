      SUBROUTINE UFSWMS(C_OLD, Q_PROC, C_NEW)
C!  Add multiple scattering to covariance matrix at origin
#ifndef DOC
      DOUBLE PRECISION C_OLD(*), Q_PROC(5,5), C_NEW(*)

      C_NEW(1)  = C_OLD(1)  + Q_PROC(1,1)
      C_NEW(2)  = C_OLD(2)  + Q_PROC(1,2)
      C_NEW(3)  = C_OLD(3)  + Q_PROC(2,2)
      C_NEW(4)  = C_OLD(4)  + Q_PROC(1,3)
      C_NEW(5)  = C_OLD(5)  + Q_PROC(2,3)
      C_NEW(6)  = C_OLD(6)  + Q_PROC(3,3)
      C_NEW(7)  = C_OLD(7)
      C_NEW(8)  = C_OLD(8)
      C_NEW(9)  = C_OLD(9)  + Q_PROC(3,4)
      C_NEW(10) = C_OLD(10) + Q_PROC(4,4)
      C_NEW(11) = C_OLD(11) + Q_PROC(1,5)
      C_NEW(12) = C_OLD(12) + Q_PROC(2,5)
      C_NEW(13) = C_OLD(13) + Q_PROC(3,5)
      C_NEW(14) = C_OLD(14) + Q_PROC(4,5)
      C_NEW(15) = C_OLD(15) + Q_PROC(5,5)
      DO I = 16, 21
        C_NEW(I) = C_OLD(I)
      ENDDO

      RETURN
      END
#endif
