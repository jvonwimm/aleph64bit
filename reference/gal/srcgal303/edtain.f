      SUBROUTINE EDTAIN(JINF,JNB)
C------------------------------------------------------------------
C    G.de BOUARD   4-OCT-87
C! Initialisation of constants for routine EDTADJ
C
C. - called from EDTADJ                              this .HLB
C------------------------------------------------------------------
      SAVE
      PARAMETER (LSTCK=3,LPHI=384,LTHET=228)
      PARAMETER (LWPLA=45,LMODU=12,LCOMP=3)
      PARAMETER (NTHSG=12,NPHSG=24)
      DIMENSION JINF(3,3),JNB(3,3)
C
      JINF(1,1) = 0
      JINF(2,1) = 0
      JINF(3,1) = 0
      JINF(1,2) = 0
      JINF(2,2) = 2
      JINF(3,2) = 0
      JINF(1,3) = 0
      JINF(2,3) = 2
      JINF(3,3) = 3
C
      JNB(1,1) = 4
      JNB(2,1) = 0
      JNB(3,1) = 0
      JNB(1,2) = 3
      JNB(2,2) = 3
      JNB(3,2) = 0
      JNB(1,3) = 3
      JNB(2,3) = 2
      JNB(3,3) = 3
C
      RETURN
      END
