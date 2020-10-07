      SUBROUTINE HCPRIND (ICODE)
C --------------------------------------------
CKEY PRINT HCAL PRINT / USER
C!    decode HLWD and HWDI and print them
C!
C!       Author      : F.Ranjard - 911121
C!
C        Input       :       ICODE  / I   = 1 print HLWD
C                                           2       HWDI
C                                           3       both
C!       Input bank  :       HWDI and HLWD
C!
C!      -Called by : user
C!      -Calls     : MVBITS  from CERNLIB
C-------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "hlwdjj.h"
#include "hwhtjj.h"
#include "hwdijj.h"
      PARAMETER (LHIT=3, LDIG=5)
      DIMENSION ITUPR(LDIG),LCLUP(LDIG),ILAPR(LDIG),MODPR(LDIG),
     +          YHIT(LHIT)
      DATA NAHLWD /0/
#include "bmacro.h"
C ----------------------------------------------
C
      IF (NAHLWD.EQ.0) THEN
         NAHLWD = NAMIND ('HLWD')
         NAHWDI = NAMIND ('HWDI')
      ENDIF
C
      LOUTIO = IW(6)
      IF(ICODE.NE.2 .AND. IW(NAHLWD).GT.0) THEN
         JHLWD = IW(NAHLWD)
         WRITE(IW(6),500) LROWS(JHLWD)
         NHLWD = LROWS(JHLWD)
         KHLWD = JHLWD + LMHLEN
         DO 30 I=1,LROWS(JHLWD),LHIT
            JMAX = MIN (LHIT,NHLWD-I+1)
            DO 20 J=1,JMAX
               N       = IW(KHLWD+1)
               ITUPR(J)= IBITS (N,0,8)
               ILAPR(J)= IBITS (N,16,8)
               MODPR(J)= IBITS (N,24,8)
               YHIT (J)= RW(KHLWD+2)
   20       KHLWD = KHLWD + LHLWDA
            WRITE (LOUTIO,510) I,I+LHIT-1,(MODPR(J),ILAPR(J),
     +      ITUPR(J),YHIT(J),J=1,JMAX)
   30    CONTINUE
      ENDIF
  500 FORMAT (/1X,'+++HCPRIND+++ HLWD hit position along the tube ',
     +         I5/ 8X,3(4X,'  Mo  La  Tu  position '))
  510 FORMAT (1X,I3,'-',I3,3(4X,3I4,F10.2,1X))
C
      IF(ICODE.GE.2 .AND. IW(NAHWDI).GT.0) THEN
         JHWDI = IW(NAHWDI)
         NHWDI = LROWS(JHWDI)
         WRITE(LOUTIO,600) NHWDI
         KHWDI = JHWDI + LMHLEN
         DO 50 I=1,NHWDI,LDIG
            JMAX = MIN (LDIG,NHWDI-I+1)
            DO 40 J=1,JMAX
               N = IW(KHWDI+1)
               ITUPR(J)= IBITS (N,0,8)
               LCLUP(J)= IBITS (N,8,8)
               ILAPR(J)= IBITS (N,16,8)
               MODPR(J)= IBITS (N,24,8)
   40       KHWDI = KHWDI + LHWDIA
            WRITE (LOUTIO,610) I,I+LDIG-1,(MODPR(J),ILAPR(J),ITUPR(J),
     &      LCLUP(J),J=1,JMAX)
   50    CONTINUE
      ENDIF
      RETURN
  600 FORMAT (/1X,'+++HCPRIND+++ HWDI McTubeDigitising ',I5/ 8X,5(4X,
     +'  Mo  La  Tu  Cl'))
  610 FORMAT (1X,I3,'-',I3,5(4X,4I4))
      END
#endif
