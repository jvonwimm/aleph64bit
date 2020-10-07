        SUBROUTINE UCTGFT
C----------------------------------------------------------------
C! Change to new helix convention in bank T1FT
C
C   Subroutine that changes the T1FT bank from the TASSO
C   convention of parameters to the mathematical convention,
C   and puts in the covariance matrix instead of the inverse of it.
C   This is done by refitting the track with UFITMS, which gives
C   its output in the desired convention.
C
C   AUTHOR:  LLUIS GARRIDO   14/6/1988
C   Modified: R. Johnson     19/5/1991   (use T1FT instead of TGFT)
C
C   Called by TFITTK
C
C   CONVERSION:
C          R_TASSO=-R_MAT
C         D0_TASSO= SIGN(R_MAT)*D0_MAT
C----------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "t1ftjj.h"
#include "t1tljj.h"
C
      DIMENSION VV0(6),EE0(21),COV(21)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
       IF (FIRST) THEN
         FIRST=.FALSE.
         NT1FT=NAMIND('T1FT')
         NT1TL=NAMIND('T1TL')
         NT1CL=NAMIND('T1CL')
       ENDIF
C
       KT1FT=IW(NT1FT)
       IF (KT1FT.EQ.0) RETURN
       NTR=LROWS(KT1FT)
       IF(NTR.EQ.0) RETURN
       KT1TL=IW(NT1TL)
       KT1CL=IW(NT1CL)
       IF (KT1TL.EQ.0 .OR. KT1CL.EQ.0) THEN
         CALL RERROR('UCTGFT',1,'Missing T1TL and T1CL banks!')
         CALL BDROP(IW,'T1FTT1TLT1CL')
         RETURN
       ENDIF
       DO 1 J=1,NTR
         IT1FT=KROW(KT1FT,J)
         NOFF=ITABL(KT1TL,J,JT1TIO)
         NTPC=ITABL(KT1TL,J,JT1TN1)
         CALL UFITMS(NTPC,0,0,IW(KT1CL+LMHLEN+NOFF+1),LIST2,LIST3,2,1
     &        ,29.6,.027,VV0,EE0,COV,CHI2,IERR)
         DO 41 K=JT1FIR,JT1FZ0
           RW(IT1FT+K)=VV0(K)
   41    CONTINUE
         DO 42 K=1,15
           RW(IT1FT+JT1FEM-1+K)=COV(K)
   42    CONTINUE
         RW(IT1FT+JT1FCD)=CHI2
         IW(IT1FT+JT1FDF)=2*NTPC-5
    1  CONTINUE
       RETURN
       END
#endif
