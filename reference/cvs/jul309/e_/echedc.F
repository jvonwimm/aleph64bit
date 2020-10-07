      SUBROUTINE ECHEDC
C----------------------------------------------------------------------
C!  - Create ECHE bank to transmit calibration used to POT
C!    Create EHGF bank to transmit calibaration to POT
C!                Author  M.N Minard
C!                        5/06/90
C!                Mod    16/09/91
C!     Bank created ECHE , EHGF
C!     Bank used ECCA  , ECGF
C!     Called from EINIRU
C?
C!======================================================================
#ifndef DOC
#include "eccajj.h"
#include "echejj.h"
#include "ehgfjj.h"
#include "ecgfjj.h"
#include "echica.h"
#include "ecmcjj.h"
#include "bcs.h"
      DIMENSION ECCAV(2,144),ECCAT(2,144),COLDGT(36)
      EXTERNAL NAMIND
      DATA NMOD / 36 /
#include "bmacro.h"
C
C-     Create ECHE bank to be written as a header
C
      NAECCA = NAMIND('ECCA')
      KECCA = IW(NAECCA)
      KREPR = IW(NAMIND('REPR'))
      KECHE = IW(NAMIND('ECHE'))
      KEHGF = IW(NAMIND('EHGF'))
C
C-     Control banks length
C
      NECCA = 0
      IF ( KECCA.NE.0) NECCA = LROWS(KECCA)
      NBUNCH = (NECCA-1)/NMOD + 1
C
C-     Set calibration to 1 if ECCA does not exist or incorrect
C
      IF ( KECCA.EQ.0.OR.NECCA.NE.NMOD*NBUNCH) THEN
        DO 10 IL = 1, NMOD*NBUNCH
          DO 10 IK = 1, 2
            ECCAV ( IK , IL ) = 1.
            ECCAT ( IK , IL ) = 1.
   10   CONTINUE
      ELSE
        CALL UCOPY ( RW(KECCA+LMHLEN+1),ECCAV(1,1),NBUNCH*NMOD*2)
        CALL UCOPY ( RW(KECCA+LMHLEN+1),ECCAT(1,1),NBUNCH*NMOD*2)
      ENDIF
      IF ( KREPR.NE.0.AND.KECHE.NE.0) THEN
        CALL UCOPY ( RW(KECHE+LMHLEN+1),ECCAV(1,1),NMOD*2*NBUNCH)
      ENDIF
      DO 51 IL =1,NMOD*NBUNCH
        COLDWR(IL) = ECCAV(2,IL)
        COLDPD(IL) = ECCAV(1,IL)
   51 CONTINUE
C
C-    Now update with ECMC bank
C
      NAECMC = NAMIND('ECMC')
      KECMC = IW ( NAECMC )
      NECMC = 0
      IF ( KECMC.NE.0) NECMC = LROWS (KECMC)
      IF ( NECMC.EQ.NMOD ) THEN
        DO 12 IL =1,NMOD
          COLDPL (IL) = RTABL(KECMC,IL,JECMCO)
   12   CONTINUE
      ELSE
        DO 22 IL =  1,  NMOD
          COLDPL (IL) = 1.
   22   CONTINUE
      ENDIF
C
C-     Built header bank
C
      LENG = LMHLEN + NMOD*LECHEA*NBUNCH
      CALL AUBOS ( 'ECHE', 0, LENG,KECHE,IGARB)
      IF ( IGARB.EQ.2 ) GO TO 900
      CALL BLIST (IW,'C+','ECHE')
      CALL UCOPY ( ECCAT(1,1),RW(KECHE+LMHLEN+1),LENG-LMHLEN)
      IW ( KECHE + LMHCOL ) = LECHEA
      IW ( KECHE + LMHROW ) = NMOD * NBUNCH
  900 CONTINUE
C
C-     Create EHGF bank to be written as a header
C
      NAECGF = NAMIND('ECGF')
      KECGF = IW(NAECGF)
C
C-     Control banks length
C
      NECGF = 0
      IF ( KECGF.NE.0) NECGF = LROWS(KECGF)
      IF ( KECGF.EQ.0.OR.NECGF.NE.NMOD) THEN
C
C-     Set calibration to 1
C
        DO 80 IL = 1, NMOD
          COLDGF ( IL ) = 0.
          COLDGT ( IL ) = 0.
   80   CONTINUE
        GO TO 90
      ENDIF
      IF ( KECGF.NE.0)
     &  CALL UCOPY ( RW(KECGF+LMHLEN+1),COLDGF(1),NMOD)
      IF ( KECGF.NE.0)
     &  CALL UCOPY ( RW(KECGF+LMHLEN+1),COLDGT(1),NMOD)
   90 CONTINUE
      IF ( KREPR.NE.0.AND.KEHGF.NE.0) THEN
        CALL UCOPY ( RW(KEHGF+LMHLEN+1),COLDGF(1),NMOD)
      ENDIF
C
C-     Built header bank
C
      IF(KECGF.NE.0) THEN
        LENG = LMHLEN + NMOD*LEHGFA
        CALL AUBOS ( 'EHGF', 0, LENG,KEHGF,IGARB)
        IF ( IGARB.EQ.2 ) GO TO 950
        CALL BLIST (IW,'C+','EHGF')
        CALL UCOPY ( COLDGT(1),RW(KEHGF+LMHLEN+1),LENG-LMHLEN)
        IW ( KEHGF + LMHCOL ) = LEHGFA
        IW ( KEHGF + LMHROW ) = NMOD
      ENDIF
  950 CONTINUE
      RETURN
      END
#endif
