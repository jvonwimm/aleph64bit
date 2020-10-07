      SUBROUTINE MINEIT
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill EIDT bank from DEID.
C
C     Author: Stephen Haywood      11-Oct-90
C     Modify: Agnieszka Jacholkowska 24-Oct-94
C
C     Logically this routine should be called MINEID - but this name
C     is already used for the routine to create DEID.
C     R5 is not calculated unless GETR5 is set true. This calculation
C     takes a long time, and is not yet done for the DST.
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "deidjj.h"
#include "dtrajj.h"
#include "eidtjj.h"
#include "minfac.h"
C
C++   KFRFT and KTEXS are initialised solely to remove compiler warning.
C
      SAVE FIRST,GETR5,R2MIN,R3MIN,R3MAX,EMASS
      LOGICAL FIRST,GETR5
      DATA FIRST,GETR5,KFRFT,KTEXS / .TRUE.,.FALSE.,2*0 /
      DATA R2MIN,R3MIN,R3MAX / -3.0,-2.4,+3.0 /
      DATA EMASS / 0.511E-03 /
C
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C++   Determine whether R5 should be calculated.
C
      IF(FIRST) THEN
         IF(NLINK('EIR5',0).GT.0) GETR5 = .TRUE.
         FIRST = .FALSE.
      ENDIF
C
      KDEID = NLINK('DEID',100)
      IF (KDEID.LE.0) THEN
         CALL MINUPD('DEID')
         KDEID = NLINK('DEID',100)
         IF (KDEID.LE.0) RETURN
      ENDIF
C
C++   Create EIDT bank.
C
      NEIDT = LROWS(KDEID)
      IF(NEIDT.LE.0) RETURN
      LEN = LMHLEN + LEIDTA * NEIDT
      CALL AUBOS('EIDT',0,LEN, KEIDT,IGARB)
      CALL BLIST(IW,'S+','EIDT')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDEID = NLINK('DEID',100)
      ENDIF
      IW(KEIDT+LMHCOL) = LEIDTA
      IW(KEIDT+LMHROW) = NEIDT
C
C++   Fill EIDT bank.
C
      DO 100 I=1,NEIDT
         IW(KROW(KEIDT,I)+JEIDIF) = ITABL(KDEID,I,JDEIQF)
         R2 = FLOAT(ITABL(KDEID,I,JDEIR2))/100.
         R3 = FLOAT(ITABL(KDEID,I,JDEIR3))/100.
         IF (R2.LT.20.475) THEN
            RW(KROW(KEIDT,I)+JEIDR2) = R2
         ELSE
            RW(KROW(KEIDT,I)+JEIDR2) = 1000.
         ENDIF
         IF (R3.LT.10.235) THEN
            RW(KROW(KEIDT,I)+JEIDR3) = R3
         ELSE
            RW(KROW(KEIDT,I)+JEIDR3) = 1000.
         ENDIF
         IPOTH = 0
         IF(R2.GE.0.) THEN
            IF(ABS(R3).LE.R3MAX) IPOTH = 1
         ELSE
            IF(R2**2+R3**2.LE.R3MAX**2) IPOTH = 1
         ENDIF
         IW(KROW(KEIDT,I)+JEIDIP) = IPOTH
         IW(KROW(KEIDT,I)+JEIDPE) = ITABL(KDEID,I,JDEIDE)
  100 CONTINUE
C
C++   Obtain dE/dx information and supply track link.
C
      IF(GETR5) THEN
         KFRFT = IW(NAMIND('FRFT'))
         KTEXS = IW(NAMIND('TEXS'))
         FIELD = ALFIEL(DUM)
      ENDIF
C
      DO 200 I=1,NEIDT
         IDTRA = ITABL(KDEID,I,JDEIDT)
         IW(KROW(KEIDT,I)+JEIDFR) = IDTRA
         IF(GETR5) THEN
            R5 = 1000.
            IF(KFRFT.GT.0 .AND. KTEXS.GT.0) THEN
               CALL TIDHYP(IDTRA,FIELD,1,0.000511,1.,
     &           RI,NS,TL,RIX,SIG,IER)
               IF(IER.EQ.0) R5 = (RI-RIX) / SIG
            ENDIF
            RW(KROW(KEIDT,I)+JEIDR5) = R5
         ENDIF
  200 CONTINUE
C
      RETURN
      END
#endif
