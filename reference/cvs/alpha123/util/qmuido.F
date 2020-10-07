      SUBROUTINE QMUIDO(ITK,IRUN,IBE,IBT,IM1,IM2,NEXP,NFIR,N10,N03,
     &  XMULT,RAPP,ANG,ISHAD,SUDNT,IDF,IMCF,IER)
CKEY MUON /USER
C----------------------------------------------------------------------
C!  - Get the muon id information from the banks MCAD,HMAD,MUID
C!
C!    Author    - G.Taylor              15-MAY-1992
C!
C!    Inputs  :  ITRAC /I = Track number in FRFT bank
C!    Outputs :  IRUN  /I = (=0)
C!               IBE   /I = Expected HCAL bit map
C!               IBT   /I = Found HCAL bit map
C!               IM1   /I = num hits in first layer of muon chambers
C!               IM2   /I = num hits in first layer of muon chambers
C!               NEXP  /I = num fired planes expected in hcal
C!               NFIR  /I = num fired planes in hcal
C!               N10   /I = num fired planes in last 10 expected
C!               N03   /I = num fired planes in last 3 expected
C!               XMULT /R = excess digital multiplicity in last 10
C!                           hcal planes
C!               RAPP  /R = (dist/dcut) for closest muon chamber hit
C!               ANG   /R = angle between shadowing track and muon
C!                          chamber track segment
C!               ISHAD /R = shadowing flag (=0 if not shadowing,
C!                          otherwise is the JULIA track number which
C!                          it was shadowing)
C!               SUDNT /R = sum of hcal residuals
C!               IDF   /I = muon id flag (see bank MUID)
C!               IMCF /I = Type of muon considered
C!                     0 : ambigous or data
C!                     1 : primary b
C!                     2 : secondary c
C!                     3 : primary c
C!                     4 : b to tau
C!                     5 : other muon
C!                     6 : non decaying hadron or electron
C!                     7 : decay hadron
C!               IER   /I = (=0)
C!======================================================================
#ifndef DOC
#include "qcde.h"
      DATA MXDMP/1/,NDMP/0/
#include "qmacro.h"
C-----------------------------------------------------------------------
C Is this the mini or the (POT/DST)?
C
      KDMUO = NLINK('DMUO',0)
      IF (KDMUO.GT.0) THEN
C
C Call the mini interface routine for old minis
C
        IRUN = 0
        SUDNT = 0.
        RAPP = 0.
        IDF = 0
        IMCEF = 0
        IER = 0
        ANG=0.
        CALL MINQMU(KTN(ITK),IBE,IBT,IM1,IM2,NEXP,NFIR,N10,N03,XMULT,
     &              ISHAD,IER)
C MAKE THE ID
C
        MULT=NINT(XMULT*100.)
        CALL MMUIDF(NEXP,NFIR,IM1,IM2,N10,N03,MULT,ISHAD,IDF)
        IF (ISHAD.NE.0) ISHAD = ABS(ISHAD) + KFCHT - 1
      ELSE
       CALL AMUID(KTN(ITK),IRUN,IBE,IBT,IM1,IM2,NEXP,NFIR,N10,N03,
     &  XMULT,RAPP,ANG,JTK,SUDNT,IDF,IMCF,IER)
       ISHAD=0
       IF(JTK.NE.0) THEN
        DO 10 LTK=KFCHT,KLCHT
          IF(KTN(LTK).EQ.JTK) ISHAD=LTK
   10   CONTINUE
       ENDIF
       IF(KRUN.LE.2000) RETURN
C
C Get the MC code
C
       MCT = KBESTM(ITK)
       IF (MCT.NE.0) THEN
        IFL = KTPCOD(MCT)
        CALL KMLEPT(MCT,ITYP,JFLV)
        IF (IFL.NE.2.AND.IFL.NE.3) THEN
          IF (ITYP.EQ.6) THEN
            IMCF = 1
          ELSEIF (ITYP.EQ.7) THEN
            IMCF = 2
          ELSEIF (ITYP.EQ.8) THEN
            IMCF = 3
          ELSEIF (ITYP.EQ.9.AND.JFLV.EQ.5) THEN
            IMCF = 4
          ELSEIF (ITYP.EQ.2.OR.ITYP.EQ.3.OR.ITYP.EQ.10.OR.ITYP.EQ.12)
     &      THEN
            IMCF = 5
          ELSEIF (ITYP.EQ.1.OR.ITYP.EQ.11) THEN
            IMCF = 6
          ELSEIF (ITYP.EQ.4.OR.ITYP.EQ.5) THEN
            IMCF = 7
          ELSE
            IMCF = 0
          ENDIF
        ELSE
          IMCF = 6
        ENDIF
       ELSE
        IMCF = 0
       ENDIF
      ENDIF
      RETURN
      END
#endif
