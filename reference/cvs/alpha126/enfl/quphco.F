      SUBROUTINE QUPHCO(IER)
C----------------------------------------------------------------
C! Check PHCO quality
C  Input   HCAL banks
C  Output  common /CUPHCO/
C----------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "qhac.h"
#include "cuphco.h"
      PARAMETER( CUT1= .72, CUT2= .88, TOGEV= .18 )
      PARAMETER( KRU89= 4000, KRU90= 6000 )
      PARAMETER( XENTW= .15 )
      PARAMETER( TH12= .21 )
      LOGICAL FBIT,FPPOB
#include "qmacro.h"
C----------------------------------------------------------------
      IER= 0
C
C - Link to PHCO
      NHCO= 0
      IPHCO= IW(NAPHCO)
      IF( IPHCO.EQ.0 ) THEN
        IER= -1
        GOTO 999
      END IF
      NHCO= LROWS(IPHCO)
      IF(NHCO.GT.NPHMAX) THEN
        IER= -2
        GOTO 999
      END IF
C----- Initialization
      CALL QHNEWR
      DO 2 L1=1,NHCO
        IOKPH(L1)= 0
        ECOPH(L1)= 0.
        ENOPH(L1)= 0.
        IDPPH(L1)= 0
        NPLPH(L1)= 0
        N10PH(L1)= 0
        EDPPH(L1)= 0
2     CONTINUE
C
C - Link to PPOB
      IPPOB= IW(NAPPOB)
C
      CALL QUPHDP
C----- Loop over PHCOs
      DO 5 L1=1,NHCO
C------- Check noise bits
        IHTU= 0
        ITRI= 0
        ISPL= 0
        IFPP= 0
        FPPOB= .FALSE.
        INF= ITABL(IPHCO,L1,JPHCNF)
        DO 10 L2=0,4
          FBIT= BTEST(INF,L2)
          IF(FBIT) ISPL= 1
10      CONTINUE
        FBIT= BTEST(INF,5)
        IF(.NOT.FBIT) FPPOB= .TRUE.
        IF( FPPOB.AND.IPPOB.EQ.0 ) THEN
          IER= -3
          GOTO 999
        END IF
C
        ENE= RTABL(IPHCO,L1,JPHCEC)
        THE= RTABL(IPHCO,L1,JPHCTH)
C No splash rescue when krun .LT. 6000
        IF(KRUN.GE.KRU89.AND.KRUN.LT.KRU90) THEN
          IF(ISPL.EQ.1) THEN
            CALL QUTWMX(L1,NTWL1,IL1MX,EL1MX)
            IF(NTWL1.EQ.-1) THEN
              IER= 5
              RETURN
            END IF
            IF(EL1MX.LT.(XENTW*ENE)) THEN
              IOKPH(L1)= -2
              GOTO 5
            END IF
          END IF
        END IF
        ECOPH(L1)= ENE
        IF(ENE     .GT.(150.)) GOTO 5
        IF(ABS(THE).GT.(1.E6)) GOTO 5
        CTH= COS(THE)
        IF(FPPOB) THEN
          IFPL= ITABL(IPPOB,IDPPH(L1),JPPOFP)
          XE= ENE-EDPPH(L1)
C         EMAX= 1.9*(XE+30.)
          EMIN= 1.12*XE
C         IF(ENE.LT.EMIN.OR.ENE.GT.EMAX) FPPOB= .FALSE.
          IF(ENE.LT.EMIN)                FPPOB= .FALSE.
          IF(NPLPH(L1).LE.2.AND.
     &       THE.GT.TH12.AND.THE.LT.(QQPI-TH12)) THEN
            IF(ISPL.EQ.1) THEN
              IOKPH(L1)= -1
              GOTO 5
            END IF
            XW= ABS(CTH)
            IF((XW.LT.CUT1.OR.XW.GT.CUT2).AND.IFPL.GE.4)
     &                                   FPPOB= .FALSE.
            IF(XW.GE.CUT1.AND.XW.LE.CUT2.AND.IFPL.GE.11)
     &                                   FPPOB= .FALSE.
          END IF
        END IF
        IF(FPPOB) IFPP=1
        IF(.NOT.FPPOB) THEN
          CALL QUPHPL(L1,NPLHTU,N10HTU,NHTHTU)
          IF(ENE.LT.(5.))      GOTO 5
          IF(NPLHTU.GT.0) THEN
            IF(NPLHTU.LT.3)    GOTO 5
            NHT0= INT(1.43*ENE)
            IF(NHTHTU.LT.NHT0) GOTO 5
            NPLPH(L1)= NPLHTU
            N10PH(L1)= N10HTU
            EDPPH(L1)= NHTHTU*TOGEV
            IHTU= 1
          ELSE
            CALL QUPHTR(L1,NPLTRI)
            IF(NPLTRI.LT.6)    GOTO 5
            NPLPH(L1)= NPLTRI
            ITRI= 1
          END IF
        END IF
        IOKPH(L1)= 1
        IF(IHTU.EQ.1) IOKPH(L1)= 2
        IF(ITRI.EQ.1) IOKPH(L1)= 3
5     CONTINUE
C
C----- Angular rescue
      DO 30 L1=1,NHCO
        IF(IOKPH(L1).NE.0) GOTO 30
        TH1= RTABL(IPHCO,L1,JPHCTH)
        PH1= RTABL(IPHCO,L1,JPHCPH)
        COS0= -1.
        ICL0= 0
        DO 31 L2=1,NHCO
          IF(IOKPH(L2).LE.0.OR.
     &       IOKPH(L2).EQ.4) GOTO 31
          TH2= RTABL(IPHCO,L2,JPHCTH)
          PH2= RTABL(IPHCO,L2,JPHCPH)
          C12= SIN(TH1)*SIN(TH2)*COS(PH1-PH2)
          C12= COS(TH1)*COS(TH2) + C12
          IF(C12.GT.COS0) THEN
            COS0= C12
            ICL0= L2
          END IF
31      CONTINUE
        IF(ICL0.GT.0.AND.COS0.GT.(.98)) THEN
          IOKPH(L1)= 4
          IDPPH(L1)= IDPPH(ICL0)
          NPLPH(L1)= NPLPH(ICL0)
          N10PH(L1)= N10PH(ICL0)
          EDPPH(L1)= EDPPH(ICL0)
        END IF
30    CONTINUE
C
C----- Look for noisy towers
      CALL QUHNOI(IERNO)
      IF(IERNO.NE.0) IER= IERNO
C
999   CONTINUE
      RETURN
      END
#endif
