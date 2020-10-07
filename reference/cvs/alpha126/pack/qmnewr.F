      SUBROUTINE QMNEWR (IROLD,IRNEW)
CKEY RUN /INTERNAL
C----------------------------------------------------------------------
C! called for new run
C! called from QMREAD,QMTERM
C!                                         Author: H.Albrecht 20.09.88
C!                                       Modified: E.Blucher  03.08.90
C!                                       Modified: J.Boucrot  03.05.93
C!                                       Modified: G.Graefe   10.02.95
C!                                       Modified: S.Wasserb. 10.05.95
C!                                       Modified: J.Boucrot. 24.09.98
C!                                       Modified: J.Boucrot. 08.06.00
C----------------------------------------------------------------------
#ifndef DOC
      SAVE IRPAST,IMD93
#include "qcdesh.h"
#include "jsumjj.h"
#include "kjobjj.h"
#include "rhahjj.h"
#include "rlepjj.h"
      COMMON / SCALMO / ISCP93,SCPF93
      COMMON / MINIRAM / QINLUMR,KNBHAMR
      CHARACTER CHAINT*4,PROGN*4
      REAL XYZ(3),DXYZ(3)
      INTEGER IRPAST(3)
      DATA IRPAST / 0, 0, 0 /
      DATA IFR93,ILR93,IMIVR,IAL94 / 20000 , 23546 , 90 , 157  /
      DATA IMD93 ,SCP93 / 0 , 0.996433 /
      DATA ILRMC,ILRLC,IFRL2 / 2000, 16500 , 40000 /
#include "bmacro.h"
C----------------------------------------------------------------------
C
      KRUN = IRNEW
      IF (XCOPYJ)  GO TO 90
      CALL QMCLR 
      IF (IRNEW .EQ. 0)  GO TO 80
      IF (KDEBUG .GT. 0)  CALL PRRHAH
      call qrksec 
C
C       ext. particle table
C
      LPART = IW(NAPART)
      IF (KRUN.LE.ILRMC) THEN
         IF (LPART.EQ.0) THEN
            IND=MDARD(IW,KUCONS,'PART',0)
            LPART = IW(NAPART)
         ENDIF 
      ENDIF
      IF (LPART .EQ. 0)  GO TO 40
C
C       get number of standard particles in PART
C
      NAM1 = INTCHA ('last')
      NAM2 = INTCHA ('_st_')
      NAM3 = INTCHA ('part')
      IADR = LPART + LMHLEN
      DO 10 KNPAST=1,IW(LPART+2)
        IF (IW(IADR+JQPANA) .EQ. NAM1 .AND.
     &      IW(IADR+JQPANA+1) .EQ. NAM2 .AND.
     &      IW(IADR+JQPANA+2) .EQ. NAM3)  GO TO 20
   10 IADR = IADR + IW(LPART+1)
      KNPAST = 0
C
C       reset particle transformation table (MC <-> int. code)
C
   20 DO 30 I = KOQTRA+1, KOQTRA+IW(KOQTRA)
C         don't reset particles set by a PTRA card !
        IF (IAND (IW(KOQPBT+IW(I)), KBIT(5)) .EQ. 0)  IW(I) = 0
   30 CONTINUE
C
C       length of particle transformation table
C
      IF (IW(LPART+2) .GE. IW(KOQTRA))
     &  CALL QSBANK ('QTRA', IW(LPART+2))
C
C Fix the syntax of some neutral antiparticles as given by KINGAL
C to make them understandable by ALPHA
C
      CALL FIXPART
C------------------------------------------------------------------------------
C Number of events in the run : 
C
 40   JJSUM = IW(NAJSUM)
      KRINNE=0
      IF (JJSUM .NE. 0)  THEN
        KRINNE = IW(JJSUM+LMHLEN+JJSUVT)
      ENDIF
C Default values of energy and fill number at run start:
      CALL GETLEP(KRUN,IFOU,KRINLF,NV,QELEP,XYZ,DXYZ)
C Default values for beam position:
      QVXNOM=0.
      QVYNOM=0.
      QVZNOM=0.
      QVXNSG=1.
      QVYNSG=1.
      QVZNSG=4.
      CALL BDROP(IW,'JCAR')
C
C LCAL Luminosity for this run :
C
      CALL GETLUM(KRUN,IOKLU,KRINDQ,KRINNZ,KRINNB,QRINLU,BK,BT)
      IF (IOKLU.NE.0) GO TO 46
C Nothing found in the Database for LCAL Lumi :
      KRINDC = 0
      KRINDQ = 0
      QRINLN = 0.
      QRINLU = 0.
      KRINNZ = 0
      KRINNB = 0
      KRINBM = 0
      KRINFR = KRUN
      KRINLR = KRUN
C
C SICAL Luminosity for this run :
 46   CALL GETSLU(KRUN,IOKSI,KRSLLQ,KRSLNB,QRSLLU,QRSLBK,QRSLEW)
      IF (IOKSI.NE.0) GO TO 48
C Nothing found in the Database for SICAL Lumi :
      KRSLNB = 0
      KRSLLQ = 0
      QRSLLU = 0.
      QRSLBK = 0.
      QRSLEW = 0.
 48   CONTINUE
      XIOKLU=.FALSE.
      XIOKSI=.FALSE.
C Mini-Ramp lumi, if relevant:
      CALL QLMINRP(XLUMRMP,NBHMRMP)
C MCarlo runs :
      IF (KRUN.LT.2000) THEN
         IF (IOKSI.GT.0) XIOKSI=.TRUE.
         IF (IOKLU.GT.0.AND..NOT.XIOKSI) XIOKLU=.TRUE.
         GO TO 50
      ENDIF
C Runs for which LCAL is the reference luminometer :
C ( 1989 -> 1992 up to run 16500 ; then LEP 1.5 and LEP 2 runs )
      IF ((KRUN.GE.2000.AND.KRUN.LE.ILRLC).OR.KRUN.GE.IFRL2) THEN
         IF (IOKLU.NE.0) XIOKLU=.TRUE.
         IF (QRINLU.LE.0.) XIOKLU=.FALSE.
         IF (.NOT.XIOKLU)   WRITE ( KUPRNT , 1011 ) KRUN
         GO TO 50
      ENDIF
C Runs for which SICAL is the reference luminometer :
C ( 1992 after run 16500 -> LEP 1 runs of 1995 )
      IF (KRUN.GT.ILRLC.AND.KRUN.LT.IFRL2) THEN
         IF (IOKSI.NE.0) XIOKSI=.TRUE.
         IF (QRSLLU.LE.0.) XIOKSI=.FALSE.
         IF (.NOT.XIOKSI)   WRITE ( KUPRNT , 1021 ) KRUN
      ENDIF
C
C   Make luminosity, bhabha, and Z totals :
C   protect against errors causing QMNEWR to be called more than once
C   for the same run.  Totals are updated only if run number was not
C   one of the last three runs to be encountered.
C
 50   ISKIP=0
      DO 75 I = 1,3
        IF (KRUN.EQ.IRPAST(I)) ISKIP=1
   75 CONTINUE
      IF (ISKIP.EQ.0) THEN
         IRPAST(1) = IRPAST(2)
         IRPAST(2) = IRPAST(3)
         IRPAST(3) = KRUN
         KNHDRN = KNHDRN + KRINNZ
         IF (XIOKLU) THEN
            QINLUM = QINLUM + QRINLU
            KNBHAB = KNBHAB + KRINNB
         ENDIF
         IF (XIOKSI) THEN
            QSILUM = QSILUM + QRSLLU
            KSBHAB = KSBHAB + KRSLNB
         ENDIF
         IF (XLUMRMP.GT.0) THEN
            QINLUMR = QINLUMR + XLUMRMP 
            KNBHAMR = KNBHAMR + NBHMRMP
         ENDIF
      ENDIF
      IF (KRINLF.EQ.0) THEN
         JRLEP=IW(NAMIND('RLEP'))
         IF (JRLEP.GT.0) KRINLF=ITABL(JRLEP,1,JRLELF)
      ENDIF
C ------------------------------------------------------------------------------
C--- Get BOM constants
C
      CALL QBOMRU
C
   60 QMFLD = ALFIEL (QMFLD)
      IF (QMFLD .LT. -20. .OR. QMFLD .GT. 20.)  THEN
        WRITE (KUPRNT,1001)  QMFLD
        IF (KUPTER .NE. 0)  WRITE (KUPTER,1001)  QMFLD
      ENDIF
      QMFLDC = QMFLD * QQIRP
C
C For all MINIs of 1993 data made before a correct reprocessing in 1994
C with MINI  version 90 and ALEPHLIB version 15.6 and before ,
C all particle momenta must be scaled ( a wrong BFIELD was used ) :
C
      ISCP93 = 0
      SCPF93 = SCP93
      JRHAH=IW(NARHAH)
      IF (JRHAH.GT.0) THEN
         DO 77 IRH = LROWS(JRHAH),1,-1
            KRHAH = KROW(JRHAH,IRH)
            PROGN = CHAINT(IW(KRHAH+JRHAPN))
            IF (INDEX(PROGN,'MIN').EQ.0) GO TO 77
            XMINI = .TRUE. 
            IF (KRUN.LT.IFR93.OR.KRUN.GT.ILR93) GO TO 77
            IF (IW(KRHAH+JRHAPV).NE.IMIVR) GO TO 77
            IF (IW(KRHAH+JRHAAV).GE.IAL94) GO TO 77
            ISCP93 = 1
            GO TO 78
 77      CONTINUE
      ENDIF
 78   IF (ISCP93.EQ.1) THEN
         IMD93 = IMD93 + 1
         IF (IMD93.EQ.1) WRITE ( KUPRNT,1023 ) SCP93
      ENDIF
C -------------------------------------------------------------------
C
C   RUN initializations for some packages :
C
C   QMUIDO initialization
C
      IF (.NOT.XMINI) CALL QMUNEW
C
C   YTOP Initialization
C
      CALL YTOIRU(IRNEW,QMFLD)
C
C
C  QSELEP initialization
C
      CALL QLINIT
      CALL BLIST(IW,'C+','PLSC')
      CALL BLIST(IW,'R+','PLSC')
C
C Initialise the VDET geometry for the current run :
C ( but not for 1989/1990 Real Data ! )
C
      IF (KRUN.GT.2000.AND.KRUN.LE.9114) GO TO 79
      CALL VRDDAF(KUCONS,IRNEW,IFLAG)
      CALL VGRDAL(KUCONS,IRNEW,IFLAG)
 79   CONTINUE
C
C   NanoDST Initialization
C
      IF (XWNANO) CALL QNNEWR(IROLD,IRNEW)
C
C User's initializations for the current run :
C
   80 CALL MQUNEWR
      CALL QUNEWR (IROLD,IRNEW)
C----------------------------------------------------------------------
 1001 FORMAT ('0_QMNEWR_ Caution : Mag. field is',G14.4,
     +        '; set to 15.')
 1011 FORMAT ('0_QMNEWR_ Warning ! LCAL Luminosity NOT KNOWN ',
     +        ' for run :',I7)
 1021 FORMAT ('0_QMNEWR_ Warning ! SICAL Luminosity NOT KNOWN ',
     +        ' for run :',I7)
 1023 FORMAT ('0_QMNEWR_ 1993 data : Scale factor applied for all ',
     +        'charged tracks : ',F9.6)
   90 END
#endif
