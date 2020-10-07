*DK sadigi
      SUBROUTINE SADIGI
C-----------------------------------------------------------------------
C!    SATR digitization
C     called from ASDIGI at the end of the event
C     call histogram routine SAHIST
C
C     evaluate the full hit information in the SAHI hit bank
C     do oring for wires which are connected to the same TDC
C     produce the SADIgi bank which simulates real data:
C     go from                     var
C     SATR side     1 to  2       isid     (The two SATR Detectors)
C          wire     1 to 14       iwir
C          distance to wire
C     to the raw data information were for every wire set
C     one 32 bit containing in the upper 16 bit the coded TDC number
C     and in the lower 16 bit the TDC information is written to output
C     the TDC number is characterized by
C                                 var
C          crate    1 to  3       JCRAT
C          card     1 to 24       JCARD
C          #tdc     1 to 16       JTDCN
C     in writing, these numbers will be decreased by one to
C     to get the #tdc number stored in 4 bits
C                                     H.Burkhardt October 1986
C
C     sort raw data according to   - crate,   - card,   - tdc-number
C     in order to simulate the sequence of the electronics read-out
C                                     H. Meinhard, Jan.1988
C
C     the TDC crate number will not be decreased by one according to the
C     behaviour of the readout system. Correct the oring mode
C                                     H. Meinhard, March 1989
#ifndef DOC
      SAVE
#include "iocom.h"
#include "jobcom.h"
#include "jqcom.h"
#include "sanamc.h"
#include "satrco.h"
      PARAMETER(NLAY=9)
      DIMENSION MODEOR(NLAY)
C     Pedestal and drift velocity is fixed for the Monte Carlo
      PARAMETER(IPED=800,TDRI=800.)
C     SIGM is the spatial resolution of wire tubes (in cm)
      PARAMETER(SIGM=1.E-4)
C     KGR(sector,oring_mode): no of tdc card within group of 4 cards
C     LGR(sector,oring_mode): 1 if first ored sector, 2 if second
      PARAMETER (NSEC=8,NOR=3)
      INTEGER KGR(NSEC,NOR), LGR(NSEC,NOR)
      DATA KGR /1,1,2,2,3,3,4,4, 1,2,1,2,3,4,3,4, 1,2,2,1,3,4,4,3/
      DATA LGR /1,2,1,2,1,2,1,2, 1,1,2,2,1,1,2,2, 1,1,2,2,1,1,2,2/
      DATA MODEOR/21,22,23, 22,23,21, 23,21,22/
#include "bmacro.h"
      KSAHT=IW(NASAHT)
      IF(KSAHT.EQ.0) GOTO 900
      NHIT=LROWS(KSAHT)
      IF(NHIT.EQ.0) GOTO 900
      KSA=KSAHT+LMHLEN
C     make SADIgi bank
C     make sure no bank exits (for example read from INPUT)
      CALL BDROP(IW,'SADI')
C     book new bank, maximum length is length of SAHI bank
      CALL ALBOS ('SADI',0,IW(KSAHT),KSADI,IGARB)
      CALL BLIST(IW,'E+','SADI')
C     define the columns length
      IW(KSADI+1)=LCSADI
C     the bank will have NHIT rows ( number of wires set before oring )
      IW(KSADI+2)=NHIT
      KSD=KSADI+LMHLEN
C     store number of wires for summary
      NWIRSA=NHIT
      XWIRSA=XWIRSA+FLOAT(NWIRSA)
C     loop over all hits
        DO 10 I=1,NHIT
C         decode hit bank
          IDAT=IW(KSA+I)
          ISID=IBITS(IDAT,28,3)
          ILAY=IBITS(IDAT,24,4)
          ISEC=IBITS(IDAT,20,4)
          IWIR=IBITS(IDAT,16,4)
C         convert dist back into cm
          DIST=FLOAT(IBITS(IDAT, 0,16))/65536.
C         resolution
          IF(SIGM.GT.5.E-4) THEN
            CALL RANNOR(RANA,RANB)
            DIST=DIST+RANA*SIGM
          ENDIF
          NHLDSA(ILAY)=NHLDSA(ILAY)+1
C         oring (some wires connected to the same TDC)
          MODE=MODEOR(ILAY) - 20
          IF (IWIR .EQ. 1 .OR. IWIR .EQ. 2) THEN
            JTDCN = IWIR
          ELSE IF (IWIR .EQ. 3) THEN
            JTDCN = 2 + LGR(ISEC,MODE)
          ELSE IF (IWIR .EQ. 4) THEN
            JTDCN = 4 + LGR(ISEC,MODE)
          ELSE IF (IWIR .GE. 5) THEN
            JTDCN = IWIR + 2
          ENDIF
          JCRAT=MOD(ILAY-1,NCRASA)+1
          JCARD=(ISID-1)*NCARSA/2+((ILAY-1)/NCRASA)*NSECSA/2+
     +      KGR(ISEC,MODE)
C         use a simple linear drift-distance relation
          ITDCC=IPED-INT(DIST*TDRI)
C         decrease JCARD,JTDCN by one each and pack into one
C         word to simulated the readout format
          IW(KSD+I)=ISHFT(JCARD-1,16)+ISHFT(JCRAT,21)+
     &                ISHFT(JTDCN-1,24)+ITDCC
   10   CONTINUE
C     after oring keep only the hit which is the closest to the wire
C     the algorithm is the same as SAASIG + additional code
C     to sort the raw data to get them in the electronic readout
C     sequence
      DO 1 IT1=1,NHIT-1
        IF(IW(KSD+IT1).EQ.0) GOTO 1
        IAD1=IBITS(IW(KSD+IT1),16,16)
        JCRA1=IBITS(IAD1,5,3)
        JCAR1=IBITS(IAD1,0,5)
        JTDC1=IBITS(IAD1,8,4)
        DO 2 IT2=IT1+1,NHIT
          IF(IW(KSD+IT2).EQ.0) GOTO 2
          IAD2=IBITS(IW(KSD+IT2),16,16)
          JCRA2=IBITS(IAD2,5,3)
          JCAR2=IBITS(IAD2,0,5)
          JTDC2=IBITS(IAD2,8,4)
          IF(IAD1.EQ.IAD2) THEN
C           look for shorter dist, mark other hit for deletion
C           by putting the word to zero
            IF(IBITS(IW(KSD+IT1),0,16).GT.
     &         IBITS(IW(KSD+IT2),0,16)) THEN
              IW(KSD+IT2)=0
            ELSE
              IW(KSD+IT1)=0
              GOTO 1
            ENDIF
          ELSE
C           addresses different, look whether swapping is required
            IF(JCRA2.LT.JCRA1 .OR.
     &        JCRA2.EQ.JCRA1.AND.JCAR2.LT.JCAR1 .OR.
     &        JCRA2.EQ.JCRA1.AND.JCAR2.EQ.JCAR1.AND.JTDC2.LT.JTDC1)THEN
C             swap
              ISAVE=IW(KSD+IT1)
              IW(KSD+IT1)=IW(KSD+IT2)
              IW(KSD+IT2)=ISAVE
              IAD1=IAD2
              JCRA1=JCRA2
              JCAR1=JCAR2
              JTDC1=JTDC2
            ENDIF
          ENDIF
    2   CONTINUE
    1 CONTINUE
      IND=0
      DO 3 IT3=1,NHIT
C       copy, skip the hits to be deleted
        IF(IW(KSD+IT3).NE.0) THEN
          IND=IND+1
          IW(KSD+IND)=IW(KSD+IT3)
        ENDIF
    3 CONTINUE
      IF(IND.LT.NHIT) THEN
C       only change bank size if wires have been ored
C       number of rows    ( = number of wires set after oring )
        IW(KSADI+2)=IND
C       now reduce SADI bank to actual size
        CALL AUBPRS('SADI')
      ENDIF
C     if debug call print routine for SADI bank
      IF(FDEBJO.AND.IPRIJO(6).EQ.1) CALL SAPRDI
C     call histogramming now where all data about the event is known
      IF(FHISJO(6)) CALL SAHIST
  900 CONTINUE
      END
#endif
