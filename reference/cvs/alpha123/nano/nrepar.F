      SUBROUTINE NREPAR(IRET)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Gerrit Graefe         10-MAY-1994
C!
C!   Inputs:
C!        - none
C!
C!   Outputs:
C!        - IRET / I        return code  0=created PART from NDAR
C!                                       1=can't find NDAR
C!                                       2=no space to book PART
C!                                       3=unknown error
C!
C!   Libraries required: ALPHA,BOS77
C!
C!   Description
C!   ===========
C!
C?   This subroutine creates the bank PART from the NDAR bank.
C?
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
#include "partjj.h"
#include "nbpdcl.h"
#include "nbnkpo.h"
#include "ndarjj.h"
      INTEGER NLINK,NVERS,NAMIND,NDROP
      INTEGER INDNAR,INDPAR,IGARB,KOPART,IRET
      INTEGER IRONAR,KRNDAR,IROWN,IROWP
      INTEGER I,J,K,ITK1,ITK2,IEXPO
      INTEGER IBAR(4),ISTR,IEBCAR(0:255),ITST,IMETH
      CHARACTER*1 CBAR,CTST
      CHARACTER*4 CSTR
      EQUIVALENCE (ITST,CTST)
      EQUIVALENCE (ISTR,CSTR)
      INTEGER INTCHA
      INTEGER M4,M8,M10,M16,M24
      REAL    MAN,RES
      DATA M4,M8,M10,M16,M24/15,255,1023,65535,16777215/
      DATA IEBCAR/ 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     &             32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     &             32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     &             32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     &             32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     &             32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     &             32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     &             32, 32, 32, 32, 32, 46, 60, 40, 43,124,
     &             38, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     &             33, 36, 42, 41, 59, 94, 45, 32, 47, 32,
     &             32, 32, 32, 32, 32, 32, 32, 44, 37, 95,
     &             62, 63, 32, 32, 32, 32, 32, 32, 32, 32,
     &             32, 96, 58, 35, 64, 39, 61, 34, 32, 97,
     &             98, 99,100,101,102,103,104,105, 32, 32,
     &             32, 32, 32, 32, 32,106,107,108,109,110,
     &            111,112,113,114, 32, 32, 32, 32, 32, 32,
     &             32,126,115,116,117,118,119,120,121,122,
     &             32, 32, 32, 91, 32, 32, 32, 32, 32, 32,
     &             32, 32, 32, 32, 32, 32, 32, 32, 32, 93,
     &             32, 32,123, 65, 66, 67, 68, 69, 70, 71,
     &             72, 73, 32, 32, 32, 32, 32, 32,125, 74,
     &             75, 76, 77, 78, 79, 80, 81, 82, 32, 32,
     &             32, 32, 32, 32, 92, 32, 83, 84, 85, 86,
     &             87, 88, 89, 90, 32, 32, 32, 32, 32, 32,
     &             48, 49, 50, 51, 52, 53, 54, 55, 56, 57,
     &             32, 32, 32, 32, 32, 32/
#include "qmacro.h"
C----------------------------------------------------------------------
      CALL BLIST(IW,'R-','PART')
      I=NDROP('PART',0)
C
C!..LINK TO BANK NDAR
C
      IRET=3
      INDNAR=IW(NAMIND('NDAR'))
      IF(INDNAR.EQ.0)THEN
        IRET=1
        RETURN
      ENDIF
      KONDAR=INDNAR
      KCNDAR=IW(KONDAR+1)
      KRNDAR=IW(KONDAR+2)
C
C!..CREATE BANK PART
C
      CALL AUBOS('PART',0,LMHLEN+LPARTA*KRNDAR,INDPAR,IGARB)
      IF(IGARB.EQ.2)THEN
        CALL QWMESE('### NREPAR ### No space to book PART')
        IRET=2
        RETURN
      ENDIF
      CALL BKFMT('PART','2I,(I,3A,I,4F,I)')
      NAPART=NAMIND('PART')
      KOPART=IW(NAPART)
      IW(KOPART+LMHCOL)=LPARTA
      IW(KOPART+LMHROW)=KRNDAR
C
C!..FILL BANK PART
C
      IROWN=KONDAR+LMHLEN
      IROWP=KOPART+LMHLEN
C
C!..FIRST FIND OUT HOW TO HANDLE CHARACTER VARIABLES
C
      ITST=135
      IF(CHAR(ITST).NE.'g')ITST=103
      IMETH=0
      DO 200 J=1,4
        IBAR(J)=IAND(ISHFT(IW(IROWN+JNDANA),-8*(J-1)),M8)
  200 CONTINUE
      DO 210 J=1,4
        IF(IBAR(J).EQ.103)IMETH=1
  210 CONTINUE
C
C!..CHARACTERS ARE IN EBCDIC (CORRECT ON IBM ONLY)
C
      IF(IMETH.EQ.0)THEN
        IF(ITST.EQ.135)THEN
C!..EBCDIC WANTED
          DO 220 J=1,4
            IF(IBAR(J).EQ.135)IMETH=1
  220     CONTINUE
        ELSE
C!..ASCII WANTED
          DO 230 J=1,4
            IBAR(J)=IEBCAR(IBAR(J))
            IF(IBAR(J).EQ.103)IMETH=-1
  230     CONTINUE
        ENDIF
      ELSE
C!..ASCII FOUND
        IF(ITST.EQ.135)THEN
C1....BUT EBCDIC WANTED
        WRITE(KUPRNT,*)
     &    '_NREPAR_:Sorry, there is no conversion from ASCII to EBCDIC'
        WRITE(KUPRNT,*)
     &    'foreseen. '
        IMETH=0
        ENDIF
      ENDIF
      IF(IMETH.EQ.0)THEN
        WRITE(KUPRNT,*)
     &    '_NREPAR_:Something odd happend while reading particle names.'
        WRITE(KUPRNT,*)
     &    'Will continue execution but the results may unpredictable if'
        WRITE(KUPRNT,*)
     &    'particle names are requested by the program.'
        IMETH=1
      ENDIF
C!..NOW LOOK FOR BYTE SWAPPING
      ISTR=IBAR(1)+ISHFT(IBAR(2),8)+ISHFT(IBAR(3),16)+ISHFT(IBAR(4),24)
      IF(CSTR.EQ.'gamm')IMETH=1*IMETH
      IF(CSTR.EQ.'agmm')IMETH=2*IMETH
      IF(CSTR.EQ.'mmga')IMETH=3*IMETH
      IF(CSTR.EQ.'mmag')IMETH=4*IMETH
      DO 100 I=1,KRNDAR
C!..GEANT NUMBER
        IW(IROWP+JPARGN)=IAND(IW(IROWN+JNDAPA),M8)
C!..ALPHA CODE
        IW(IROWP+JPARCO)=IAND(ISHFT(IW(IROWN+JNDAPA),-8),M8)
C!..CHARGE
        RW(IROWP+JPARCH)=(FLOAT(IAND(ISHFT(IW(IROWN+JNDAPA),-16),M4))
     &     -6.0)/3.0
C!..ANTI PARTICLE
        IW(IROWP+JPARAN)=IAND(ISHFT(IW(IROWN+JNDAPA),-20),M10)
C!..NAME
        DO 130 K=0,2
          IW(IROWP+JPARNA+K)=0
          DO 120 J=1,4
            IBAR(J)=IAND(ISHFT(IW(IROWN+JNDANA+K),-8*(J-1)),M8)
            IF(IMETH.LT.0)IBAR(J)=IEBCAR(IBAR(J))
  120     CONTINUE
          IF(ABS(IMETH).EQ.1)THEN
            IW(IROWP+JPARNA+K)=IBAR(1)+ISHFT(IBAR(2),8)+
     &                         ISHFT(IBAR(3),16)+ISHFT(IBAR(4),24)
          ELSEIF(ABS(IMETH).EQ.2)THEN
            IW(IROWP+JPARNA+K)=IBAR(2)+ISHFT(IBAR(1),8)+
     &                         ISHFT(IBAR(4),16)+ISHFT(IBAR(3),24)
          ELSEIF(ABS(IMETH).EQ.3)THEN
            IW(IROWP+JPARNA+K)=IBAR(3)+ISHFT(IBAR(4),8)+
     &                         ISHFT(IBAR(1),16)+ISHFT(IBAR(2),24)
          ELSEIF(ABS(IMETH).EQ.4)THEN
            IW(IROWP+JPARNA+K)=IBAR(4)+ISHFT(IBAR(3),8)+
     &                         ISHFT(IBAR(2),16)+ISHFT(IBAR(1),24)
          ENDIF
  130   CONTINUE
C!..MASS
        MAN=FLOAT(IAND(ISHFT(IW(IROWN+JNDAMA),-8),M24))
        IEXPO=IAND(IW(IROWN+JNDAMA),M8)
        MAN=(MAN-1000000.0)/1000000.0
        IEXPO=(IEXPO-40)
        CALL NGPUT(MAN,IEXPO,RES)
        RW(IROWP+JPARMA)=RES
C!..LIFETIME
        MAN=FLOAT(IAND(ISHFT(IW(IROWN+JNDALT),-8),M24))
        IEXPO=IAND(IW(IROWN+JNDALT),M8)
        MAN=(MAN-1000000.0)/1000000.0
        IEXPO=(IEXPO-40)
        CALL NGPUT(MAN,IEXPO,RES)
        RW(IROWP+JPARLT)=RES
C!..WIDTH
        MAN=FLOAT(IAND(ISHFT(IW(IROWN+JNDAMW),-8),M24))
        IEXPO=IAND(IW(IROWN+JNDAMW),M8)
        MAN=(MAN-1000000.0)/1000000.0
        IEXPO=(IEXPO-40)
        CALL NGPUT(MAN,IEXPO,RES)
        RW(IROWP+JPARMW)=RES
        IROWN=IROWN+LNDARA
        IROWP=IROWP+LPARTA
  100 CONTINUE
      CALL BLIST(IW,'R+','PART')
      IRET=0
  999 RETURN
      END
#endif
