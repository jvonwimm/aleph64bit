      SUBROUTINE TTRUNK
C
C-------------------------------------------------------------------
C! Fix TPC raw data for truncated sectors by making the hit list
C  agree with the number of digitizations.
C
C  Author:   R. Johnson    3-08-89
C  Modified: R. Johnson   21-01-91  include TRIR and TRDI banks
C  Modified: W. Wiedenmann 3-02-92  include TSIR and TSDI banks
C                                      "    TANF/TWNF banks
C
C-------------------------------------------------------------------
#ifndef DOC
C
#include "tparac.h"
#include "tpgpar.h"
#include "tmonit.h"
#include "tinfjj.h"
#include "tanfjj.h"
#include "twnfjj.h"
#include "bcs.h"
C
      LOGICAL FIRST
      CHARACTER TEXT*74,NAM1*4,NAM2*4
      DATA FIRST/.TRUE./
      LOGICAL FSMART
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTINF=NAMIND('TINF')
        NTANF=NAMIND('TANF')
        NTWNF=NAMIND('TWNF')
      ENDIF
C
C++   section with old TINF bank
C++   --------------------------
C
      KTINF=IW(NTINF)
  121 IF (KTINF.EQ.0) GO TO 151
        LENO=IW(KTINF+JTINOL)
        IF (LENO.NE.0) THEN
          NR=IW(KTINF-2)
          IF (NR.GT.LTSECT .AND. NR.LE.2*LTSECT) THEN
            ISLOT=NR-LTSECT
            WRITE(TEXT,128) ISLOT
  128       FORMAT('Wire data for TPC sector ',I2,
     &             ' HAS BEEN TRUNCATED.&',
     &             'Some hits will be missing.')
            CALL RERROR('TTRUNK',1,TEXT)
            NWTKTM(ISLOT)=NWTKTM(ISLOT)+1
          ELSEIF (NR.GT.0 .AND. NR.LE.LTSECT) THEN
            ISLOT=NR
            WRITE(TEXT,129) ISLOT
  129       FORMAT('Pad data for TPC sector ',I2
     &             ,' HAS BEEN TRUNCATED.&'
     &             ,'Some hits will be missing.')
            CALL RERROR('TTRUNK',2,TEXT)
            NPTKTM(ISLOT)=NPTKTM(ISLOT)+1
          ENDIF
          IF (NR.LE.LTSECT .AND. NR.GT.0) THEN
            KTPAD=NLINK('TPAD',ISLOT)
            KTPDI=NLINK('TPDI',ISLOT)
C
C++         If pad digitizations are truncated, then drop all hit
C++         words with missing digitizations.
C
            IF (KTPAD.GT.0) THEN
              IF (KTPDI.EQ.0) THEN
                IND=NDROP('TPAD',ISLOT)
              ELSE
                LENTP=IW(KTPAD)
                LENDG=IW(KTPDI)*32/LTPDNB
                IOFRB=2
                NSTOT=0
  132           IF (IOFRB.GT.LENTP) GO TO 149
                  NHITR=IW(KTPAD+IOFRB)
                  DO 137 IP=1,NHITR
                    NBUCK=IBITS(IW(KTPAD+IOFRB+IP),16,8)
                    IF (NSTOT+NBUCK.GT.LENDG) THEN
                      IF (IP.EQ.1) THEN
                        LENTP=IOFRB-2
                      ELSE
                        LENTP=IOFRB+IP-1
                        IW(KTPAD+IOFRB)=IP-1
                      ENDIF
                      GO TO 138
                    ELSE
                      NSTOT=NSTOT+NBUCK
                    ENDIF
  137             CONTINUE
  138             CONTINUE
                  IOFRB=IOFRB+NHITR+2
                  GO TO 132
  149           CONTINUE
                IF (LENTP.NE.IW(KTPAD)) THEN
                  CALL AUBOS('TPAD',ISLOT,LENTP,KTPAD,IGARB)
                ENDIF
              ENDIF
            ENDIF
          ELSEIF (NR.GT.LTSECT .AND. NR.LE.2*LTSECT) THEN
C
C++         If wire digitizations are truncated, then drop all hit
C++         words with missing digitizations.
C
            KTWIR=NLINK('TWIR',ISLOT)
            KTWDI=NLINK('TWDI',ISLOT)
            IF (KTWIR.EQ.0 .OR. KTWDI.EQ.0) THEN
              KTSIR=NLINK('TSIR',ISLOT)
              KTSDI=NLINK('TSDI',ISLOT)
              IF (KTSIR.NE.0 .OR. KTSDI.NE.0) THEN
                 KTWIR=NLINK('TSIR',ISLOT)
                 KTWDI=NLINK('TSDI',ISLOT)
                 NAM1='TSIR'
                 NAM2='TSDI'
                 FSMART=.TRUE.
              ELSE
                 KTWIR=NLINK('TRIR',ISLOT)
                 KTWDI=NLINK('TRDI',ISLOT)
                 NAM1='TRIR'
                 NAM2='TRDI'
                 FSMART=.FALSE.
              ENDIF
            ELSE
              NAM1='TWIR'
              NAM2='TWDI'
            ENDIF
            IF (KTWIR.GT.0) THEN
              IF (KTWDI.EQ.0) THEN
                IND=NDROP(NAM1,ISLOT)
              ELSE
                NSTOT=0
                LENTW=IW(KTWIR)
                LENDG=IW(KTWDI)*32/LTPDNB
                DO 180 I=1,LENTW
                  IWHTL=IW(KTWIR+I)
                  IF ((NAM1.EQ.'TRIR').OR.(NAM1.EQ.'TSIR')) THEN
                    IRFLG=IBITS(IWHTL,13,1)
                    IF (NAM1.EQ.'TSIR') THEN
                       IF (IBITS(IWHTL,14,1).NE.0) IRFLG=1
                       IF (IBITS(IWHTL,15,1).NE.0) IRFLG=1
                    ENDIF
                  ELSE
                    IRFLG=0
                  ENDIF
                  IF (IRFLG.EQ.0) THEN
                    NWSMP=IBITS(IWHTL,16,8)
                    IF (NSTOT+NWSMP.GT.LENDG) THEN
                      LENTW=I-1
                      IF (LENTW.EQ.0) THEN
                        IND=NDROP(NAM1,ISLOT)
                        IND=NDROP(NAM2,ISLOT)
                      ELSE
                        CALL AUBOS(NAM1,ISLOT,LENTW,KTWIR,IGARB)
                      ENDIF
                      GO TO 181
                    ENDIF
                    NSTOT=NSTOT+NWSMP
                  ENDIF
  180           CONTINUE
  181           CONTINUE
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        KTINF=IW(KTINF-1)
        GO TO 121
  151 CONTINUE
C
C
C++   section with new TANF bank
C++   --------------------------
C
      KTANF=IW(NTANF)
  221 IF (KTANF.EQ.0) GO TO 251
        LENO=IW(KTANF+JTANTW)
        IF (LENO.NE.0) THEN
          NR=IW(KTANF-2)
          ISLOT=NR
          WRITE(TEXT,229) ISLOT
  229     FORMAT('Pad data for TPC sector ',I2
     &          ,' HAS BEEN TRUNCATED.&'
     &          ,'Some hits will be missing.')
          CALL RERROR('TTRUNK',2,TEXT)
          NPTKTM(ISLOT)=NPTKTM(ISLOT)+1
            KTPAD=NLINK('TPAD',ISLOT)
            KTPDI=NLINK('TPDI',ISLOT)
C
C++         If pad digitizations are truncated, then drop all hit
C++         words with missing digitizations.
C
            IF (KTPAD.GT.0) THEN
              IF (KTPDI.EQ.0) THEN
                IND=NDROP('TPAD',ISLOT)
              ELSE
                LENTP=IW(KTPAD)
                LENDG=IW(KTPDI)*32/LTPDNB
                IOFRB=2
                NSTOT=0
  232           IF (IOFRB.GT.LENTP) GO TO 249
                  NHITR=IW(KTPAD+IOFRB)
                  DO 237 IP=1,NHITR
                    NBUCK=IBITS(IW(KTPAD+IOFRB+IP),16,8)
                    IF (NSTOT+NBUCK.GT.LENDG) THEN
                      IF (IP.EQ.1) THEN
                        LENTP=IOFRB-2
                      ELSE
                        LENTP=IOFRB+IP-1
                        IW(KTPAD+IOFRB)=IP-1
                      ENDIF
                      GO TO 238
                    ELSE
                      NSTOT=NSTOT+NBUCK
                    ENDIF
  237             CONTINUE
  238             CONTINUE
                  IOFRB=IOFRB+NHITR+2
                  GO TO 232
  249           CONTINUE
                IF (LENTP.NE.IW(KTPAD)) THEN
                  CALL AUBOS('TPAD',ISLOT,LENTP,KTPAD,IGARB)
                ENDIF
              ENDIF
            ENDIF
        ENDIF
        KTANF=IW(KTANF-1)
        GO TO 221
  251 CONTINUE
C
C
C++   section with new TWNF bank
C++   --------------------------
C
      KTWNF=IW(NTWNF)
  321 IF (KTWNF.EQ.0) GO TO 351
        LENO=IW(KTWNF+JTWNTW)
        IF (LENO.NE.0) THEN
          NR=IW(KTWNF-2)
          ISLOT=NR
          WRITE(TEXT,328) ISLOT
  328     FORMAT('Wire data for TPC sector ',I2,
     &           ' HAS BEEN TRUNCATED.&',
     &           'Some hits will be missing.')
          CALL RERROR('TTRUNK',1,TEXT)
          NWTKTM(ISLOT)=NWTKTM(ISLOT)+1
C
C++         If wire digitizations are truncated, then drop all hit
C++         words with missing digitizations.
C
            KTWIR=NLINK('TWIR',ISLOT)
            KTWDI=NLINK('TWDI',ISLOT)
            IF (KTWIR.EQ.0 .OR. KTWDI.EQ.0) THEN
              KTSIR=NLINK('TSIR',ISLOT)
              KTSDI=NLINK('TSDI',ISLOT)
              IF (KTSIR.NE.0 .OR. KTSDI.NE.0) THEN
                 KTWIR=NLINK('TSIR',ISLOT)
                 KTWDI=NLINK('TSDI',ISLOT)
                 NAM1='TSIR'
                 NAM2='TSDI'
                 FSMART=.TRUE.
              ELSE
                 KTWIR=NLINK('TRIR',ISLOT)
                 KTWDI=NLINK('TRDI',ISLOT)
                 NAM1='TRIR'
                 NAM2='TRDI'
                 FSMART=.FALSE.
              ENDIF
            ELSE
              NAM1='TWIR'
              NAM2='TWDI'
            ENDIF
            IF (KTWIR.GT.0) THEN
              IF (KTWDI.EQ.0) THEN
                IND=NDROP(NAM1,ISLOT)
              ELSE
                NSTOT=0
                LENTW=IW(KTWIR)
                LENDG=IW(KTWDI)*32/LTPDNB
                DO 380 I=1,LENTW
                  IWHTL=IW(KTWIR+I)
                  IF ((NAM1.EQ.'TRIR').OR.(NAM1.EQ.'TSIR')) THEN
                    IRFLG=IBITS(IWHTL,13,1)
                    IF (NAM1.EQ.'TSIR') THEN
                       IF (IBITS(IWHTL,14,1).NE.0) IRFLG=1
                       IF (IBITS(IWHTL,15,1).NE.0) IRFLG=1
                    ENDIF
                  ELSE
                    IRFLG=0
                  ENDIF
                  IF (IRFLG.EQ.0) THEN
                    NWSMP=IBITS(IWHTL,16,8)
                    IF (NSTOT+NWSMP.GT.LENDG) THEN
                      LENTW=I-1
                      IF (LENTW.EQ.0) THEN
                        IND=NDROP(NAM1,ISLOT)
                        IND=NDROP(NAM2,ISLOT)
                      ELSE
                        CALL AUBOS(NAM1,ISLOT,LENTW,KTWIR,IGARB)
                      ENDIF
                      GO TO 381
                    ENDIF
                    NSTOT=NSTOT+NWSMP
                  ENDIF
  380           CONTINUE
  381           CONTINUE
              ENDIF
            ENDIF
        ENDIF
        KTWNF=IW(KTWNF-1)
        GO TO 321
  351 CONTINUE
C
      RETURN
      END
#endif
