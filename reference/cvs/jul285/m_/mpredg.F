         SUBROUTINE MPREDG
C----------------------------------------------------------------------
C!  - Steering routine for MUON hit data decoding with new data base
C!
C! P.Campana    890608
C! P.Campana    910530 Corr. for 2 layer
C! G.Bagliesi   961009 Ignore bits 8-11 in data from new Astros
C!
C! Input:  MUDG raw data bank
C! Output: MHIT ALEPH coord. hits bank
C!
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rflags.h"
#include "rlunit.h"
#include "rcurnt.h"
#include "mghpar.h"
#include "mhitjj.h"
#include "mujosu.h"
#include "mgnpar.h"
#include "mgdeca.h"
#include "mgeeca.h"
#include "mgdmda.h"
#include "mgemda.h"
#include "mgdbar.h"
#include "mgebar.h"
#include "mdebug.h"
C
         INTEGER IPAN(9,NHITMG),IEL(12)
         PARAMETER(NCLUMX=500)
C
         INTEGER IVSTRP(NCLUMX),IVCLUS(NCLUMX),IVPSEU(NCLUMX),
     +           IVASTR(NCLUMX)
         INTEGER MLBBA(NRTABA,NRBUBA-2),MUBBA(NRTABA,NRBUBA-2),
     +           MLBEC(NRTAEC,NRBUEC),MUBEC(NRTAEC,NRBUEC),
     +           MLBMA(NRTAMA,NRBUMA),MUBMA(NRTAMA,NRBUMA)
C
         REAL SLTCO(4),ALECO(3)
         CHARACTER*4 CHAINT,CHAR
         INTEGER NDIG(88,2,2),IOFFS(3)
         INTEGER ALGTDB
         CHARACTER LISTB*12
C
         DATA IOFFS/0,34,50/
         DATA IMES1,IMES2,IMES3,IMES4,IMES5/5*0/
         DATA ILOAD/0/
         DATA LISTB/'MBLGMELGMMLG'/
C
#include "bmacro.h"
C
C Load bus limits from data base or data cards
      IF(ILOAD.EQ.0) THEN
         JMBLG=IW(NAMIND('MBLG'))
         JMELG=IW(NAMIND('MELG'))
         JMMLG=IW(NAMIND('MMLG'))
         IF(JMBLG.EQ.0.OR.JMELG.EQ.0.OR.JMMLG.EQ.0) THEN
            IBOS=ALGTDB(LRGEOM,LISTB,IRUNRC)
            ILOAD=1
            IF(IBOS.EQ.0) THEN
             CALL RERROR('MPREDG',9,'No bus limits banks available')
             ILOAD=-1
            ENDIF
         ELSE
            ILOAD=1
         ENDIF
         JMBLG=IW(NAMIND('MBLG'))
         JMELG=IW(NAMIND('MELG'))
         JMMLG=IW(NAMIND('MMLG'))
         IF(JMBLG.NE.0) THEN
             DO 751 IB=1,NRTABA
             DO 751 JB=1,NRBUBA-2
             MLBBA(IB,JB)=ITABL(JMBLG,JB,4+(IB-1)*2)
 751         MUBBA(IB,JB)=ITABL(JMBLG,JB,4+(IB-1)*2+1)
         ENDIF
         IF(JMELG.NE.0) THEN
             DO 752 IE=1,NRTAEC
             DO 752 JE=1,NRBUEC
             MLBEC(IE,JE)=ITABL(JMELG,JE,4+(IE-1)*2)
 752         MUBEC(IE,JE)=ITABL(JMELG,JE,4+(IE-1)*2+1)
         ENDIF
         IF(JMMLG.NE.0) THEN
             DO 753 IM=1,NRTAMA
             DO 753 JM=1,NRBUMA
             MLBMA(IM,JM)=ITABL(JMMLG,JM,4+(IM-1)*2)
 753         MUBMA(IM,JM)=ITABL(JMMLG,JM,4+(IM-1)*2+1)
         ENDIF
C
      ENDIF
C
      IMPRIN=0
C =========================
      JMUDE=NLINK('MUDE',0)
      IF(JMUDE.NE.0) IMPRIN=3
C =========================
      IF (FDEBRF) IMPRIN = JDBDRF(JULMU)
         IHIST=JHISRF(JULMU)
C Takes data from MUDG
         JMUDG=NLINK('MUDG',0)
         IF(JMUDG.EQ.0) RETURN
         NCL=IW(JMUDG+2)
         NNMUDI=NNMUDI+1
         NCLTOT=NCLTOT+NCL
C! NCLMAX defined in MINIJO
         IF(NCL.GT.NCLMAX) NCLMAX=NCL
C         IF(NCL.GT.NCLUMX) THEN
C              NCL=NCLUMX
C               CALL RERROR('MPREDG',0,'Max # of clusters reached')
C         ENDIF
         NMHIT=0
C drop existing mu-banks if we are reading a POT (G. Capon 4-dec-1990)
         CALL BDROP(IW,'MHITMTHRMCAD')
C Creates MHIT bank
         LNHIT =  2+ NHITMG*LMHITG
         CALL AUBOS('MHIT',0,LNHIT,JMHIT,IGARB)
         IF (IGARB.EQ.1) THEN
            JMUDG=NLINK('MUDG',0)
         ENDIF
         IF (IGARB.EQ.2) THEN
            CALL RERROR('MPREDG',1,'Error creating MHIT bank')
            RETURN
         ENDIF
         KMHIT=JMHIT+2
C
C To debug Monte Carlo Data
C
         IF(IMPRIN.GT.2) THEN
         NAMI=NAMIND('IMPA')
         JIMPA=IW(NAMI)
         ND=IW(JIMPA+2)
         CHAR=CHAINT(IW(JIMPA+(ND-1)*8+3))
         IF(CHAR.EQ.'MUON') THEN
            XI=RTABL(JIMPA,ND,2)
            YI=RTABL(JIMPA,ND,3)
            ZI=RTABL(JIMPA,ND,4)
            WRITE(LDEBRL,*) 'IMPA COOR. ', XI,YI,ZI
            AI=RTABL(JIMPA,ND,5)
            BI=RTABL(JIMPA,ND,6)
            CI=RTABL(JIMPA,ND,7)
            RI=SQRT(XI**2+YI**2+ZI**2)
            TI=ATAN2(SQRT(XI**2+YI**2),ZI)
            IF(TI.LT.0.) TI=3.14159+TI
            TF=ATAN2(YI,XI)
            IF(TF.LT.0.) TF=2.*3.14159+TF
            WRITE(LDEBRL,*) 'IMPA POLA. ', RI,TI,TF
         ENDIF
         ENDIF
C
C Remove bad clusters
         JN=0
         JMUNO=IW(NAMIND('MUNO'))
         DO 21 J=1,NCL
         IWORD=ITABL(JMUDG,J,1)
C                        skip noisy addresses (stored in bank MUNO)
         IF(JMUNO.GT.0) THEN
            DO 22 NH=1,LROWS(JMUNO)
            INOIS=ITABL(JMUNO,NH,1)
            IF(INOIS.EQ.IWORD) GO TO 21
 22         CONTINUE
         ENDIF
         JS=IBITS(IWORD, 0,8)
         JC=IBITS(IWORD, 8,4)
         JB=IBITS(IWORD,16,8)
         JA=IBITS(IWORD,24,8)
C Check on cluster size (le.8)
         IF(JC.EQ.0.OR.JC.GE.8) GOTO 21
C Check on bus number
         IF(JB.GE.24.AND.IMES4.LT.100) THEN
               IMES4=IMES4+1
               CALL RERROR('MPREDG',6,' Bus # out of limit')
               WRITE(LDEBRL,*) ' in ASTROS # ',JA
               GOTO 21
         ENDIF
C
         JN=JN+1
         IF (JN.GT.NCLUMX) THEN
            CALL RERROR('MPREDG',0,' Max clust. no. reached')
            RETURN
         ENDIF
C *Here bus still start from 0
C ------------------------ Corrections
C Bad cabling in MA6B (in A162 bus#3 inverted with bus#14 only for strips
C 0-95) PLC 27.jan.93
         IF(IRUNRC.GT.2001.AND.IRUNRC.LT.17890) THEN
            JBB=JB
            IF(JA.EQ.162.AND.JB.EQ.3.AND.JS.LE.95)  JBB=14
            IF(JA.EQ.162.AND.JB.EQ.14.AND.JS.LE.95) JBB=3
            JB=JBB
         ENDIF
C Correction for bad cabling in AS. 150(*bus 0 inverted with bus 12)
CA.A 2-MAY 1992
         IF(IRUNRC.GT.10000.AND.IRUNRC.LT.14000) THEN
             JBB=JB
             IF(JA.EQ.150.AND.JB.EQ.0)  JBB=12
             IF(JA.EQ.150.AND.JB.EQ.12) JBB=0
             JB=JBB
         ENDIF
CA.A 2-MAY 1992
C Correction for bad cabling in AS. 126(*bus 7 invert. with bus 8,9-10)
         IF(IRUNRC.GT.14000.AND.IRUNRC.LT.14520) THEN
             JBB=JB
             IF(JA.EQ.126.AND.JB.EQ.7) JBB=8
             IF(JA.EQ.126.AND.JB.EQ.8) JBB=7
             IF(JA.EQ.126.AND.JB.EQ.9) JBB=10
             IF(JA.EQ.126.AND.JB.EQ.10)JBB=9
             JB=JBB
         ENDIF
C Correction for bad cabling in AS. 130(*bus 19 invert. with bus 20)
         IF(IRUNRC.GT.10000) THEN
             JBB=JB
             IF(JA.EQ.130.AND.JB.EQ.19) JBB=20
             IF(JA.EQ.130.AND.JB.EQ.20) JBB=19
             JB=JBB
         ENDIF
C Correction for strip mismatch in AS. 128 *bus 2-3
         IF(IRUNRC.GT.10000) THEN
            IF(JA.EQ.128.AND.(JB.EQ.2.OR.JB.EQ.3).AND.
     +        (JS.GE.120.AND.JS.LE.127)) JS=JS-8
         ENDIF
C Correction for bad cabling in AS. 128(*bus 21 invert. with bus 22)
         IF(IRUNRC.GT.10000) THEN
             JBB=JB
             IF(JA.EQ.128.AND.JB.EQ.21) JBB=22
             IF(JA.EQ.128.AND.JB.EQ.22) JBB=21
             JB=JBB
         ENDIF
C Correction for End Cap 2 B internal muon chamb.
CA.A 2-MAY 1992
         IF(IRUNRC.GT.10000.AND.IRUNRC.LT.14000) THEN
             JBB=JB
             JSS= JS
            IF(JA.EQ.150.AND.JB.EQ.3.AND.JS.GT.32)  THEN
                JBB=15
                JSS = JSS + 64
             ENDIF
             IF(JA.EQ.150.AND.JB.EQ.15.AND.JS.GT.96) THEN
                JBB=3
                JSS = JSS - 64
             ENDIF
             JB=JBB
             JS = JSS
         ENDIF
C PLC 25.5.94 Correction for bad cabling in AS. 142 (Barrel 11M int.)
C (*bus 0 invert. with bus 1)
         IF(IRUNRC.GT.25000.AND.IRUNRC.LE.25895) THEN
             JBB=JB
             IF(JA.EQ.142.AND.JB.EQ.0) JBB=1
             IF(JA.EQ.142.AND.JB.EQ.1) JBB=0
             JB=JBB
         ENDIF
C PLC 4.11.94 Correction for bad cabling in AS. 142 (Barrel 11M)
C (*y strips miscabled)
         IF(IRUNRC.GT.25000.AND.IRUNRC.LT.25796) THEN
             IF(JA.EQ.142.AND.(JB.GE.12.AND.JB.LE.15)) THEN
                IF(JS.GE.32.AND.JS.LE.63)      JSS=JS+96
                IF(JS.GE.64.AND.JS.LE.95)      JSS=JS-32
                IF(JS.GE.96.AND.JS.LE.127)     JSS=JS+64
                IF(JS.GE.128.AND.JS.LE.159)    JSS=JS-64
                IF(JS.GE.160.AND.JS.LE.191)    JSS=JS+32
                IF(JS.GE.192.AND.JS.LE.223)    JSS=JS-96
                JS=JSS
             ENDIF
         ENDIF
C -------------------------------------------- end corrections
C
         IVCLUS(JN)=JC
         IVSTRP(JN)=JS
         IVPSEU(JN)=JB
         IVASTR(JN)=JA
 21      CONTINUE
C Loop on good clusters (overwrites NCL)
         NCL=JN
         CALL VZERO(NDIG,88*4)
         DO 20 J=1,NCL
C
         DO 2002 KI=1,9
 2002    IPAN(KI,J)=0
C
         JSL=0
         JSUBC=0
         ICLOK=0
         JSTRP=IVSTRP(J)
         JCLUS=IVCLUS(J)
         JPSEU=IVPSEU(J)
         JASTR=IVASTR(J)
         JPSEU=JPSEU+1
         JBUS=JPSEU
C
C Look at subc. & type.
C Loop on barrel slots
         DO 101 I=1,NRSLBA
         IF(MBNAST(I).EQ.JASTR) THEN
             JTYAS=MBTAST(I)
             JSUBC=1
             GOTO 200
         ENDIF
 101     CONTINUE
C Loop on endcap slots
         DO 102 I=1,NRSLEC
         IF(MENAST(I).EQ.JASTR) THEN
              JTYAS=METAST(I)
              JTYBU=METYBU(JTYAS,JPSEU)
              IF(JTYBU.GT.0.AND.JTYBU.LE.4) THEN
                  JSL=I
              ELSE
                  JSL=I+4
                  JTYAS=METAST(JSL)
                  JTYBU=METYBU(JTYAS,JPSEU)
              ENDIF
              JSUBC=2
              GOTO 200
         ENDIF
 102     CONTINUE
C Loop on middle angle slots
         DO 103 I=1,NRSLMA
         IF(JASTR.EQ.MANAST(I)) THEN
             JSUBC=3
             JTYAS=MATAST(I)
             JTYBU=MATYBU(JTYAS,JPSEU)
             JSL=MAOFBU(JTYAS,JPSEU)+MAOAST(I)
             GOTO 200
         ENDIF
 103     CONTINUE
C Check on subc.
 200     IF(JSUBC.EQ.0) THEN
             IMES1=IMES1+1
             IF(IMES1.LE.100.AND.IMPRIN.GT.1)
     +       WRITE(LDEBRL,*) ' Unidentified Astros # ',JASTR,
     +       ' in cluster #' ,J
             CALL RERROR('MPREDG',4,'unidentified astros')
             GOTO 20
         ENDIF
         IF(JSUBC-2) 201,202,203
C Barrel
C From type & pseudolayer to slot
 201     IF(JTYAS.LE.2) THEN
             JTYBU=MBTYBU(JTYAS,JPSEU)
             IF(JTYBU.GT.0.AND.JTYBU.LE.4) THEN
                JSL=I
             ELSE
                JSL=I+NRSLBA/2
                JTYAS=MBTAST(JSL)
                JTYBU=MBTYBU(JTYAS,JPSEU)
             ENDIF
             IF(JTYAS.EQ.2) THEN
C special case of 9M,11M
                 IF((JPSEU.GE.1.AND.JPSEU.LE.4).OR.
     +              (JPSEU.GE.13.AND.JPSEU.LE.16)) JSL=JSL+1
C special case of 9B,11B
                 IF((JPSEU.GE.9.AND.JPSEU.LE.12).OR.
     +              (JPSEU.GE.21.AND.JPSEU.LE.24)) JSL=JSL+2
             ENDIF
         ENDIF
         IF(JTYAS.EQ.3) THEN
             JTYBU=MBTYBU(JTYAS,JPSEU)
             JSL=MBSLPN(1,JPSEU)
         ENDIF
         JPOLD=JPSEU
         IF(JTYAS.EQ.4) THEN
             IF(JPSEU.LE.4) THEN
                JPSEU=JPSEU*2
                IF(JSTRP.LE.MBUPBU(JTYAS,JPSEU-1)) JPSEU=JPSEU-1
             ELSE
                JPSEU=JPSEU+2
             ENDIF
             JTYBU=MBTYBU(JTYAS,JPSEU)
             JSL=MBSLPN(2,JPSEU)
         ENDIF
C Check on the slot #
         IF(JSL.LE.0.OR.JSL.GT.NRSLBA) THEN
             IMES2=IMES2+1
             IF(IMES2.LE.100.AND.IMPRIN.GT.1)
     +       WRITE(LDEBRL,*) ' Unidentified slot # ',JSL,
     +        ' in cluster # ',J
             CALL RERROR('MPREDG',5,'unidentified slot')
             GOTO 20
         ENDIF
         GOTO 300
C End Cap
 202     CONTINUE
C Check on the slot #
         IF(JSL.LE.0.OR.JSL.GT.NRSLEC) THEN
             IMES2=IMES2+1
             IF(IMES2.LE.100.AND.IMPRIN.GT.1)
     +       WRITE(LDEBRL,*) ' Unidentified slot # ',JSL,
     +        ' in cluster # ',J
             CALL RERROR('MPREDG',5,'unidentified slot')
             GOTO 20
         ENDIF
         GOTO 300
C Middle angle
 203     CONTINUE
         IF(JSL.LE.0.OR.JSL.GT.NRSLMA) THEN
             IMES2=IMES2+1
             IF(IMES2.LE.100.AND.IMPRIN.GT.1)
     +       WRITE(LDEBRL,*) ' Unidentified slot # ',JSL,
     +        ' in cluster # ',J
             CALL RERROR('MPREDG',2,'unindentified slot')
             GOTO 20
         ENDIF
C Check if the hit is compatible with the bus when the bank
C is available if ILOAD=1
 300     ICLOK=1
C ----
         IF(ILOAD.GT.0) THEN
         IF(JSUBC.EQ.1.AND.JMBLG.NE.0) THEN
C    (takes into account inversion bus 14&15 in slot 17)
          JBNEW=JBUS
          IF(JSL.EQ.17.AND.JBUS.EQ.15) JBNEW=14
          IF(JSL.EQ.17.AND.JBUS.EQ.14) JBNEW=15
C    (takes into account inversion in slot 13)
          IF(JSL.EQ.13.AND.(JBUS.GE.13.AND.JBUS.LE.18))
     +       JBNEW=31-JBUS
C
          IF(JSTRP.LT.MLBBA(JTYAS,JBNEW).OR.
     +       JSTRP.GT.MUBBA(JTYAS,JBNEW)) ICLOK=0
         ENDIF
         IF(JSUBC.EQ.2.AND.JMELG.NE.0) THEN
            NUP=MUBEC(JTYAS,JBUS)
            NLO=MLBEC(JTYAS,JBUS)
C    (takes into account error in cabling 4A,i.e. slot 4)
            IF(JSL.EQ.4.AND.(JBUS.EQ.1.OR.JBUS.EQ.3)) NUP=63
            IF(JSL.EQ.4.AND.(JBUS.EQ.2.OR.JBUS.EQ.4)) NUP=255
C    (takes into account X strip location in 1A)
            IF(JSL.EQ.1.AND.(JBUS.EQ.1.OR.JBUS.EQ.4)) NLO=8
            IF(JSL.EQ.1.AND.(JBUS.EQ.3.OR.JBUS.EQ.6)) NUP=64
C    (takes into account X strip location in 2B)
            IF(JSL.EQ.10.AND.(JBUS.EQ.1.OR.JBUS.EQ.3)) NLO=8
            IF(JSL.EQ.10.AND.(JBUS.EQ.2.OR.JBUS.EQ.4)) NUP=192
C    (takes into account Y strip location in 2A,4A)
            IF((JSL.EQ.2.OR.JSL.EQ.4).AND.
     +                     (JBUS.EQ.13.OR.JBUS.EQ.15)) NLO=16
            IF((JSL.EQ.2.OR.JSL.EQ.4).AND.
     +                     (JBUS.EQ.14.OR.JBUS.EQ.16)) NUP=255
C
            IF(JSTRP.LT.NLO.OR.JSTRP.GT.NUP) ICLOK=0
         ENDIF
         IF(JSUBC.EQ.3.AND.JMMLG.NE.0) THEN
            IF(JSTRP.LT.MLBMA(JTYAS,JBUS).OR.
     +         JSTRP.GT.MUBMA(JTYAS,JBUS)) ICLOK=0
         ENDIF
         IF(ICLOK.EQ.0.AND.IMES5.LT.100) THEN
            IMES5=IMES5+1
            WRITE(LDEBRL,754)  JASTR,JBUS,JSTRP
 754        FORMAT(1H ,
     +' Strip out of limits: Astros# ',I5,' Bus# ',I5,' Strip# ',I5)
            CALL RERROR('MPREDG',7,'Strip out of bus limits ')
         ENDIF
         ENDIF
C -------
C Fill vectors
C ================
         IPAN(1,J)=JSUBC
C                                     ! subc.
         IPAN(2,J)=JSL
C                                     ! slot
         IPAN(3,J)=2
C                                     ! plane
         IF(JTYBU.EQ.1.OR.JTYBU.EQ.2.OR.JTYBU.EQ.5.OR.JTYBU.EQ.6)
     +                                                  IPAN(3,J)=1
         IPAN(4,J)=2
C                                     ! view (x=1)
         IF(JTYBU.EQ.1.OR.JTYBU.EQ.3.OR.JTYBU.EQ.5.OR.JTYBU.EQ.7)
     +                                                  IPAN(4,J)=1
         IPAN(5,J)=JPSEU
C                                     ! pseudolay in bus banks
         IPAN(6,J)=JSTRP
C                                     ! strip address
         IPAN(7,J)=JCLUS
C                                     ! clust. width
         IPAN(8,J)=JTYAS
C                                     ! astros type
C ----------------------------------------------------------------
C Correction for bus 1-3 / 13-15 in slot 3A end cap after run 6000
         IF(IRUNRC.GT.6000.AND.IRUNRC.LT.7814.AND.JSUBC.EQ.2.
     + AND.JSL.EQ.3) THEN
            IF(JBUS.EQ.1.OR.JBUS.EQ.3) THEN
               IPAN(5,J)=JBUS+12
               IPAN(4,J)=2
            ENDIF
            IF(JBUS.EQ.13.OR.JBUS.EQ.15) THEN
               IPAN(5,J)=JBUS-12
               IPAN(4,J)=1
            ENDIF
         ENDIF
C Correction for x str. (bus 1&2) in EC slots before run 5022
         IF(IRUNRC.LT.5022.AND.JSUBC.EQ.2) THEN
            IF(IPAN(4,J).EQ.1.AND.(JBUS.EQ.1.OR.JBUS.EQ.2))
     + IPAN(6,J)=32*(JSTRP/16)-JSTRP+JCLUS+14
         ENDIF
C Correction for Y2.1,Y2.2 bad cabling slot 4d,4c
         IF(IRUNRC.GT.6000.AND.IRUNRC.LT.8084.AND.JSUBC.EQ.1) THEN
C
          IF((JSL.EQ.4.AND.(JBUS.EQ.22.OR.JBUS.EQ.23)).OR.
     +       (JSL.EQ.5.AND.(JBUS.EQ.10.OR.JBUS.EQ.11))) THEN
            IF(JSTRP.LE.31)                  IPAN(6,J)=JSTRP+128
            IF(JSTRP.GE.32.AND.JSTRP.LE.63)  IPAN(6,J)=JSTRP+64
            IF(JSTRP.GE.96.AND.JSTRP.LE.127) IPAN(6,J)=JSTRP-64
            IF(JSTRP.GE.128.AND.JSTRP.LE.159) IPAN(6,J)=JSTRP-128
          ENDIF
         ENDIF
C Correction for bad cabling in m.a. slot 4 bus 18
         IF(JSUBC.EQ.3.AND.JSL.EQ.4.AND.JBUS.EQ.18) IPAN(6,J)=JSTRP-8
C Correction for bad cabling in ba. slot 15 bus 13
         IF(.NOT.FMCRUN.AND.IRUNRC.LE.8163.AND.JSUBC.EQ.1) THEN
            IF(JSL.EQ.15.AND.JBUS.EQ.13.AND.JSTRP.LE.8)
     +                      IPAN(6,J)=JSTRP+8
         ENDIF
C Printing for debug
         IF(IMPRIN.GT.2) THEN
            WRITE(LDEBRL,1099) J,JASTR,JBUS,IPAN(6,J),IPAN(7,J),
     +      JSUBC,JSL,IPAN(5,J),JTYBU,IPAN(4,J),IPAN(3,J),ICLOK
 1099    FORMAT(1H ,I3,' A',I4,' B',I3,' S',I4,' CL',I3,5X,' sub',I3,
     +' sl',I3,' ps',I3,' ty',I3,' view',I3,' pl',I3,' iok',I3)
         ENDIF
C
C Set the cluster status
         IPAN(9,J)=ICLOK
C
         CALL HFILL(8198+IPAN(4,J),FLOAT(JCLUS),0.,1.)
C Fill the digit matrix
         ISLOT=JSL+IOFFS(JSUBC)
         NDIG(ISLOT,IPAN(3,J),IPAN(4,J))=
     +        NDIG(ISLOT,IPAN(3,J),IPAN(4,J))+ICLOK
C
 20      CONTINUE
C
C Association & decoding of clusters
C (status ok; same subc,slot; orthogonal views)
C X & Y may lay on different planes
C
         DO 39 I=1,NCL-1
         IF (IPAN(9,I).EQ.0) GO TO 39
         JSUBC=IPAN(1,I)
         ISLOT=IPAN(2,I)+IOFFS(JSUBC)
C Pairing number on same layer
         NPSPL=NDIG(ISLOT,1,1)*NDIG(ISLOT,1,2)+
     +         NDIG(ISLOT,2,1)*NDIG(ISLOT,2,2)
C
         NDIGX=NDIG(ISLOT,1,1)+NDIG(ISLOT,2,1)
         NDIGY=NDIG(ISLOT,1,2)+NDIG(ISLOT,2,2)
         NPTOT=NDIGX*NDIGY
C No pairings found
         IF(NPTOT.EQ.0) GOTO 39
C
         DO 30 J=I+1,NCL
C Same subc. and slot
         DO 31 L=1,2
 31      IF(IPAN(L,I).NE.IPAN(L,J)) GOTO 30
C
         IF(IPAN(9,J).EQ.0) GOTO 30
C Here x1y2 or x2y1 pairings are allowed if pairings on same plane do no
         IF(IPAN(3,I).NE.IPAN(3,J).AND.NPSPL.GT.0) GOTO 30
C Different view
         IF(IPAN(4,I).EQ.IPAN(4,J)) GOTO 30
C
         IEL(1)=IPAN(1,I)
C                                    ! subc
         IEL(2) =IPAN(2,I)
C                                    ! slot
         IF(IPAN(4,I).EQ.1) THEN
              IX=I
              IY=J
         ELSE
              IX=J
              IY=I
         ENDIF
C                                    ! planes X & Y
         IEL(11)=IPAN(3,IX)
         IEL(12)=IPAN(3,IY)
C
         IEL(3)=IPAN(8,IX)
C                                    ! x astr. type
         IEL(4)=IPAN(8,IY)
C                                    ! y
         IEL(5)=IPAN(6,IX)
C                                    ! x strip add.
         IEL(6)=IPAN(6,IY)
C                                    ! y
         IEL(7)=IPAN(7,IX)
C                                    ! x cl. wid.
         IEL(8)=IPAN(7,IY)
C                                    ! y
         IEL(9)=IPAN(5,IX)
C                                    ! x pseud.
         IEL(10)=IPAN(5,IY)
C                                    ! y
C Transformation to slot coordinates
         DO 623 KS=1,4
 623     SLTCO(KS)=0.
C
         CALL MUSLCO(IEL,SLTCO,IOK)
         IF(IOK.NE.1.AND.IMPRIN.GE.1) THEN
            IMES3=IMES3+1
            IF(IMES3.LE.100)
     +      WRITE(LDEBRL,*) '+ Hit assoc. flag =',IOK,' in clust.',IX,IY
         ENDIF
         IF(IOK.NE.1) GOTO 30
C Transformation to ALEPH coordinates
         CALL MUALCO(IEL,SLTCO,ALECO)
         NMHIT=NMHIT+1
C
         IF(NMHIT.GT.NHITMG) THEN
            CALL RERROR('MPREDG',2,'Max # of muon hits reached')
            GOTO 40
         ENDIF
C
          IF(IMPRIN.GT.2) THEN
             WRITE(LDEBRL,4567) I,J,SLTCO,ALECO
 4567        FORMAT(1H ,2I3,' slco ',2F7.2,2F5.1,' alco',3F9.4)
          ENDIF
C
C Fill MHIT bank
C Set subc # to ALEPH convention (EC=1,MA=2,BA=3)
         NSUBC=IEL(1)-1
         IF(NSUBC.EQ.0) NSUBC=3
C
         IW(KMHIT+JMHISC)=NSUBC
C                                            ! subc
         IW(KMHIT+JMHISN)=IEL(2)
C                                            ! slot
         IW(KMHIT+JMHILN)=IEL(11)
C                                            ! plane X
         RW(KMHIT+JMHIXL)=SLTCO(1)
C                                            ! x slot
         RW(KMHIT+JMHIYL)=SLTCO(2)
C                                            ! y slot
         RW(KMHIT+JMHIRH)=ALECO(1)
C                                            ! rho ALEPH r.s.
         RW(KMHIT+JMHITH)=ALECO(2)
C                                            ! theta
         RW(KMHIT+JMHIPH)=ALECO(3)
C                                            ! phi
         KMHIT=KMHIT+LMHITG
C Fill histos
         IF(IHIST.GT.0) THEN
            CALL HFILL(8001,ALECO(2),0.,1.)
            CALL HFILL(8002,ALECO(3),0.,1.)
C Slot hist.: 1-16 EC, 21-58 MA, 61-94 BA
            SLOTN=FLOAT(IEL(2))
            IF(NSUBC.EQ.3) SLOTN=SLOTN+60.
            IF(NSUBC.EQ.2) SLOTN=SLOTN+20.
            CALL HFILL(8003,SLOTN,0.,1.)
         ENDIF
C
 30      CONTINUE
 39      CONTINUE
C
 40      IF(IHIST.GT.0) CALL HFILL(8004,FLOAT(NMHIT),0.,1.)
         IF(IMPRIN.GT.0) WRITE(LDEBRL,*) NMHIT,' muon hits in the event'
C Adjust MHIT bank length
         IF(NMHIT.EQ.0) THEN
             IND=NDROP('MHIT',0)
         ELSE
             IW(JMHIT+1)=LMHITG
             IW(JMHIT+2)=NMHIT
             LNHIT=2+NMHIT*LMHITG
             CALL AUBOS('MHIT',0,LNHIT,JMHIT,IGARB)
             IF(IMPRIN.GE.3) IND=NPRNT('MHIT',0)
         ENDIF
C
C? Compare with mtc data
C
         IF(NMHIT.GT.0.AND.JHISRF(JULMU).GE.2) CALL MUCOMP
C
         RETURN
         END
#endif
