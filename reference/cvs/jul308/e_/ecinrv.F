      SUBROUTINE ECINRV
C***********************************************************************
C!DEFINITION OF  "ROSE DES VENTS" IROSEV   ***  (28/9/84)
C!
C***********************************************************************
#ifndef DOC
#include "eclcom.h"
      PARAMETER (LAP=20,LOV=40)
      DIMENSION LISJD(28),LSNRD(28)
      DIMENSION JRCLI(5),NSTEP(5)
      DIMENSION NOFSE(7)
      DIMENSION LAPNR(LAP),LAPJ(LAP),LAPK(LAP),LAPNC(LAP)
      DIMENSION IOVNR(LOV),IOVJ(LOV),IOVK(LOV)
      DIMENSION IPHI(6)
C!
C!.      DATA USE TO COMPUTE ENCODED (I,J,K,NR) ADDRESS
C!
      DATA JRCLI/1,9,25,41,45/
      DATA NSTEP/0,8,40,88,128/
*/Correction for better definition of overlaps of Ecal.
C
C!.      DATA FOR THE OVERLAP REGIONS
C!
      DATA IPHI/5,6,7,8,1,0/
      DATA LAPNR/5,5,5,5,5,5,5,4,4,4,4,4,4,0,0,0,0,0,0,0/
      DATA LAPJ/45,46,47,47,48,49,50,45,46,47,48,49, 50,0,0,0,0,0,0,0/
      DATA LAPK/1,1,1,2,2,2,2,1,1,1,1,1,1,0,0,0,0,0,0,0/
      DATA LAPNC/0,3,6,7,10,13,15,16,17,19,23,26,29,32,0,0,0,0,0,0/
      DATA IOVNR/4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4, 5,5,5,5,5,5,5,5,5,
     +5,5,5,5,5,5,5,0,0,0,0,0,0,0,0/
      DATA IOVJ/45,46,47,46,47,48,47,47,48,49,48,49,50,49,50,50,
     +45,45,46,45,46,47,47,46,47,48,47,48,49,48,49,50,0,0,0,0,0,0,0,0/
      DATA IOVK/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,2,2,2,
     +2,2,2,2,2,0,0,0,0,0,0,0,0/
C
C
C?.      DATA FOR BARREL OR ENCAP ALONE
C?
      DATA LISJD/1,2,8,9,10,24,25,26,40,41,42,50, 45,46,172,167,168,176,
     +177,178,192,193,194,208,209,210,216,217/
      DATA LSNRD/1,1,1,2,2,2,3,3,3,4,4,4, 5,5,5, 4,4,4,3,3,3,2,2,2,1,1,
     +1,0/
      DATA NOFSE/-1,-2,0,-1,0,-1,0/
C
C? Arrays initialization
      CALL VZERO(IROSEV,21900)
      CALL VZERO(NINDEB, 4110)
C?.      DEFINITION OF ARRAY NINDEB THAT COUNTAINS POINTER TO IROSEV
C?
      INC=0
C
      DO 40 K=1,3
         DO 30 LIS=1,27
            NR=LSNRD(LIS)
            MUL=NR-INT(NR/5)
            DO 20 II=0,MUL+1
               I=INT((MUL*96-II)/(MUL*96))*MUL* 96+ II
               INC=INC+1
               NLIS=LISJD(LIS)
               MLIS=LISJD(LIS+1)-1
               IF(MLIS.LT.NLIS) MLIS=NLIS
               DO 10 J=NLIS,MLIS
                  NDIR=(K-1)*228* 6+6*(J-1+INT(NR/5)*6+INT(J/167)*12-INT
     +            (NR/5)*INT(J/167)*12) +MOD(I-1,NR-INT(NR/5))+ 1+INT
     +            ((MUL*96+1-I)/(MUL*96))*MUL+INT(I/(MUL*96))*2
                  NINDEB(NDIR)=INC
   10          CONTINUE
   20       CONTINUE
   30    CONTINUE
   40 CONTINUE
C?
C?.      COMPUTATION OF THE OFFSET IN THE GENERAL CASE
C?
      LIS=0
   50 LIS=LIS+1
      JD=LISJD(LIS)
      NRD=LSNRD(LIS)
      MULD=NRD-INT(NRD/5)
      NTHID=MULD*96
      DO 110 IID=0,MULD+1
         ID=INT((NTHID-IID)/NTHID)*NTHID + IID
         DO 100 KD=1,3
            IF((NRD.NE.5).AND.(JD.GE.167))THEN
               JD=217-JD
               IDEP=(KD-1)* 73728+768*(INT((ID+MULD-1)/MULD)-1)+NSTEP
     +         (NRD) +(JD-JRCLI(NRD))*MULD+MOD(ID-1,MULD)+ 1+640
               JD=217-JD
            ELSE
               IDEP=(KD-1)* 73728+768*(INT((ID+MULD-1)/MULD)-1)+NSTEP
     +         (NRD) +(JD-JRCLI(NRD))*MULD+MOD(ID-1,MULD)+1
            ENDIF
C
            NDIR=(KD-1)*228* 6+6*(JD-1+INT(NRD/5)*6+INT(JD/167)*12-INT
     +      (NRD/5)*INT(JD/167)*12) +MOD(ID-1,NRD-INT(NRD/5))+ 1+INT
     +      ((MULD*96+1-ID)/(MULD*96))*MULD+INT(ID/(MULD*96))*2
C
            NDEBT=40*NINDEB(NDIR)
C
            DO 80 KKA=KD-1,KD+1
               IF((KKA.EQ.0).OR.(KKA.EQ.4))                      GOTO 80
               KA=KKA
               DO 70 JJA=JD-1,JD+1
                  IF(NRD.LT.5) THEN
                     IF(JJA.LE.216) NRA=1
                     IF(JJA.LE.208) NRA=2
                     IF(JJA.LE.192) NRA=3
                     IF(JJA.LE.176) NRA=4
                     IF(JJA.LE.50) NRA=4
                     IF(JJA.LE.40) NRA=3
                     IF(JJA.LE.24) NRA=2
                     IF(JJA.LE.8) NRA=1
                  ELSE
                     NRA=5
                  ENDIF
                  IF((JJA.EQ.217).OR.(JJA.EQ.0))                 GOTO 70
                  IF((NRA.EQ.4).AND.((JJA.EQ.51).OR.(JJA.EQ.166)))
     +                                                           GOTO 70
                  IF((NRA.EQ.5).AND.((JJA.EQ.44).OR.(JJA.EQ.173)))
     +                                                           GOTO 70
                  JA=JJA
                  MULA=NRA-INT(NRA/5)
                  IANIS=((3*MULD+MULA-3)/2)*ABS(MULD-MULA)+1
                  DO 60 IOFSE=NOFSE(IANIS),1
                     IIA=(MULA*ID)/MULD+IOFSE
                     IF(IIA.EQ.0) IIA=MULA*96
                     IF(IIA.EQ.MULA*96+1) IIA=1
                     IA=IIA
C?      EXCEPTION CASES
C?
                     IF((KA.EQ.KD).AND.(JA.EQ.JD).AND.(IOFSE.EQ.0))
     +                                                           GOTO 60
                     IF((IANIS.EQ.6).AND.(ID.EQ.2).AND.(IOFSE.EQ.-1))
     +                                                           GOTO 60
                     IF((NRA.NE.5).AND.(JA.GE.167))THEN
                        JA=217-JA
                        IARR=(KA-1)* 73728+768*(INT((IA+MULA-1)/MULA)-1)
     +                  +NSTEP(NRA) +(JA-JRCLI(NRA))*MULA+MOD(IA-1,MULA)
     +                  + 1+640
                        JA=217-JA
                     ELSE
                        IARR=(KA-1)* 73728+768*(INT((IA+MULA-1)/MULA)-1)
     +                  +NSTEP(NRA) +(JA-JRCLI(NRA))*MULA+MOD(IA-1,MULA)
     +                  +1
                     ENDIF
                     NDLTA=IARR-IDEP
                     NDEBT=NDEBT+1
                     IROSEV(NDEBT)=NDLTA
   60             CONTINUE
   70          CONTINUE
   80       CONTINUE
            DO 90 JDEB=NDEBT+1,40*NINDEB(NDIR)+40
               IROSEV(JDEB)=0
   90       CONTINUE
  100    CONTINUE
  110 CONTINUE
      IF(LIS.NE.27)                                              GOTO 50
C
C?.       OVERLAP REGIONS
C?
      MIROR=0
  120 KLAP=0
      KOV=0
  130 KLAP=KLAP+1
      NRD=LAPNR(KLAP)
      JD=LAPJ(KLAP)
      IF(MIROR.EQ.1) JD=217-LAPJ(KLAP)
      KD=LAPK(KLAP)
      MULD=NRD-INT(NRD/5)
      DO 170 NA=1,6
         KOV=LAPNC(KLAP)
         INC=INC+1
         NDBT2=INC*40
         ID=IPHI(NA)
         IF(NA.EQ.6) THEN
            ID=96*(NRD-INT(NRD/5))
         ENDIF
         IF((NRD.NE.5).AND.(JD.GE.167))THEN
            JD=217-JD
            IADD1=(KD-1)* 73728+768*(INT((ID+MULD-1)/MULD)-1)+NSTEP(NRD)
     +      +(JD-JRCLI(NRD))*MULD+MOD(ID-1,MULD)+ 1+640
            JD=217-JD
         ELSE
            IADD1=(KD-1)* 73728+768*(INT((ID+MULD-1)/MULD)-1)+NSTEP(NRD)
     +      +(JD-JRCLI(NRD))*MULD+MOD(ID-1,MULD)+1
         ENDIF
C
         NDIR=(KD-1)*228*6+ 6*(JD-1+INT(NRD/5)*6+INT(JD/167)*12-INT
     +   (NRD/5)*INT(JD/167)*12) +MOD(ID-1,NRD-INT(NRD/5))+ 1+INT((MULD
     +   *96+1-ID)/(MULD*96))*MULD+INT(ID/(MULD*96))*2
C
         KDEB=NINDEB(NDIR)
         NINDEB(NDIR)=INC
         DO 140 NDEBT=40*KDEB+1,40*KDEB+40
            IF(IROSEV(NDEBT).EQ.0)                              GOTO 150
            NDBT2=NDBT2+1
            IROSEV(NDBT2)=IROSEV(NDEBT)
  140    CONTINUE
  150    KOV=KOV+1
         NR2=IOVNR(KOV)
         J2=IOVJ(KOV)
         IF(MIROR.EQ.1) J2=217-IOVJ(KOV)
         K2=IOVK(KOV)
         MUL2=NR2-INT(NR2/5)
         DO 160 II2=ID-1,ID+1
            I2=II2
            IF(II2.EQ.0) THEN
               I2=(NR2-INT(NR2/5))*96
            ENDIF
            IF(II2.EQ.96*(NRD-INT(NRD/5))+1) THEN
               I2=1
            ENDIF
            IF((NR2.NE.5).AND.(J2.GE.167))THEN
               J2=217-J2
               IADD2=(K2-1)* 73728+768*(INT((I2+MUL2-1)/MUL2)-1)+NSTEP
     +         (NR2) +(J2-JRCLI(NR2))*MUL2+MOD(I2-1,MUL2)+ 1+640
               J2=217-J2
            ELSE
               IADD2=(K2-1)* 73728+768*(INT((I2+MUL2-1)/MUL2)-1)+NSTEP
     +         (NR2) +(J2-JRCLI(NR2))*MUL2+MOD(I2-1,MUL2)+1
            ENDIF
            NDLTA=IADD2-IADD1
            NDBT2=NDBT2+1
            IROSEV(NDBT2)=NDLTA
  160    CONTINUE
         IF(KOV.NE.LAPNC(KLAP+1))                               GOTO 150
  170 CONTINUE
      IF(LAPNR(KLAP+1).NE.0)                                    GOTO 130
      LIMT2=40*(INC+1)
      IF(MIROR.EQ.1)                                            GOTO 180
      MIROR=1
                                                                GOTO 120
  180 CONTINUE
      RETURN
      END
#endif
