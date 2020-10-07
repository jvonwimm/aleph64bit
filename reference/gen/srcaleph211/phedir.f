      SUBROUTINE PHEDIR (SNGPHO)
C-----------------------------------------------------------------------
CKEY EDIR DEF CLASS21
C! Selects events containing one or more photons for ALEPH
C! event directories.
C-
C   Input  : None
C   Output : SNGPHO = Class 21 logical flag
C-
C   Called by   : SELEVT
C   Calls  : MODFND
C   Input banks : REVH,PFRT,XTEB,PECO,EWHE
C-
C                               Author: Lee Thompson, 31st January 1990
C-----------------------------------------------------------------------
      SAVE
C --
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JREVDS=1,JREVFE=2,JREVNE=4,JREVSB=6,JREVTI=7,JREVRB=8,
     +          JREVEC=10,LREVHA=10)
      PARAMETER(JPFRNV=1,JPFRNI=2,JPFRNE=3,JPFRNT=4,JPFRNR=5,LPFRTA=5)
      PARAMETER(JXTET1=1,JXTET2=2,JXTEL2=3,JXTEHT=4,JXTEHW=16,JXTELW=28,
     +          JXTEEW=40,JXTELT=52,JXTETE=56,JXTEIT=58,JXTETP=62,
     +          LXTEBA=65)
      PARAMETER(JPECER=1,JPECE1=2,JPECE2=3,JPECTH=4,JPECPH=5,JPECEC=6,
     +          JPECKD=7,JPECCC=8,JPECRB=9,JPECPC=10,LPECOA=10)
      PARAMETER(JEWHSD=1,JEWHNU=2,LEWHEA=2)
      INTEGER ALTRIG
      LOGICAL LHVTPC,SNGPHO
      LOGICAL BTEST
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C --
      SNGPHO = .FALSE.
C --
C   If the event has TPC HT off or then do no further processing.
C   do no further processing.
C --
C     LHVTPC = .FALSE.
C     JREVH = IW(NAMIND('REVH'))
C     IF (JREVH .NE. 0) THEN
C         KHVSTA = ITABL(JREVH,1,JREVDS)
C         LHVTPC = BTEST(KHVSTA,15)
C     END IF
C     IF (.NOT. LHVTPC) RETURN
C --
C   If the event has at least one good TPC track (more than 4 space
C   points) then do no further processing.
C --
      JPFRT = IW(NAMIND('PFRT'))
      IF (JPFRT .NE. 0) THEN
          NTRACK = LROWS(JPFRT)
          DO 1 ITRACK = 1 , NTRACK
              IF (ITABL(JPFRT,ITRACK,JPFRNT) .GE. 4) RETURN
 1        CONTINUE
      END IF
C --
C   Next check the trigger - require that the SNG_N_EM trigger
C   is present in the Level 1 trigger word otherwise return.
C --
      ISNGEM = 2
      ICODE = ALTRIG(ITLVL1,ITLVL2,ITLVL3)
      IF(ICODE.EQ.0) RETURN
      IF (.NOT. BTEST(ITLVL1,ISNGEM)) RETURN
C
C     JXTEB = IW(NAMIND('XTEB'))
C     IF (JXTEB .NE. 0) THEN
C         ITLVL1 = ITABL(JXTEB,1,JXTET1)
C         IF (.NOT. BTEST(ITLVL1,ISNGEM)) RETURN
C     END IF
C --
C   Now start a loop over ECAL clusters
C --
      JPECO = IW(NAMIND('PECO'))
      IF (JPECO .EQ. 0) GOTO 999
      NCLUST = LROWS(JPECO)
      DO 10 ICLUST = 1 , NCLUST
C --
C   If the cluster has :
C      a) Energy > 1.0 GeV
C      b) Cluster energy is not all in one stack
C      c) 20 degrees < Theta < 160 degrees
C   then continue, otherwise go to the next cluster.
C --
          ECLUST = RTABL(JPECO,ICLUST,JPECER)
          IF (ECLUST .LT. 1.0) GOTO 10
          S1 = RTABL(JPECO,ICLUST,JPECE1)
          S2 = RTABL(JPECO,ICLUST,JPECE2)
          S3 = 1.0 - S1 - S2
          IF (S1.GT.0.99.OR.S2.GT.0.99.OR.S3.GT.0.99) GOTO 10
C         IF (ECLUST*S2 .LT. 0.1) GOTO 10
C         IF (ECLUST*S1 .LT. 0.1 .AND. ECLUST*S3 .LT. 0.1) GOTO 10
          THETAR = RTABL(JPECO,ICLUST,JPECTH)
          PHIR   = RTABL(JPECO,ICLUST,JPECPH)
          THETAD = THETAR * 180.0 / ACOS(-1.0)
          IF (THETAD .LT. 13.0 .OR. THETAD .GT. 167.0) GOTO 10
C --
C   Any cluster getting this far looks good on pad information
C   so finally check the wire information.
C --
C   Find the module in which the cluster barycentre lies.
C --
          CALL MODFND(THETAR,PHIR,IREG,MODULE,IRC)
          IF (IRC .EQ. 0) THEN
              JEWHE = IW(NAMIND('EWHE'))
              IF (JEWHE .NE. 0) THEN
                  MODWIR = ITABL(JEWHE,MODULE,JEWHNU)
                  IF (MODWIR .EQ. MODULE) THEN
                      EWIRES = FLOAT(ITABL(JEWHE,MODULE,JEWHSD))*1.0E-6
C --
C   If wire energy OK flag event and return,
C   otherwise, go to next cluster.
C --
                      IF (EWIRES .GE. 1.0) THEN
                          SNGPHO = .TRUE.
                          RETURN
                      END IF
                  END IF
              END IF
          END IF
C --
   10 CONTINUE
C --
  999 RETURN
      END
