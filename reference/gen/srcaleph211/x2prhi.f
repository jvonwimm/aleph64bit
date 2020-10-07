      SUBROUTINE X2PRHI
C-----------------------------------------------------------------------
CKEY TRIG LEVEL2 PRINT
C! dump X2DF banks: martin's tabular format, agrees with DDL
C - T. Medcalf, 4/10/89
C-----------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (LWBNK = 500,JWPHT = 3,IDFLEN = 1000
     +    ,NUMWB = 24, NZONE = 2 )
      COMMON /X2NAMC/ NAX2DF,JWORKB(NUMWB),NAX2MS,NAX2TB,
     +                JX2TRK(NZONE), JX2SOR(NZONE)
      PARAMETER (NIGN = 3,NPEXI = 11,NPUSE = 8,IOS = 2,NBMAX = 62,
     +   NVMAX = 31,NTMAX = 256,ILM = 15,IUM = 31,ISMX = 9,ICHX = 4,
     +   MSKLN = 60 )
      COMMON /X2CONS/ TDVELO,ADVELO,IGNPAD(NIGN),IPDCON(NPEXI)
     +  ,RADPAD(NPUSE,IOS),CLOCKR
     +  ,NTBINS(IOS),ZACPMM,IZBMAX
     +  ,ITHETA(IOS,0:NTMAX,NPUSE),ITHSUB(IOS,0:NTMAX,NPUSE)
     +  ,ITHOVR(IOS,0:NTMAX,NPUSE),ITHOSB(IOS,0:NTMAX,NPUSE)
     +  ,IHTMAX(IOS,NBMAX),ITHRSH(NPUSE),ITVOTE(0:NVMAX,0:NVMAX)
     +  ,IPADPR(IOS,ICHX),IZRDLK(ILM,IUM,ISMX,ICHX,IOS)
     +  ,IX2PRL,IRWDTH,IDIGNZ
     +  ,MASCON(NBMAX,IOS,IOS),IX2MSK(MSKLN),IX2HIS,IX2RUN
C
 9500   FORMAT(/' +++X2TRIG+++  Event accepted by Level 2 Trigger'/)
 9510   FORMAT(/' +++X2TRIG+++  Event rejected by Level 2 Trigger'/)
C
C Extract yes/no decision
C
      SAVE
      NYES = 0
      IX2MS = IW(NAX2MS)
      IF (IX2MS.GT.0) THEN
        DO 9200 IMN = 1, MSKLN
          IF (IW(IX2MS+LMHLEN+IMN).EQ.1) NYES = 1
 9200   CONTINUE
      ELSE
        WRITE(IW(6),*)' No X2MS bank found'
      ENDIF
C
C  Announce result
C
      IF (NYES.EQ.1) THEN
         WRITE(IW(6),9500)
      ELSE
         WRITE(IW(6),9510)
      ENDIF
C
      NAX2DF = NAMIND('X2DF')
      IF ( IW(NAX2DF).EQ.0) THEN
         WRITE(IW(6),*)' No X2DF bank found'
         GOTO 999
      END IF
C
      IPX2DF = 0
      ISTART = 0
      JX2DF  = NAX2DF+1
   30 CONTINUE
      JX2DF = IW(JX2DF-1)
      IF ( JX2DF.EQ.0 ) GO TO 9999
C
C     IPX2DF is pointer to start of processor information
      IPX2DF = JX2DF+2+1
      ISTART = IPX2DF
C
      WRITE (IW(6),4000) IW(JX2DF+1),IW(JX2DF+2)
 4000 FORMAT('0 ==> X2DF BANK FOUND WITH:',/,
     +       '      ',I5,' COLUMNS AND ',I5,' ROWS')
      NWORDS = IW(JX2DF+2)
C
C     Loop over processors
C
      DO 10 I = 1, 24
C
      WRITE (IW(6),1000) IW(IPX2DF),IW(IPX2DF+1),IW(IPX2DF+2)
 1000 FORMAT
     +(1X,' ==> Processor:',I5,'   No. hits:',I10,'   No. tracks:',I10)
C
C     Loop over hits for this processor
      IHIT = IW(IPX2DF+1)
      DO 40 J = 1, IHIT
      IF ( J.EQ.1 ) WRITE (IW(6),1030)
 1030 FORMAT('  ==>  Hit number:     Hit pattern:    Drift time:')
      WRITE (IW(6),1040) J,IW(IPX2DF+2+J),IW(IPX2DF+2+IHIT+J)
 1040 FORMAT('  ==>     ',I5,'          ',Z8,' ',I18)
   40 CONTINUE
C
C     Loop over tracks for this processor
      IOFF = IW(IPX2DF+1)*2
      DO 20 J = 1, IW(IPX2DF+2)
      IF ( J.EQ.1 ) WRITE (IW(6),1010)
 1010 FORMAT('  ==>  Track number:   Track theta:')
      WRITE (IW(6),1020) J,IW(IPX2DF+2+IOFF+J)
 1020 FORMAT('  ==> ',I10,8X,I10)
   20 CONTINUE
C
C     Reset pointer to processor
      IPX2DF = IPX2DF+IOFF+IW(IPX2DF+2)+3
   10 CONTINUE
      GO TO 30
C
 9999 CONTINUE
      LENGTH = IPX2DF-ISTART
      WRITE (IW(6),1050) LENGTH
 1050 FORMAT(1X,' ==> NUMBER OF WORDS IN X2DF BANK WAS ',I6)
C
  999 RETURN
      END
