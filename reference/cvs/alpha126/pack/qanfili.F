      SUBROUTINE QANFILI(CARD,TAPE,FILE)
C ----------------------------------------------------------------------
C! Examine the content of a FILI CARD to extract tape + file #           
C  J. Boucrot        14-Mar-2000
C  Input argument : 
C    CARD = FILI CARD
C  Output arguments :                                           
C    TAPE = tape name e.g. R01234 , blank if any error
C    FILE = file number (< 1000 ) , blank if any error
C ----------------------------------------------------------------------
#ifndef DOC
      character*(*) card,tape,file
      character cardf*200,subf*80
      PARAMETER ( MAXFI = 999 )
      INTEGER BKINCA
C ----------------------------------------------------------------------
      CARDF=' '
      TAPE =' '
      FILE =' '
      CARDF=CARD(1:LNBLNK(CARD))
      CALL CLTOU(CARDF)
      LN=LNBLNK(CARDF)
      IF (LN.EQ.0) GO TO 999
C
C Is it a 'FILI' card with 'CART Y0XXXX.ZZZ' syntax ?
C
      ICART=INDEX(CARDF,'CART ')
      IF (ICART.EQ.0) GO TO 200
      IDSL=INDEX(CARDF,'.SL ')
      IF (IDSL.EQ.0) IDSL=INDEX(CARDF,'.AL ')
      IF (IDSL.EQ.0) IDSL=INDEX(CARDF,'.UL ')
      IF (IDSL.EQ.0) IDSL=INDEX(CARDF,'.NL ')
      IF (IDSL.EQ.0) GO TO 999
      SUBF=CARDF(ICART+4:IDSL-1)
      LL=LNBLNK(SUBF)
      ITAP=ICFNBL(SUBF,1,LL)
      IF (ITAP.LE.0) GO TO 999
      IDOT=INDEX(SUBF,'.')
      IF (IDOT.LE.0) GO TO 999
      TAPE=SUBF(ITAP:IDOT-1)
      FILE=SUBF(IDOT+1:LL)
      GO TO 300
C
C Is it a 'FILI' card with 'r0xxxx_yyy.edir' syntax ?
C
 200  IEDIR=INDEX(CARD,'.edir')
      IF (IEDIR.EQ.0) GO TO 999
      LSUB=IEDIR-1
      SUBF(1:)=CARDF(1:LSUB)
      INDUN=INDEX(SUBF,'_')
      IF (INDUN.EQ.0) GO TO 999
      TAPE=SUBF(indun-6:INDUN-1)
      FILE=SUBF(INDUN+1:LSUB)
C
C Tape and file found: check that the file # is reasonable:
C
 300  IFILE=BKINCA(FILE)
      IF (IFILE.GT.MAXFI) FILE=' '
C
 999  RETURN
      END
#endif
