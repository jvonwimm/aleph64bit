      SUBROUTINE DMPFMT (NCOL,AFI,FMT)
C ---------------------------------------------------------
C - M.Rumpf - 880400          F.Ranjard - 900323
CKEY PRESS FORMAT / USER
C! build a BOS bank format
C
C - Input    : NCOL    / I   = number of columns
C              AFI     / A   = character string : one letter (A,F,I)
C                              per column
C
C - Output   : FMT     / A   = BOS format
C
C ------------------------------------------------------------
#ifndef DOC
      SAVE
      CHARACTER*(*) AFI,FMT
      CHARACTER*512 STRING
      CHARACTER BLANK,CHOLD,CHS
      CHARACTER*3 HEAD
      CHARACTER*4 IPROV,IWFMT
      DATA HEAD /'2I,'/
      DATA BLANK /' '/
C -------------------------------------------------------------
C
      IF (INDEX(AFI,'F').EQ.0 .AND. INDEX(AFI,'A').EQ.0) THEN
         FMT = 'I'
         RETURN
      ENDIF
C
      CHOLD = BLANK
      FMT = BLANK
      N = 1
      NSAME = 1
      STRING = AFI
      STRING(NCOL+1:NCOL+1) = BLANK
      DO 10 I=1,NCOL+1
         IF (NSAME.GT.9999) GOTO 996
         IF (STRING(I:I).NE.CHOLD) THEN
            IF (NSAME.GT.1) THEN
               NN=1
               IF (NSAME.GT.9) NN=2
               IF (NSAME.GT.99) NN=3
               IF (NSAME.GT.999) NN=4
               WRITE (CHS,'(I1)') NN
               IWFMT = '(I'//CHS//')'
               WRITE(IPROV,IWFMT) NSAME
               FMT = FMT(:N-2)//','//IPROV(:NN)//CHOLD
               N=N+NN
               NSAME=1
            ENDIF
            FMT = FMT(:N)//','//STRING(I:I)
            N = N+2
         ELSE
            NSAME = NSAME+1
         ENDIF
         CHOLD = STRING(I:I)
 10   CONTINUE
C
      STRING = FMT(3:N-2)
      FMT = HEAD//'('//STRING(1:N-4)//')'
C
 996  CONTINUE
      END
#endif
