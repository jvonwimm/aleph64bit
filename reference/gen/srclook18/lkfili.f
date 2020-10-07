C*EI
C*DK LKFILI
C*DF UNIX
      SUBROUTINE LKFILI(FNAME,FORMA,IER)
C-----------------------------------------------------------------------
C! Add FILI bank  ,  Nr = 0
C  Entries:   LKFILO
C ---------------------------------------------------------------------
C*IF .NOT.DOC
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C*CC BCS
        CHARACTER*200 FFNAM
        CHARACTER*(*) FNAME,FORMA
        CHARACTER*4 NAME
        INTEGER ACARD1
        LOGICAL LEX
        DATA IBLK /0/
        DATA IV/0/,IVMAX/0/,IR/-1/,IROLD/0/
C -------------------------------------------------------------------
C
        IF (FNAME.EQ.'    ') GOTO 999
C
        IF (FORMA.NE.' ') THEN
           LE  = LNBLNK(FNAME)
           IFO = INDEX(FNAME(1:LE),FORMA(1:3))
           IF (IFO.EQ.0) THEN
              IF (INDEX(FNAME(1:LE),' ').EQ.0  .AND.
     &            INDEX(FNAME(1:LE),'.').EQ.0) THEN
                 FNAME = FNAME(1:LE)// ' '//FORMA(1:4)
              ENDIF
           ENDIF
        ENDIF
C
        NAME='FILI'
        GOTO 20
C
        ENTRY LKFILO(FNAME,FORMA,IER)
C-----------------------------------------------------------------------
        NAME='FILO'
   20   IER=0
        IF (IBLK.EQ.0) THEN
           IBLK = INTCHA ('    ')
        ENDIF
C
        IND= ACARD1(' ')
        IND= NDROP (NAME,0)
        IND= NBANK (NAME,0,80)
C
        LE = LNBLNK(FNAME)
        DO  IBG=1,LE,4
          I=(IBG+3)/4
          IW(IND+I)=IBLK
          IW(IND+I)=INTCHA(FNAME(IBG:IBG+3) )
        ENDDO
C
        IF(FORMA.NE.' ') THEN
          IW(IND+I+1)=INTCHA(' |  ')
          IW(IND+I+2)=INTCHA(FORMA(1:4))
          I = I+2
        ENDIF
        IND = NBANK (NAME,0,I)
C
 999    CONTINUE
        END
