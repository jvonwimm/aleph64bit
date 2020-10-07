      SUBROUTINE BBEPRD(LUN,IACM,NRECA,NRECR,NSEGA,NSEGR,
     +                  MODE,NHEDA,IHEDA,IBUF,IER)
*     this subroutine handles the modern and beautiful CERN standard
*     IO package EPIO - input
*
*     EPIO read / skip
*
*        LUN     = unit
*        IACM    = 0 sequential = 1 direct access    = 2 special
*        NRECA   = actual record number                       updated
*        NRECR   = number of record, to be read
*        NSEGA   = actual segment number                      updated
*        NSEGR   = number of segment, to be read
*        MODE    = EPIO mode = 30 or 20 or 13
*        NHEDA   = length of header or data            -      defined
*        IHEDA   = header or data area                 -       filled
*        IBUF    = buffer area                         -       filled
*        IER     = EPIO error code                     -      defined
*
      CHARACTER*4 CHAINT
      INTEGER IHEDA(*),IBUF(*)

      JACM=IACM
*     only standard EPIO sequential access available so far
C     IF(IACM.EQ.1) JACM=0
      IF(IACM.EQ.2) JACM=0
      IF(JACM.EQ.0) THEN
*        sequential
         NREC=NRECA
         NSEG=NSEGA
         IF(MODE.EQ.30) THEN
*           read physical record header, ignoring NRECR
            CALL EPREAD(LUN,30,NHEDA,IHEDA,IBUF,IER)
            CALL EPGETW(LUN,11,NREC,IDUMER)
            NRECA=NREC
            NSEGA=0
         ELSE IF(MODE.EQ.20) THEN
*           logical record header
            IF(NRECR.LT.NREC) THEN
*              rewind because of reverse order
               CALL EPRWND(LUN,IBUF,IER)
               NREC=0
               NSEG=0
            ELSE IF(NRECR.EQ.NREC) THEN
*              check within record for segment
               IF(NSEGR.LE.NSEGA) THEN
                  CALL EPRWND(LUN,IBUF,IER)
                  NREC=0
                  NSEG=0
               END IF
            END IF
   10       IF(NRECR.GT.NREC) THEN
*              read next physical record
               CALL EPREAD(LUN,30,NHEDA,IHEDA,IBUF,IER)
               CALL EPGETW(LUN,11,NREC,IDUMER)
               NSEG=0
               GOTO 10
            END IF
   20       NRECD=NREC
            CALL EPREAD(LUN,MODE,NHEDA,IHEDA,IBUF,IER)
            CALL EPGETW(LUN,11,NREC,IDUMER)
            IF(NREC.NE.NRECD) NSEG=0
            IF(IER.NE.0) GOTO 100
            NSEG=NSEG+1
C           IF(NSEGR.GT.NSEG) GOTO 20
            CALL EPGETW(LUN,11,NREC,IDUMER)
            NRECA=NREC
            NSEGA=NSEG
         ELSE IF(MODE.EQ.13) THEN
            CALL EPREAD(LUN,MODE,NHEDA,IHEDA,IBUF,IER)
         END IF

      ELSE IF(JACM.EQ.1) THEN
*        direct access
         IF(MODE.EQ.30) THEN

            IF(NRECR.NE.0) THEN
C              read new EPIO record directly
              CALL EPDACR(LUN,NRECR,4,IER)
              CALL EPREAD(LUN,MODE,NHEDA,IHEDA,IBUF,IER)
              NRECA=NRECR
              NSEGA=0
              NSEG=0
            ELSE
C               1. time one has to define record length
            LHREC=IHEDA(1)
C            record length comes from BUNIT
            CALL EPSETW(LUN,1,LHREC,IER)
            ENDIF
         ELSE IF(MODE.EQ.20) THEN
           IF(NRECA.NE.NRECR) THEN  
C              read new EPIO record directly
            CALL EPDACR(LUN,NRECR,4,IER)
            CALL EPREAD (LUN,30 ,NHEDA,IHEDA,IBUF,IER)  
            NRECA=NRECR 
            NSEGA=0 
            NSEG=0  
          ENDIF 
 120        CALL EPREAD (LUN,MODE,NHEDA,IHEDA,IBUF,IER)
            IF(IER.NE.0) GOTO 100
            NSEG=NSEG+1
*              check within record for segment
            IF(NSEGR.GT.NSEG) GOTO 120
            NRECA=NRECR
            NSEGA=NSEGR
         ELSE IF(MODE.EQ.13) THEN
            CALL EPREAD(LUN,MODE,NHEDA,IHEDA,IBUF,IER)
         ENDIF
      ELSE IF(JACM.EQ.2) THEN
*        special access - TO BE CODED BY EXPERT
      END IF
  100 RETURN
      END
