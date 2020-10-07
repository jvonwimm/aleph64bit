      SUBROUTINE AOPEWR(LINDAT, LUTSEL, LUTDAT, IER)
C-----------------------------------------------------------------------
CKEY ALREAD OPEN EDIR
C  Author:     D.Schlatter                          April  89
C  Modified by M.Talby                              August 89
C  Modified by M.Talby                              May    90
C
C! OPENs output data files (EPIO, NATIVE) or event directory files
C  (TEXT) in WRITE mode. Multiple output cards go on parallel output
C  streams (=MOUTPT). Returns logical output units.
C  Steering is done with FILO,POT,DST,MDST cards as defined in AIOCAR
C
C-----------------------------------------------------------------------
C  Input :     LINDAT  /I   logical unit for existing data file
C                            ( only with new select file needed)
C  Output:     LUTSEL  /I   logical unit for new select file
C                           only one output select file stream allowed
C              LUTDAT  /I   logical unit for new  data  file
C                           up to MOUTPT parallel streams possible
C              ICASE   /I = 0    no select files to write
C                         = 1    select file refers to input data
C                                which is the input stream # 1 defined
C                                on the FILI card
C                         = 2    select file refers to output data
C                                which is the output stream # defined
C                                in this routine
C              IER     /I = 0    successful opening
C
C  Called by: ABOPEN
C  CALLS:     AFILOU,AOPENW,BKFMT,BUNIT,BWRSB,NSWAP,JUNIAL
C
C  ENTRY AOPEWW (JWITH): returns IFWITH in JWITH
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "alrcom.h"
      CHARACTER*80 FEDIR
      CHARACTER*80 NAMSEL,FNAMP,FDEV,FNAME
      CHARACTER*50 CPARA
      CHARACTER*4  CARIN(MINPUT),CAROUT(MOUTPT)
      CHARACTER*6  FTYPP,FTYPE
      CHARACTER*4  CHAINT,BFORMA,NAME
      CHARACTER*3  FTYP3
      INTEGER      LUTDAT(*),LINDAT(*),LUTSEL(*),ISAME
      INTEGER      ACARD0,ACARD3
      LOGICAL FIRST, FSTOP
      DATA FIRST/.TRUE./

      IF(FIRST) THEN
        FIRST = .FALSE.
        JRECL = JALREC(0)
        IRET=0
        ICASE = 0
        FNAMP = ' '
        FTYPP = ' '
        IFWITH = 0
        CALL AIOCAR(CARIN,MINPUT,CAROUT,MOUTPT)
        CALL BKFMT ('FILM','68A')
      ENDIF
C
C --
C - next entry
C --
C     Analize the new output streams if any
C
      IO = 0
      ICARD  = 0
 50   ICARD = ICARD + 1
C
C        get an output file card if any
      IER = 0
      CALL ACDARG (CAROUT(ICARD),'NATIVE','A',FNAME,FTYPE,FDEV,IRFILO)
      IF (IRFILO.GT.0) THEN
         IF (IW(6).GT.0) CALL AWERRC (IW(6),'ACDARG',FNAME,IRFILO)
         IER = IRFILO
         GOTO 999
      ENDIF
C
C        IF there is no more card FILO/POT/.. (IRFILO=-1) AND the case
C               of the previous output file was 0 (no EDIR file) THEN
C           look for another card, do not increase the stream # IO
C        ELSEIF no more FILO card AND the case of the previous card
C               was 1 (EDIR on input file) THEN
C           increase the stream # IO
C           try to write another FILM card on the same EDIR file (ISAME=1)
C           skip the setting part of the EDIR file
C        ELSEIF no more FILO card AND the case of the previous card
C               was 2 (EDIR on output file) THEN
C           RETURN with the logical units numbers defined as before
C        ENDIF
C
         ISAME = 0
         IF(IRFILO.EQ.-1) THEN
            IF(ICASE.LE.1) GOTO 40
            IF(ICASE.EQ.2) RETURN
            ISAME = 1
            IER = 0
            FNAME = FNAMP
            FTYPE = FTYPP
            IO = IO+1
         ELSE
            IO = IO+1
            FNAMP = FNAME
            FTYPP = FTYPE
            IFWITH=ACARD3(2,'WITH',CPARA)
            IF(IFWITH.NE.0) FEDIR=CPARA(6:IFWITH)
            NAFILO(IO) = CAROUT(ICARD)
            NAMI = ACARD0 (NRFILO(IO))
         ENDIF
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         IF(FNAME(1:1).EQ.' ' .AND. FTYPE.NE.'    ')THEN
C           FILO card given with no filename
C           lets assume that filedef or  assign is used.
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            IER = 0
            LUTDAT(IO) = JUNIAL( 'OUT', 2*IO-1,0 )
            LUNIT = LUTDAT(IO)
            BFORMA='FORT'
            IF(FTYPE(1:3).EQ.'EPI') THEN
               LREC = JRECL/2
               BFORMA='EPIO'
            ELSEIF(FTYPE(1:3).EQ.'EDI') THEN
               LUTSEL(IO) = JUNIAL( 'OUT', 2*IO,0 )
               LUNIT = LUTSEL(IO)
               BFORMA='TEXT'
               LREC = 80
            ENDIF
            CALL BUNIT(LUNIT,BFORMA,LREC)
C
C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         ELSEIF (FNAME(1:1).NE.' ') THEN
C           FILO card is given
C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
            IER = 0
            ICASE = 0
C --
C          =====================================================
            IF(FTYPE(1:3).EQ.'EDI'.OR.IFWITH.NE.0) THEN
C             select file should be written
C          =====================================================
C
C--  Select file refers to output: ICASE=2
C    write an output file WITH an event dir. related to it
C    FILO fnout.ftout | atyp | WITH fnout.EDIR
              IF(IFWITH.NE.0) THEN
                ICASE = 2
                IEDIR = INDEX(FEDIR,'EDIR')
                NAMSEL= FEDIR(1:IEDIR+4)//GMODEN
              ELSE
C----Select file refers to input: ICASE=1
C    read an input file defined by a FILI fnin.ftin
C    write an event directory related to it
C    FILO   fnout.EDIR
                ICASE = 1
                NAMSEL= FNAME
              ENDIF
C
C----Units for selectfiles are from even indices
              LUTSEL(IO) = JUNIAL('OUT',2*IO,0)
C----Open select file ( write mode )
              IF(ISAME.EQ.0) THEN
                CALL AOPENW(LUTSEL(IO),NAMSEL,'EDIR',FDEV,IER)
                IF(IER.NE.0) THEN
                  IF (IW(6).GT.0) WRITE(IW(6),*)
     &               '_AOPEWR_CANNOT open output sel. file'
                  LUTSEL(IO) = 0
                  GOTO 999
                ENDIF
              ENDIF
C
            ENDIF
C
C         =====================================================
            IF(FTYPE(1:3).NE.'EDI') THEN
C             output data file should be written
C         =====================================================
C
C----Units for data are from odd  indices
              LUTDAT(IO) = JUNIAL( 'OUT', 2*IO-1, 0 )
              CALL AOPENW(LUTDAT(IO),FNAME,FTYPE,FDEV,IER)
#if defined(ARDEB)
      WRITE(6,*) ' *=*AOPEWR FDEV=',FDEV,'<=FT=>',FTYPE,'<=FN=>',FNAME
#endif
              IF(IER.NE.0) THEN
                IF (IW(6).GT.0) WRITE(IW(6),*)
     &            '_AOPEWR_  CANNOT open output data file'
                LUTDAT(IO) = 0
                GO TO 999
              ENDIF
            ENDIF
C
C         ======================================================
            IF (IFWITH.NE.0) THEN
C             write EDIR file for output data: write FILM bank
C         =====================================================
              CALL CRFILM
            ENDIF
C ++++++++++++++++++++++++++++++++++++++++++++++++++
         ENDIF
C ++++++++++++++++++++++++++++++++++++++++++++++++++
C --
C     look for another output file if no EDIR are required
      IF (ICASE.NE.0) GOTO 999
C
  40  CONTINUE
C --
C     look for an other output data card if any left
      IF (ICARD.LT.MOUTPT) GOTO 50
C --
  999 RETURN
C ==========================================================
      ENTRY AOPEWW (JFWITH)
      JFWITH = IFWITH
      END
#endif
