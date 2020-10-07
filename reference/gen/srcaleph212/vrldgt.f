      SUBROUTINE VRLDGT( IER)
C!-------------------------------------------------------------------
C! fill /VRLDCOM/ from VRLD data base bank
C! Extract the parameters dealing with the effects of multiple
C! scattering from the data base bank VRLD
CKEY VDET TRACK
C!
C!    AUTHOR: G. Taylor   22/6/95
C!
C!    OUTPUT: See description of VRLD bank for details
C!            IER= 0  ok
C!            IER=-1 database bank not found
C!-------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C --
      INTEGER JVRLBR,JVRLBL,JVRLIR,JVRLIL,JVRLIG,JVRLIT,JVRLTL,JVRLTG,
     +          JVRLZI,JVRLZO,JVRLII,JVRLIO,JVRLOI,JVRLOO,JVRLSI,JVRLSO,
     +          LVRLDA
      PARAMETER(JVRLBR=1,JVRLBL=2,JVRLIR=3,JVRLIL=4,JVRLIG=5,JVRLIT=6,
     +          JVRLTL=7,JVRLTG=8,JVRLZI=9,JVRLZO=10,JVRLII=11,
     +          JVRLIO=12,JVRLOI=13,JVRLOO=14,JVRLSI=15,JVRLSO=16,
     +          LVRLDA=16)
C! multiple-scattering constants in VDET,ITC,TPC
      COMMON /VRLDCOM/  UKRITC,UKSITC,UKRTPC,UKSTPC,UKSPITC,UKSPTPC,
     &                  UKRVAC,UKSVAC,UKZICA,UKRIICA,UKROICA,UKSICA,
     &                  UKZOCA,UKRIOCA,UKROOCA,UKSOCA
      REAL UKRITC,UKSITC,UKRTPC,UKSTPC,UKSPITC,UKSPTPC,UKRVAC,UKSVAC,
     &     UKZICA,UKRIICA,UKROICA,UKSICA,UKZOCA,UKRIOCA,UKROOCA,UKSOCA
C --
      INTEGER GTSTUP, ALGTDB
      INTEGER NVDRL,IROLD,IVDOLD
      DATA NVDRL,IROLD,IVDOLD/0,0,-9/
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
C
C ----------------------------------------------------------------
C-
C get the multiple scattering  material description from the database
C-
      IER=0
      IF (NVRLD .EQ.0) NVRLD = NAMIND('VRLD')
      CALL ABRUEV (IRUN,IEVT)
C
      IF (IRUN.NE.IROLD) THEN
       IROLD = IRUN
       IVDSTP = GTSTUP ('VD',IRUN)
C
       IF (IVDSTP.NE.IVDOLD) THEN
        IVDOLD = IVDSTP
        IRET= ALGTDB(JUNIDB(0),'VRLD',-IVDSTP)
        JVRLD = IW(NVRLD)
C
        IF(JVRLD.LE.0) THEN
         CALL ALTELL('VRLDGT:  Needs a VRLD bank - JOB SHOULD STOP',0,
     &               'RETURN')
         IER=-1
        ELSE
C
C beam pipe
C
          UKRVAC     = RTABL(JVRLD,1,JVRLBR)
          UKSVAC     = RTABL(JVRLD,1,JVRLBL)
C
C ITC/TPC part
C
          UKRITC     = RTABL(JVRLD,1,JVRLIR)
          UKSITC     = RTABL(JVRLD,1,JVRLIL)
          UKSPITC    = RTABL(JVRLD,1,JVRLIG)
          UKRTPC     = RTABL(JVRLD,1,JVRLIT)
          UKSTPC     = RTABL(JVRLD,1,JVRLTL)
          UKSPTPC    = RTABL(JVRLD,1,JVRLTG)
C
C vdet
C
          UKZICA     = RTABL(JVRLD,1,JVRLZI)
          UKZOCA     = RTABL(JVRLD,1,JVRLZO)
          UKRIICA    = RTABL(JVRLD,1,JVRLII)
          UKRIOCA    = RTABL(JVRLD,1,JVRLIO)
          UKROICA    = RTABL(JVRLD,1,JVRLOI)
          UKROOCA    = RTABL(JVRLD,1,JVRLOO)
          UKSICA     = RTABL(JVRLD,1,JVRLSI)
          UKSOCA     = RTABL(JVRLD,1,JVRLSO)
        ENDIF
       ENDIF
      ENDIF
C
      END
