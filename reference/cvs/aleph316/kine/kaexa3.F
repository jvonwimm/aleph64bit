#if defined(KINETEST)
      SUBROUTINE ASKUSI(IGCOD)
C--------------------------------------------------------------------
C        Author : B. Bloch-Devaux sept. 87
C! Init routine example #3
CKEY KINE KINGAL EXAMPLE   /USER
C     structure : subroutine
C
C     input     : none
C
C     output    : generator code IGCOD as defined in the KINGAL library
C  Example of a Lundlike   generator :
C  -internal particle numbering is not Aleph one
C  -extra particles are needed by the generator
C  -some decay was allowed in the generator
C--------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      SAVE
      INTEGER ALTABL
#include "lundcom.h"
      PARAMETER (LPDEC=48)
      INTEGER NODEC(LPDEC)
C
C    IGCOD is 3001 for this generator
      PARAMETER ( IGCO  =  3001 )
      DIMENSION GPAR(20)
      DATA GPAR / 20*1./
C
C   Return generator code IGCOD
C
      IGCOD = IGCO
      CALL KXLUPA (IPART,IKLIN)
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN
         WRITE (IW(6),'(1X,''error in PART or KLIN bank - STOP - ''
     +                 ,2I3)') IPART,IKLIN
         GOTO 20
      ENDIF
C
C   Inhibit decays
C
      MXDEC=KNODEC(NODEC,LPDEC)
      MXDEC=MIN(MXDEC,LPDEC)
      IF (MXDEC.GT.0) THEN
         DO 10 I=1,MXDEC
            IF (NODEC(I).GT.0) IDBLU3(NODEC(I))=0
   10    CONTINUE
      ENDIF
C
C   get Generator parameters ( from a data card if you want
C    or by default values if you prefer)
C
      NAPARA=NAMIND('PARA')
      ID=IW(NAPARA)
      IF (ID.NE.0) THEN
         NPA = IW(ID)
         CALL UCOPY(IW(ID+1),GPAR(1),NPA)
      ENDIF
C
C   dump the generator parameters for this run in a bank
C assume all parameters are real and stored as a single row
C
      IND=ALTABL('KGPA',NPA,1,GPAR,'2I,F','C')
C  Print PART and KLIN banks
C
      CALL PRPART
C
   20 RETURN
      END
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)
C--------------------------------------------------------------------
C        Author : B. Bloch-Devaux sept. 87
C! Process routine example #3
CKEY KINE KINGAL EXAMPLE   /USER
C  In this Lund like case you need:
C -generate the primary vertex coordinates
C -call your generator routine to generate one event
C - fill KINE , VERT , KHIS banks
C  using routine KXLUAL which will do everything for you
C
C     structure : subroutine
C
C     input     : none
C
C     output    : 6 arguments
C          IDPRO  : process identification if meanigful
C          ISTAT  : status flag ( 0 means ok), use it to reject
C                   unwanted events
C          NTRK   : number of tracks generated and kept
C                  (i.e. # KINE banks  written)
C          NVRT   : number of vertices generated
C                   (i.e. # VERT banks written)
C          ECMS   : center of mass energy for the event (may be
C                   different from nominal cms energy)
C          WEIT   : event weight ( not 1 if a weighting method is used)
C--------------------------------------------------------------------
#include "bcs.h"
#include "lundcom.h"
      COMMON /KGCOMM/ IST,ECM,WEI,IDP
      DIMENSION SVERT(3),VERT(4),TABL(3)
      DATA IFI /0/
      LENTRY = 2
C
C  Generate vertex postion
C
      IF ( IFI.EQ.0 ) THEN
C  if you need the standard interaction point
C  you may get the sigmas of the gaussion smearing
C  from a data card if you like it
C
      NASVER=NAMIND('SVER')
      JSVER=IW(NASVER)
      IF (JSVER.NE.0) THEN
         SVERT(1)=RW(JSVER+1)
         SVERT(2)=RW(JSVER+2)
         SVERT(3)=RW(JSVER+3)
      ENDIF
      IFI = IFI+1
      ENDIF
      CALL RANNOR(RX,RY)
      CALL RANNOR(RZ,DUM)
      VERT(1)=RX*SVERT(1)
      VERT(2)=RY*SVERT(2)
      VERT(3)=RZ*SVERT(3)
      VERT(4)=0.
C
      IFL=0
C
      CALL LUGEN(LENTRY)
      IDPR = IDP
      CALL KXLUAL(VERT,IST,NVRT,NTRK)
      ECMS = ECM
      WEIT = WEI
C
C  You can use the status word to decide not to keep the event
C  as you may generate only part of the particles spectra
C
      ISTA = IST
      RETURN
   10 ISTA=1
      RETURN
      END
#endif
#endif
