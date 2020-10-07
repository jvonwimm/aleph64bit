#if defined(KINETEST)
      SUBROUTINE ASKUSI(IGCOD)
C--------------------------------------------------------------------
C        Author : B. Bloch-Devaux sept. 87
C! Init routine example #2
CKEY KINE KINGAL EXAMPLE   /USER
C     structure : subroutine
C
C     input     : none
C
C     output    : generator code IGCOD as defined in the KINGAL library
C  Example of a more complex generator :
C  -internal particle numbering is not Aleph one
C  -extra particles are needed by the generator
C  -some decay was allowed in the generator
C--------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      SAVE
      INTEGER ALTABL
      PARAMETER( LUSGE=60)
      COMMON/GENMAS/ GMAS(LUSGE),GCHA(LUSGE),GTLIF(LUSGE),IDECAY(LUSGE)
      DIMENSION GPAR(10)
      PARAMETER (LPDEC=48)
      INTEGER NODEC(LPDEC)
      CHARACTER TNAM*12
      COMMON/GENNAM/USNAME(LUSGE)
      CHARACTER*12 USNAME
      DIMENSION IUSAL(LPDEC)
C    IGCOD is 2001 for this generator
      PARAMETER ( IGCO  =  2001 )
      DATA IUSAL/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19, 20,21,
     +22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38, 39,40,41,42,
     +43,44,45,46,47, 48/
#include "bmacro.h"
#include "kmacro.h"
      LENTRY = 1
C
C   Return generator code IGCOD
C
      IGCOD = IGCO
      NAPAR=NAMIND('PART')
C
      KNOTR=ITABL(IW(NAMIND('KRUN')),1,2)
C
C
C  Force generator's masses for standard particles to be Aleph ones
C  and build KLIN bank for the Aleph particles ( array IUSAL contains
C   corresponding user particle codes)
      NMAX=LROWS(IW(NAPAR))
      DO 10 I=1,NMAX
         JKLIN=KBKLIN(I,IUSAL(I))
         IF (JKLIN.LE.0) GO TO 40
         GMAS(I)=PARMAS(I)
   10 CONTINUE
C
C  Add extra particles to PART bank and extend KLIN bank
C   the generator knows LUSGE particles
C
      DO 20 I=1,LUSGE
         IF (IUCOMP(I,IUSAL,ILUAL).GT.0) GO TO 20
C   get name,mass,charge,life time of current particle
C   from common or by function call , as prefered by your generator
C
         CHAR=GCHA(I)
         TNAM=USNAME(I)
         TLIF=GTLIF(I)
         ZMAS=GMAS(I)
C   store new part ( which will never be tracked later by GALEPH)
         IPART=KBPART(KNOTR,TNAM,KNOTR,ZMAS,CHAR,TLIF)
         IF (IPART.LE.0) GO TO 40
         JKLIN=KBKLIN(IPART,I)
         IF (JKLIN.LE.0) GO TO 40
   20 CONTINUE
C
      CALL AUBPRS('PARTKLIN')
C
C   Inhibit decays
C
      MXDEC=KNODEC(NODEC,LPDEC)
      MXDEC=MIN(MXDEC,LPDEC)
      IF (MXDEC.GT.0) THEN
         DO 30 I=1,MXDEC
            IF (NODEC(I).GT.0) IDECAY(NODEC(I))=0
   30    CONTINUE
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
C  Then init the generator
C
C------------------------------------------------------------------
C      let's assume we init the generator by the following call
CCCC  CALL INIGEN(GPAR)
C----------------------------------------------------------------------
C
C   dump the generator parameters for this run in a bank
C assume all parameters are real and stored as a single row
C
      IND=ALTABL('KGPA',NPA,1,GPAR,'2I,F','C')
C  Print PART and KLIN banks
C
      CALL PRPART
C
      RETURN
   40 CONTINUE
      WRITE (IW(6),'(1X,''error  with PART or KLIN bank - STOP - ''
     +              ,2I3)') IPART,IKLIN
      RETURN
      END
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)
C--------------------------------------------------------------------
C        Author : B. Bloch-Devaux sept. 87
C! Process routine example #2
CKEY KINE KINGAL EXAMPLE   /USER
C  In this more complex case yo need:
C -generate the primary vertex coordinates
C -call your generator routine to generate one event
C - fill KINE , VERT , KHIS banks
C  using routine KFEVBK which will also propagate secondary vertices
C   according to proper life-time of decayed particle.
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
      INTEGER ALTABL
      COMMON /KGCOMM/ IST,ECM,WEI,IDP
      PARAMETER (LGEN=100)
      COMMON/GMOMEM/NTGEN,P(4,LGEN),IHIS(LGEN),IVX(2,LGEN),ICOD(LGEN)
      DIMENSION RPARA(4,LGEN),IPARA(3,LGEN)
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
C  You can use the status word to decide not to keep the event
C  as you may generate only part of the particles spectra
C
C----------------------------------------------------------------------
C     let's assume we get an event generated by the following call
CCCC  CALL EVGEN(LENTRY)
C
C   Book KINE banks for beam electrons
C   assuming they are in positions 1 and 2 in 4-momentum array
      IND=KBKINE(-1,P(1,1),2,0)
      JND=KBKINE(-2,P(1,2),3,0)
      IF (IND*JND.EQ.0) GO TO 20
      IF (NTGEN.LE.0) GO TO 20
C
C   Prepare data for KFEVBK
C
      DO 10 I=3,NTGEN
         RPARA(1,I-2)=P(1,I)
         RPARA(2,I-2)=P(2,I)
         RPARA(3,I-2)=P(3,I)
         RPARA(4,I-2)=0.
         IPARA(1,I-2)=IVX(1,I)
         IPARA(2,I-2)=IVX(2,I)
         IPARA(3,I-2)=KGPART(ICOD(I))
   10 CONTINUE
C
      IDPR = IDP
      NTRK = NTGEN
C     NVRT = max number of vertices to be booked
      ECMS = ECM
      WEIT = WEI
      CALL KFEVBK(VERT,RPARA,IPARA,NTGEN,ISTA)
      IF (ISTA.NE.0) GO TO 20
C    Fill the history bank KHIS
C
      IND=ALTABL('KHIS',1,NTRK,IHIS(3),'I','E')
      IF (IND.LE.0) GO TO 20
      ISTA = IST
      RETURN
   20 ISTA=1
      RETURN
      END
#endif
#endif
