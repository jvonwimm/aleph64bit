       SUBROUTINE FYKILL (IBFUL)
C-----------------------------------------------------------------------
CKEY FYXX  / INTERNAL
C! Tests which KINE/VERT banks are to be killed
C J. Boucrot  880525
C Modified: S Wasserbaech, 980312  Don't kill vertices in TPC passive
C    material at large angles (membrane et al., resistor chain, laser
C    mirrors and prisms)
C
C - Output argument : IBFUL = -1 if not enough BOS space
C
C - Input banks   : INDKIN, INDVER
C - output banks  : JDKEKS, JDVNFO
C   1 word / KINE or VERT bank  = 0 if the KINE or VERT is killed
C                                 new serial # if KINE or VERT is kept
C Called from FYKINE                           from ALEPHLIB
C -------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "fyrela.h"
      EXTERNAL NAMIND,CHAINT
      PARAMETER ( TANGM = 0.23 )
      CHARACTER*4 NAMVO,CHAINT,NAMVL,NAMV
      CHARACTER*1 FIRSL,FLVOR,FIRS
      LOGICAL INTRK,INCAL,INPIP,INITC,INTPC,FKKIL,FVKIL,NOMU
      LOGICAL FTINO,FDAU,FINLU
C
#include "bmacrod.h"
      DATA NKVOL /0/
      DATA EPSIL / 1.E-09/
C
      NOMU(I)=I.LT.5.OR.I.GT.6
      INPIP(FIRS)=FIRS.EQ.' '.OR.FIRS.EQ.'B'
      INTRK(FIRS)=FIRS.EQ.'V'.OR.FIRS.EQ.'I'.OR.FIRS.EQ.'T'
      INCAL(FIRS,NAMV)=(FIRS.EQ.'E'.OR.FIRS.EQ.'C'.OR.
     +                  FIRS.EQ.'L'.OR.FIRS.EQ.'S'.OR.
     +                  FIRS.EQ.'M'.OR.FIRS.EQ.'Q'.OR.
     +                  FIRS.EQ.'H') .AND.(NAMV.NE.'CDET')
      INITC(NAMV)=NAMV.EQ.'ITC '.OR.NAMV.EQ.'ITWI'.OR.
     +            NAMV.EQ.'ITWO'.OR.NAMV.EQ.'ITCR'
C ---------------------------------------------
C     Modified: SRW, 980312:
      INTPC(NAMV) = (NAMV .EQ. 'TPC ') .OR.
     >              (NAMV .EQ. 'TPHF') .OR.
     >              (NAMV .EQ. 'TPWI') .OR.
     >              (NAMV(1:3) .EQ. 'TPM') .OR.
     >              (NAMV(1:3) .EQ. 'TPR') .OR.
     >              (NAMV(1:2) .EQ. 'TM')
C ---------------------------------------------
      FINLU(NAMV)=NAMV(1:2).EQ.'LC '.OR.NAMV(1:2).EQ.'SI'
#include "bmacro.h"
#include "kmacro.h"
      FKKIL(NT) = IW(KROW(JDKEKS,NT)+1) .EQ. 0
      FVKIL(NV) = IW(KROW(JDVNFO,NV)+1) .EQ. 0
      FTINO(JVK)= KINTYP(JVK).GE.48 .AND. KINTYP(JVK).LE.52
C ----------------------------------------------------------------------
       IF (NKVOL .EQ. 0) THEN
          NKVOL = NAMIND ('KVOL')
       ENDIF
C
C - book JDKEKS and JDVNFO
      IBFUL = 0
      CALL WBANK (IW,JDKEKS,IW(INDKIN)+LMHLEN,*990)
      IW(JDKEKS+LMHCOL) = 1
      IW(JDKEKS+LMHROW) = IW(INDKIN)
      CALL WBANK (IW,JDVNFO,IW(INDVER)+LMHLEN,*990)
      IW(JDVNFO+LMHCOL) = 1
      IW(JDVNFO+LMHROW) = IW(INDVER)
C
C - fill JDKEKS and JDVNFO with KINE# and VERT#
      DO 1 IT =1,IW(INDKIN)
 1       IW(JDKEKS+LMHLEN+IT) = IT
      DO 2 IV =1,IW(INDVER)
 2       IW(JDVNFO+LMHLEN+IV) = IV
C
C ======================= end of initialization ===================
C
C Loop over all mother tracks to kill geantinos and the related vertices
      DO 20 IMOM = 1,IW(INDKIN)
         JKINE = IW(INDKIN+IMOM)
#if defined(FYDEB)
         WRITE(6,*) 'IMOM NOFVK ',IMOM,NOFVK(JKINE)
#endif
         IF (NOFVK(JKINE) .GT. 2) THEN
            DO 22 IV=2,NOFVK(JKINE)
               NVG = IW(KLISVK(JKINE)+IV)
               JVERT = IW(INDVER+NVG)
C              find if the daughter is a geantino (FDAU=.T.)
               FDAU = .FALSE.
#if defined(FYDEB)
               WRITE(6,*) 'NVG NOFVK ',NVG,NOFVK(JVERT)
#endif
               IF (NOFVK(JVERT).EQ.1) THEN
                  NKI = IW(KLISVK(JVERT)+1)
                  JKI = IW(INDKIN+NKI)
                  FDAU = FTINO(JKI)
#if defined(FYDEB)
                  WRITE(6,*) 'NKI KINTYP FDAU ',NKI,KINTYP(JKI),FDAU
#endif
               ENDIF
C              if the daughter is a geantino then kill other GEANTINO
C              vertices
               IF (FDAU) THEN
                   DO 21 IVG = 2,NOFVK(JKINE)
                      IF (IVG.EQ.IV)  GOTO 21
                      JVERT = IW(INDVER+IW(KLISVK(JKINE)+IVG))
                      IF (NOFVK(JVERT).EQ.1) THEN
                         NKI = IW(KLISVK(JVERT)+1)
                         JKI = IW(INDKIN+NKI)
                         FDAU = FTINO(JKI)
                         IF (.NOT.FDAU)  GOTO 21
                         NVG = IW(KLISVK(JKINE)+IVG)
#if defined(FYDEB)
                         WRITE(6,*) 'NVG= ',NVG
#endif
                         IW(KROW(JDVNFO,NVG)+1) = 0
                      ENDIF
 21               CONTINUE
                  GOTO 20
               ENDIF
 22         CONTINUE
         ENDIF
 20   CONTINUE
C
C - loop on all generated vertices
C
      JKVOL=IW(NKVOL)
      DO 5 NUMV = KINVER+1,IW(INDVER)
         JVERT = IW(INDVER+NUMV)
         IF (JVERT.EQ.0) GOTO 5
C Begin tests on vertices :
C -------------------------
C If the "mother track" of this vertex was already thrown , this vertex
C must also be thrown away :
         IDROV=0
         MOTRK=INPTRK(JVERT)
         IF (FKKIL(MOTRK))  THEN
            IDROV=1
            GO TO 3
         ENDIF
C Find first letter of the vertex volume ; if it is inside any
C calorimeter , keep it but kill all outgoing tracks
         FLVOR=' '
         NAMVO=' '
         IF (JKVOL.NE.0) THEN
            IVIN=ITABL(JKVOL,NUMV,1)
            NAMVO=CHAINT(IVIN)
            FLVOR=NAMVO(1:1)
         ENDIF
         IF (FSHDRO.AND.INCAL(FLVOR,NAMVO)) THEN
            IDROV=2
            IF (FINLU(NAMVO)) IDROV=3
            GO TO 10
         ENDIF
C If the vertex occured in ITC/TPC end plate or electronics ,keep it
C but kill all outgoing tracks
         IF (FTKDRO) THEN
            IF (FLVOR.EQ.'T'.AND..NOT.INTPC(NAMVO)) IDROV=2
            IF (FLVOR.EQ.'I'.AND..NOT.INITC(NAMVO)) IDROV=2
         ENDIF
C If this vertex was tagged previously with IDROV=2 , all outgoing track
C must be thrown away
         IF (FVKIL(NUMV)) IDROV=2
C Throw away the vertex if necessary
  3       CONTINUE
         IF (IDROV.EQ.1) IW(KROW(JDVNFO,NUMV)+1) = 0
C
C  Loop on all outgoing tracks for the current vertex :
C
 10      DO 100 ITK=1,NOFVK(JVERT)
            NTRAK=IW(KLISVK(JVERT)+ITK)
            JKINE=IW(INDKIN+NTRAK)
            IF (JKINE.EQ.0) GO TO 100
C
C           Do not drop primary tracks ( from KINGAL generator list )
C
            IF (NTRAK.LE.KINTRK) GO TO 100
            ITYPA=KINTYP(JKINE)
C
C           Drop all daughter particles coming from vertex # NUMV ,
C           if the flag IDROV is .NE.0 .An exception must be made for
C           muons if their mother vertex was kept ( IDROV = 2 ) ,
C           since muons may pass through the calorimeters .
            IF (IDROV.EQ.1.OR.IDROV.EQ.3.OR.
     +         (IDROV.EQ.2.AND.NOMU(ITYPA))) THEN
               IW(KROW(JDKEKS,NTRAK)+1) = 0
C              Kill all vertices generated by this track
               DO 101 IV = 2,NOFVK(JKINE)
                  NV = IW(KLISVK(JKINE)+IV)
                  IW(KROW(JDVNFO,NV)+1) = 0
 101           CONTINUE
            ELSE
C        Find first letter of the volume name of the first
C        generated vertex if any
               NAMVL=' '
               FIRSL=' '
               IF (NOFVK(JKINE).GE.2) THEN
                  IVFIR=IW(KLISVK(JKINE)+2)
                  IF (IVFIR.EQ.NUMV) GO TO 100
                  IF (IVFIR.NE.0.AND.JKVOL.NE.0) THEN
                     INFIR=ITABL(JKVOL,IVFIR,1)
                     NAMVL=CHAINT(INFIR)
                     FIRSL=NAMVL(1:1)
                  ENDIF
               ENDIF
C
C Begin tests on the current track:
C ---------------------------------
C
C First test : Low-momentum particle coming from a vertex in tracking
C devices : throw away the track and all its daughter vertices
C
               PTRAK=PMODVK(JKINE)
               IF (PTRAK.LT.CUTRAC) THEN
                  IW(KROW(JDKEKS,NTRAK)+1) = 0
                  DO 102 IV = 2,NOFVK(JKINE)
                     NV = IW(KLISVK(JKINE)+IV)
                     IW(KROW(JDVNFO,NV)+1) = 0
 102              CONTINUE
               ELSE
C
C Second test : "primary particle" ( from vertex inside BPIP or VDET )
C with low angle and first interaction in tracking devices :
C keep the track but throw away all its daughter vertices :
C
                  PZTRA=RW(KPARVK(JKINE)+3)
                  IF (FTKDRO.AND.ABS(PZTRA).GT.EPSIL) THEN
                     PTTRA=SQRT((PTRAK+PZTRA)*(PTRAK-PZTRA))
                     TANG=ABS(PTTRA/PZTRA)
                     IF (INPIP(FLVOR).AND.INTRK(FIRSL).AND.
     +                   TANG.LT.TANGM) THEN
C                    kill all the vertices generated by this track
C                    but the 1st one
                        DO 103 IV = 3,NOFVK(JKINE)
                           NV = IW(KLISVK(JKINE)+IV)
                           IW(KROW(JDVNFO,NV)+1) = 0
 103                    CONTINUE
                     ENDIF
                  ENDIF
C
C Third test : First generated vertex is in calorimeters : kill all
C vertices generated by this track , but the first one ( to know where
C the track ends )
C
                  IF (FSHDRO .AND. INCAL(FIRSL,NAMVL)) THEN
C                 kill all the vertices generated by this track
C                 but the 1st one
                     DO 104 IV = 3,NOFVK(JKINE)
                        NV = IW(KLISVK(JKINE)+IV)
                        IW(KROW(JDVNFO,NV)+1) = 0
 104                 CONTINUE
                  ENDIF
               ENDIF
            ENDIF
 100     CONTINUE
 5    CONTINUE
C
C ========================= end of selection ==========================
C
C - Count # of good tracks and vertices
      LTKNUM = 0
      KKEKS = JDKEKS + LMHLEN
      DO 200 IT = 1,LROWS(JDKEKS)
         IF (IW(KKEKS+1) .GT. 0) THEN
            LTKNUM = LTKNUM + 1
            IW(KKEKS+1) = LTKNUM
         ENDIF
  200 KKEKS = KKEKS + LCOLS(JDKEKS)
C
      LVXNUM = 0
      KVNFO = JDVNFO + LMHLEN
      DO 201 IV = 1,LROWS(JDVNFO)
         IF (IW(KVNFO+1) .GT. 0) THEN
            LVXNUM = LVXNUM + 1
            IW(KVNFO+1) = LVXNUM
         ENDIF
  201 KVNFO = KVNFO + LCOLS(JDVNFO)
C
      RETURN
C - not enough space
 990  CONTINUE
      IBFUL = -1
      END
#endif
