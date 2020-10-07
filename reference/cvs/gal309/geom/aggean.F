*DK aggean
      SUBROUTINE AGGEAN (DET,ISTUP)
C------------------------------------------------------
C!  Implement detector  geometry
C - F.Ranjard - 901212
C - Input :   - DET     / CHA*2  : detector initials
C - Output:   - ISTUP   / I      : setup code
C.  -Called by AGEOME                  from this .HLB
C.  -Calls GSTMED,GSVOLU,GSPOS,GSROTM  from  GEANT3
C.
C. -Stores extra Tracking Media needed
C. -Builds geometry levels below  'CDET'levels for the VDET parts
C. -Initialises some search optimisation
C.
C------------------------------------------------
#ifndef DOC
      CHARACTER*(*) DET
      CHARACTER*4 CHAINT,TYPE
      CHARACTER*16 NAME,NAMX
      CHARACTER*20 NAMG
      INTEGER INAME(5)
      PARAMETER (LNAM=200, LCHNAM=16, LALL=LNAM*LCHNAM, NWRDS=4)
      CHARACTER*3200 ALLNAM
      INTEGER GTSTUP, ALGTDB
      PARAMETER (LCOMP=10)
      REAL AA(LCOMP),ZZ(LCOMP),WW(LCOMP),EPS(LCOMP)
      INTEGER LIS(LCOMP)
      CHARACTER*44 BKLIST
#include "gcbank.h"
#include "iocom.h"
#include "jobcom.h"
#include "bcs.h"
#include "agcons.h"
#include "agecom.h"
#include "alfgeo.h"
      PARAMETER(JMATMN=1,JMATAW=5,JMATAN=6,JMATDE=7,JMATRL=8,JMATAL=9,
     +          LMATEA=9)
      PARAMETER(JMIXMN=1,JMIXMT=5,JMIXDE=6,LMIXTA=6)
      PARAMETER(JMELVM=1,JMELEL=2,JMELNU=6,LMELEA=6)
      PARAMETER(JMMAVM=1,JMMAMA=2,JMMATH=6,LMMATA=6)
      PARAMETER(JGPAVT=1,JGPAPA=2,JGPAPV=4,LGPARA=4)
      PARAMETER(JPCOVV=1,JPCOZB=2,JPCORN=3,JPCORX=4,LPCONA=4)
      PARAMETER(JPOSDA=1,JPOSMO=2,JPOSFL=3,JPOSVR=4,JPOSCN=5,JPOSPO=6,
     +          LPOSIA=8)
      PARAMETER(JROTA1=1,JROTP1=2,JROTA2=3,JROTP2=4,JROTA3=5,JROTP3=6,
     +          LROTAA=6)
      PARAMETER(JTMETN=1,JTMEMA=5,JTMEFD=9,JTMEMS=10,JTMEDE=11,
     +          JTMEEP=12,JTMEST=13,JTMEVO=14,JTMEGP=15,JTMEFL=16,
     +          LTMEDA=16)
      PARAMETER(JVOLVO=1,JVOLSH=2,JVOLVT=3,JVOLNP=4,JVOLPA=5,LVOLUA=9)
      PARAMETER(JATTVO=1,JATTAN=2,JATTAV=3,LATTRA=3)
#include "bmacro.h"
C
C ----------------------------------------------------------------------
C
C - get the setup code
C
      ISTUP = 0
      ISTUP = GTSTUP (DET,1)
      IF (ISTUP.LE.0) THEN
         ALLNAM = ' '
         ALLNAM = 'AGGEAN: '//DET//' does not exist '
         CALL ALTELL (ALLNAM(1:LENOCC(ALLNAM)),0,'STOP')
      ENDIF

      BKLIST = ' MAT MIX MEL MMA TME GPA VOL PCO POS ROT ATT'
      DO I=1,LEN(BKLIST),4
         BKLIST(I:I) = DET(1:1)
      ENDDO
      IRET = ALGTDB (LRDBIO,BKLIST,ISTUP)
      JMAT = IW(NAMIND(DET(1:1)//'MAT'))
      JMIX = IW(NAMIND(DET(1:1)//'MIX'))
      JMEL = IW(NAMIND(DET(1:1)//'MEL'))
      JMMA = IW(NAMIND(DET(1:1)//'MMA'))
      JTME = IW(NAMIND(DET(1:1)//'TME'))
      JGPA = IW(NAMIND(DET(1:1)//'GPA'))
      JVOL = IW(NAMIND(DET(1:1)//'VOL'))
      JPCO = IW(NAMIND(DET(1:1)//'PCO'))
      JPOS = IW(NAMIND(DET(1:1)//'POS'))
      JROT = IW(NAMIND(DET(1:1)//'ROT'))
      JATT = IW(NAMIND(DET(1:1)//'ATT'))
C
      IF (DET.EQ.'GE') IGEST = ISTUP
      JGROT = NLINK('GROT',IGEST)
      JGTME = NLINK('GTME',IGEST)
      DO 10 L=1,LDET
        IF (DET .NE.TGEOJO(L)) GOTO 10
        JDET = L
        GOTO 20
  10  CONTINUE
      JDET = LDET+1
 20   CONTINUE
C
C - store additional materials needed by DET ========================
C
      INDMAT = IAGMAT
      IF (JMAT.NE.0) THEN
C
        DO 100 I=1,LROWS(JMAT)
          IAGMAT=IAGMAT+1
          KMAT = KROW(JMAT,I)
          CALL ALSTIN (IW(KMAT+JMATMN),NWRDS,NAME)
          CALL GSMATE (IAGMAT,NAME,RW(KMAT+JMATAW),RW(KMAT+JMATAN),
     &       RW(KMAT+JMATDE),RW(KMAT+JMATRL),RW(KMAT+JMATAL),0,0)
  100   CONTINUE
      ENDIF
C
C - get all existing materials, store them into ALLNAM
C
      NMAT = IAGMAT
      IF ( JMIX.GT.0) NMAT = IAGMAT+LROWS( JMIX)
      IF (NMAT*LCHNAM.GT.LALL) GOTO 998
      ALLNAM = ' '
      DO 200 I=1,IAGMAT
        IF (GVERSN.LT.3.15) THEN
           CALL GFMATE (I,INAME,AD,ZD,DD,RD,AL,DUM,ND)
           CALL ALSTHO (INAME,5,NAMG)
        ELSE
           CALL GFMATE (I,NAMG,AD,ZD,DD,RD,AL,DUM,ND)
        ENDIF
        L = LNBLNK (NAMG)
        IND = INDEX (NAMG,'$')
        IF (IND.NE.0) L=IND-1
        L = MIN (L,LCHNAM)
        IC = (I-1)*LCHNAM
        ALLNAM(IC+1:IC+L)  = NAMG(1:L)
  200 CONTINUE
      LAS = IAGMAT*LCHNAM
C
C - define mixtures needed by DET =====================================
C
      IF ( JMIX.NE.0) THEN
C
C
C       loop over MIXT rows
        DO 201 I=1,LROWS( JMIX)
          NCOMP = 0
          KMIX = KROW( JMIX,I)
          CALL ALSTIN (IW(KMIX+JMIXMN),NWRDS,NAMX)
          TYPE = CHAINT(IW(KMIX+JMIXMT))
C
          IF (TYPE .EQ.'ELEM') THEN
C            it is a mixture of ELEMents
            IF (JMEL.EQ.0) GOTO 997
            DENS = RW(KMIX+JMIXDE)
            IS = -1
C             loop over MELE rows : take those which refer to
C                                   the MIXT row # I
            DO 202 J=1,LROWS(JMEL)
              IF (ITABL(JMEL,J,JMELVM).EQ.I) THEN
                NCOMP = NCOMP+1
                KMEL = KROW(JMEL,J)
                CALL ALSTIN (IW(KMEL+JMELEL),NWRDS,NAME)
                IND = INDEX(ALLNAM,NAME)
                IF (IND.EQ.0) GOTO 996
                IMAT = IND/LCHNAM + 1
                IF (GVERSN.LT.3.15) THEN
                   CALL GFMATE (IMAT,INAME,AA(NCOMP),ZZ(NCOMP),
     &                          DD,RD,AL,DUM,ND)
                ELSE
                   CALL GFMATE (IMAT,NAMG,AA(NCOMP),ZZ(NCOMP),
     &                          DD,RD,AL,DUM,ND)
                ENDIF
                WW(NCOMP) = ITABL(JMEL,J,JMELNU)
              ENDIF
  202       CONTINUE
C
          ELSEIF (TYPE.EQ.'MATE' .OR. TYPE.EQ.'COMP') THEN
C            it is a mixture of materials or a compound
            IF (JMMA.EQ.0) GOTO 997
            IS  = 1
C             loop over MMAT rows: take those which refer to
C                                  the MIXT row # I
            DO 203 J=1,LROWS(JMMA)
              IF (ITABL(JMMA,J,JMMAVM).EQ.I) THEN
                NCOMP = NCOMP+1
                KMMA = KROW(JMMA,J)
                CALL ALSTIN (IW(KMMA+JMMAMA),NWRDS,NAME)
                IND = INDEX (ALLNAM,NAME)
                IF (IND.EQ.0) GOTO 996
                IMAT = IND/LCHNAM + 1
                LIS(NCOMP) = IMAT
                EPS(NCOMP)  = RTABL(JMMA,J,JMMATH)
              ENDIF
  203       CONTINUE
            CALL AGMIX (NCOMP,LIS,EPS,AA,ZZ,WW,DENS)
            IF (TYPE.EQ.'MATE') THEN
C                the density given by AGMIX must be scaled
              DENS = DENS * RW(KMIX+JMIXDE)
            ELSEIF(TYPE.EQ.'COMP') THEN
C                the density and weights are read from the bank
              DENS = RW(KMIX+JMIXDE)
              DO 204 N=1,NCOMP
                WW(N) = EPS(N)
  204         CONTINUE
            ENDIF
C
          ELSE
C            unknown type of mixture
            GOTO 995
          ENDIF
C
          IF (NCOMP.EQ.0) GOTO 994
C          store the new material in GEANT tables and in ALLNAM
          IAGMAT = IAGMAT+1
          CALL GSMIXT (IAGMAT,NAMX,AA,ZZ,DENS,IS*NCOMP,WW)
          ALLNAM(LAS+1:LAS+LCHNAM) = NAMX
          LAS = LAS + LCHNAM
  201   CONTINUE
      ENDIF
C
C - define tracking media used in DET      =====================
C
      INDMED = IAGMED
      IF (JTME.NE.0) THEN
        DO 301 I=1,LROWS(JTME)
          KTME = KROW(JTME,I)
          CALL ALSTIN (IW(KTME+JTMEMA),NWRDS,NAME)
          IND = INDEX (ALLNAM,NAME)
          IF (IND.EQ.0) GOTO 996
          IMAT = IND/LCHNAM + 1
          CALL ALSTIN (IW(KTME+JTMETN),NWRDS,NAMX)
          IAGMED = IAGMED+1
          ISVOL = 0
          IF (IW(KTME+JTMEVO).EQ.1) THEN
             IF (JDET.GT.LDET) GOTO 990
             ISVOL = IDETJO(JDET)
          ENDIF
          IFLD = IW(KTME+JTMEFL)
          IF (ALFIEL.EQ.0.) IFLD = 0
          CALL GSTMED (IAGMED,NAMX,IMAT,ISVOL,IFLD,ALFIEL,
     &        RW(KTME+JTMEFD),RW(KTME+JTMEMS),RW(KTME+JTMEDE),
     &        RW(KTME+JTMEEP),RW(KTME+JTMEST),0.,0.)
          IF (IW(KTME+JTMEGP).EQ.1) THEN
            IF (JGPA.EQ.0) GOTO 993
            DO 302 J=1,LROWS(JGPA)
              IF (ITABL(JGPA,J,JGPAVT).EQ. I) THEN
                KGPA = KROW(JGPA,J)
                CALL ALSTIN (IW(KGPA+JGPAPA),2,NAME)
                L = LENOCC(NAME)
                CALL GSTPAR (IAGMED,NAME(1:L),RW(KGPA+JGPAPV))
              ENDIF
  302       CONTINUE
          ENDIF
  301   CONTINUE
      ENDIF
C
C - define volumes used in DET  =====================================
C
      IF (JVOL.NE.0) THEN
        DO 401 I=1,LROWS(JVOL)
          KVOL = KROW(JVOL,I)
          NAME(1:4) = CHAINT(IW(KVOL+JVOLVO))
          NAMX(1:4) = CHAINT(IW(KVOL+JVOLSH))
          NPAR      = IW(KVOL+JVOLNP)
          DO 403 N=1,NPAR
            PTAB(N) = RW(KVOL+JVOLPA-1+N)
  403     CONTINUE
          IF (NAMX(1:4).EQ.'PCON' .OR. NAMX(1:4).EQ.'TRAP') THEN
            IF (JPCO.EQ.0) GOTO 992
            DO 402 J=1,LROWS(JPCO)
              IF (ITABL(JPCO,J,JPCOVV).EQ.I) THEN
                KGPC = KROW(JPCO,J)
                PTAB(NPAR+1) = RTABL(JPCO,J,JPCOZB)
                PTAB(NPAR+2) = RTABL(JPCO,J,JPCORN)
                PTAB(NPAR+3) = RTABL(JPCO,J,JPCORX)
                NPAR = NPAR+3
              ENDIF
  402       CONTINUE
          ENDIF
          ITME = IW(KVOL+JVOLVT)
          IF (ITME.LT.0) THEN
C             the tracking medium number is the absolute track.med. #
            ITME = -ITME
          ELSEIF (ITME.GT.0) THEN
C             the tracking medium # is relative to the 1st one entered
C             for this detector
            ITME = INDMED + ITME
          ENDIF
          CALL GSVOLU (NAME(1:4),NAMX(1:4),ITME,PTAB,NPAR,IVOL)
          IF (I.EQ.1) INDVOL = IVOL-1
  401   CONTINUE
      ENDIF
C
C - store rotations matrices used in DET  ============================
C
      IF (JROT.NE.0) THEN
        INDROT = IAGROT
        DO 501 I=1,LROWS(JROT)
          KROT = KROW(JROT,I)
          IAGROT = IAGROT+1
          CALL GSROTM (IAGROT,RW(KROT+JROTA1),RW(KROT+JROTP1),
     &                          RW(KROT+JROTA2),RW(KROT+JROTP2),
     &                          RW(KROT+JROTA3),RW(KROT+JROTP3))
  501   CONTINUE
      ENDIF
C
C - position daughter volumes into mother volumes ======================
C
      IF (JPOS.NE.0) THEN
        DO 601 I=1,LROWS(JPOS)
          KPOS = KROW(JPOS,I)
          NAME = CHAINT (IW(KPOS+JPOSDA))
          NAMX = CHAINT (IW(KPOS+JPOSMO))
          TYPE = CHAINT (IW(KPOS+JPOSFL))
          IROT = IW(KPOS+JPOSVR)
          IF (IROT.LT.0) THEN
C             the rotation number is the absolute rotation #
            IROT = -IROT
          ELSEIF (IROT.GT.0) THEN
C             the rotation number is relative to the 1st one entered
C             for this detector
            IROT = INDROT + IROT
          ENDIF
          CALL GSPOS (NAME(1:4),IW(KPOS+JPOSCN),NAMX(1:4),RW(KPOS+
     &        JPOSPO),RW(KPOS+JPOSPO+1),RW(KPOS+JPOSPO+2),IROT,TYPE)
  601   CONTINUE
      ENDIF
C
C - set volumes drawing attributes ======================================
C
      IF (JATT.NE.0) THEN
        DO 701 I=1,LROWS(JATT)
           KATT = KROW(JATT,I)
           NAME = CHAINT (IW(KATT+JATTVO))
           NAMX = CHAINT (IW(KATT+JATTAN))
           CALL GSATT (NAME(1:4),NAMX(1:4),IW(KATT+JATTAV))
 701    CONTINUE
      ENDIF
C
C - end ===========================================================
C
      GOTO 999
C
C - errors ============================================================
C
C   this detector is not a sensitive one: error in data base
  990 CONTINUE
      CALL PRTABL (DET(1:1)//'TME',IW(JTME-2))
      ALLNAM = ' '
      ALLNAM = 'AGGEAN: '//DET//' cannot be sensitive '
      CALL ALTELL (ALLNAM(1:LENOCC(ALLNAM)),0,'STOP')
C   GPCO is missing
  992 CONTINUE
      WRITE (LOUTIO,*) ' AGGEAN : PCON is missing but required IROW= ',I
      CALL PRTABL (DET(1:1)//'PCO',IW(JPCO-2))
      GOTO 991
C   GPAR is missing
  993 CONTINUE
      WRITE (LOUTIO,*) ' AGGEAN : GPAR is missing but required IROW= ',I
      CALL PRTABL (DET(1:1)//'TME',IW(JTME-2))
      GOTO 991
C    no mixture : NCOMP=0
  994 CONTINUE
      WRITE (LOUTIO,*) ' AGGEAN : no mixture NCOMP=0 , IROW= ',I
      CALL PRTABL (DET(1:1)//'MIX',IW(JMIX-2))
      CALL PRTABL (DET(1:1)//'MEL',IW(JTME-2))
      CALL PRTABL (DET(1:1)//'MMA',IW(JMMA-2))
      GOTO 991
C    type of mixture is unknown
  995 CONTINUE
      WRITE (LOUTIO,*) ' AGGEAN : unknown type of mixture , IROW= ',I
      CALL PRTABL (DET(1:1)//'MIX',IW(JMIX-2))
      GOTO 991
C    MATE or ELEM or TMED name is unknown
  996 CONTINUE
      WRITE (LOUTIO,*) ' AGGEAN : unknown name , I= ',I,' J= ',J
     &               , ' NAME= ',NAME
      WRITE (LOUTIO,*) ' ALLNAM= ',ALLNAM(1:LAS)
      CALL PRTABL (DET(1:1)//'MEL',IW(JMEL-2))
      CALL PRTABL (DET(1:1)//'MMA',IW(JMMA-2))
      CALL PRTABL (DET(1:1)//'TME',IW(JTME-2))
      GOTO 991
C    MELE or MMAT is missing but MIXT is there
  997 CONTINUE
      WRITE (LOUTIO,*) ' AGGEAN : MMAT or MELE is missing , IROW= ',I
      CALL PRTABL (DET(1:1)//'MIX',IW(JMIX-2))
      GOTO 991
C    too many materials: increase ALLNAM length
  998 CONTINUE
      WRITE (LOUTIO,*)
     &  ' AGGEAN : too many materials, increase ALLNAM length , NMAT= ',
     &  NMAT
      GOTO 991
C
  991 CONTINUE
      ALLNAM = ' '
      ALLNAM = 'AGGEAN: cannot build '//DET//' geometry '
      CALL ALTELL (ALLNAM(1:LENOCC(ALLNAM)),0,'STOP')
C - end
C
  999 CONTINUE
      RETURN
      END
#endif
