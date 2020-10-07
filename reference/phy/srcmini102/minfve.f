      SUBROUTINE MINFVE
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill FVER from DVMC.
C
C     Author: Stephen Haywood      21-Nov-90
C
C     Input  : DVMC bank
C     Output : FVER bank
C
C     Called by MINFIL
C-----------------------------------------------------------------------
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JFVEVX=1,JFVEVY=2,JFVEVZ=3,JFVETO=4,JFVEIP=5,JFVEIS=6,
     +          JFVENS=7,JFVEVN=8,JFVEVM=9,LFVERA=9)
      PARAMETER(JDTMPX=1,JDTMPY=2,JDTMPZ=3,JDTMMA=4,JDTMPA=5,JDTMOV=6,
     +          JDTMEV=7,JDTMHC=8,LDTMCA=8)
      PARAMETER(JDVMVX=1,JDVMVY=2,JDVMVZ=3,JDVMIP=4,JDVMVN=5,JDVMVM=6,
     +          LDVMCA=6)
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      INTEGER NBITW, NBYTW, LCHAR
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
C
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
*     PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
C
C++   Speed of light in cm/s.
C
      PARAMETER (CCMPS=1.E9*CLGHT)
C
C++   Labels for vertex mechanism.
C
      PARAMETER (NVMEC=31)
      DIMENSION VMEC(NVMEC),IVMEC(NVMEC)
      CHARACTER*4 VMEC
      DATA VMEC /
     &  'NEXT','MULS','LOSS','FIEL','DCAY','PAIR','COMP','PHOT',
     &  'BREM','DRAY','ANNI','HADR','ECOH','EVAP','FISS','ABSO',
     &  'ANNH','CAPT','EINC','INHE','MUNU','TOFM','PFIS','SCUT',
     &  'RAYL','NONE','PRED','LOOP','NULL','STOP','SHOW' /
      LOGICAL FIRST
      SAVE FIRST,IVMEC,NULL
      DATA FIRST,IVMEC,NULL / .TRUE.,NVMEC*0,0 /
C
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
C++   Pick up DVMC bank.
C
      KDVMC = NLINK('DVMC',0)
      IF (KDVMC.LE.0) RETURN
      NDVMC = LROWS(KDVMC)
      IF(NDVMC.LE.0) RETURN
C
C++   Initialisation for vertex mechanism labels.
C
      IF(FIRST) THEN
         DO I=1,NVMEC
            IVMEC(I) = INTCHA(VMEC(I))
         ENDDO
         NULL = INTCHA('    ')
         FIRST = .FALSE.
      ENDIF
C
C++   Create the FVER bank.
C
      LEN = LMHLEN + LFVERA * NDVMC
      CALL AUBOS('FVER',0,LEN, KFVER,IGARB)
      CALL BLIST(IW,'S+','FVER')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDVMC = NLINK('DVMC',0)
      ENDIF
      IW(KFVER+LMHCOL) = LFVERA
      IW(KFVER+LMHROW) = NDVMC
C
C++   Fill FVER bank.
C
      DO I=1,NDVMC
         RW(KROW(KFVER,I)+JFVEVX) = FLOAT(ITABL(KDVMC,I,JDVMVX))/DFACTM
         RW(KROW(KFVER,I)+JFVEVY) = FLOAT(ITABL(KDVMC,I,JDVMVY))/DFACTM
         RW(KROW(KFVER,I)+JFVEVZ) = FLOAT(ITABL(KDVMC,I,JDVMVZ))/DFACTM
C        CT = FLOAT(ITABL(KDVMC,I,JDVMTO))/DFACTM
C        RW(KROW(KFVER,I)+JFVETO) = CT / CCMPS
         IW(KROW(KFVER,I)+JFVEIP) = ITABL(KDVMC,I,JDVMIP)
         IW(KROW(KFVER,I)+JFVEVN) = ITABL(KDVMC,I,JDVMVN)
C
C++      Reassign the vertex mechanism.
C
         LABEL = ITABL(KDVMC,I,JDVMVM)
         IF(LABEL.EQ.0) THEN
            JVMEC = NULL
         ELSE
            IF(LABEL.GT.0 .AND. LABEL.LE.NVMEC) THEN
               JVMEC = IVMEC(LABEL)
            ELSE
               JVMEC = LABEL
            ENDIF
         ENDIF
         IW(KROW(KFVER,I)+JFVEVM) = JVMEC
      ENDDO
C
C++   Reconstruct the relationships to the FKIN bank.
C++   The code to identify the mother of the vertex from the DTMC (FKIN)
C++   bank is not used since some tracks end at more than one vertex.
C
      KDTMC = NLINK('DTMC',0)
      IF(KDTMC.LE.0) RETURN
      NDTMC = LROWS(KDTMC)
      IF(NDTMC.LE.0) RETURN
C
      DO 10 I=1,NDTMC
         IORIG = ITABL(KDTMC,I,JDTMOV)
         IF (IORIG.LE.0.OR.IORIG.GT.NDVMC) GO TO 10
         IF(ITABL(KFVER,IORIG,JFVENS).EQ.0) THEN
            IW(KROW(KFVER,IORIG)+JFVEIS) = I - 1
         ENDIF
         IW(KROW(KFVER,IORIG)+JFVENS) = IW(KROW(KFVER,IORIG)+JFVENS) + 1
 10   CONTINUE
C
      RETURN
      END
