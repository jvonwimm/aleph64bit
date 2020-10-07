      SUBROUTINE TALINI
C
C---------------------------------------------------------------------
C - R.Johnson - 880627
C! Establish TPC alignment matrices at run beginning
C
C----------------------------------------------------------------------
      SAVE
C
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (LTPDRO=21,LTTROW=19,LTSROW=12,LTWIRE=200,LTSTYP=3,
     +           LTSLOT=12,LTCORN=6,LTSECT=LTSLOT*LTSTYP,LTTPAD=4,
     +           LMXPDR=150,LTTSRW=11)
      COMMON/TALIGN/ ASTOGL(3,3,LTSECT),DSTOGL(3,LTSECT),
     &               AGLTOS(3,3,LTSECT),DGLTOS(3,LTSECT),
     &               TAPRRD(LTPDRO,LTSTYP),TAPRPH(LTPDRO,LTSTYP),
     &               ASTOTP(3,3,LTSECT),DSTOTP(3,LTSECT),
     &               ATPTOS(3,3,LTSECT),DTPTOS(3,LTSECT)
C
      PARAMETER(JTCGID=1,JTCGVR=2,JTCGCN=4,JTCGNS=5,JTCGNC=6,JTCGNE=7,
     +          JTCGAS=8,JTCGRS=9,JTCGPH=10,JTCGPS=11,JTCGPW=12,
     +          JTCGGW=13,JTCGEW=14,JTCGES=15,JTCGBD=16,JTCGTS=17,
     +          JTCGTH=18,JTCGWP=19,JTCGWD=20,JTCGTO=21,JTCGTT=24,
     +          JTCGWT=27,JTCGWE=28,JTCGWW=29,JTCGWK=30,JTCGFT=33,
     +          LTCGDA=33)
      PARAMETER(JTCROC=1,JTCRNC=2,JTCRN1=3,LTCRLA=3)
      PARAMETER(JTMTID=1,JTMTVR=2,JTMTMO=4,JTMTPP=5,JTMTRF=6,JTMTNP=7,
     +          JTMTPR=8,JTMTRT=20,JTMTNT=21,JTMTTR=22,JTMTAT=33,
     +          JTMTTC=34,JTMTPW=38,JTMTNW=39,JTMTEF=40,JTMTEL=41,
     +          JTMTWF=42,LTMTYA=45)
      PARAMETER(JTPCIN=1,JTPCRV=2,JTPCPH=3,JTPCZV=4,JTPCSR=5,JTPCSZ=6,
     +          JTPCOF=7,JTPCTN=8,JTPCCN=9,JTPCIT=10,JTPCRR=11,
     +          JTPCRZ=12,LTPCOA=12)
      PARAMETER(JTPHIT=1,LTPHEA=1)
      PARAMETER(JTPHKT=1,JTPHCI=2,JTPHPH=3,JTPHZV=4,JTPHDD=5,JTPHDZ=6,
     +          LTPHTA=6)
      PARAMETER(JTPTKT=1,JTPTXA=2,JTPTYA=3,JTPTZA=4,JTPTDX=5,JTPTDY=6,
     +          JTPTDZ=7,JTPTPV=8,JTPTLN=9,JTPTTF=10,JTPTPM=11,
     +          JTPTCH=12,LTPTEA=12)
      PARAMETER(JTSGID=1,JTSGVR=2,JTSGNC=4,JTSGXC=5,JTSGYC=10,JTSGTM=15,
     +          LTSGMA=15)
      PARAMETER(JTSLID=1,JTSLVR=2,JTSLSN=4,JTSLSB=5,JTSLSS=6,JTSLDS=7,
     +          JTSLAS=8,JTSLRS=9,JTSLTM=10,JTSLTS=11,LTSLOA=11)
      PARAMETER(JTTHIT=1,LTTHEA=1)
      PARAMETER(JTTHKT=1,JTTHCI=2,JTTHPH=3,JTTHZV=4,JTTHDD=5,JTTHDZ=6,
     +          LTTHTA=6)
      PARAMETER(JTSCID=1,JTSCVR=2,JTSCSN=4,JTSCNS=5,JTSCRP=6,JTSCAZ=9,
     +          JTSCAX=10,JTSCAY=11,JTSCSG=12,JTSCTC=13,LTSCOA=13)
      PARAMETER(JT1FID=1,JT1FVR=2,JT1FLL=4,JT1FUL=5,JT1FSS=6,JT1FNP=7,
     +          LT1FCA=7)
      PARAMETER(JT2FID=1,JT2FVR=2,JT2FR1=4,JT2FR2=916,JT2FP1=1828,
     +          JT2FP2=2740)
      PARAMETER(JTPOID=1,JTPOVR=2,JTPOTL=4,JTPORT=7,JTPOIT=10,JTPOTC=11,
     +          LTPOSA=11)
C
      DIMENSION ASTOE(3,3),DSTOE(3),AETOG(3,3),DETOG(3)
      DIMENSION GTRN(3),GROT(3,3)
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
      KTSCO=IW(NAMIND('TSCO'))
      KTCGD=IW(NAMIND('TCGD'))
      KTSLO=IW(NAMIND('TSLO'))
      KTMTY=IW(NAMIND('TMTY'))
      KTPOS=IW(NAMIND('TPOS'))
C
C++   Get translation from ALEPH to TPC-ITC coordinates
C
      DO 32 I=1,3
        GTRN(I)=RTABL(KTPOS,1,JTPOTL-1+I)
   32 CONTINUE
      THE=RTABL(KTPOS,1,JTPORT)
      DEL=RTABL(KTPOS,1,JTPORT+1)
      PHI=RTABL(KTPOS,1,JTPORT+2)
      GROT(1,1)= COS(DEL)*COS(PHI)
      GROT(1,2)=-COS(DEL)*SIN(PHI)
      GROT(1,3)= SIN(DEL)
      GROT(2,1)= SIN(THE)*SIN(DEL)*COS(PHI) + COS(THE)*SIN(PHI)
      GROT(2,2)= COS(THE)*COS(PHI) - SIN(THE)*SIN(DEL)*SIN(PHI)
      GROT(2,3)=-SIN(THE)*COS(DEL)
      GROT(3,1)= SIN(THE)*SIN(PHI) - COS(THE)*SIN(DEL)*COS(PHI)
      GROT(3,2)= SIN(THE)*COS(PHI) + COS(THE)*SIN(DEL)*SIN(PHI)
      GROT(3,3)= COS(THE)*COS(DEL)
C
C++   First build the relation from sector to global coordinates.  It wi
C++   then be inverted to get the inverse relation.
C
      RSTEP=RTABL(KTCGD,1,JTCGRS)
      DO 500 IS=1,LTSECT
        ISLOT= ITABL(KTSLO,IS,JTSLSB)
        ISTYP= ITABL(KTSLO,IS,JTSLTM)
        ISS=   ITABL(KTSLO,IS,JTSLSS)
        IEND=  ITABL(KTSLO,IS,JTSLTS)
C
C++     Get distance from the padrow centers to the midpoint between
C++     the first and last padrows
C
        NPADR= ITABL(KTMTY,ISTYP,JTMTNP)
        RFST= RTABL(KTMTY,ISTYP,JTMTRF)
        RLST= RFST + FLOAT(NPADR-1)*RSTEP
        XC= 0.5*(RFST+RLST)
C
C++     Get half-length of TPC active volume
C
        ZMX= RTABL(KTCGD,1,JTCGTO+2)-RTABL(KTCGD,1,JTCGTT+2)
C
C++     Get the nominal angle from x axis to sector position
C
        ANFIR= RTABL(KTMTY,ISTYP,JTMTPP)
        ANGSP= RTABL(KTCGD,1,JTCGAS)
        PHI0= ANFIR + ANGSP*FLOAT(MOD(ISS-1,6))
C
C++     Get the correction to phi0
C
        DELTA= RTABL(KTSLO,IS,JTSLAS)
C
C++     Get angle of rotation of sector about its center
C
        ETA= RTABL(KTSLO,IS,JTSLRS)
C
C++     Get radial shift of the sector
C
        DELR=RTABL(KTSLO,IS,JTSLDS)
C
C++     Construct matrix to rotate from sector frame to endplate frame
C
        ASTOE(1,1)= COS(PHI0+DELTA+ETA)
        ASTOE(1,2)=-SIN(PHI0+DELTA+ETA)
        ASTOE(1,3)= 0.
        ASTOE(2,1)= SIN(PHI0+DELTA+ETA)
        ASTOE(2,2)= COS(PHI0+DELTA+ETA)
        ASTOE(2,3)= 0.
        ASTOE(3,1)= 0.
        ASTOE(3,2)= 0.
        ASTOE(3,3)= 1.
C
C++     For endplate A, FIRST rotate by pi radians about the sector
C++     x axis.
C
        IF (IEND.EQ.1) THEN
          ASTOE(1,2)=-ASTOE(1,2)
          ASTOE(2,2)=-ASTOE(2,2)
          ASTOE(3,3)=-ASTOE(3,3)
        ENDIF
C
C++     Construct translation from sector frame to endplate frame
C
        DSTOE(1)= (COS(PHI0+DELTA)-COS(PHI0+DELTA+ETA))*XC
     &           + COS(PHI0+DELTA)*DELR
        DSTOE(2)= (SIN(PHI0+DELTA)-SIN(PHI0+DELTA+ETA))*XC
     &           + SIN(PHI0+DELTA)*DELR
        DSTOE(3)= 0.
C
C++     Construct rotation from endplate frame to global frame
C
        THE= RTABL(KTSCO,IEND,JTSCAX)
        DEL=   RTABL(KTSCO,IEND,JTSCAY)
        PHI=   RTABL(KTSCO,IEND,JTSCAZ)
        AETOG(1,1)= COS(DEL)*COS(PHI)
        AETOG(1,2)=-COS(DEL)*SIN(PHI)
        AETOG(1,3)= SIN(DEL)
        AETOG(2,1)= SIN(THE)*SIN(DEL)*COS(PHI) + COS(THE)*SIN(PHI)
        AETOG(2,2)= COS(THE)*COS(PHI) - SIN(THE)*SIN(DEL)*SIN(PHI)
        AETOG(2,3)=-SIN(THE)*COS(DEL)
        AETOG(3,1)= SIN(THE)*SIN(PHI) - COS(THE)*SIN(DEL)*COS(PHI)
        AETOG(3,2)= SIN(THE)*COS(PHI) + COS(THE)*SIN(DEL)*SIN(PHI)
        AETOG(3,3)= COS(THE)*COS(DEL)
C
C++     Construct translation from endplate frame to global frame
C
        DETOG(1)= RTABL(KTSCO,IEND,JTSCRP+1)
        DETOG(2)= RTABL(KTSCO,IEND,JTSCRP+2)
        DETOG(3)= RTABL(KTSCO,IEND,JTSCRP)
C
C++     Muliply two transformations together to get transformation
C++     from the sector frame to the TPC frame
C
        DO 156 I=1,3
          DSTOTP(I,ISLOT)= DETOG(I)
          DO 154 J=1,3
            ASTOTP(I,J,ISLOT)=0.
            DSTOTP(I,ISLOT)= DSTOTP(I,ISLOT)
     &                        + AETOG(I,J)*DSTOE(J)
            DO 146 L=1,3
              ASTOTP(I,J,ISLOT)=ASTOTP(I,J,ISLOT)
     &                      + AETOG(I,L)*ASTOE(L,J)
  146       CONTINUE
  154     CONTINUE
  156   CONTINUE
C
C++     Now multiply the three transformations together to get
C++     transformation from the sector frame to the ALEPH frame
C
        DO 100 I=1,3
          DSTOGL(I,ISLOT)=GTRN(I)
          DO 50 J=1,3
            DSTOGL(I,ISLOT)= DSTOGL(I,ISLOT) + GROT(I,J)*DETOG(J)
            ASTOGL(I,J,ISLOT)=0.
            DO 40 L=1,3
              DSTOGL(I,ISLOT)= DSTOGL(I,ISLOT)
     &                + GROT(I,L)*AETOG(L,J)*DSTOE(J)
              DO 35 K=1,3
                ASTOGL(I,J,ISLOT)=ASTOGL(I,J,ISLOT)
     &                      + GROT(I,L)*AETOG(L,K)*ASTOE(K,J)
   35         CONTINUE
   40       CONTINUE
   50     CONTINUE
  100   CONTINUE
C
C++     Now, invert the relation in order to go from global to sector
C
        DO 200 I=1,3
          DO 150 J=1,3
            AGLTOS(I,J,ISLOT)=ASTOGL(J,I,ISLOT)
            ATPTOS(I,J,ISLOT)=ASTOTP(J,I,ISLOT)
  150     CONTINUE
  200   CONTINUE
C
        DO 300 I=1,3
          DGLTOS(I,ISLOT)=0.
          DTPTOS(I,ISLOT)=0.
          DO 250 J=1,3
            DGLTOS(I,ISLOT)=DGLTOS(I,ISLOT)
     &                         - AGLTOS(I,J,ISLOT)*DSTOGL(J,ISLOT)
            DTPTOS(I,ISLOT)=DTPTOS(I,ISLOT)
     &                         - ATPTOS(I,J,ISLOT)*DSTOTP(J,ISLOT)
  250     CONTINUE
  300   CONTINUE
  500 CONTINUE
C
  999 CONTINUE
      RETURN
      END
