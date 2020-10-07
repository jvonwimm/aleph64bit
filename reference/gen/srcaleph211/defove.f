      SUBROUTINE DEFOVE
C.----------------------------------------------------------------------
CKEY GAMPACK ECAL GAMPEX CONSTANT / INTERNAL
C   J.C.Brient      Creation  1/10/91
C! define overlap region fron dbase bank ECGN and basic parameters
C   Input : None
C   Output: STORE ECOXA and  COMCUX common
C   Calls : EPLMST , ERWIDT
C   Called by GAMPEX
C.----------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER ( NPLWA = 45 )
      COMMON/ECOXA/ITOV1,ITOV2,ITOV3,ITOV4,ITHTO,
     &             RESTK1 , ZESTK1 , E4ETB,
     &             STWIDT , STRPHI(45)
      PARAMETER(JECGID=1,JECGLS=2,JECGLP=3,JECGSC=4,JECGSY=5,JECGMD=6,
     +          JECGPL=7,JECGST=8,JECGCL=9,JECGRG=10,JECGSS=11,
     +          JECGRW=12,JECGXW=13,JECGEC=14,JECGXG=15,JECGNP=16,
     +          JECGPR=17,JECGBL=18,JECGBO=19,JECGEI=20,JECGEW=21,
     +          JECGEL=22,JECGGP=23,JECGWS=24,JECGAP=25,JECGAL=26,
     +          JECGDM=27,JECGTI=43,JECGC1=44,JECGC2=45,JECGM1=46,
     +          JECGM2=47,JECGR1=48,JECGR2=49,LECGNA=49)
      COMMON/COMCUX/ RECPAR(10) , NRCOM  , NECOM
C
      DIMENSION PLL(4)
      DIMENSION RLONG(NPLWA)  , RLARG(NPLWA)
      EXTERNAL AGETDB
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
C read in data base the bank EGRP which contains the reconstruction
C parameters of GAMPEX
C
      LBASE = JUNIDB (0)
      JEGRP = MDARD (IW,LBASE,'EGRP',0 )
      IF(JEGRP .GT. 0) THEN
        DO I = 1 , LCOLS(JEGRP)
          RECPAR(I) = RTABL(JEGRP,1,I)
        ENDDO
      ELSE
        RECPAR(1) = 0.030
        RECPAR(2) = 0.075
        RECPAR(3) = 0.150
        RECPAR(4) = 0.200
        RECPAR(5) = 0.25
        RECPAR(6) = 2.0
        RECPAR(7) = 1.
        RECPAR(8) = 1.
      ENDIF
C
C E4/Etot mean value from J.Badier , for the Barrel storeys width
C-----------------------------------------------------------------
      E4ETB = 0.851
C
C R and Z middle stack1 ECAL
C---------------------------
      CALL EPLMST('ALEPH',1,1,1,PLL)
      ZESTK1 = ABS( PLL(4) )
      CALL EPLMST('ALEPH',2,1,1,PLL)
      RESTK1 = ABS( PLL(4) )
C
C width of storey from H.Videau
C -------------------------------
      CALL ERWIDT(RLONG,RLARG)
      SUMD= 0.
      DO I = 1 , NPLWA
        SUMD = SUMD + RLONG(I)
      ENDDO
      STWIDT = SUMD/FLOAT(NPLWA)
      DO I = 1 , NPLWA
        STRPHI(I) = RLARG(I)
      ENDDO
      JECGN  = IW(NAMIND('ECGN'))
      I17    = ITABL(JECGN,1,JECGPR)
      I12    = ITABL(JECGN,1,JECGRW)
      I13    = ITABL(JECGN,1,JECGXW)
      NOV    = (I13-I12)/2
C
      ITOV1  = I17+1
C --itov1=46
      ITOV2  = I17+NOV
C --itov2=50
      ITOV3  = I13+1-ITOV1
C --itov3=179
      ITOV4  = I13+1-ITOV2
C --itov4=183
      ITHTO  = I13
C --ithto=228
      RETURN
      END
