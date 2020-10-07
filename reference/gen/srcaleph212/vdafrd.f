      INTEGER FUNCTION VDAFRD (LUNDAF,IGET)
C ----------------------------------------------------------------------
CKEY VDETDES READ DBASE / INTERNAL
C!  Read VDET geometry banks from DAF
C - Steve Wasserbaech, January 1994
C   (Based on VRDDAF, G. Triggiani, 17/02/87.)
C   Modified 31 July 1995, S. Wasserbaech: add NWFBIT to VRDOCO
C
C  Called by: VRDDAF
C  Calls:     ALGTDB
C
C - Input:
C   LUNDAF / I  Logical unit number of DAF file
C   IGET   / I  VDET setup code to be read
C
C - Output:
C   VDAFRD / I  = VDOK if successful
C               = VDERR if an error occurred
C ----------------------------------------------------------------------
C     IMPLICIT NONE
C!    Parameters for VDET geometry package
C ----------------------------------------------------------------------
C
C     Labels for return codes:
C
      INTEGER VDERR, VDOK
      PARAMETER (VDERR = -1)
      PARAMETER (VDOK  = 1)
C
C     Labels for views:
C
      INTEGER VVIEWZ, VVIEWP
      PARAMETER (VVIEWZ = 1)
      PARAMETER (VVIEWP = 2)
C
C     Fixed VDET geometry parameters:
C
      INTEGER NVLAYR, NVMODF, NVVIEW, NPROMM, IROMAX
      PARAMETER (NVLAYR = 2)
      PARAMETER (NVMODF = 2)
      PARAMETER (NVVIEW = 2)
      PARAMETER (NPROMM = 1)
      PARAMETER (IROMAX = 4)
C
C     Array dimensions:
C
      INTEGER NVWMMX, NVWFMX, NVFLMX, NVFMAX, NVMMAX, NVWMAX
      INTEGER NVZRMX, NVPRMX
      PARAMETER (NVWMMX = 3)
      PARAMETER (NVWFMX = NVWMMX*NVMODF)
      PARAMETER (NVFLMX = 15)
      PARAMETER (NVFMAX = 24)
      PARAMETER (NVMMAX = NVFMAX*NVMODF)
      PARAMETER (NVWMAX = NVFMAX*NVWFMX)
      PARAMETER (NVZRMX = NVFMAX*IROMAX)
      PARAMETER (NVPRMX = NVMMAX*NPROMM)
C
C!    Common for VSLT data: VDET slots
C ----------------------------------------------------------------------
      INTEGER NSLOTS, JJLAYF, ISSFLG
      REAL PHIOFF
C
      COMMON / VSLTCO / NSLOTS, JJLAYF(NVFMAX), PHIOFF(NVFMAX),
     >                  ISSFLG(NVFMAX)
C
C!    Common for VDLA data: VDET layers
C ----------------------------------------------------------------------
      INTEGER IORIEN
      REAL RWVDLA, WATILT
C
      COMMON / VDLACO / RWVDLA(NVLAYR), WATILT(NVLAYR), IORIEN(NVLAYR)
C
C!    Common for VZPW data: Wafer z positions
C ----------------------------------------------------------------------
      INTEGER NWAFEF
      REAL WAFERZ
C
      COMMON / VZPWCO / NWAFEF, WAFERZ(NVWFMX)
C
C!    Common for VWGE data: Wafer geometry
C ----------------------------------------------------------------------
      INTEGER NZSTRP, NPSTRP
      REAL WSIZEA, WSIZEB, STPITZ, STPITP, STLENZ, STLENP
      REAL AMNSRZ, AMNSRP, BMNSRZ, BMNSRP, WTHICK
C
      COMMON / VWGECO / WSIZEA, WSIZEB, NZSTRP, NPSTRP, STPITZ, STPITP,
     >                  STLENZ, STLENP, AMNSRZ, AMNSRP, BMNSRZ, BMNSRP,
     >                  WTHICK
C
C!    Common for VRDO data: Readout configuration
C ----------------------------------------------------------------------
      INTEGER NRDSTZ, NRDSTP, NREFRZ, NREFRP, NOFRDZ, NOFRDP
      INTEGER NZRSSC, IECORZ, IECORP, NZEROM, NPEROM, NWFBIT
C
      COMMON / VRDOCO / NRDSTZ, NRDSTP, NREFRZ, NREFRP, NOFRDZ, NOFRDP,
     >                  NZRSSC, IECORZ, IECORP, NZEROM, NPEROM, NWFBIT
C
C!    Common for lookup tables for index manipulation
C ----------------------------------------------------------------------
      INTEGER IVSTUP, NMODUL, NWAFER, NFACEL, NWAFEM, JJFACM, JJMODW
      INTEGER JIFACF, JIMODM, JIWAFW, IJFACE, IJMODU, IJWAFR, JIWFFW
      INTEGER IJWFFR
      CHARACTER*4 TXMODU
C
      COMMON / VGINDX / IVSTUP, NMODUL, NWAFER, NFACEL(NVLAYR),
     >                  NWAFEM, JJFACM(NVMMAX), JJMODW(NVWMAX),
     >                  JIFACF(NVFMAX), JIMODM(NVMMAX),
     >                  JIWAFW(NVWMAX), IJFACE(NVLAYR,NVFLMX),
     >                  IJMODU(NVLAYR,NVFLMX,NVMODF),
     >                  IJWAFR(NVLAYR,NVFLMX,NVMODF,NVWMMX),
     >                  JIWFFW(NVWMAX), IJWFFR(NVLAYR,NVFLMX,NVWFMX),
     >                  TXMODU(NVLAYR,NVFMAX,NVMODF)
C
C!    Common for miscellaneous calculated VDET geometry quantities
C ----------------------------------------------------------------------
      REAL RVDMIN, RVDMAX, ZVDMAX, WAXCEN, WAYCEN, WAZCEN
      REAL WARHOC, WAPHIC, CPHIOF, SPHIOF, TNWTLT, AMXSRZ, AMXSRP
      REAL BMXSRZ, BMXSRP
      INTEGER NZSROM, NPSROM, NZSMOD, NPSMOD, NPRSSC, NZROMM
      INTEGER MSVWAF, MSVNST, ISSLAY, ISSNST
      LOGICAL LZMULT
C
      COMMON / VDETGE / RVDMIN, RVDMAX, ZVDMAX,
     >                  WAXCEN(NVWMAX), WAYCEN(NVWMAX), WAZCEN(NVWMAX),
     >                  WARHOC(NVFMAX), WAPHIC(NVFMAX), CPHIOF(NVFMAX),
     >                  SPHIOF(NVFMAX), TNWTLT(NVLAYR), AMXSRZ, AMXSRP,
     >                  BMXSRZ, BMXSRP, NZSROM, NPSROM, NZSMOD, NPSMOD,
     >                  NPRSSC, NZROMM, LZMULT, MSVWAF, MSVNST, ISSLAY,
     >                  ISSNST
C
      INTEGER JVSLLA, JVSLPH, JVSLSS, LVSLTA
      PARAMETER (JVSLLA=1, JVSLPH=2, JVSLSS=3, LVSLTA=3)
      INTEGER JVDLRA, JVDLTA, JVDLOR, LVDLAA
      PARAMETER (JVDLRA=1, JVDLTA=2, JVDLOR=3, LVDLAA=3)
      INTEGER JVZPZC, LVZPWA
      PARAMETER (JVZPZC=1, LVZPWA=1)
      INTEGER JVWGSA, JVWGSB, JVWGNZ, JVWGNP, JVWGPZ, JVWGPP
      INTEGER JVWGLZ, JVWGLP, JVWGAZ, JVWGAP, JVWGBZ, JVWGBP
      INTEGER JVWGWT, LVWGEA
      PARAMETER (JVWGSA=1, JVWGSB=2, JVWGNZ=3, JVWGNP=4, JVWGPZ=5,
     >           JVWGPP=6, JVWGLZ=7, JVWGLP=8, JVWGAZ=9, JVWGAP=10,
     >           JVWGBZ=11, JVWGBP=12, JVWGWT=13, LVWGEA=13)
      INTEGER JVRDRZ, JVRDRP, JVRDFZ, JVRDFP, JVRDOZ, JVRDOP, JVRDSZ
      INTEGER JVRDDZ, JVRDDP, JVRDEZ, JVRDEP, JVRDWB, LVRDOA
      PARAMETER (JVRDRZ=1, JVRDRP=2, JVRDFZ=3, JVRDFP=4, JVRDOZ=5,
     >           JVRDOP=6, JVRDSZ=7, JVRDDZ=8, JVRDDP=9, JVRDEZ=10,
     >           JVRDEP=11, JVRDWB=12, LVRDOA=12)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
C     Arguments:
      INTEGER LUNDAF, IGET
C
C     Parameters:
C     List of VDET geometry banks to read:
      CHARACTER*20 LIST1
      PARAMETER (LIST1 = 'VSLTVDLAVZPWVWGEVRDO')
C
C     Local variables
      INTEGER LOUT, IRET, I
      INTEGER KVSLT, KVDLA, KVZPW, KVWGE, KVRDO
      INTEGER ILAY, IFAC, IMOD
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
      LOGICAL MISS
C
C     External functions:
      INTEGER ALGTDB, NAMIND
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
C ----------------------------------------------------------------------
C
      VDAFRD = VDERR
      LOUT = IW(6)
C
C     Initialize all VDET geometry commons:
C
C     VSLTCO:
      NSLOTS = 0
      CALL VZERO(JJLAYF,NVFMAX)
      CALL VZERO(PHIOFF,NVFMAX)
      CALL VZERO(ISSFLG,NVFMAX)
C
C     VDLACO:
      CALL VZERO(RWVDLA,NVLAYR)
      CALL VZERO(WATILT,NVLAYR)
      CALL VZERO(IORIEN,NVLAYR)
C
C     VZPWCO:
      NWAFEF = 0
      CALL VZERO(WAFERZ,NVWFMX)
C
C     VWGECO:
      WSIZEA = 0.
      WSIZEB = 0.
      NZSTRP = 0
      NPSTRP = 0
      STPITZ = 0.
      STPITP = 0.
      STLENZ = 0.
      STLENP = 0.
      AMNSRZ = 0.
      AMNSRP = 0.
      BMNSRZ = 0.
      BMNSRP = 0.
      WTHICK = 0.
C
C     VRDOCO:
      NRDSTZ = 0
      NRDSTP = 0
      NREFRZ = 0
      NREFRP = 0
      NOFRDZ = 0
      NOFRDP = 0
      NZRSSC = 0
      IECORZ = 0
      IECORP = 0
      NZEROM = 0
      NPEROM = 0
      NWFBIT = 0
C
C     VGINDX:
      IVSTUP = 0
      NMODUL = 0
      NWAFER = 0
      CALL VZERO(NFACEL,NVLAYR)
      NWAFEM = 0
      CALL VZERO(JJFACM,NVMMAX)
      CALL VZERO(JJMODW,NVWMAX)
      CALL VZERO(JIFACF,NVFMAX)
      CALL VZERO(JIMODM,NVMMAX)
      CALL VZERO(JIWAFW,NVWMAX)
      CALL VZERO(IJFACE,NVLAYR*NVFLMX)
      CALL VZERO(IJMODU,NVLAYR*NVFLMX*NVMODF)
      CALL VZERO(IJWAFR,NVLAYR*NVFLMX*NVMODF*NVWMMX)
      CALL VZERO(JIWFFW,NVWMAX)
      CALL VZERO(IJWFFR,NVLAYR*NVFLMX*NVWFMX)
      DO IMOD=1,NVMODF
        DO IFAC=1,NVFMAX
          DO ILAY=1,NVLAYR
            TXMODU(ILAY,IFAC,IMOD) = '    '
          ENDDO
        ENDDO
      ENDDO
C
C     VDETGE:
      RVDMIN = 0.
      RVDMAX = 0.
      ZVDMAX = 0.
      CALL VZERO(WAXCEN,NVWMAX)
      CALL VZERO(WAYCEN,NVWMAX)
      CALL VZERO(WAZCEN,NVWMAX)
      CALL VZERO(WARHOC,NVFMAX)
      CALL VZERO(WAPHIC,NVFMAX)
      CALL VZERO(CPHIOF,NVFMAX)
      CALL VZERO(SPHIOF,NVFMAX)
      CALL VZERO(TNWTLT,NVLAYR)
      AMXSRZ = 0.
      AMXSRP = 0.
      BMXSRZ = 0.
      BMXSRP = 0.
      NZSROM = 0
      NPSROM = 0
      NZSMOD = 0
      NPSMOD = 0
      NPRSSC = 0
      NZROMM = 0
      LZMULT = .FALSE.
      MSVWAF = 0
      MSVNST = 0
      ISSLAY = 0
      ISSNST = 0
C
C ----------------------------------------------------------------------
C
C     Read the banks from the DAF:
      IRET = ALGTDB(LUNDAF,LIST1,-IGET)
C
      IF (IRET .EQ. 0) THEN
C       Something went wrong...
        WRITE (LOUT,'(1X,3(A,I5))')
     >        '+++VDAFRD+++  Error reading DAF on unit', LUNDAF,
     >        ', setup', IGET, ': iret =', IRET
        GO TO 1000
      ENDIF
C
C ----------------------------------------------------------------------
C
C     Get the indices to banks just read in:
C
      KVSLT = IW(NAMIND('VSLT'))
      KVDLA = IW(NAMIND('VDLA'))
      KVZPW = IW(NAMIND('VZPW'))
      KVWGE = IW(NAMIND('VWGE'))
      KVRDO = IW(NAMIND('VRDO'))
C
      MISS = (KVSLT .LE. 0) .OR. (KVDLA .LE. 0) .OR.
     >       (KVZPW .LE. 0) .OR. (KVWGE .LE. 0) .OR.
     >       (KVRDO .LE. 0)
C
      IF (MISS) THEN
        WRITE (LOUT,'(1X,A)') '+++VDAFRD+++  Missing bank(s)!'
        WRITE (LOUT,'(1X,A,I10)') '+++VDAFRD+++  KVSLT =', KVSLT
        WRITE (LOUT,'(1X,A,I10)') '+++VDAFRD+++  KVDLA =', KVDLA
        WRITE (LOUT,'(1X,A,I10)') '+++VDAFRD+++  KVZPW =', KVZPW
        WRITE (LOUT,'(1X,A,I10)') '+++VDAFRD+++  KVWGE =', KVWGE
        WRITE (LOUT,'(1X,A,I10)') '+++VDAFRD+++  KVRDO =', KVRDO
        GO TO 1000
      ENDIF
C
C ----------------------------------------------------------------------
C
C     Fill the commons which correspond to database banks:
C
C     VSLTCO (VDET slots/faces):
C
      NSLOTS = LROWS(KVSLT)
      DO I=1,NSLOTS
        JJLAYF(I) = ITABL(KVSLT,I,JVSLLA)
        PHIOFF(I) = RTABL(KVSLT,I,JVSLPH)
        ISSFLG(I) = ITABL(KVSLT,I,JVSLSS)
      ENDDO
C
C     VDLACO (VDET layers):
C
      IF (LROWS(KVDLA) .NE. NVLAYR) THEN
        WRITE (LOUT,'(1X,A,I10)')
     >       '+++VDAFRD+++  Invalid number of rows in VDLA:',
     >       LROWS(KVDLA)
        GO TO 1000
      ENDIF
      DO I=1,NVLAYR
        RWVDLA(I) = RTABL(KVDLA,I,JVDLRA)
        WATILT(I) = RTABL(KVDLA,I,JVDLTA)
        IORIEN(I) = ITABL(KVDLA,I,JVDLOR)
      ENDDO
C
C     Fill VZPWCO:
C
      NWAFEF = LROWS(KVZPW)
      DO I=1,NWAFEF
        WAFERZ(I) = RTABL(KVZPW,I,JVZPZC)
      ENDDO
C
C     Fill VWGECO:
C
      WSIZEA = RTABL(KVWGE,1,JVWGSA)
      WSIZEB = RTABL(KVWGE,1,JVWGSB)
      NZSTRP = ITABL(KVWGE,1,JVWGNZ)
      NPSTRP = ITABL(KVWGE,1,JVWGNP)
      STPITZ = RTABL(KVWGE,1,JVWGPZ)
      STPITP = RTABL(KVWGE,1,JVWGPP)
      STLENZ = RTABL(KVWGE,1,JVWGLZ)
      STLENP = RTABL(KVWGE,1,JVWGLP)
      AMNSRZ = RTABL(KVWGE,1,JVWGAZ)
      AMNSRP = RTABL(KVWGE,1,JVWGAP)
      BMNSRZ = RTABL(KVWGE,1,JVWGBZ)
      BMNSRP = RTABL(KVWGE,1,JVWGBP)
      WTHICK = RTABL(KVWGE,1,JVWGWT)
C
C     Fill VRDOCO:
C
      IF (LCOLS(KVRDO) .LT. JVRDWB) THEN
        WRITE (LOUT,'(/1X,70A1)') ('=', I=1,70)
        WRITE (LOUT,'(2A)')  ' +++VDAFRD+++ Error reading VRDO--',
     >                       'please use DB version > 199'
        WRITE (LOUT,'(1X,70A1)') ('=', I=1,70)
        WRITE (LOUT,'(1X)')
        GO TO 1000
      ENDIF
      NRDSTZ = ITABL(KVRDO,1,JVRDRZ)
      NRDSTP = ITABL(KVRDO,1,JVRDRP)
      NREFRZ = ITABL(KVRDO,1,JVRDFZ)
      NREFRP = ITABL(KVRDO,1,JVRDFP)
      NOFRDZ = ITABL(KVRDO,1,JVRDOZ)
      NOFRDP = ITABL(KVRDO,1,JVRDOP)
      NZRSSC = ITABL(KVRDO,1,JVRDSZ)
      IECORZ = ITABL(KVRDO,1,JVRDDZ)
      IECORP = ITABL(KVRDO,1,JVRDDP)
      NZEROM = ITABL(KVRDO,1,JVRDEZ)
      NPEROM = ITABL(KVRDO,1,JVRDEP)
      NWFBIT = ITABL(KVRDO,1,JVRDWB)
C
C ----------------------------------------------------------------------
C
C     Success!
C
      VDAFRD = VDOK
C
 1000 CONTINUE
      RETURN
      END
