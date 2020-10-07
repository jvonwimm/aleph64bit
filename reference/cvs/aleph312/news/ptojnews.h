 ! ALEPHLIB 30.6
    PITMAJ,TUN1NC : fix variable type inconsistencies in function calls, 
                    for Linux                            (A.Waananen)
 ! ALEPHLIB 30.5 correction file no.3
    TFICOR - Don't make corrections for new alignment (W.Wiedenmann)

 ! ALEPHLIB 30.3 correction file no.3
    PJPFXT - New: Create the FXTR bank and fill it from PFXT  (D.Casper)

 ! ALEPHLIB 30.2 correction file no.9
    TUN1NC - fix unintentional disabling of wire coordinates
             print a message indicating whether wire coordinates will be used
             avoid error messages when reading junk events by removing error
             return on absence of PCOI (return silently instead) (D.Casper)
 ! ALEPHLIB 30.2
   new alignment (W.Wiedenman), new Kalman filter (D.Casper) and many
   corrections in the TPC reconstructions are introduced.

   PTPCOJ - support for wire coordinates
   TUN1NC - include wire coord and error when unpacking PTNC


 ! ALEPHLIB 21.5
   FPTOJ, IPTOJ, PHSTOJ - Replace LENOCC by LNBLNK (M.Cattaneo)

 ! ALEPHLIB 21.2
   FUPKCM - Speed up by computing DSQRT of constants on first call and
            remembering for subsequent calls (O. Callot)
 ! ALEPHLIB 21.1
   PTEXSJ - Also transfer PTPX to TPXS (D.Casper)

 ! ALEPHLIB 14.6
   JPLIST : use LNBLNK instead of LENOCC.
   Add JLIST(1:LNBLNK(JLIST)) or PLIST (1:LNBLNK(PLIST)) to avoid bank
   with blanc name.
   -------------------------------------------------------------------
              POT to JUL package
              ==================

    The PTOJ package (historian SET) contains routines used to unpack
  POT banks into JUL banks and related HAC parameters.


    The following naming convention has been used whenever possible:

   - steering routine to unpack x-detector POT banks to JUL: xPTOJ
   - unpacking of a POT 'Pabc' bank to a JUL 'abcd' bank   : PabcdJ
   - name-indices start with N : NPabc, Nabcd
   - BOS indices start with  J : JPabc, Jabcd
   - row indices start with  K : KPabc, Kabcd

   - miniheader length         : LMHLEN
   - # of columns offset       : LMHCOL
   - # of rows offset          : LMHROW

   - # of columns in 'Pabc'    : LPabcA or LCOLS(JPabc)
   - # of rows in 'Pabc'       : LROWS(JPabc)


    The calling sequence MUST contain at least 1 input argument: LIST
  and 1 output argument: IER.

    LIST is a character variable of 1 or 2 characters. The first one
  is the BOS event list. The second character , if equal to '-' , means
  that the unpacked POT bank(s) will be dropped and removed from the
  BOS event list.

    The user MUST set a local character variable PLIST which contains
  the list of POT banks which could be dropped by the routine.

    The user MUST set a local character variable JLIST which contains
  the list of JUL banks created by the routine. JLIST must be updated
  eveytime a new bank has been successfully created.

    These 3 variables are used by the macro  JPLIST kept as a *CD.
  This macro MUST be called at the end of the routine when at least
  one bank has been created.


    The error flag IER is set as follow: = 0  OK
                                           1  at least one POT bank missing
                                           2  not enough space
                                          -1  OK but garbage collection

    Such a routine MUST contain the following steps:

    1. - set name-indices and bank formats at 1st entry.
    2. - check that input bank exist. Return with IER=1 if not there or
         empty.
    3. - Create the output bank. Return with IER=2 if not enough space.
         set JLIST with the name of the bank just created.
         if more banks have to be created then
            if   IER=2 then goto 5.
            else append JLIST(1:LENOCC(JLIST)) with the name of the bank
                 just created.
    4. - Fill the bank using the HAC parameters.
    5. - Call macro JPLIST to drop POT bank when required and put JUL
         bank on S-list.
    6. - Set IER=-1 in case of garbage collection.

    An example is given below:

C$AF
$CD PabcJJ   HAC parameters of Pabc bank
$CD abcrJJ   HAC parameters of abcr bank
$CD abcdJJ   HAC parameters of abcd bank
$DK PabcdJ
      SUBROUTINE PabcdJ (LIST,IER)
C----------------------------------------------------------------------
C! Create JULIA 'abcd' and 'abcr' banks from POT 'Pabc' bank
C - F.Ranjard - 881201
C
C     Input :    LIST /C   BOS event list
C                          if LIST(2:2).eq.'-' drop POT banks
C
C     Output:    IER  /I   = 0  successful
C                          = 1  input bank does not exist or is empty
C                          = 2  not enough space
C                          =-1  OK but garbage collection
C======================================================================
$IF .NOT.DOC
$CA BCS         BOS array
$CA abcdJJ      'abcd'.HAC parameters
$CA abcrJJ      'abcr'.HAC parameters
$CA PabcJJ      'Pabc'.HAC parameters
C
C - LIST is the input argument which is used by the macro *CD JPLIST
C   to drop or not drop POT banks.
C   PLIST is used by *CD JPLIST : it contains the list of POT banks
C   JLIST is used by *CD JPLIST : it contains the list of JUL banks
C   !!!!!!!!!! DO NOT CHANGE THE NAMES OF THESE 3 VARIABLES!!!!!!!!
      CHARACTER*(*) LIST, PLIST*4, JLIST*4
C
      LOGICAL FIRST,BTEST
      DATA FIRST/.TRUE./
$CA BMACRO
C
C - set name-indices and bank formats at 1st entry  ===================
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        NPabc=NAMIND('Pabc')
        Nabcd=NAMIND('abcd')
        Nabcr=NAMIND('abcr')
        CALL BKFMT('abcr','I')
        CALL BKFMT('abcd','2I,(5I,2F,5I)')
      ENDIF
C
C - check existence of input banks, return if not there or empty =====
C   IER is set to 1 in this case
C
      IER=1
      JPabc = IW(JPabc)
      IF (JPabc.LE.0) GOTO 999
      NPROW = LROWS(JPabc)
      IF (NPROW.EQ.0) GOTO 999
C
C - create output bank(s), return if not enough space ================
C
      LEN = LMHLEN+NPROW*LabcdA
      CALL AUBOS('abcd',0,LEN,Jabcd,IER)
      IF (IER.EQ.2) GOTO 999
      JLIST = 'abcd'
      JPabc = IW(NPabc)
      IW(Jabcd+LMHCOL)=LabcdA
      IW(Jabcd+LMHROW)=NPROW
C
      LEN = LMHLEN + NPROW*LabcrA
      CALL AUBOS('abcr',0,LEN,Jabcr,IER)
      IF (IER.EQ.2) GOTO 998
      JLIST = JLIST(1:LENOCC(JLIST)) // 'abcr'
      JPabc = IW(NPabc)
      Jabcd = IW(Nabcd)
      IW(Jabcr+LMHCOL) = LabcrA
      IW(Jabcr+LMHROW) = NPROW
C
C - fill output bank(s) ===========================================
C
      Kabcd = Jabcd + LMHLEN
      DO 10 I=1,NPROW
C        RW(Kabcd+Jabcxy) = ITABL(JPabc,I,JPabxy) * 0.01
C
C       ------ user code ----
  10  Kabcd = Kabcd +LabcdA
C
  998 CONTINUE
C - get the drop flag if any, then drop POT banks if required, =======
C   add JUL banks to S-list
C   POT banks are on PLIST, JUL banks on JLIST
C
      PLIST = 'Pabc'
$CA JPLIST
C
C - if garbage collection then set IER = -1  =======================
C   if the AUBOS return code is 1 it means that a garbage collection
C   occured: set IER to -1 in this case.
C
      IF (IER .EQ. 1) IER = -1
C
C - return  ======================================================
999   END
$EI
