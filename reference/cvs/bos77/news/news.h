   ALE:BOS77.HLB, .OLB, _D.OLB, .FOR, .NEWS
   BOS77 OLDLIB GENERA, TXTLIB GENERA, NEWS GENERA

   +++++++++++++++++++ BECAREFUL ++++++++++++++++++++++++++++++++++
   OpenVMS version must be extracted with -DALEPH_DEC
   OSF1    version must be extracted with -DUNIX -DALEPH_DEC
   SGI     version must be extracted with -DUNIX -DALEPH_SGI
   HP      version must be extracted with -DUNIX -DALEPH_HP
   IBM     version must be extracted with -DIBM
   LINUX   version must be extracted with -DUNIX -DALEPH_LINUX
   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   -------------------------------------------------------------------
 ! 970115 - 3489     - introduce ALEPH_LINUX flag (A.Waananen)
                       CHAINTP, FRASC, INTCHAP, NAMCHA, TOASC
                       add ALEPH_LINUX to ALEPH_DEC.
                       OPENA, OPENB, OPENC
                       add ALEPH_LINUX to other machines.
                       MPARAM
                       add ALEPH_LINUX flag.
                     - mods in BEPWR (flr)
                       check the EPOUTS return code just after the
                       call to EPOUTS as it is done in BBEPF.
 ! 961023 - 3389     - mods in WGARB from M.Cattaneo 
                       there was a bug when removing more than 200
                       non concecutive work banks.
 ! 960513 - 3289     - remove eprwnd because EPIO 1_76 released with
                       cernlib 96a has the correction.
 ! 960419 - 3189     - remove mods in bbeprd given by O.Callot for
                       the previous version (3089) because it was no
                       longer possible to read MINI MC.
 ! 960221 - 3089     - mods FLR2389 everywhere
                       this version is prepared for cvs
                       suppress ETA and CY205 flags
                       replace VAX flag with DEC flag
                       introduce UNIX flag to distinguish between
                       DEC OpenVMS (previous VAX flag) and DEC OSF1
                     - mods SWA2389 in
                       NSWAP
                       reset links as it is done in BSWAP.(SWA)
                     - mods OCA2389 in
                       BBEPRD
                       try to avoid to read several times the same
                       physical record.
                       BLO32W
                       special VAX version to speed up the reading
                     - mods FLR2389B in
                       BBEPF, BBOSF
                       introduce a facility to NOT write format on
                       output file. Require a call to ALFMT before
                       trying to read the output file.
                       to NOT write format put the data card:
                             COMP 'NOFM'
 ! 941020 - 23890205 - mods FLR2289 in
                       DATE,NEWS
                       BEPRD
                       return with EOF flag when file is not opened.
                       EPBLIN,EPHEAD,EPOPEN
                       add a return code in EPOPEN calling sequence.
                     - mods EPIO2289 in
                       EPOPEN
                       add READONLY flag.
 ! 930426 - 22890205 - mods GTY2189 in
                       DATE,NEWS
                       DARED
                       change the test IF(ILO.LT.0) THEN to
                       IF (ILO.LT.1) THEN
                       to solve the problem seen with a bank which has
                       more than 200 NRs.
 ! 930331 - 21890205 - mods FLR2089 in
                       DATE,NEWS
                       BTERD
                       return 1 in case of error in read statment
 ! 930121 - 20890205 - mods FLR1989 in
                       DATE,NEWS
                       INITDA
                       if number of records > 10000 then
                          real numb. of records = MOD (nrec,10000)
                          numb. of directory records = nrec/10000
                       endif

 ! 921104 - 19890205 - mods FLR1889 in
                       DATE,NEWS
                       MPARAM, OPENA, OPENB, OPENC, UNLOCK
                       introduce SGI flag
 ! 920921 - 18890205 - mods FLR1789 in
                       DATE,NEWS
                       DELAY
                       introduce a flag UNIX to avoid VAX code
                       new routines
                       NLISTB copy of NLIST to avois confusion
                       with MULTINET, TCP/IP NLIST routine.
                       on VAX only: EPDACR,EPDEFU,EPINIT
                                    as long as EPIO 1.68 will not
                                    ne installed on VAX.
                                    EPBLIN,EPHEAD,EPOPEN
                                    to use standard fortran version.
                       on UNIX only: CFRIBM,CTOIBM
                                     L.Bauerdick version
 ! 920629 - 17890205 - mods FLR1689 in:
                       DATE,NEWS
                       MPARM,OPENA,OPENB,OPENC
                       add HP machine
                       BASEVT
                       add SAVE statment, remove some unecessary labels
                       BINSEL,BINSE2,BDADIR,MREADC
                       add SAVE statment
 ! 911121 - 16890205 - mods FLR1589 in:
                       DATE,NEWS
                       BEPRD,BOSRD
                       reset JW(1) TO 0 when JW(1)<0
                       BTERD
                       suppress the print statment when EOF found.
                       FPRNN
                       use GLUNP to get the print unit number and
                       then use it in all WRITE statment.
 ! 910916 - 15890205 - mods VBL1489 in:
                       DATE,NEWS
                       MSWAP,NSWAP
                       new version sent by V.Blobel
                       link to next bank was not correctly updated.
 ! 910809 - 14890205 - mods FLR1389 in:
                       DATE,NEWS
                       BBNIC
                       change the default format from F to I at reading
                       time too.
                       BCLASW,BINSEL,BUNDSL
                       to restore original BOS code to write
                       several small $PTS on selection file.
                       Read next $PTS bank until the length of the
                       bank is equal to 4 * number of entries.
                       INTEGER FUNCTION IGTFMT('NAME')
                       returns the work bank format index for
                       'NAME' bank.
                       INTEGER FUNCTION IGTLEN (IW,LIST)
                       returns the number of banks in LIST-list.
 ! 910704 - 13890205 - mods FLR1289 in:
                       DATE,NEWS
                       BEPRD to reset record number to 0 when
                       reading a new file in direct access.
 ! 910114 - 12890205 - mods FLR1189 in:
                       DATE,NEWS
                       BEPRW   to initialize EPIO in any case
                     - mods MRU1189 in:
                       NUMSHFT wrong *IF .. *EL
 ! 900724 - 11890205 - mods FLR1089 in:
                       DATE,NEWS
                       BBEPF,BEPWR
                       to get the right physical record number.
 ! 900530 - 10890205 - mods FLR989 in:
                       DATE,NEWS
                       MREADC
                       add entry point MRESET(LDUM) to reset LUNC=-1
                       and IW(5)=LDUM
                     - mods MTA989, FLR989B in:
                       BINSEL introduce the second select file.
                       call the new routine BASEVT.
                       BUNDSL
                       increase the size of $PTS to 1002
                       BASEVT (JW,IRUN,IEVT,IER)
                       new routine which makes the event selection according to
                       SEVT,SRUN,IRUN,NEVT,TIME data cards.
                       BINSE2 (KRUN,KEVT,IER)
                       new routine which handles 2nd select file.
 ! 900501 - 9890205 - mods FLR889 in:
                      DATE,NEWS
                      BUNDSL to supress a print statment.
                      BOSTA to modify a FORMAT.
                      NLISTF(JW,N,LIST) new function :NLIST fast version
                      assuming that LIST is a CHARACTER*1 list.
                      BDADIR (LUN,NAME,IPOS) new routine : returns the
                      position of bank NAME in the sorted list of banks
                      stored on DA file. If the bank is not on the file
                      returns a negative value.
 ! 900223 - 8890205 - mods FLR789 in:
                      DATE,NEWS
                      BEPRD to use the block length set by BUNIT when
                      it is set (mandatory on CRAY).
 ! 891218 - 7890205 - mods FLR689 in:
                      DATE, NEWS
                      BINSEL to set the number of entries in $PTS bank
                      IVMAX to 0 in case there is no $PTS bank.
                      That allows a run without any selected record.
                      In this case BINSEL returns NRCPTR=-1 which means
                      end of data.
 ! 891115 - 6890205 - mods FLR589 in :
                      DATE, NEWS
                      BBCIN, BOSFM, BOSFMT to change the default format F
                                           to I.
                      BCLASW, BINSEL to allow more than 500 pointers in
                                     event directories.
                      BINSEL to call the logical function BSELEC to decide
                             whether the record is kept or not.
                      MREADC skip 1st character if it is a blank as it is
                             done in BTERD.
                      add
                      LOGICAL FUNCTION BSELEC (IWORD,MASK) called by BINSEL
 ! 890801 - 5890205 - mods FLR489 in :
                      DATE, NEWS
                      BBCIN  reset ID to IW(IDFMT+NAMJ-NSYST) in case of
                             ISC=5  because of a possible garbage coll.
                    - mods WDS489 in :
                      BBEPRD, BEPRD, BINSEL to read EPIO format in direct
                                            access mode.
 ! 890315 - 4890205 - mods FLR389 in:
                      DATE, NEWS
                      BEPRD  change IAMC to IOAC (IAMC was not set, so
                           it worked as long as the memory was set to 0)
                      BCLASW the call to BBSPC dhad not enough arguments
 ! 890309 - 3890205 - mods FLR289 in:
                      DATE, NEWS
                      WBANK add a ID(1)=JW(LD) after a WGARB to force
                            the IBM compiler to take the new value.
 ! 890227 - 2890205 - mods FLR189 in:
                      DATE, NEWS
                      BOSRD remove a *CA UNPAKIO
                      BUNIT replace 0 by O in few variables
 ! 890224 - 1890205 - mods FLR089 in:
                      DATE, NEWS
                      BEPRW to initialize EPIO if not yet done
                      BSEQH define NAME as CHARACTER*4
 ! 890205 - 0890205 - new BOS version installed by V.Blobel
