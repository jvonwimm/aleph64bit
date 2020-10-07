C!  First entry of <QPI0PK> set of ALPHA QPI0DO routines
C=====================================================================
C                                                                    +
C QPI0DO user guide:                                                 +
C ------------------                                                 +
C To build pi0's using QPI0DO, just add in your main program         +
C a CALL QPI0DO, and the COMMON GAMPI0 will be filled. its           +
C content is described below.                                        +
C Beware that the mass cut used to find pi0 candidates from gampec   +
C photons is quite wide: this mass window is +/- 2*sigma around the  +
C mean gampec reconstructed pi0 mass and is parametrised in routine  +
C PI0LIM(E,AMEAN,SIGM).  For some specific analysis, a tighter cut   +
C might be needed, in that case one has to come back to the original +
C GAMPEC photons and check their invariant mass                      +
c control histos 9001 to 9005 (mass gamma gamma 1--> all types       +
c  9001+TY for pair type TY (see below) can optionnaly be filled     +
C if QPI0BK is called                                                +
C DESCRIPTION OF COMMON GAMPI0:                                      +
C------------------------------                                      +
C IQPI0           : return code  0-->OK, 1-->0, 2-->N pi0>MXPI0      +
C PIOMOM(4,MXPI0) : PI0 refitted 4 momentum                          +
C IPI0GAM(2,MXPI0): gam 1 & 2 number in the GAT section              +
C CHIPI0(MXPI0)   : chi2 value after refit (-999. if no convergence  +
C ITYPI0(MXPI0)   : pi0 type, see below                              +
C Description of pi0 types:                                          +
C--------------------------                                          +
C  TY=1: 2 photons in same PECO, with N=2 photons in the PECO        +
C  TY=2: 2 photons in same PECO, with N>2 photons in the PECO        +
C  TY=3: 2 photons in different PECO, with N=1 photons in each PECO  +
C  TY=4: 2 photons in different PECO, with N>1 photons in one PECO   +
C=====================================================================
