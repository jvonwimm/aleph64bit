CKEY ITC
C! various ITC routines not included into ITCDES
 ! 980902 ALEPHLIB 309.0
    IDDIST : Modify to use polynom. or spline drift-time coeffs. 
             depending on the flag IFLGID in /IDRPCC/            (J.Sedgbeer)

 ! ALEPHLIB 20.6
   IDDIST, ITDRIF - Protect for v.large times beyond reasonable input
                    range of t-d relation.

 ! ALEPHLIB 20.5
   modifications for bunch-train (J.Sedgbeer)
   IFECON - Remove obsolete Rphi TDC bin-width test
   ICRCOO - Get Bunch-train info. (Call RQBUNC)
   ITDRIF - Use bunch train info to calculate drift-time from TDC value
   IBUNCC - New Comdeck with Bunch train info.

 ! ALEPHLIB 15.4
   ICDRIF, ITDRIF - Use function IDDIST for drift-distance calculation
   IDDIST - new function:
            Calculate drift-distance from params. in /IDRPCC/ and
            drift-time.

 Various ITC utility routines used in ALPHA or JULIA, or for users.

 Prepare data package:
   ICRCOO - Create ITC coords. from digitisings
            with ITSWCO, ITDRIF, ITROTN

 Coordinate correction and resolution information package (W.Atwood):
   IMICCO - Make corrected ITC coords. bank ICCO
   IASIGA - gives coord. and resolution info. for one track.
            with ILATCH, ILIVEW, ITXING, ITCFLG, ITQUAL, IUPCCO,
                 ITXFLG, ICDRIF, ITROTN, IUDOCA, IUVCRS
            (IXHITS obsolete? - superceded by ITXING)

 Print routines:
   ITPRDI - print digitisings
   ITPRHT - print hits (Monte Carlo)

 Various:
   ITDCOR - create Monte Carlo track to ITC digitisings relations

