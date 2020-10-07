C!  First entry of <QLEPTAG> set of ALPHA routines
C=====================================================================
        QTRUTH   D. Abbaneo, F. Ligabue    24/2/93 ( FINLEP )
                 modified for ALPHA under the name QTRUTH  27/07/94

Abstract:  A routine for the determination of the truth for Monte Carlo
           tracks (candidate leptons), as to the physical process the
           track originated from, with particular regard to
           semileptonic decays of heavy-flavoured hadrons, and to the
           processes which may fake them.

Description:

         This package is the improved version of a piece of code
         originally written by Dave Cinabro within the QMUIDO
         package, for the determination of the origin of candidate
         Monte Carlo muons. With respect to the original code, this
         routine contains a number of improvements, it also deals with
         electrons and  contemplates a much wider spectrum of possible
         "physical" as well as "background" processes.
         Originally, the package is meant to be used mainly by people
         dealing with semileptonic decays of Heavy Flavours;
         nevertheless, the routine may be used by a wider population,
         namely by anyone wishing to know, whether or not a Monte Carlo
         reconstructed track is actually a lepton, and which kind of
         process it comes from, in case the answer is positive.

Usage:
         QTRUTH can be called within any ALPHA job.
         It is aimed to be used with Monte Carlo q-qbar events, but it
         also works with Z -> tau+ tau- events; if it is called for
         data tracks it gives a dummy answer.
         Given the ALPHA code of a reconstructed (Monte Carlo) track,
         the routine QTRUTH returns
         1) the LUND code of the primary quark flavour
            (it answers 0 if the event is not a MC Z -> qqbar one)

         2) a return code which specifies whether the input track is
            actually a lepton or a hadron; in the former case the lepton
            is classified according to a mumber of possible sources of
            prompt and non-prompt leptons (A lepton is here an electron
            or a muon).
            The following sources are considered:

            *  lepton from primary b decay (b -> lepton)
               The routine separates the two cases  b -> c  and  b -> u

            *  lepton from secondary b decays:
                     b -> tau   -> lepton
                     b -> c     -> lepton
                     b -> c-bar -> lepton
                     b -> c     -> tau    -> lepton
               (this last channel also contains  b -> cbar -> tau -> l
                which is very rare)

            *  lepton from primary c decay (c -> lepton)

            *  lepton from secondary c decays:
                     c -> tau -> lepton

            *  lepton from tau decay (tau -> lepton) in
               Z -> tau+ tau-  events

            *  non-prompt lepton from one of the following decay
               channels:

                     K     -> lepton
                     pion  -> lepton
                     gamma -> e+ e-
                     J/psi -> lepton
                     Psi'  -> lepton

               N.B. the track is DEFINED to come from a decay if the
                    decay vertex lies within the outer wall of the
                    TPC. If the vertex lies within the calorimeter,
                    the track is flagged as a hadron.

               * (misidentified) hadrons

               * electrons from muon decays

   We have also studied the case of heavy quarks coming from a hard gluon
   radiated before fragmentation. In case the lepton comes from such a
   "non-primary" heavy quark, we have given the return code a negative
   sign

 Calling sequence: see the routine's header .
C=====================================================================
