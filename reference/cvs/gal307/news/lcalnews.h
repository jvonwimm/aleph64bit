*CD lcalnews
C! 1st entry in LCAL set
 ! GALEPH 30.1
 There has been problems with the simulated Energy
 spectrum in Lcal for qqbar events. While the data spectrum
 falls smoothly with energy, the simulated spectrum has a
 strange 'shoulder' at about 3 GeV.

 Another complaint is that the energy in wireplanes
 in the MC look like discrete lines when plotted in
 fine bins of a few MeV.

 a) A new shower parametrization, used
    for the last Bhabha production, is incorporated.
    This makes better use of the results from Ecal testbeam
    at low energies where there was no Lcal testbeam.
    It also fluctuates some of the parameters, and reproduces
    better the longitudinal shower fluctuations seen in data.

    While this does not help on any of the above problems,
    it does make the EM showers a bit more realistic.
    The parameters of the new shower is given in a data-
    base bank LSHO, which I have submitted to Francoise.
    I have also changed the bank LECA 1 so that the energy
    is calibrated like data with 45.5 GeV electrons.

 b) The mysterious shoulder turns out to be a minimum ionising
    peak from hadrons that are tracked with GEANT through the
    volume. The energy deposited in each plane by the
    routine LCTRAK was way too high (typically 56 MeV).
    I have reduced it to 9.1 MeV (the min ionising energy
    in bank LSHO). This number may change a little after
    more studies.

 c) The energy is quantised in 40 MeV packages. This
    causes the right sampling fluctuations in EM showers
    and keeps down the computertime. This is why discrete
    lines are seen in the plots.
    The 40 MeV corresponds reasonably well to the zero
    suppression in the pads, but the wires are actually 16
    times more sensitive.

    Each "hit" in LCSHOW is weighed by a
    factor which is Poisson fluctuated around 16, and
    also multiplied the hit multiplicity in LCTRAK by 16.
    In LADC I divide again by 16. This way the wire energy
    is better reproduced - especially for min ionising
    tracks, and some smearing of the lines is provided.

 d) The end-of-job summary is changed so that it becomes
    meaningful.

 ! GALEPH 25.4
   LCIRUN : use ALGTDB instecd of MDARD to get bank depending on
            the setup code

