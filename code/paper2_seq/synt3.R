
growth_constrained_rest_free = '

# Growth factors

iHE =~ 1*HE1 + 1*HE2 + 1*HE3 + 1*HE4 + 1*HE5 + 1*HE6 + 1*HE7
ias =~ 1*as1 + 1*as2 + 1*as3 + 1*as4 + 1*as5 + 1*as6 + 1*as7
ics =~ 1*cs1 + 1*cs2 + 1*cs3 + 1*cs4 + 1*cs5 + 1*cs6 + 1*cs7
ihc =~ 1*hc1 + 1*hc2 + 1*hc3 + 1*hc4 + 1*hc5 + 1*hc6 + 1*hc7
ien =~ 1*en1 + 1*en2 + 1*en3 + 1*en4 + 1*en5 + 1*en6 + 1*en7
ilo =~ 1*lo1 + 1*lo2 + 1*lo3 + 1*lo4 + 1*lo5 + 1*lo6 + 1*lo7
ifr =~ 1*fr1 + 1*fr2 + 1*fr3 + 1*fr4 + 1*fr5 + 1*fr6 + 1*fr7
sHE =~ 0*HE1 + 1*HE2 + 2*HE3 + 3*HE4 + 4*HE5 + 5*HE6 + 6*HE7
sas =~ 0*as1 + 1*as2 + 2*as3 + 3*as4 + 4*as5 + 5*as6 + 6*as7
scs =~ 0*cs1 + 1*cs2 + 2*cs3 + 3*cs4 + 4*cs5 + 5*cs6 + 6*cs7
shc =~ 0*hc1 + 1*hc2 + 2*hc3 + 3*hc4 + 4*hc5 + 5*hc6 + 6*hc7
sen =~ 0*en1 + 1*en2 + 2*en3 + 3*en4 + 4*en5 + 5*en6 + 6*en7
slo =~ 0*lo1 + 1*lo2 + 2*lo3 + 3*lo4 + 4*lo5 + 5*lo6 + 6*lo7
sfr =~ 0*fr1 + 1*fr2 + 2*fr3 + 3*fr4 + 4*fr5 + 5*fr6 + 6*fr7

# Variancies                 

iHE ~~ var_iHE*iHE
ias ~~ var_ias*ias
ics ~~ var_ics*ics
ihc ~~ var_ihc*ihc
ien ~~ var_ien*ien
ilo ~~ var_ilo*ilo
ifr ~~ var_ifr*ifr
sHE ~~ var_sHE*sHE
sas ~~ var_sas*sas
scs ~~ var_scs*scs
shc ~~ var_shc*shc
sen ~~ var_sen*sen
slo ~~ var_slo*slo
sfr ~~ var_sfr*sfr

# Means

iHE ~ var_iHE*1
ias ~ var_ias*1
ics ~ var_ics*1
ihc ~ var_ihc*1
ien ~ var_ien*1
ilo ~ var_ilo*1
ifr ~ var_ifr*1
sHE ~ var_sHE*1
sas ~ var_sas*1
scs ~ var_scs*1
shc ~ var_shc*1
sen ~ var_sen*1
slo ~ var_slo*1
sfr ~ var_sfr*1

# Intercepts

HE1 + HE2 + HE3 + HE4 + HE5 + HE6 + HE7 ~ 0*1
as1 + as2 + as3 + as4 + as5 + as6 + as7 ~ 0*1
cs1 + cs2 + cs3 + cs4 + cs5 + cs6 + cs7 ~ 0*1
hc1 + hc2 + hc3 + hc4 + hc5 + hc6 + hc7 ~ 0*1
en1 + en2 + en3 + en4 + en5 + en6 + en7 ~ 0*1
lo1 + lo2 + lo3 + lo4 + lo5 + lo6 + lo7 ~ 0*1
fr1 + fr2 + fr3 + fr4 + fr5 + fr6 + fr7 ~ 0*1

e_HE1 + e_HE2 + e_HE3 + e_HE4 + e_HE5 + e_HE6 + e_HE7 ~ 0*1
e_as1 + e_as2 + e_as3 + e_as4 + e_as5 + e_as6 + e_as7 ~ 0*1
e_cs1 + e_cs2 + e_cs3 + e_cs4 + e_cs5 + e_cs6 + e_cs7 ~ 0*1
e_hc1 + e_hc2 + e_hc3 + e_hc4 + e_hc5 + e_hc6 + e_hc7 ~ 0*1
e_en1 + e_en2 + e_en3 + e_en4 + e_en5 + e_en6 + e_en7 ~ 0*1
e_lo1 + e_lo2 + e_lo3 + e_lo4 + e_lo5 + e_lo6 + e_lo7 ~ 0*1
e_fr1 + e_fr2 + e_fr3 + e_fr4 + e_fr5 + e_fr6 + e_fr7 ~ 0*1

# Covariance between growth factors

                    

iHE ~~ cov_iHE.ias*ias
iHE ~~ cov_iHE.ics*ics
iHE ~~ cov_iHE.ihc*ihc
iHE ~~ cov_iHE.ien*ien
iHE ~~ cov_iHE.ilo*ilo
iHE ~~ cov_iHE.ifr*ifr
iHE ~~ cov_iHE.sHE*sHE
iHE ~~ cov_iHE.sas*sas
iHE ~~ cov_iHE.scs*scs
iHE ~~ cov_iHE.shc*shc
iHE ~~ cov_iHE.sen*sen
iHE ~~ cov_iHE.slo*slo
iHE ~~ cov_iHE.sfr*sfr
ias ~~ cov_ias.ics*ics
ias ~~ cov_ias.ihc*ihc
ias ~~ cov_ias.ien*ien
ias ~~ cov_ias.ilo*ilo
ias ~~ cov_ias.ifr*ifr
ias ~~ cov_ias.sHE*sHE
ias ~~ cov_ias.sas*sas
ias ~~ cov_ias.scs*scs
ias ~~ cov_ias.shc*shc
ias ~~ cov_ias.sen*sen
ias ~~ cov_ias.slo*slo
ias ~~ cov_ias.sfr*sfr
ics ~~ cov_ics.ihc*ihc
ics ~~ cov_ics.ien*ien
ics ~~ cov_ics.ilo*ilo
ics ~~ cov_ics.ifr*ifr
ics ~~ cov_ics.sHE*sHE
ics ~~ cov_ics.sas*sas
ics ~~ cov_ics.scs*scs
ics ~~ cov_ics.shc*shc
ics ~~ cov_ics.sen*sen
ics ~~ cov_ics.slo*slo
ics ~~ cov_ics.sfr*sfr
ihc ~~ cov_ihc.ien*ien
ihc ~~ cov_ihc.ilo*ilo
ihc ~~ cov_ihc.ifr*ifr
ihc ~~ cov_ihc.sHE*sHE
ihc ~~ cov_ihc.sas*sas
ihc ~~ cov_ihc.scs*scs
ihc ~~ cov_ihc.shc*shc
ihc ~~ cov_ihc.sen*sen
ihc ~~ cov_ihc.slo*slo
ihc ~~ cov_ihc.sfr*sfr
ien ~~ cov_ien.ilo*ilo
ien ~~ cov_ien.ifr*ifr
ien ~~ cov_ien.sHE*sHE
ien ~~ cov_ien.sas*sas
ien ~~ cov_ien.scs*scs
ien ~~ cov_ien.shc*shc
ien ~~ cov_ien.sen*sen
ien ~~ cov_ien.slo*slo
ien ~~ cov_ien.sfr*sfr
ilo ~~ cov_ilo.ifr*ifr
ilo ~~ cov_ilo.sHE*sHE
ilo ~~ cov_ilo.sas*sas
ilo ~~ cov_ilo.scs*scs
ilo ~~ cov_ilo.shc*shc
ilo ~~ cov_ilo.sen*sen
ilo ~~ cov_ilo.slo*slo
ilo ~~ cov_ilo.sfr*sfr
ifr ~~ cov_ifr.sHE*sHE
ifr ~~ cov_ifr.sas*sas
ifr ~~ cov_ifr.scs*scs
ifr ~~ cov_ifr.shc*shc
ifr ~~ cov_ifr.sen*sen
ifr ~~ cov_ifr.slo*slo
ifr ~~ cov_ifr.sfr*sfr
sHE ~~ cov_sHE.sas*sas
sHE ~~ cov_sHE.scs*scs
sHE ~~ cov_sHE.shc*shc
sHE ~~ cov_sHE.sen*sen
sHE ~~ cov_sHE.slo*slo
sHE ~~ cov_sHE.sfr*sfr
sas ~~ cov_sas.scs*scs
sas ~~ cov_sas.shc*shc
sas ~~ cov_sas.sen*sen
sas ~~ cov_sas.slo*slo
sas ~~ cov_sas.sfr*sfr
scs ~~ cov_scs.shc*shc
scs ~~ cov_scs.sen*sen
scs ~~ cov_scs.slo*slo
scs ~~ cov_scs.sfr*sfr
shc ~~ cov_shc.sen*sen
shc ~~ cov_shc.slo*slo
shc ~~ cov_shc.sfr*sfr
sen ~~ cov_sen.slo*slo
sen ~~ cov_sen.sfr*sfr
slo ~~ cov_slo.sfr*sfr

# Impulses: deviations from growth trajectories                       

e_HE1 =~ 1*HE1
e_HE2 =~ 1*HE2
e_HE3 =~ 1*HE3
e_HE4 =~ 1*HE4
e_HE5 =~ 1*HE5
e_HE6 =~ 1*HE6
e_HE7 =~ 1*HE7
e_as1 =~ 1*as1
e_as2 =~ 1*as2
e_as3 =~ 1*as3
e_as4 =~ 1*as4
e_as5 =~ 1*as5
e_as6 =~ 1*as6
e_as7 =~ 1*as7
e_cs1 =~ 1*cs1
e_cs2 =~ 1*cs2
e_cs3 =~ 1*cs3
e_cs4 =~ 1*cs4
e_cs5 =~ 1*cs5
e_cs6 =~ 1*cs6
e_cs7 =~ 1*cs7
e_hc1 =~ 1*hc1
e_hc2 =~ 1*hc2
e_hc3 =~ 1*hc3
e_hc4 =~ 1*hc4
e_hc5 =~ 1*hc5
e_hc6 =~ 1*hc6
e_hc7 =~ 1*hc7
e_en1 =~ 1*en1
e_en2 =~ 1*en2
e_en3 =~ 1*en3
e_en4 =~ 1*en4
e_en5 =~ 1*en5
e_en6 =~ 1*en6
e_en7 =~ 1*en7
e_lo1 =~ 1*lo1
e_lo2 =~ 1*lo2
e_lo3 =~ 1*lo3
e_lo4 =~ 1*lo4
e_lo5 =~ 1*lo5
e_lo6 =~ 1*lo6
e_lo7 =~ 1*lo7
e_fr1 =~ 1*fr1
e_fr2 =~ 1*fr2
e_fr3 =~ 1*fr3
e_fr4 =~ 1*fr4
e_fr5 =~ 1*fr5
e_fr6 =~ 1*fr6
e_fr7 =~ 1*fr7

HE1 ~~ 0*HE1
HE2 ~~ 0*HE2
HE3 ~~ 0*HE3
HE4 ~~ 0*HE4
HE5 ~~ 0*HE5
HE6 ~~ 0*HE6
HE7 ~~ 0*HE7
as1 ~~ 0*as1
as2 ~~ 0*as2
as3 ~~ 0*as3
as4 ~~ 0*as4
as5 ~~ 0*as5
as6 ~~ 0*as6
as7 ~~ 0*as7
cs1 ~~ 0*cs1
cs2 ~~ 0*cs2
cs3 ~~ 0*cs3
cs4 ~~ 0*cs4
cs5 ~~ 0*cs5
cs6 ~~ 0*cs6
cs7 ~~ 0*cs7
hc1 ~~ 0*hc1
hc2 ~~ 0*hc2
hc3 ~~ 0*hc3
hc4 ~~ 0*hc4
hc5 ~~ 0*hc5
hc6 ~~ 0*hc6
hc7 ~~ 0*hc7
en1 ~~ 0*en1
en2 ~~ 0*en2
en3 ~~ 0*en3
en4 ~~ 0*en4
en5 ~~ 0*en5
en6 ~~ 0*en6
en7 ~~ 0*en7
lo1 ~~ 0*lo1
lo2 ~~ 0*lo2
lo3 ~~ 0*lo3
lo4 ~~ 0*lo4
lo5 ~~ 0*lo5
lo6 ~~ 0*lo6
lo7 ~~ 0*lo7
fr1 ~~ 0*fr1
fr2 ~~ 0*fr2
fr3 ~~ 0*fr3
fr4 ~~ 0*fr4
fr5 ~~ 0*fr5
fr6 ~~ 0*fr6
fr7 ~~ 0*fr7

e_HE1 ~~ c(evarHE1, evarHE2)*e_HE1
e_HE2 ~~ c(evarHE1, evarHE2)*e_HE2
e_HE3 ~~ c(evarHE1, evarHE2)*e_HE3
e_HE4 ~~ c(evarHE1, evarHE2)*e_HE4
e_HE5 ~~ c(evarHE1, evarHE2)*e_HE5
e_HE6 ~~ c(evarHE1, evarHE2)*e_HE6
e_HE7 ~~ c(evarHE1, evarHE2)*e_HE7
e_as1 ~~ c(evaras1, evaras2)*e_as1
e_as2 ~~ c(evaras1, evaras2)*e_as2
e_as3 ~~ c(evaras1, evaras2)*e_as3
e_as4 ~~ c(evaras1, evaras2)*e_as4
e_as5 ~~ c(evaras1, evaras2)*e_as5
e_as6 ~~ c(evaras1, evaras2)*e_as6
e_as7 ~~ c(evaras1, evaras2)*e_as7
e_cs1 ~~ c(evarcs1, evarcs2)*e_cs1
e_cs2 ~~ c(evarcs1, evarcs2)*e_cs2
e_cs3 ~~ c(evarcs1, evarcs2)*e_cs3
e_cs4 ~~ c(evarcs1, evarcs2)*e_cs4
e_cs5 ~~ c(evarcs1, evarcs2)*e_cs5
e_cs6 ~~ c(evarcs1, evarcs2)*e_cs6
e_cs7 ~~ c(evarcs1, evarcs2)*e_cs7
e_hc1 ~~ c(evarhc1, evarhc2)*e_hc1
e_hc2 ~~ c(evarhc1, evarhc2)*e_hc2
e_hc3 ~~ c(evarhc1, evarhc2)*e_hc3
e_hc4 ~~ c(evarhc1, evarhc2)*e_hc4
e_hc5 ~~ c(evarhc1, evarhc2)*e_hc5
e_hc6 ~~ c(evarhc1, evarhc2)*e_hc6
e_hc7 ~~ c(evarhc1, evarhc2)*e_hc7
e_en1 ~~ c(evaren1, evaren2)*e_en1
e_en2 ~~ c(evaren1, evaren2)*e_en2
e_en3 ~~ c(evaren1, evaren2)*e_en3
e_en4 ~~ c(evaren1, evaren2)*e_en4
e_en5 ~~ c(evaren1, evaren2)*e_en5
e_en6 ~~ c(evaren1, evaren2)*e_en6
e_en7 ~~ c(evaren1, evaren2)*e_en7
e_lo1 ~~ c(evarlo1, evarlo2)*e_lo1
e_lo2 ~~ c(evarlo1, evarlo2)*e_lo2
e_lo3 ~~ c(evarlo1, evarlo2)*e_lo3
e_lo4 ~~ c(evarlo1, evarlo2)*e_lo4
e_lo5 ~~ c(evarlo1, evarlo2)*e_lo5
e_lo6 ~~ c(evarlo1, evarlo2)*e_lo6
e_lo7 ~~ c(evarlo1, evarlo2)*e_lo7
e_fr1 ~~ c(evarfr1, evarfr2)*e_fr1
e_fr2 ~~ c(evarfr1, evarfr2)*e_fr2
e_fr3 ~~ c(evarfr1, evarfr2)*e_fr3
e_fr4 ~~ c(evarfr1, evarfr2)*e_fr4
e_fr5 ~~ c(evarfr1, evarfr2)*e_fr5
e_fr6 ~~ c(evarfr1, evarfr2)*e_fr6
e_fr7 ~~ c(evarfr1, evarfr2)*e_fr7

# Long- and short-run effects

as2 ~ c(b_asHE1,b_asHE2)*HE1 + c(b_asas1,b_asas2)*as1 + c(b_ascs1,b_ascs2)*cs1 + c(b_ashc1,b_ashc2)*hc1 + c(b_asen1,b_asen2)*en1 + c(b_aslo1,b_aslo2)*lo1 + c(b_asfr1,b_asfr2)*fr1 + c(d_asHE1,d_asHE2)*e_HE1 + c(d_asas1,d_asas2)*e_as1 + c(d_ascs1,d_ascs2)*e_cs1 + c(d_ashc1,d_ashc2)*e_hc1 + c(d_asen1,d_asen2)*e_en1 + c(d_aslo1,d_aslo2)*e_lo1 + c(d_asfr1,d_asfr2)*e_fr1
as3 ~ c(b_asHE1,b_asHE2)*HE2 + c(b_asas1,b_asas2)*as2 + c(b_ascs1,b_ascs2)*cs2 + c(b_ashc1,b_ashc2)*hc2 + c(b_asen1,b_asen2)*en2 + c(b_aslo1,b_aslo2)*lo2 + c(b_asfr1,b_asfr2)*fr2 + c(d_asHE1,d_asHE2)*e_HE2 + c(d_asas1,d_asas2)*e_as2 + c(d_ascs1,d_ascs2)*e_cs2 + c(d_ashc1,d_ashc2)*e_hc2 + c(d_asen1,d_asen2)*e_en2 + c(d_aslo1,d_aslo2)*e_lo2 + c(d_asfr1,d_asfr2)*e_fr2
as4 ~ c(b_asHE1,b_asHE2)*HE3 + c(b_asas1,b_asas2)*as3 + c(b_ascs1,b_ascs2)*cs3 + c(b_ashc1,b_ashc2)*hc3 + c(b_asen1,b_asen2)*en3 + c(b_aslo1,b_aslo2)*lo3 + c(b_asfr1,b_asfr2)*fr3 + c(d_asHE1,d_asHE2)*e_HE3 + c(d_asas1,d_asas2)*e_as3 + c(d_ascs1,d_ascs2)*e_cs3 + c(d_ashc1,d_ashc2)*e_hc3 + c(d_asen1,d_asen2)*e_en3 + c(d_aslo1,d_aslo2)*e_lo3 + c(d_asfr1,d_asfr2)*e_fr3
as5 ~ c(b_asHE1,b_asHE2)*HE4 + c(b_asas1,b_asas2)*as4 + c(b_ascs1,b_ascs2)*cs4 + c(b_ashc1,b_ashc2)*hc4 + c(b_asen1,b_asen2)*en4 + c(b_aslo1,b_aslo2)*lo4 + c(b_asfr1,b_asfr2)*fr4 + c(d_asHE1,d_asHE2)*e_HE4 + c(d_asas1,d_asas2)*e_as4 + c(d_ascs1,d_ascs2)*e_cs4 + c(d_ashc1,d_ashc2)*e_hc4 + c(d_asen1,d_asen2)*e_en4 + c(d_aslo1,d_aslo2)*e_lo4 + c(d_asfr1,d_asfr2)*e_fr4
as6 ~ c(b_asHE1,b_asHE2)*HE5 + c(b_asas1,b_asas2)*as5 + c(b_ascs1,b_ascs2)*cs5 + c(b_ashc1,b_ashc2)*hc5 + c(b_asen1,b_asen2)*en5 + c(b_aslo1,b_aslo2)*lo5 + c(b_asfr1,b_asfr2)*fr5 + c(d_asHE1,d_asHE2)*e_HE5 + c(d_asas1,d_asas2)*e_as5 + c(d_ascs1,d_ascs2)*e_cs5 + c(d_ashc1,d_ashc2)*e_hc5 + c(d_asen1,d_asen2)*e_en5 + c(d_aslo1,d_aslo2)*e_lo5 + c(d_asfr1,d_asfr2)*e_fr5
as7 ~ c(b_asHE1,b_asHE2)*HE6 + c(b_asas1,b_asas2)*as6 + c(b_ascs1,b_ascs2)*cs6 + c(b_ashc1,b_ashc2)*hc6 + c(b_asen1,b_asen2)*en6 + c(b_aslo1,b_aslo2)*lo6 + c(b_asfr1,b_asfr2)*fr6 + c(d_asHE1,d_asHE2)*e_HE6 + c(d_asas1,d_asas2)*e_as6 + c(d_ascs1,d_ascs2)*e_cs6 + c(d_ashc1,d_ashc2)*e_hc6 + c(d_asen1,d_asen2)*e_en6 + c(d_aslo1,d_aslo2)*e_lo6 + c(d_asfr1,d_asfr2)*e_fr6
cs2 ~ c(b_csHE1,b_csHE2)*HE1 + c(b_csas1,b_csas2)*as1 + c(b_cscs1,b_cscs2)*cs1 + c(b_cshc1,b_cshc2)*hc1 + c(b_csen1,b_csen2)*en1 + c(b_cslo1,b_cslo2)*lo1 + c(b_csfr1,b_csfr2)*fr1 + c(d_csHE1,d_csHE2)*e_HE1 + c(d_csas1,d_csas2)*e_as1 + c(d_cscs1,d_cscs2)*e_cs1 + c(d_cshc1,d_cshc2)*e_hc1 + c(d_csen1,d_csen2)*e_en1 + c(d_cslo1,d_cslo2)*e_lo1 + c(d_csfr1,d_csfr2)*e_fr1
cs3 ~ c(b_csHE1,b_csHE2)*HE2 + c(b_csas1,b_csas2)*as2 + c(b_cscs1,b_cscs2)*cs2 + c(b_cshc1,b_cshc2)*hc2 + c(b_csen1,b_csen2)*en2 + c(b_cslo1,b_cslo2)*lo2 + c(b_csfr1,b_csfr2)*fr2 + c(d_csHE1,d_csHE2)*e_HE2 + c(d_csas1,d_csas2)*e_as2 + c(d_cscs1,d_cscs2)*e_cs2 + c(d_cshc1,d_cshc2)*e_hc2 + c(d_csen1,d_csen2)*e_en2 + c(d_cslo1,d_cslo2)*e_lo2 + c(d_csfr1,d_csfr2)*e_fr2
cs4 ~ c(b_csHE1,b_csHE2)*HE3 + c(b_csas1,b_csas2)*as3 + c(b_cscs1,b_cscs2)*cs3 + c(b_cshc1,b_cshc2)*hc3 + c(b_csen1,b_csen2)*en3 + c(b_cslo1,b_cslo2)*lo3 + c(b_csfr1,b_csfr2)*fr3 + c(d_csHE1,d_csHE2)*e_HE3 + c(d_csas1,d_csas2)*e_as3 + c(d_cscs1,d_cscs2)*e_cs3 + c(d_cshc1,d_cshc2)*e_hc3 + c(d_csen1,d_csen2)*e_en3 + c(d_cslo1,d_cslo2)*e_lo3 + c(d_csfr1,d_csfr2)*e_fr3
cs5 ~ c(b_csHE1,b_csHE2)*HE4 + c(b_csas1,b_csas2)*as4 + c(b_cscs1,b_cscs2)*cs4 + c(b_cshc1,b_cshc2)*hc4 + c(b_csen1,b_csen2)*en4 + c(b_cslo1,b_cslo2)*lo4 + c(b_csfr1,b_csfr2)*fr4 + c(d_csHE1,d_csHE2)*e_HE4 + c(d_csas1,d_csas2)*e_as4 + c(d_cscs1,d_cscs2)*e_cs4 + c(d_cshc1,d_cshc2)*e_hc4 + c(d_csen1,d_csen2)*e_en4 + c(d_cslo1,d_cslo2)*e_lo4 + c(d_csfr1,d_csfr2)*e_fr4
cs6 ~ c(b_csHE1,b_csHE2)*HE5 + c(b_csas1,b_csas2)*as5 + c(b_cscs1,b_cscs2)*cs5 + c(b_cshc1,b_cshc2)*hc5 + c(b_csen1,b_csen2)*en5 + c(b_cslo1,b_cslo2)*lo5 + c(b_csfr1,b_csfr2)*fr5 + c(d_csHE1,d_csHE2)*e_HE5 + c(d_csas1,d_csas2)*e_as5 + c(d_cscs1,d_cscs2)*e_cs5 + c(d_cshc1,d_cshc2)*e_hc5 + c(d_csen1,d_csen2)*e_en5 + c(d_cslo1,d_cslo2)*e_lo5 + c(d_csfr1,d_csfr2)*e_fr5
cs7 ~ c(b_csHE1,b_csHE2)*HE6 + c(b_csas1,b_csas2)*as6 + c(b_cscs1,b_cscs2)*cs6 + c(b_cshc1,b_cshc2)*hc6 + c(b_csen1,b_csen2)*en6 + c(b_cslo1,b_cslo2)*lo6 + c(b_csfr1,b_csfr2)*fr6 + c(d_csHE1,d_csHE2)*e_HE6 + c(d_csas1,d_csas2)*e_as6 + c(d_cscs1,d_cscs2)*e_cs6 + c(d_cshc1,d_cshc2)*e_hc6 + c(d_csen1,d_csen2)*e_en6 + c(d_cslo1,d_cslo2)*e_lo6 + c(d_csfr1,d_csfr2)*e_fr6
en2 ~ c(b_enHE1,b_enHE2)*HE1 + c(b_enas1,b_enas2)*as1 + c(b_encs1,b_encs2)*cs1 + c(b_enhc1,b_enhc2)*hc1 + c(b_enen1,b_enen2)*en1 + c(b_enlo1,b_enlo2)*lo1 + c(b_enfr1,b_enfr2)*fr1 + c(d_enHE1,d_enHE2)*e_HE1 + c(d_enas1,d_enas2)*e_as1 + c(d_encs1,d_encs2)*e_cs1 + c(d_enhc1,d_enhc2)*e_hc1 + c(d_enen1,d_enen2)*e_en1 + c(d_enlo1,d_enlo2)*e_lo1 + c(d_enfr1,d_enfr2)*e_fr1
en3 ~ c(b_enHE1,b_enHE2)*HE2 + c(b_enas1,b_enas2)*as2 + c(b_encs1,b_encs2)*cs2 + c(b_enhc1,b_enhc2)*hc2 + c(b_enen1,b_enen2)*en2 + c(b_enlo1,b_enlo2)*lo2 + c(b_enfr1,b_enfr2)*fr2 + c(d_enHE1,d_enHE2)*e_HE2 + c(d_enas1,d_enas2)*e_as2 + c(d_encs1,d_encs2)*e_cs2 + c(d_enhc1,d_enhc2)*e_hc2 + c(d_enen1,d_enen2)*e_en2 + c(d_enlo1,d_enlo2)*e_lo2 + c(d_enfr1,d_enfr2)*e_fr2
en4 ~ c(b_enHE1,b_enHE2)*HE3 + c(b_enas1,b_enas2)*as3 + c(b_encs1,b_encs2)*cs3 + c(b_enhc1,b_enhc2)*hc3 + c(b_enen1,b_enen2)*en3 + c(b_enlo1,b_enlo2)*lo3 + c(b_enfr1,b_enfr2)*fr3 + c(d_enHE1,d_enHE2)*e_HE3 + c(d_enas1,d_enas2)*e_as3 + c(d_encs1,d_encs2)*e_cs3 + c(d_enhc1,d_enhc2)*e_hc3 + c(d_enen1,d_enen2)*e_en3 + c(d_enlo1,d_enlo2)*e_lo3 + c(d_enfr1,d_enfr2)*e_fr3
en5 ~ c(b_enHE1,b_enHE2)*HE4 + c(b_enas1,b_enas2)*as4 + c(b_encs1,b_encs2)*cs4 + c(b_enhc1,b_enhc2)*hc4 + c(b_enen1,b_enen2)*en4 + c(b_enlo1,b_enlo2)*lo4 + c(b_enfr1,b_enfr2)*fr4 + c(d_enHE1,d_enHE2)*e_HE4 + c(d_enas1,d_enas2)*e_as4 + c(d_encs1,d_encs2)*e_cs4 + c(d_enhc1,d_enhc2)*e_hc4 + c(d_enen1,d_enen2)*e_en4 + c(d_enlo1,d_enlo2)*e_lo4 + c(d_enfr1,d_enfr2)*e_fr4
en6 ~ c(b_enHE1,b_enHE2)*HE5 + c(b_enas1,b_enas2)*as5 + c(b_encs1,b_encs2)*cs5 + c(b_enhc1,b_enhc2)*hc5 + c(b_enen1,b_enen2)*en5 + c(b_enlo1,b_enlo2)*lo5 + c(b_enfr1,b_enfr2)*fr5 + c(d_enHE1,d_enHE2)*e_HE5 + c(d_enas1,d_enas2)*e_as5 + c(d_encs1,d_encs2)*e_cs5 + c(d_enhc1,d_enhc2)*e_hc5 + c(d_enen1,d_enen2)*e_en5 + c(d_enlo1,d_enlo2)*e_lo5 + c(d_enfr1,d_enfr2)*e_fr5
en7 ~ c(b_enHE1,b_enHE2)*HE6 + c(b_enas1,b_enas2)*as6 + c(b_encs1,b_encs2)*cs6 + c(b_enhc1,b_enhc2)*hc6 + c(b_enen1,b_enen2)*en6 + c(b_enlo1,b_enlo2)*lo6 + c(b_enfr1,b_enfr2)*fr6 + c(d_enHE1,d_enHE2)*e_HE6 + c(d_enas1,d_enas2)*e_as6 + c(d_encs1,d_encs2)*e_cs6 + c(d_enhc1,d_enhc2)*e_hc6 + c(d_enen1,d_enen2)*e_en6 + c(d_enlo1,d_enlo2)*e_lo6 + c(d_enfr1,d_enfr2)*e_fr6
fr2 ~ c(b_frHE1,b_frHE2)*HE1 + c(b_fras1,b_fras2)*as1 + c(b_frcs1,b_frcs2)*cs1 + c(b_frhc1,b_frhc2)*hc1 + c(b_fren1,b_fren2)*en1 + c(b_frlo1,b_frlo2)*lo1 + c(b_frfr1,b_frfr2)*fr1 + c(d_frHE1,d_frHE2)*e_HE1 + c(d_fras1,d_fras2)*e_as1 + c(d_frcs1,d_frcs2)*e_cs1 + c(d_frhc1,d_frhc2)*e_hc1 + c(d_fren1,d_fren2)*e_en1 + c(d_frlo1,d_frlo2)*e_lo1 + c(d_frfr1,d_frfr2)*e_fr1
fr3 ~ c(b_frHE1,b_frHE2)*HE2 + c(b_fras1,b_fras2)*as2 + c(b_frcs1,b_frcs2)*cs2 + c(b_frhc1,b_frhc2)*hc2 + c(b_fren1,b_fren2)*en2 + c(b_frlo1,b_frlo2)*lo2 + c(b_frfr1,b_frfr2)*fr2 + c(d_frHE1,d_frHE2)*e_HE2 + c(d_fras1,d_fras2)*e_as2 + c(d_frcs1,d_frcs2)*e_cs2 + c(d_frhc1,d_frhc2)*e_hc2 + c(d_fren1,d_fren2)*e_en2 + c(d_frlo1,d_frlo2)*e_lo2 + c(d_frfr1,d_frfr2)*e_fr2
fr4 ~ c(b_frHE1,b_frHE2)*HE3 + c(b_fras1,b_fras2)*as3 + c(b_frcs1,b_frcs2)*cs3 + c(b_frhc1,b_frhc2)*hc3 + c(b_fren1,b_fren2)*en3 + c(b_frlo1,b_frlo2)*lo3 + c(b_frfr1,b_frfr2)*fr3 + c(d_frHE1,d_frHE2)*e_HE3 + c(d_fras1,d_fras2)*e_as3 + c(d_frcs1,d_frcs2)*e_cs3 + c(d_frhc1,d_frhc2)*e_hc3 + c(d_fren1,d_fren2)*e_en3 + c(d_frlo1,d_frlo2)*e_lo3 + c(d_frfr1,d_frfr2)*e_fr3
fr5 ~ c(b_frHE1,b_frHE2)*HE4 + c(b_fras1,b_fras2)*as4 + c(b_frcs1,b_frcs2)*cs4 + c(b_frhc1,b_frhc2)*hc4 + c(b_fren1,b_fren2)*en4 + c(b_frlo1,b_frlo2)*lo4 + c(b_frfr1,b_frfr2)*fr4 + c(d_frHE1,d_frHE2)*e_HE4 + c(d_fras1,d_fras2)*e_as4 + c(d_frcs1,d_frcs2)*e_cs4 + c(d_frhc1,d_frhc2)*e_hc4 + c(d_fren1,d_fren2)*e_en4 + c(d_frlo1,d_frlo2)*e_lo4 + c(d_frfr1,d_frfr2)*e_fr4
fr6 ~ c(b_frHE1,b_frHE2)*HE5 + c(b_fras1,b_fras2)*as5 + c(b_frcs1,b_frcs2)*cs5 + c(b_frhc1,b_frhc2)*hc5 + c(b_fren1,b_fren2)*en5 + c(b_frlo1,b_frlo2)*lo5 + c(b_frfr1,b_frfr2)*fr5 + c(d_frHE1,d_frHE2)*e_HE5 + c(d_fras1,d_fras2)*e_as5 + c(d_frcs1,d_frcs2)*e_cs5 + c(d_frhc1,d_frhc2)*e_hc5 + c(d_fren1,d_fren2)*e_en5 + c(d_frlo1,d_frlo2)*e_lo5 + c(d_frfr1,d_frfr2)*e_fr5
fr7 ~ c(b_frHE1,b_frHE2)*HE6 + c(b_fras1,b_fras2)*as6 + c(b_frcs1,b_frcs2)*cs6 + c(b_frhc1,b_frhc2)*hc6 + c(b_fren1,b_fren2)*en6 + c(b_frlo1,b_frlo2)*lo6 + c(b_frfr1,b_frfr2)*fr6 + c(d_frHE1,d_frHE2)*e_HE6 + c(d_fras1,d_fras2)*e_as6 + c(d_frcs1,d_frcs2)*e_cs6 + c(d_frhc1,d_frhc2)*e_hc6 + c(d_fren1,d_fren2)*e_en6 + c(d_frlo1,d_frlo2)*e_lo6 + c(d_frfr1,d_frfr2)*e_fr6
hc2 ~ c(b_hcHE1,b_hcHE2)*HE1 + c(b_hcas1,b_hcas2)*as1 + c(b_hccs1,b_hccs2)*cs1 + c(b_hchc1,b_hchc2)*hc1 + c(b_hcen1,b_hcen2)*en1 + c(b_hclo1,b_hclo2)*lo1 + c(b_hcfr1,b_hcfr2)*fr1 + c(d_hcHE1,d_hcHE2)*e_HE1 + c(d_hcas1,d_hcas2)*e_as1 + c(d_hccs1,d_hccs2)*e_cs1 + c(d_hchc1,d_hchc2)*e_hc1 + c(d_hcen1,d_hcen2)*e_en1 + c(d_hclo1,d_hclo2)*e_lo1 + c(d_hcfr1,d_hcfr2)*e_fr1
hc3 ~ c(b_hcHE1,b_hcHE2)*HE2 + c(b_hcas1,b_hcas2)*as2 + c(b_hccs1,b_hccs2)*cs2 + c(b_hchc1,b_hchc2)*hc2 + c(b_hcen1,b_hcen2)*en2 + c(b_hclo1,b_hclo2)*lo2 + c(b_hcfr1,b_hcfr2)*fr2 + c(d_hcHE1,d_hcHE2)*e_HE2 + c(d_hcas1,d_hcas2)*e_as2 + c(d_hccs1,d_hccs2)*e_cs2 + c(d_hchc1,d_hchc2)*e_hc2 + c(d_hcen1,d_hcen2)*e_en2 + c(d_hclo1,d_hclo2)*e_lo2 + c(d_hcfr1,d_hcfr2)*e_fr2
hc4 ~ c(b_hcHE1,b_hcHE2)*HE3 + c(b_hcas1,b_hcas2)*as3 + c(b_hccs1,b_hccs2)*cs3 + c(b_hchc1,b_hchc2)*hc3 + c(b_hcen1,b_hcen2)*en3 + c(b_hclo1,b_hclo2)*lo3 + c(b_hcfr1,b_hcfr2)*fr3 + c(d_hcHE1,d_hcHE2)*e_HE3 + c(d_hcas1,d_hcas2)*e_as3 + c(d_hccs1,d_hccs2)*e_cs3 + c(d_hchc1,d_hchc2)*e_hc3 + c(d_hcen1,d_hcen2)*e_en3 + c(d_hclo1,d_hclo2)*e_lo3 + c(d_hcfr1,d_hcfr2)*e_fr3
hc5 ~ c(b_hcHE1,b_hcHE2)*HE4 + c(b_hcas1,b_hcas2)*as4 + c(b_hccs1,b_hccs2)*cs4 + c(b_hchc1,b_hchc2)*hc4 + c(b_hcen1,b_hcen2)*en4 + c(b_hclo1,b_hclo2)*lo4 + c(b_hcfr1,b_hcfr2)*fr4 + c(d_hcHE1,d_hcHE2)*e_HE4 + c(d_hcas1,d_hcas2)*e_as4 + c(d_hccs1,d_hccs2)*e_cs4 + c(d_hchc1,d_hchc2)*e_hc4 + c(d_hcen1,d_hcen2)*e_en4 + c(d_hclo1,d_hclo2)*e_lo4 + c(d_hcfr1,d_hcfr2)*e_fr4
hc6 ~ c(b_hcHE1,b_hcHE2)*HE5 + c(b_hcas1,b_hcas2)*as5 + c(b_hccs1,b_hccs2)*cs5 + c(b_hchc1,b_hchc2)*hc5 + c(b_hcen1,b_hcen2)*en5 + c(b_hclo1,b_hclo2)*lo5 + c(b_hcfr1,b_hcfr2)*fr5 + c(d_hcHE1,d_hcHE2)*e_HE5 + c(d_hcas1,d_hcas2)*e_as5 + c(d_hccs1,d_hccs2)*e_cs5 + c(d_hchc1,d_hchc2)*e_hc5 + c(d_hcen1,d_hcen2)*e_en5 + c(d_hclo1,d_hclo2)*e_lo5 + c(d_hcfr1,d_hcfr2)*e_fr5
hc7 ~ c(b_hcHE1,b_hcHE2)*HE6 + c(b_hcas1,b_hcas2)*as6 + c(b_hccs1,b_hccs2)*cs6 + c(b_hchc1,b_hchc2)*hc6 + c(b_hcen1,b_hcen2)*en6 + c(b_hclo1,b_hclo2)*lo6 + c(b_hcfr1,b_hcfr2)*fr6 + c(d_hcHE1,d_hcHE2)*e_HE6 + c(d_hcas1,d_hcas2)*e_as6 + c(d_hccs1,d_hccs2)*e_cs6 + c(d_hchc1,d_hchc2)*e_hc6 + c(d_hcen1,d_hcen2)*e_en6 + c(d_hclo1,d_hclo2)*e_lo6 + c(d_hcfr1,d_hcfr2)*e_fr6
HE2 ~ c(b_HEHE1,b_HEHE2)*HE1 + c(b_HEas1,b_HEas2)*as1 + c(b_HEcs1,b_HEcs2)*cs1 + c(b_HEhc1,b_HEhc2)*hc1 + c(b_HEen1,b_HEen2)*en1 + c(b_HElo1,b_HElo2)*lo1 + c(b_HEfr1,b_HEfr2)*fr1 + c(d_HEHE1,d_HEHE2)*e_HE1 + c(d_HEas1,d_HEas2)*e_as1 + c(d_HEcs1,d_HEcs2)*e_cs1 + c(d_HEhc1,d_HEhc2)*e_hc1 + c(d_HEen1,d_HEen2)*e_en1 + c(d_HElo1,d_HElo2)*e_lo1 + c(d_HEfr1,d_HEfr2)*e_fr1
HE3 ~ c(b_HEHE1,b_HEHE2)*HE2 + c(b_HEas1,b_HEas2)*as2 + c(b_HEcs1,b_HEcs2)*cs2 + c(b_HEhc1,b_HEhc2)*hc2 + c(b_HEen1,b_HEen2)*en2 + c(b_HElo1,b_HElo2)*lo2 + c(b_HEfr1,b_HEfr2)*fr2 + c(d_HEHE1,d_HEHE2)*e_HE2 + c(d_HEas1,d_HEas2)*e_as2 + c(d_HEcs1,d_HEcs2)*e_cs2 + c(d_HEhc1,d_HEhc2)*e_hc2 + c(d_HEen1,d_HEen2)*e_en2 + c(d_HElo1,d_HElo2)*e_lo2 + c(d_HEfr1,d_HEfr2)*e_fr2
HE4 ~ c(b_HEHE1,b_HEHE2)*HE3 + c(b_HEas1,b_HEas2)*as3 + c(b_HEcs1,b_HEcs2)*cs3 + c(b_HEhc1,b_HEhc2)*hc3 + c(b_HEen1,b_HEen2)*en3 + c(b_HElo1,b_HElo2)*lo3 + c(b_HEfr1,b_HEfr2)*fr3 + c(d_HEHE1,d_HEHE2)*e_HE3 + c(d_HEas1,d_HEas2)*e_as3 + c(d_HEcs1,d_HEcs2)*e_cs3 + c(d_HEhc1,d_HEhc2)*e_hc3 + c(d_HEen1,d_HEen2)*e_en3 + c(d_HElo1,d_HElo2)*e_lo3 + c(d_HEfr1,d_HEfr2)*e_fr3
HE5 ~ c(b_HEHE1,b_HEHE2)*HE4 + c(b_HEas1,b_HEas2)*as4 + c(b_HEcs1,b_HEcs2)*cs4 + c(b_HEhc1,b_HEhc2)*hc4 + c(b_HEen1,b_HEen2)*en4 + c(b_HElo1,b_HElo2)*lo4 + c(b_HEfr1,b_HEfr2)*fr4 + c(d_HEHE1,d_HEHE2)*e_HE4 + c(d_HEas1,d_HEas2)*e_as4 + c(d_HEcs1,d_HEcs2)*e_cs4 + c(d_HEhc1,d_HEhc2)*e_hc4 + c(d_HEen1,d_HEen2)*e_en4 + c(d_HElo1,d_HElo2)*e_lo4 + c(d_HEfr1,d_HEfr2)*e_fr4
HE6 ~ c(b_HEHE1,b_HEHE2)*HE5 + c(b_HEas1,b_HEas2)*as5 + c(b_HEcs1,b_HEcs2)*cs5 + c(b_HEhc1,b_HEhc2)*hc5 + c(b_HEen1,b_HEen2)*en5 + c(b_HElo1,b_HElo2)*lo5 + c(b_HEfr1,b_HEfr2)*fr5 + c(d_HEHE1,d_HEHE2)*e_HE5 + c(d_HEas1,d_HEas2)*e_as5 + c(d_HEcs1,d_HEcs2)*e_cs5 + c(d_HEhc1,d_HEhc2)*e_hc5 + c(d_HEen1,d_HEen2)*e_en5 + c(d_HElo1,d_HElo2)*e_lo5 + c(d_HEfr1,d_HEfr2)*e_fr5
HE7 ~ c(b_HEHE1,b_HEHE2)*HE6 + c(b_HEas1,b_HEas2)*as6 + c(b_HEcs1,b_HEcs2)*cs6 + c(b_HEhc1,b_HEhc2)*hc6 + c(b_HEen1,b_HEen2)*en6 + c(b_HElo1,b_HElo2)*lo6 + c(b_HEfr1,b_HEfr2)*fr6 + c(d_HEHE1,d_HEHE2)*e_HE6 + c(d_HEas1,d_HEas2)*e_as6 + c(d_HEcs1,d_HEcs2)*e_cs6 + c(d_HEhc1,d_HEhc2)*e_hc6 + c(d_HEen1,d_HEen2)*e_en6 + c(d_HElo1,d_HElo2)*e_lo6 + c(d_HEfr1,d_HEfr2)*e_fr6
lo2 ~ c(b_loHE1,b_loHE2)*HE1 + c(b_loas1,b_loas2)*as1 + c(b_locs1,b_locs2)*cs1 + c(b_lohc1,b_lohc2)*hc1 + c(b_loen1,b_loen2)*en1 + c(b_lolo1,b_lolo2)*lo1 + c(b_lofr1,b_lofr2)*fr1 + c(d_loHE1,d_loHE2)*e_HE1 + c(d_loas1,d_loas2)*e_as1 + c(d_locs1,d_locs2)*e_cs1 + c(d_lohc1,d_lohc2)*e_hc1 + c(d_loen1,d_loen2)*e_en1 + c(d_lolo1,d_lolo2)*e_lo1 + c(d_lofr1,d_lofr2)*e_fr1
lo3 ~ c(b_loHE1,b_loHE2)*HE2 + c(b_loas1,b_loas2)*as2 + c(b_locs1,b_locs2)*cs2 + c(b_lohc1,b_lohc2)*hc2 + c(b_loen1,b_loen2)*en2 + c(b_lolo1,b_lolo2)*lo2 + c(b_lofr1,b_lofr2)*fr2 + c(d_loHE1,d_loHE2)*e_HE2 + c(d_loas1,d_loas2)*e_as2 + c(d_locs1,d_locs2)*e_cs2 + c(d_lohc1,d_lohc2)*e_hc2 + c(d_loen1,d_loen2)*e_en2 + c(d_lolo1,d_lolo2)*e_lo2 + c(d_lofr1,d_lofr2)*e_fr2
lo4 ~ c(b_loHE1,b_loHE2)*HE3 + c(b_loas1,b_loas2)*as3 + c(b_locs1,b_locs2)*cs3 + c(b_lohc1,b_lohc2)*hc3 + c(b_loen1,b_loen2)*en3 + c(b_lolo1,b_lolo2)*lo3 + c(b_lofr1,b_lofr2)*fr3 + c(d_loHE1,d_loHE2)*e_HE3 + c(d_loas1,d_loas2)*e_as3 + c(d_locs1,d_locs2)*e_cs3 + c(d_lohc1,d_lohc2)*e_hc3 + c(d_loen1,d_loen2)*e_en3 + c(d_lolo1,d_lolo2)*e_lo3 + c(d_lofr1,d_lofr2)*e_fr3
lo5 ~ c(b_loHE1,b_loHE2)*HE4 + c(b_loas1,b_loas2)*as4 + c(b_locs1,b_locs2)*cs4 + c(b_lohc1,b_lohc2)*hc4 + c(b_loen1,b_loen2)*en4 + c(b_lolo1,b_lolo2)*lo4 + c(b_lofr1,b_lofr2)*fr4 + c(d_loHE1,d_loHE2)*e_HE4 + c(d_loas1,d_loas2)*e_as4 + c(d_locs1,d_locs2)*e_cs4 + c(d_lohc1,d_lohc2)*e_hc4 + c(d_loen1,d_loen2)*e_en4 + c(d_lolo1,d_lolo2)*e_lo4 + c(d_lofr1,d_lofr2)*e_fr4
lo6 ~ c(b_loHE1,b_loHE2)*HE5 + c(b_loas1,b_loas2)*as5 + c(b_locs1,b_locs2)*cs5 + c(b_lohc1,b_lohc2)*hc5 + c(b_loen1,b_loen2)*en5 + c(b_lolo1,b_lolo2)*lo5 + c(b_lofr1,b_lofr2)*fr5 + c(d_loHE1,d_loHE2)*e_HE5 + c(d_loas1,d_loas2)*e_as5 + c(d_locs1,d_locs2)*e_cs5 + c(d_lohc1,d_lohc2)*e_hc5 + c(d_loen1,d_loen2)*e_en5 + c(d_lolo1,d_lolo2)*e_lo5 + c(d_lofr1,d_lofr2)*e_fr5
lo7 ~ c(b_loHE1,b_loHE2)*HE6 + c(b_loas1,b_loas2)*as6 + c(b_locs1,b_locs2)*cs6 + c(b_lohc1,b_lohc2)*hc6 + c(b_loen1,b_loen2)*en6 + c(b_lolo1,b_lolo2)*lo6 + c(b_lofr1,b_lofr2)*fr6 + c(d_loHE1,d_loHE2)*e_HE6 + c(d_loas1,d_loas2)*e_as6 + c(d_locs1,d_locs2)*e_cs6 + c(d_lohc1,d_lohc2)*e_hc6 + c(d_loen1,d_loen2)*e_en6 + c(d_lolo1,d_lolo2)*e_lo6 + c(d_lofr1,d_lofr2)*e_fr6

# Covariances between residuals

e_HE1 ~~ c(ecov_HEas1, ecov_HEas2)*e_as1
e_HE1 ~~ c(ecov_HEcs1, ecov_HEcs2)*e_cs1
e_HE1 ~~ c(ecov_HEhc1, ecov_HEhc2)*e_hc1
e_HE1 ~~ c(ecov_HEen1, ecov_HEen2)*e_en1
e_HE1 ~~ c(ecov_HElo1, ecov_HElo2)*e_lo1
e_HE1 ~~ c(ecov_HEfr1, ecov_HEfr2)*e_fr1
e_as1 ~~ c(ecov_ascs1, ecov_ascs2)*e_cs1
e_as1 ~~ c(ecov_ashc1, ecov_ashc2)*e_hc1
e_as1 ~~ c(ecov_asen1, ecov_asen2)*e_en1
e_as1 ~~ c(ecov_aslo1, ecov_aslo2)*e_lo1
e_as1 ~~ c(ecov_asfr1, ecov_asfr2)*e_fr1
e_cs1 ~~ c(ecov_cshc1, ecov_cshc2)*e_hc1
e_cs1 ~~ c(ecov_csen1, ecov_csen2)*e_en1
e_cs1 ~~ c(ecov_cslo1, ecov_cslo2)*e_lo1
e_cs1 ~~ c(ecov_csfr1, ecov_csfr2)*e_fr1
e_hc1 ~~ c(ecov_hcen1, ecov_hcen2)*e_en1
e_hc1 ~~ c(ecov_hclo1, ecov_hclo2)*e_lo1
e_hc1 ~~ c(ecov_hcfr1, ecov_hcfr2)*e_fr1
e_en1 ~~ c(ecov_enlo1, ecov_enlo2)*e_lo1
e_en1 ~~ c(ecov_enfr1, ecov_enfr2)*e_fr1
e_lo1 ~~ c(ecov_lofr1, ecov_lofr2)*e_fr1
e_HE2 ~~ c(ecov_HEas1, ecov_HEas2)*e_as2
e_HE2 ~~ c(ecov_HEcs1, ecov_HEcs2)*e_cs2
e_HE2 ~~ c(ecov_HEhc1, ecov_HEhc2)*e_hc2
e_HE2 ~~ c(ecov_HEen1, ecov_HEen2)*e_en2
e_HE2 ~~ c(ecov_HElo1, ecov_HElo2)*e_lo2
e_HE2 ~~ c(ecov_HEfr1, ecov_HEfr2)*e_fr2
e_as2 ~~ c(ecov_ascs1, ecov_ascs2)*e_cs2
e_as2 ~~ c(ecov_ashc1, ecov_ashc2)*e_hc2
e_as2 ~~ c(ecov_asen1, ecov_asen2)*e_en2
e_as2 ~~ c(ecov_aslo1, ecov_aslo2)*e_lo2
e_as2 ~~ c(ecov_asfr1, ecov_asfr2)*e_fr2
e_cs2 ~~ c(ecov_cshc1, ecov_cshc2)*e_hc2
e_cs2 ~~ c(ecov_csen1, ecov_csen2)*e_en2
e_cs2 ~~ c(ecov_cslo1, ecov_cslo2)*e_lo2
e_cs2 ~~ c(ecov_csfr1, ecov_csfr2)*e_fr2
e_hc2 ~~ c(ecov_hcen1, ecov_hcen2)*e_en2
e_hc2 ~~ c(ecov_hclo1, ecov_hclo2)*e_lo2
e_hc2 ~~ c(ecov_hcfr1, ecov_hcfr2)*e_fr2
e_en2 ~~ c(ecov_enlo1, ecov_enlo2)*e_lo2
e_en2 ~~ c(ecov_enfr1, ecov_enfr2)*e_fr2
e_lo2 ~~ c(ecov_lofr1, ecov_lofr2)*e_fr2
e_HE3 ~~ c(ecov_HEas1, ecov_HEas2)*e_as3
e_HE3 ~~ c(ecov_HEcs1, ecov_HEcs2)*e_cs3
e_HE3 ~~ c(ecov_HEhc1, ecov_HEhc2)*e_hc3
e_HE3 ~~ c(ecov_HEen1, ecov_HEen2)*e_en3
e_HE3 ~~ c(ecov_HElo1, ecov_HElo2)*e_lo3
e_HE3 ~~ c(ecov_HEfr1, ecov_HEfr2)*e_fr3
e_as3 ~~ c(ecov_ascs1, ecov_ascs2)*e_cs3
e_as3 ~~ c(ecov_ashc1, ecov_ashc2)*e_hc3
e_as3 ~~ c(ecov_asen1, ecov_asen2)*e_en3
e_as3 ~~ c(ecov_aslo1, ecov_aslo2)*e_lo3
e_as3 ~~ c(ecov_asfr1, ecov_asfr2)*e_fr3
e_cs3 ~~ c(ecov_cshc1, ecov_cshc2)*e_hc3
e_cs3 ~~ c(ecov_csen1, ecov_csen2)*e_en3
e_cs3 ~~ c(ecov_cslo1, ecov_cslo2)*e_lo3
e_cs3 ~~ c(ecov_csfr1, ecov_csfr2)*e_fr3
e_hc3 ~~ c(ecov_hcen1, ecov_hcen2)*e_en3
e_hc3 ~~ c(ecov_hclo1, ecov_hclo2)*e_lo3
e_hc3 ~~ c(ecov_hcfr1, ecov_hcfr2)*e_fr3
e_en3 ~~ c(ecov_enlo1, ecov_enlo2)*e_lo3
e_en3 ~~ c(ecov_enfr1, ecov_enfr2)*e_fr3
e_lo3 ~~ c(ecov_lofr1, ecov_lofr2)*e_fr3
e_HE4 ~~ c(ecov_HEas1, ecov_HEas2)*e_as4
e_HE4 ~~ c(ecov_HEcs1, ecov_HEcs2)*e_cs4
e_HE4 ~~ c(ecov_HEhc1, ecov_HEhc2)*e_hc4
e_HE4 ~~ c(ecov_HEen1, ecov_HEen2)*e_en4
e_HE4 ~~ c(ecov_HElo1, ecov_HElo2)*e_lo4
e_HE4 ~~ c(ecov_HEfr1, ecov_HEfr2)*e_fr4
e_as4 ~~ c(ecov_ascs1, ecov_ascs2)*e_cs4
e_as4 ~~ c(ecov_ashc1, ecov_ashc2)*e_hc4
e_as4 ~~ c(ecov_asen1, ecov_asen2)*e_en4
e_as4 ~~ c(ecov_aslo1, ecov_aslo2)*e_lo4
e_as4 ~~ c(ecov_asfr1, ecov_asfr2)*e_fr4
e_cs4 ~~ c(ecov_cshc1, ecov_cshc2)*e_hc4
e_cs4 ~~ c(ecov_csen1, ecov_csen2)*e_en4
e_cs4 ~~ c(ecov_cslo1, ecov_cslo2)*e_lo4
e_cs4 ~~ c(ecov_csfr1, ecov_csfr2)*e_fr4
e_hc4 ~~ c(ecov_hcen1, ecov_hcen2)*e_en4
e_hc4 ~~ c(ecov_hclo1, ecov_hclo2)*e_lo4
e_hc4 ~~ c(ecov_hcfr1, ecov_hcfr2)*e_fr4
e_en4 ~~ c(ecov_enlo1, ecov_enlo2)*e_lo4
e_en4 ~~ c(ecov_enfr1, ecov_enfr2)*e_fr4
e_lo4 ~~ c(ecov_lofr1, ecov_lofr2)*e_fr4
e_HE5 ~~ c(ecov_HEas1, ecov_HEas2)*e_as5
e_HE5 ~~ c(ecov_HEcs1, ecov_HEcs2)*e_cs5
e_HE5 ~~ c(ecov_HEhc1, ecov_HEhc2)*e_hc5
e_HE5 ~~ c(ecov_HEen1, ecov_HEen2)*e_en5
e_HE5 ~~ c(ecov_HElo1, ecov_HElo2)*e_lo5
e_HE5 ~~ c(ecov_HEfr1, ecov_HEfr2)*e_fr5
e_as5 ~~ c(ecov_ascs1, ecov_ascs2)*e_cs5
e_as5 ~~ c(ecov_ashc1, ecov_ashc2)*e_hc5
e_as5 ~~ c(ecov_asen1, ecov_asen2)*e_en5
e_as5 ~~ c(ecov_aslo1, ecov_aslo2)*e_lo5
e_as5 ~~ c(ecov_asfr1, ecov_asfr2)*e_fr5
e_cs5 ~~ c(ecov_cshc1, ecov_cshc2)*e_hc5
e_cs5 ~~ c(ecov_csen1, ecov_csen2)*e_en5
e_cs5 ~~ c(ecov_cslo1, ecov_cslo2)*e_lo5
e_cs5 ~~ c(ecov_csfr1, ecov_csfr2)*e_fr5
e_hc5 ~~ c(ecov_hcen1, ecov_hcen2)*e_en5
e_hc5 ~~ c(ecov_hclo1, ecov_hclo2)*e_lo5
e_hc5 ~~ c(ecov_hcfr1, ecov_hcfr2)*e_fr5
e_en5 ~~ c(ecov_enlo1, ecov_enlo2)*e_lo5
e_en5 ~~ c(ecov_enfr1, ecov_enfr2)*e_fr5
e_lo5 ~~ c(ecov_lofr1, ecov_lofr2)*e_fr5
e_HE6 ~~ c(ecov_HEas1, ecov_HEas2)*e_as6
e_HE6 ~~ c(ecov_HEcs1, ecov_HEcs2)*e_cs6
e_HE6 ~~ c(ecov_HEhc1, ecov_HEhc2)*e_hc6
e_HE6 ~~ c(ecov_HEen1, ecov_HEen2)*e_en6
e_HE6 ~~ c(ecov_HElo1, ecov_HElo2)*e_lo6
e_HE6 ~~ c(ecov_HEfr1, ecov_HEfr2)*e_fr6
e_as6 ~~ c(ecov_ascs1, ecov_ascs2)*e_cs6
e_as6 ~~ c(ecov_ashc1, ecov_ashc2)*e_hc6
e_as6 ~~ c(ecov_asen1, ecov_asen2)*e_en6
e_as6 ~~ c(ecov_aslo1, ecov_aslo2)*e_lo6
e_as6 ~~ c(ecov_asfr1, ecov_asfr2)*e_fr6
e_cs6 ~~ c(ecov_cshc1, ecov_cshc2)*e_hc6
e_cs6 ~~ c(ecov_csen1, ecov_csen2)*e_en6
e_cs6 ~~ c(ecov_cslo1, ecov_cslo2)*e_lo6
e_cs6 ~~ c(ecov_csfr1, ecov_csfr2)*e_fr6
e_hc6 ~~ c(ecov_hcen1, ecov_hcen2)*e_en6
e_hc6 ~~ c(ecov_hclo1, ecov_hclo2)*e_lo6
e_hc6 ~~ c(ecov_hcfr1, ecov_hcfr2)*e_fr6
e_en6 ~~ c(ecov_enlo1, ecov_enlo2)*e_lo6
e_en6 ~~ c(ecov_enfr1, ecov_enfr2)*e_fr6
e_lo6 ~~ c(ecov_lofr1, ecov_lofr2)*e_fr6
e_HE7 ~~ c(ecov_HEas1, ecov_HEas2)*e_as7
e_HE7 ~~ c(ecov_HEcs1, ecov_HEcs2)*e_cs7
e_HE7 ~~ c(ecov_HEhc1, ecov_HEhc2)*e_hc7
e_HE7 ~~ c(ecov_HEen1, ecov_HEen2)*e_en7
e_HE7 ~~ c(ecov_HElo1, ecov_HElo2)*e_lo7
e_HE7 ~~ c(ecov_HEfr1, ecov_HEfr2)*e_fr7
e_as7 ~~ c(ecov_ascs1, ecov_ascs2)*e_cs7
e_as7 ~~ c(ecov_ashc1, ecov_ashc2)*e_hc7
e_as7 ~~ c(ecov_asen1, ecov_asen2)*e_en7
e_as7 ~~ c(ecov_aslo1, ecov_aslo2)*e_lo7
e_as7 ~~ c(ecov_asfr1, ecov_asfr2)*e_fr7
e_cs7 ~~ c(ecov_cshc1, ecov_cshc2)*e_hc7
e_cs7 ~~ c(ecov_csen1, ecov_csen2)*e_en7
e_cs7 ~~ c(ecov_cslo1, ecov_cslo2)*e_lo7
e_cs7 ~~ c(ecov_csfr1, ecov_csfr2)*e_fr7
e_hc7 ~~ c(ecov_hcen1, ecov_hcen2)*e_en7
e_hc7 ~~ c(ecov_hclo1, ecov_hclo2)*e_lo7
e_hc7 ~~ c(ecov_hcfr1, ecov_hcfr2)*e_fr7
e_en7 ~~ c(ecov_enlo1, ecov_enlo2)*e_lo7
e_en7 ~~ c(ecov_enfr1, ecov_enfr2)*e_fr7
e_lo7 ~~ c(ecov_lofr1, ecov_lofr2)*e_fr7

# Control variables

iHE ~ public_health_mean + inc_mean + pop_census11 + nonwhite + females + older + n + rural + London + SD
ias ~ public_health_mean + inc_mean + pop_census11 + nonwhite + females + older + n + rural + London + SD
ics ~ public_health_mean + inc_mean + pop_census11 + nonwhite + females + older + n + rural + London + SD
ihc ~ public_health_mean + inc_mean + pop_census11 + nonwhite + females + older + n + rural + London + SD
ien ~ public_health_mean + inc_mean + pop_census11 + nonwhite + females + older + n + rural + London + SD
ilo ~ public_health_mean + inc_mean + pop_census11 + nonwhite + females + older + n + rural + London + SD
ifr ~ public_health_mean + inc_mean + pop_census11 + nonwhite + females + older + n + rural + London + SD
sHE ~ public_health_mean + inc_mean + pop_census11 + nonwhite + females + older + n + rural + London + SD
sas ~ public_health_mean + inc_mean + pop_census11 + nonwhite + females + older + n + rural + London + SD
scs ~ public_health_mean + inc_mean + pop_census11 + nonwhite + females + older + n + rural + London + SD
shc ~ public_health_mean + inc_mean + pop_census11 + nonwhite + females + older + n + rural + London + SD
sen ~ public_health_mean + inc_mean + pop_census11 + nonwhite + females + older + n + rural + London + SD
slo ~ public_health_mean + inc_mean + pop_census11 + nonwhite + females + older + n + rural + London + SD
sfr ~ public_health_mean + inc_mean + pop_census11 + nonwhite + females + older + n + rural + London + SD

'
growth_constrained_rest_free_fit_dep_1 = sem(growth_constrained_rest_free,
                                               data = df_lv_1, 
                                               estimator = "mlr",
                                               orthogonal = T, 
                                               cluster = 'LAD21CD',
                                               group = 'lsoa_ses_score1')
beepr::beep()
gc()
#current time
Sys.time()
anova(all_free_fit_dep_1, growth_constrained_rest_free_fit_dep_1)

