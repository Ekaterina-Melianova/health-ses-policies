
all_free = '

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

iHE ~~ c(var_iHE1,var_iHE2)*iHE
ias ~~ c(var_ias1,var_ias2)*ias
ics ~~ c(var_ics1,var_ics2)*ics
ihc ~~ c(var_ihc1,var_ihc2)*ihc
ien ~~ c(var_ien1,var_ien2)*ien
ilo ~~ c(var_ilo1,var_ilo2)*ilo
ifr ~~ c(var_ifr1,var_ifr2)*ifr
sHE ~~ c(var_sHE1,var_sHE2)*sHE
sas ~~ c(var_sas1,var_sas2)*sas
scs ~~ c(var_scs1,var_scs2)*scs
shc ~~ c(var_shc1,var_shc2)*shc
sen ~~ c(var_sen1,var_sen2)*sen
slo ~~ c(var_slo1,var_slo2)*slo
sfr ~~ c(var_sfr1,var_sfr2)*sfr

# Means

iHE ~ c(mean_i_HE1,var_iHE12)*1
ias ~ c(mean_i_as1,var_ias12)*1
ics ~ c(mean_i_cs1,var_ics12)*1
ihc ~ c(mean_i_hc1,var_ihc12)*1
ien ~ c(mean_i_en1,var_ien12)*1
ilo ~ c(mean_i_lo1,var_ilo12)*1
ifr ~ c(mean_i_fr1,var_ifr12)*1
sHE ~ c(mean_s_HE1,var_sHE12)*1
sas ~ c(mean_s_as1,var_sas12)*1
scs ~ c(mean_s_cs1,var_scs12)*1
shc ~ c(mean_s_hc1,var_shc12)*1
sen ~ c(mean_s_en1,var_sen12)*1
slo ~ c(mean_s_lo1,var_slo12)*1
sfr ~ c(mean_s_fr1,var_sfr12)*1

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

                    

iHE ~~ c(cov_iHE.ias1,cov_iHE.ias2)*ias
iHE ~~ c(cov_iHE.ics1,cov_iHE.ics2)*ics
iHE ~~ c(cov_iHE.ihc1,cov_iHE.ihc2)*ihc
iHE ~~ c(cov_iHE.ien1,cov_iHE.ien2)*ien
iHE ~~ c(cov_iHE.ilo1,cov_iHE.ilo2)*ilo
iHE ~~ c(cov_iHE.ifr1,cov_iHE.ifr2)*ifr
iHE ~~ c(cov_iHE.sHE1,cov_iHE.sHE2)*sHE
iHE ~~ c(cov_iHE.sas1,cov_iHE.sas2)*sas
iHE ~~ c(cov_iHE.scs1,cov_iHE.scs2)*scs
iHE ~~ c(cov_iHE.shc1,cov_iHE.shc2)*shc
iHE ~~ c(cov_iHE.sen1,cov_iHE.sen2)*sen
iHE ~~ c(cov_iHE.slo1,cov_iHE.slo2)*slo
iHE ~~ c(cov_iHE.sfr1,cov_iHE.sfr2)*sfr
ias ~~ c(cov_ias.ics1,cov_ias.ics2)*ics
ias ~~ c(cov_ias.ihc1,cov_ias.ihc2)*ihc
ias ~~ c(cov_ias.ien1,cov_ias.ien2)*ien
ias ~~ c(cov_ias.ilo1,cov_ias.ilo2)*ilo
ias ~~ c(cov_ias.ifr1,cov_ias.ifr2)*ifr
ias ~~ c(cov_ias.sHE1,cov_ias.sHE2)*sHE
ias ~~ c(cov_ias.sas1,cov_ias.sas2)*sas
ias ~~ c(cov_ias.scs1,cov_ias.scs2)*scs
ias ~~ c(cov_ias.shc1,cov_ias.shc2)*shc
ias ~~ c(cov_ias.sen1,cov_ias.sen2)*sen
ias ~~ c(cov_ias.slo1,cov_ias.slo2)*slo
ias ~~ c(cov_ias.sfr1,cov_ias.sfr2)*sfr
ics ~~ c(cov_ics.ihc1,cov_ics.ihc2)*ihc
ics ~~ c(cov_ics.ien1,cov_ics.ien2)*ien
ics ~~ c(cov_ics.ilo1,cov_ics.ilo2)*ilo
ics ~~ c(cov_ics.ifr1,cov_ics.ifr2)*ifr
ics ~~ c(cov_ics.sHE1,cov_ics.sHE2)*sHE
ics ~~ c(cov_ics.sas1,cov_ics.sas2)*sas
ics ~~ c(cov_ics.scs1,cov_ics.scs2)*scs
ics ~~ c(cov_ics.shc1,cov_ics.shc2)*shc
ics ~~ c(cov_ics.sen1,cov_ics.sen2)*sen
ics ~~ c(cov_ics.slo1,cov_ics.slo2)*slo
ics ~~ c(cov_ics.sfr1,cov_ics.sfr2)*sfr
ihc ~~ c(cov_ihc.ien1,cov_ihc.ien2)*ien
ihc ~~ c(cov_ihc.ilo1,cov_ihc.ilo2)*ilo
ihc ~~ c(cov_ihc.ifr1,cov_ihc.ifr2)*ifr
ihc ~~ c(cov_ihc.sHE1,cov_ihc.sHE2)*sHE
ihc ~~ c(cov_ihc.sas1,cov_ihc.sas2)*sas
ihc ~~ c(cov_ihc.scs1,cov_ihc.scs2)*scs
ihc ~~ c(cov_ihc.shc1,cov_ihc.shc2)*shc
ihc ~~ c(cov_ihc.sen1,cov_ihc.sen2)*sen
ihc ~~ c(cov_ihc.slo1,cov_ihc.slo2)*slo
ihc ~~ c(cov_ihc.sfr1,cov_ihc.sfr2)*sfr
ien ~~ c(cov_ien.ilo1,cov_ien.ilo2)*ilo
ien ~~ c(cov_ien.ifr1,cov_ien.ifr2)*ifr
ien ~~ c(cov_ien.sHE1,cov_ien.sHE2)*sHE
ien ~~ c(cov_ien.sas1,cov_ien.sas2)*sas
ien ~~ c(cov_ien.scs1,cov_ien.scs2)*scs
ien ~~ c(cov_ien.shc1,cov_ien.shc2)*shc
ien ~~ c(cov_ien.sen1,cov_ien.sen2)*sen
ien ~~ c(cov_ien.slo1,cov_ien.slo2)*slo
ien ~~ c(cov_ien.sfr1,cov_ien.sfr2)*sfr
ilo ~~ c(cov_ilo.ifr1,cov_ilo.ifr2)*ifr
ilo ~~ c(cov_ilo.sHE1,cov_ilo.sHE2)*sHE
ilo ~~ c(cov_ilo.sas1,cov_ilo.sas2)*sas
ilo ~~ c(cov_ilo.scs1,cov_ilo.scs2)*scs
ilo ~~ c(cov_ilo.shc1,cov_ilo.shc2)*shc
ilo ~~ c(cov_ilo.sen1,cov_ilo.sen2)*sen
ilo ~~ c(cov_ilo.slo1,cov_ilo.slo2)*slo
ilo ~~ c(cov_ilo.sfr1,cov_ilo.sfr2)*sfr
ifr ~~ c(cov_ifr.sHE1,cov_ifr.sHE2)*sHE
ifr ~~ c(cov_ifr.sas1,cov_ifr.sas2)*sas
ifr ~~ c(cov_ifr.scs1,cov_ifr.scs2)*scs
ifr ~~ c(cov_ifr.shc1,cov_ifr.shc2)*shc
ifr ~~ c(cov_ifr.sen1,cov_ifr.sen2)*sen
ifr ~~ c(cov_ifr.slo1,cov_ifr.slo2)*slo
ifr ~~ c(cov_ifr.sfr1,cov_ifr.sfr2)*sfr
sHE ~~ c(cov_sHE.sas1,cov_sHE.sas2)*sas
sHE ~~ c(cov_sHE.scs1,cov_sHE.scs2)*scs
sHE ~~ c(cov_sHE.shc1,cov_sHE.shc2)*shc
sHE ~~ c(cov_sHE.sen1,cov_sHE.sen2)*sen
sHE ~~ c(cov_sHE.slo1,cov_sHE.slo2)*slo
sHE ~~ c(cov_sHE.sfr1,cov_sHE.sfr2)*sfr
sas ~~ c(cov_sas.scs1,cov_sas.scs2)*scs
sas ~~ c(cov_sas.shc1,cov_sas.shc2)*shc
sas ~~ c(cov_sas.sen1,cov_sas.sen2)*sen
sas ~~ c(cov_sas.slo1,cov_sas.slo2)*slo
sas ~~ c(cov_sas.sfr1,cov_sas.sfr2)*sfr
scs ~~ c(cov_scs.shc1,cov_scs.shc2)*shc
scs ~~ c(cov_scs.sen1,cov_scs.sen2)*sen
scs ~~ c(cov_scs.slo1,cov_scs.slo2)*slo
scs ~~ c(cov_scs.sfr1,cov_scs.sfr2)*sfr
shc ~~ c(cov_shc.sen1,cov_shc.sen2)*sen
shc ~~ c(cov_shc.slo1,cov_shc.slo2)*slo
shc ~~ c(cov_shc.sfr1,cov_shc.sfr2)*sfr
sen ~~ c(cov_sen.slo1,cov_sen.slo2)*slo
sen ~~ c(cov_sen.sfr1,cov_sen.sfr2)*sfr
slo ~~ c(cov_slo.sfr1,cov_slo.sfr2)*sfr

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

iHE ~ c(a1,a2)*public_health_mean + c(b1,b2)*inc_mean + c(c1,c2)*pop_census11 + c(d1,d2)*nonwhite + c(e1,e2)*females + c(f1,f2)*older + c(g1,g2)*n + c(h1,h2)*rural + c(i1,i2)*London + c(j1,j2)*SD
ias ~ c(a1,a2)*public_health_mean + c(b1,b2)*inc_mean + c(c1,c2)*pop_census11 + c(d1,d2)*nonwhite + c(e1,e2)*females + c(f1,f2)*older + c(g1,g2)*n + c(h1,h2)*rural + c(i1,i2)*London + c(j1,j2)*SD
ics ~ c(a1,a2)*public_health_mean + c(b1,b2)*inc_mean + c(c1,c2)*pop_census11 + c(d1,d2)*nonwhite + c(e1,e2)*females + c(f1,f2)*older + c(g1,g2)*n + c(h1,h2)*rural + c(i1,i2)*London + c(j1,j2)*SD
ihc ~ c(a1,a2)*public_health_mean + c(b1,b2)*inc_mean + c(c1,c2)*pop_census11 + c(d1,d2)*nonwhite + c(e1,e2)*females + c(f1,f2)*older + c(g1,g2)*n + c(h1,h2)*rural + c(i1,i2)*London + c(j1,j2)*SD
ien ~ c(a1,a2)*public_health_mean + c(b1,b2)*inc_mean + c(c1,c2)*pop_census11 + c(d1,d2)*nonwhite + c(e1,e2)*females + c(f1,f2)*older + c(g1,g2)*n + c(h1,h2)*rural + c(i1,i2)*London + c(j1,j2)*SD
ilo ~ c(a1,a2)*public_health_mean + c(b1,b2)*inc_mean + c(c1,c2)*pop_census11 + c(d1,d2)*nonwhite + c(e1,e2)*females + c(f1,f2)*older + c(g1,g2)*n + c(h1,h2)*rural + c(i1,i2)*London + c(j1,j2)*SD
ifr ~ c(a1,a2)*public_health_mean + c(b1,b2)*inc_mean + c(c1,c2)*pop_census11 + c(d1,d2)*nonwhite + c(e1,e2)*females + c(f1,f2)*older + c(g1,g2)*n + c(h1,h2)*rural + c(i1,i2)*London + c(j1,j2)*SD
sHE ~ c(a1,a2)*public_health_mean + c(b1,b2)*inc_mean + c(c1,c2)*pop_census11 + c(d1,d2)*nonwhite + c(e1,e2)*females + c(f1,f2)*older + c(g1,g2)*n + c(h1,h2)*rural + c(i1,i2)*London + c(j1,j2)*SD
sas ~ c(a1,a2)*public_health_mean + c(b1,b2)*inc_mean + c(c1,c2)*pop_census11 + c(d1,d2)*nonwhite + c(e1,e2)*females + c(f1,f2)*older + c(g1,g2)*n + c(h1,h2)*rural + c(i1,i2)*London + c(j1,j2)*SD
scs ~ c(a1,a2)*public_health_mean + c(b1,b2)*inc_mean + c(c1,c2)*pop_census11 + c(d1,d2)*nonwhite + c(e1,e2)*females + c(f1,f2)*older + c(g1,g2)*n + c(h1,h2)*rural + c(i1,i2)*London + c(j1,j2)*SD
shc ~ c(a1,a2)*public_health_mean + c(b1,b2)*inc_mean + c(c1,c2)*pop_census11 + c(d1,d2)*nonwhite + c(e1,e2)*females + c(f1,f2)*older + c(g1,g2)*n + c(h1,h2)*rural + c(i1,i2)*London + c(j1,j2)*SD
sen ~ c(a1,a2)*public_health_mean + c(b1,b2)*inc_mean + c(c1,c2)*pop_census11 + c(d1,d2)*nonwhite + c(e1,e2)*females + c(f1,f2)*older + c(g1,g2)*n + c(h1,h2)*rural + c(i1,i2)*London + c(j1,j2)*SD
slo ~ c(a1,a2)*public_health_mean + c(b1,b2)*inc_mean + c(c1,c2)*pop_census11 + c(d1,d2)*nonwhite + c(e1,e2)*females + c(f1,f2)*older + c(g1,g2)*n + c(h1,h2)*rural + c(i1,i2)*London + c(j1,j2)*SD
sfr ~ c(a1,a2)*public_health_mean + c(b1,b2)*inc_mean + c(c1,c2)*pop_census11 + c(d1,d2)*nonwhite + c(e1,e2)*females + c(f1,f2)*older + c(g1,g2)*n + c(h1,h2)*rural + c(i1,i2)*London + c(j1,j2)*SD
'
all_free_fit_dep_1 = sem(all_free,
                           data = df_lv_1, 
                           estimator = "mlr",
                           orthogonal = T, 
                           cluster = 'LAD21CD',
                           group = 'lsoa_ses_score1')
#summary(all_free_fit_dep_1)
#corlv = inspect(all_free_fit_dep_1,"cor.lv")[[1]]
beepr::beep()
gc()
#current time
Sys.time()
#setwd to the output folder
setwd('C:/Users/ru21406/YandexDisk/PhD Research/health-ses-policies2/output/paper2')
saveRDS(all_free_fit_dep_1, "all_free_fit_dep_1.rds")


