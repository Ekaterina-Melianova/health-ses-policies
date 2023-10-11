

synt_data = '

# Growth factors

iHE =~ 1*HE1 + 1*HE2 + 1*HE3 + 1*HE4 + 1*HE5 + 1*HE6 + 1*HE7
ias =~ 1*as1 + 1*as2 + 1*as3 + 1*as4 + 1*as5 + 1*as6 + 1*as7
sHE =~ 0*HE1 + 1*HE2 + 2*HE3 + 3*HE4 + 4*HE5 + 5*HE6 + 6*HE7
sas =~ 0*as1 + 1*as2 + 2*as3 + 3*as4 + 4*as5 + 5*as6 + 6*as7

# Variancies                 

iHE ~~ 0.5*iHE
ias ~~ 0.5*ias
sHE ~~ 0.5*sHE
sas ~~ 0.5*sas

# Means

iHE ~ 0.6*1
ias ~ 0.6*1

sHE ~ 0.3*1
sas ~ 0.3*1

# Intercepts

HE1 + HE2 + HE3 + HE4 + HE5 + HE6 + HE7 ~ 0*1
as1 + as2 + as3 + as4 + as5 + as6 + as7 ~ 0*1

e_HE1 + e_HE2 + e_HE3 + e_HE4 + e_HE5 + e_HE6 + e_HE7 ~ 0*1
e_as1 + e_as2 + e_as3 + e_as4 + e_as5 + e_as6 + e_as7 ~ 0*1

# Covariance between growth factors

                    

iHE ~~ 0.3*ias
iHE ~~ 0.3*sHE
iHE ~~ 0.3*sas
ias ~~ 0.3*sHE
ias ~~ 0.3*sas
sHE ~~ 0.3*sas

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

e_HE1 ~~ 0.2*e_HE1
e_HE2 ~~ 0.2*e_HE2
e_HE3 ~~ 0.2*e_HE3
e_HE4 ~~ 0.2*e_HE4
e_HE5 ~~ 0.2*e_HE5
e_HE6 ~~ 0.2*e_HE6
e_HE7 ~~ 0.2*e_HE7
e_as1 ~~ 0.3*e_as1
e_as2 ~~ 0.3*e_as2
e_as3 ~~ 0.3*e_as3
e_as4 ~~ 0.3*e_as4
e_as5 ~~ 0.3*e_as5
e_as6 ~~ 0.3*e_as6
e_as7 ~~ 0.3*e_as7

# Long- and short-run effects

as2 ~ 0.5*HE1 + (0.8)*as1 + 0.2*e_HE1 + 0.3*e_as1
as3 ~ 0.5*HE2 + (0.8)*as2 + 0.2*e_HE2 + 0.3*e_as2
as4 ~ 0.5*HE3 + (0.8)*as3 + 0.2*e_HE3 + 0.3*e_as3
as5 ~ 0.5*HE4 + (0.8)*as4 + 0.2*e_HE4 + 0.3*e_as4
as6 ~ 0.5*HE5 + (0.8)*as5 + 0.2*e_HE5 + 0.3*e_as5
as7 ~ 0.5*HE6 + (0.8)*as6 + 0.2*e_HE6 + 0.3*e_as6
HE2 ~ 0.3*HE1 + 0.5*as1 + 0.2*e_HE1 + 0.3*e_as1
HE3 ~ 0.3*HE2 + 0.5*as2 + 0.2*e_HE2 + 0.3*e_as2
HE4 ~ 0.3*HE3 + 0.5*as3 + 0.2*e_HE3 + 0.3*e_as3
HE5 ~ 0.3*HE4 + 0.5*as4 + 0.2*e_HE4 + 0.3*e_as4
HE6 ~ 0.3*HE5 + 0.5*as5 + 0.2*e_HE5 + 0.3*e_as5
HE7 ~ 0.3*HE6 + 0.5*as6 + 0.2*e_HE6 + 0.3*e_as6

# Covariances between residuals

e_HE1 ~~ 0.1*e_as1
e_HE2 ~~ 0.1*e_as2
e_HE3 ~~ 0.1*e_as3
e_HE4 ~~ 0.1*e_as4
e_HE5 ~~ 0.1*e_as5
e_HE6 ~~ 0.1*e_as6
e_HE7 ~~ 0.1*e_as7


'
