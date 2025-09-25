################################################
# Implementing data fixes for NIDs pre collapse
# To be done post collapse_prep
################################################

# Load libraries

# Format: read in NIDS, implement fixes, save out

#NID 467681: remove answer from primary/priv_prim and move to pub_hosp/hosp
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_467681.csv")
data = data %>% mutate(pub_hosp = if_else(delivery_location == "public: chc/rural hospital/block phc", 1, pub_hosp),
                       hospital = if_else(delivery_location == "public: chc/rural hospital/block phc", 1, hospital),
                       primary = if_else(delivery_location == "public: chc/rural hospital/block phc", 0, primary),
                       pub_prim = if_else(delivery_location == "public: chc/rural hospital/block phc", 0, pub_prim),
                       delivery_location_mapped = if_else(delivery_location == "public: chc/rural hospital/block phc", "pub_hosp", delivery_location_mapped))
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_467681.csv", row.names = F)
#NID 157050: same as above
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_157050.csv")
data = data %>% mutate(pub_hosp = if_else(delivery_location == "public: chc/rural hospital/block phc", 1, pub_hosp),
                       hospital = if_else(delivery_location == "public: chc/rural hospital/block phc", 1, hospital),
                       primary = if_else(delivery_location == "public: chc/rural hospital/block phc", 0, primary),
                       pub_prim = if_else(delivery_location == "public: chc/rural hospital/block phc", 0, pub_prim),
                       delivery_location_mapped = if_else(delivery_location == "public: chc/rural hospital/block phc", "pub_hosp", delivery_location_mapped)) #this is the same as 467681, but with a different delivery location name)
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_157050.csv", row.names = F)

#NID 23183: move phc/chc to public hospital and out of public primary. sub-centre to public primary out of no_prim
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_23183.csv")
data = data %>% mutate(pub_hosp = if_else(delivery_location == "phc/chc", 1, pub_hosp),
                       hospital = if_else(delivery_location == "phc/chc", 1, hospital),
                       primary = if_else(delivery_location == "phc/chc", 0, primary),
                       pub_prim = if_else(delivery_location == "phc/chc", 0, pub_prim,),
                       pub_prim = if_else(delivery_location == "sub-centre", 1, pub_prim),
                       no_prim = if_else(delivery_location == "sub-centre", 0, no_prim),
                       primary = if_else(delivery_location == "sub-centre", 1, primary), #sub-centre is in no_prim, but should be in primary
                       delivery_location_mapped = if_else(delivery_location == "phc/chc", "pub_hosp", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location == "sub-centre", "pub_prim", delivery_location_mapped) #sub-centre is in no_prim, but should be in primary
                       )
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_23183.csv", row.names = F)
#NID 45777 move phc/chc to public hospital and out of public primary
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_45777.csv")
data = data %>% mutate(pub_hosp = if_else(delivery_location == "phc/chc", 1, pub_hosp),
                       hospital = if_else(delivery_location == "phc/chc", 1, hospital),
                       primary = if_else(delivery_location == "phc/chc", 0, primary),
                       pub_prim = if_else(delivery_location == "phc/chc", 0, pub_prim),
                       delivery_location_mapped = if_else(delivery_location == "phc/chc", "pub_hosp", delivery_location_mapped) #phc/chc is in public primary, but should be in public hospital
                       )
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_45777.csv", row.names = F)

#NID 31750: private maternity home deleted from priv primary and primary and moved to priv hospital and hospital to be consistent with 218563
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_31750.csv")
data = data %>% mutate(priv_hosp = if_else(delivery_location == "private  maternity home", 1, priv_hosp),
                       hospital = if_else(delivery_location == "private  maternity home", 1, hospital),
                       priv_prim = if_else(delivery_location == "private  maternity home", 0, priv_prim),
                       primary = if_else(delivery_location == "private  maternity home", 0, primary),
                       delivery_location_mapped = if_else(delivery_location == "private  maternity home", "priv_hosp", delivery_location_mapped)) #private maternity home is in private primary, but should be in private hospital)
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_31750.csv", row.names = F)

#NID 21331: move "dispensary" and "village health post" into the public primary category instead of priv primary and no_prim currently 
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_21331.csv")
data = data %>% mutate(pub_prim = if_else(delivery_location %in% c("dispensary", "village health post"), 1, pub_prim),
                       priv_prim = if_else(delivery_location %in% c("dispensary", "village health post"), 0, priv_prim),
                       no_prim = if_else(delivery_location %in% c("dispensary", "village health post"), 0, no_prim),
                       primary = if_else(delivery_location %in% c("dispensary", "village health post"), 1, primary),
                       delivery_location_mapped = if_else(delivery_location %in% c("dispensary", "village health post"), "pub_prim", delivery_location_mapped) #dispensary and village health post are in private primary, but should be in public primary
                       )
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_21331.csv", row.names = F)

#NID 77395:"dispensary" is in private primary, even though there is a separate "private dispensary". dispensary is instead in public primary, along with health centre, village health post which are currently in n
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_77395.csv")
data = data %>% mutate(pub_prim = if_else(delivery_location %in% c("dispensary", "health centre", "village health post"), 1, pub_prim),
                       priv_prim = if_else(delivery_location %in% c("dispensary", "health centre", "village health post"), 0, priv_prim),
                       pub_none = if_else(delivery_location %in% c("dispensary", "health centre", "village health post"), 0, pub_none),
                       primary = if_else(delivery_location %in% c("dispensary", "health centre", "village health post"), 1, primary),
                       delivery_location_mapped = if_else(delivery_location %in% c("dispensary", "health centre", "village health post"), "pub_prim", delivery_location_mapped)) #dispensary, health centre and village health post are in private primary, but should be in public primary
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_77395.csv", row.names = F)

#NID 20865: outlier, move private hosp/clinic to private primary
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_20865.csv")
data = data %>% mutate(priv_prim = if_else(delivery_location %in% c("private hospital/clinic"), 1, priv_prim),
                       priv_hosp = if_else(delivery_location %in% c("private hospital/clinic"), 0, priv_hosp),
                       hospital = if_else(delivery_location %in% c("private hospital/clinic"), 0, hospital),
                       primary = if_else(delivery_location %in% c("private hospital/clinic"), 1, primary),
                       delivery_location_mapped = if_else(delivery_location %in% c("private hospital/clinic"), "priv_prim", delivery_location_mapped))#private hospital/clinic is in private hospital, but should be in private primary
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_20865.csv", row.names = F)

# #NID 341838: move family medicine center, health house, maternity home to public primary and out of no_prim
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_341838.csv")
data = data %>% mutate(pub_prim = if_else(delivery_location %in% c("family medicine center", "health house", "maternity home"), 1, pub_prim),
                       no_prim = if_else(delivery_location %in% c("family medicine center", "health house", "maternity home"), 0, no_prim),
                       primary = if_else(delivery_location %in% c("family medicine center", "health house", "maternity home"), 1, primary),
                       delivery_location_mapped = if_else(delivery_location %in% c("family medicine center", "health house", "maternity home"), "pub_prim", delivery_location_mapped)) #family medicine center, health house and maternity home are in no_prim, but should be in public primary
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_341838.csv", row.names = F)

#NID 12455, move maternity home and sub to public primary and out of no_prim
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_12455.csv")
data = data %>% mutate(pub_prim = if_else(delivery_location %in% c("maternity home", "sub"), 1, pub_prim),
                       no_prim = if_else(delivery_location %in% c("maternity home", "sub"), 0, no_prim),
                       primary = if_else(delivery_location %in% c("maternity home", "sub"), 1, primary),
                       delivery_location_mapped = if_else(delivery_location %in% c("maternity home", "sub"), "pub_prim", delivery_location_mapped)) #maternity home and sub are in no_prim, but should be in public primary)
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_12455.csv", row.names = F)

 #NID 234733: move "integrated health center" to public primary
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_234733.csv")
data = data %>% mutate(pub_prim = if_else(delivery_location %in% c("integrated clinic/health center"), 1, pub_prim),
                       no_prim = if_else(delivery_location %in% c("integrated clinic/health center"), 0, no_prim),
                       primary = if_else(delivery_location %in% c("integrated clinic/health center"), 1, primary),
                       delivery_location_mapped = if_else(delivery_location %in% c("integrated clinic/health center"), "pub_prim", delivery_location_mapped)) #integrated clinic/health center is in no_prim, but should be in public primary
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_234733.csv", row.names = F)


#NID 107174,107172,107123,236187,236213,265259,335931,387644 code health secretariat,imss opportunities,sedena,semar, to pub_hosp and hospital, private medical unit to priv_hosp and hosp,
#other public unit to pub_none, other place to other, not specified to unknown, public route to other. add pweight of 1 for census collapse
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_107174.csv")
data = data %>% mutate(pub_hosp = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), 1, pub_hosp),
                       hospital = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), 1, hospital),
                       priv_hosp = if_else(delivery_location %in% c("private medical unit"), 1, priv_hosp),
                       hospital = if_else(delivery_location %in% c("private medical unit"), 1, hospital),
                       pub_none = if_else(delivery_location %in% c("other public unit"), 1, pub_none),
                       other = if_else(delivery_location %in% c("other place"), 1, other),
                       unknown = if_else(delivery_location %in% c("not specified"), 1, unknown),
                       other = if_else(delivery_location %in% c("public route"), 1, other),
                       delivery_location_mapped = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), "pub_hosp", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("private medical unit"), "priv_hosp", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("other public unit"), "pub_none", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("other place"), "other", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("not specified"), "unknown", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("public route"), "other", delivery_location_mapped)) %>%
  mutate(pweight = 1)
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_107174.csv", row.names = F)
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_107172.csv")
data = data %>% mutate(pub_hosp = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), 1, pub_hosp),
                       hospital = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), 1, hospital),
                       priv_hosp = if_else(delivery_location %in% c("private medical unit"), 1, priv_hosp),
                       hospital = if_else(delivery_location %in% c("private medical unit"), 1, hospital),
                       pub_none = if_else(delivery_location %in% c("other public unit"), 1, pub_none),
                       other = if_else(delivery_location %in% c("other place"), 1, other),
                       unknown = if_else(delivery_location %in% c("not specified"), 1, unknown),
                       other = if_else(delivery_location %in% c("public route"), 1, other),
                       delivery_location_mapped = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), "pub_hosp", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("private medical unit"), "priv_hosp", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("other public unit"), "pub_none", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("other place"), "other", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("not specified"), "unknown", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("public route"), "other", delivery_location_mapped)) %>%
  mutate(pweight = 1)
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_107172.csv", row.names = F)
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_107123.csv")
data = data %>% mutate(pub_hosp = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), 1, pub_hosp),
                       hospital = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), 1, hospital),
                       priv_hosp = if_else(delivery_location %in% c("private medical unit"), 1, priv_hosp),
                       hospital = if_else(delivery_location %in% c("private medical unit"), 1, hospital),
                       pub_none = if_else(delivery_location %in% c("other public unit"), 1, pub_none),
                       other = if_else(delivery_location %in% c("other place"), 1, other),
                       unknown = if_else(delivery_location %in% c("not specified"), 1, unknown),
                       other = if_else(delivery_location %in% c("public route"), 1, other),
                       delivery_location_mapped = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), "pub_hosp", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("private medical unit"), "priv_hosp", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("other public unit"), "pub_none", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("other place"), "other", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("not specified"), "unknown", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("public route"), "other", delivery_location_mapped)) %>%
  mutate(pweight = 1)
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_107123.csv", row.names = F)
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_236187.csv")
data = data %>% mutate(pub_hosp = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), 1, pub_hosp),
                       hospital = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), 1, hospital),
                       priv_hosp = if_else(delivery_location %in% c("private medical unit"), 1, priv_hosp),
                       hospital = if_else(delivery_location %in% c("private medical unit"), 1, hospital),
                       pub_none = if_else(delivery_location %in% c("other public unit"), 1, pub_none),
                       other = if_else(delivery_location %in% c("other place"), 1, other),
                       unknown = if_else(delivery_location %in% c("not specified"), 1, unknown),
                       other = if_else(delivery_location %in% c("public route"), 1, other),
                       delivery_location_mapped = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), "pub_hosp", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("private medical unit"), "priv_hosp", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("other public unit"), "pub_none", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("other place"), "other", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("not specified"), "unknown", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("public route"), "other", delivery_location_mapped)) %>%
  mutate(pweight = 1)
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_236187.csv", row.names = F)
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_236213.csv")
data = data %>% mutate(pub_hosp = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), 1, pub_hosp),
                       hospital = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), 1, hospital),
                       priv_hosp = if_else(delivery_location %in% c("private medical unit"), 1, priv_hosp),
                       hospital = if_else(delivery_location %in% c("private medical unit"), 1, hospital),
                       pub_none = if_else(delivery_location %in% c("other public unit"), 1, pub_none),
                       other = if_else(delivery_location %in% c("other place"), 1, other),
                       unknown = if_else(delivery_location %in% c("not specified"), 1, unknown),
                       other = if_else(delivery_location %in% c("public route"), 1, other),
                       delivery_location_mapped = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), "pub_hosp", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("private medical unit"), "priv_hosp", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("other public unit"), "pub_none", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("other place"), "other", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("not specified"), "unknown", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("public route"), "other", delivery_location_mapped)) %>%
  mutate(pweight = 1)
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_236213.csv", row.names = F)
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_265259.csv")
data = data %>% mutate(pub_hosp = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), 1, pub_hosp),
                       hospital = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), 1, hospital),
                       priv_hosp = if_else(delivery_location %in% c("private medical unit"), 1, priv_hosp),
                       hospital = if_else(delivery_location %in% c("private medical unit"), 1, hospital),
                       pub_none = if_else(delivery_location %in% c("other public unit"), 1, pub_none),
                       other = if_else(delivery_location %in% c("other place"), 1, other),
                       unknown = if_else(delivery_location %in% c("not specified"), 1, unknown),
                       other = if_else(delivery_location %in% c("public route"), 1, other),
                       delivery_location_mapped = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), "pub_hosp", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("private medical unit"), "priv_hosp", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("other public unit"), "pub_none", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("other place"), "other", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("not specified"), "unknown", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("public route"), "other", delivery_location_mapped)) %>%
  mutate(pweight = 1)
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_265259.csv", row.names = F)
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_335931.csv")
data = data %>% mutate(pub_hosp = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), 1, pub_hosp),
                       hospital = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), 1, hospital),
                       priv_hosp = if_else(delivery_location %in% c("private medical unit"), 1, priv_hosp),
                       hospital = if_else(delivery_location %in% c("private medical unit"), 1, hospital),
                       pub_none = if_else(delivery_location %in% c("other public unit"), 1, pub_none),
                       other = if_else(delivery_location %in% c("other place"), 1, other),
                       unknown = if_else(delivery_location %in% c("not specified"), 1, unknown),
                       other = if_else(delivery_location %in% c("public route"), 1, other),
                       delivery_location_mapped = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), "pub_hosp", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("private medical unit"), "priv_hosp", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("other public unit"), "pub_none", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("other place"), "other", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("not specified"), "unknown", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("public route"), "other", delivery_location_mapped)) %>%
  mutate(pweight = 1)
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_335931.csv", row.names = F)
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_387644.csv")
data = data %>% mutate(pub_hosp = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), 1, pub_hosp),
                       hospital = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), 1, hospital),
                       priv_hosp = if_else(delivery_location %in% c("private medical unit"), 1, priv_hosp),
                       hospital = if_else(delivery_location %in% c("private medical unit"), 1, hospital),
                       pub_none = if_else(delivery_location %in% c("other public unit"), 1, pub_none),
                       other = if_else(delivery_location %in% c("other place"), 1, other),
                       unknown = if_else(delivery_location %in% c("not specified"), 1, unknown),
                       other = if_else(delivery_location %in% c("public route"), 1, other),
                       delivery_location_mapped = if_else(delivery_location %in% c("health secretariat", "imss opportunities", "sedena", "semar"), "pub_hosp", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("private medical unit"), "priv_hosp", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("other public unit"), "pub_none", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("other place"), "other", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("not specified"), "unknown", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("public route"), "other", delivery_location_mapped)) %>%
  mutate(pweight = 1)
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_387644.csv", row.names = F)

#NID 19787 sub-centre to public primary from no_prim
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_19787.csv")
data = data %>% mutate(pub_prim = if_else(delivery_location %in% c("sub-centre"), 1, pub_prim),
                       no_prim = if_else(delivery_location %in% c("sub-centre"), 0, no_prim),
                       primary = if_else(delivery_location %in% c("sub-centre"), 1, primary),
                       delivery_location_mapped = if_else(delivery_location %in% c("sub-centre"), "pub_prim", delivery_location_mapped)) #sub-centre is in no_prim, but should be in primary)
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_19787.csv", row.names = F)

#NID 23219 sub-centre to public primary from no_prim. primary health centre to public primary from no_prim
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_23219.csv")
data = data %>% mutate(pub_prim = if_else(delivery_location %in% c("sub-centre"), 1, pub_prim),
                       no_prim = if_else(delivery_location %in% c("sub-centre"), 0, no_prim),
                       primary = if_else(delivery_location %in% c("sub-centre"), 1, primary),
                       pub_prim = if_else(delivery_location %in% c("primary health centre"), 1, pub_prim),
                       no_prim = if_else(delivery_location %in% c("primary health centre"), 0, no_prim),
                       primary = if_else(delivery_location %in% c("primary health centre"), 1, primary), #primary health centre is in no_prim, but should be in primary
                       delivery_location_mapped = if_else(delivery_location %in% c("sub-centre"), "pub_prim", delivery_location_mapped),
                       delivery_location_mapped = if_else(delivery_location %in% c("primary health centre"), "pub_prim", delivery_location_mapped)) #sub-centre is in no_prim, but should be in primary)
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_23219.csv", row.names = F)

#NID 19950 public: chc/rural hospital/block phc to public primary from public hospital
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_19950.csv")
data = data %>% mutate(pub_prim = if_else(delivery_location %in% c("public chc/rural hospital/block phc"), 1, pub_prim),
                       pub_hosp = if_else(delivery_location %in% c("public chc/rural hospital/block phc"), 0, pub_hosp),
                       primary = if_else(delivery_location %in% c("public chc/rural hospital/block phc"), 1, primary),
                       hospital = if_else(delivery_location %in% c("public chc/rural hospital/block phc"), 0, hospital), #public: chc/rural hospital/block phc is in public_hosp, but should be in primary
                       delivery_location_mapped = if_else(delivery_location %in% c("public chc/rural hospital/block phc"), "pub_prim", delivery_location_mapped)) #public: chc/rural hospital/block phc is in public_hosp, but should be in primary
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_19950.csv", row.names = F)

#NIDs 154897, 159617, 26842,19521. Egypt move private doctor from private primary to private hospital
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_154897.csv")
data = data %>% mutate(priv_hosp = if_else(delivery_location %in% c("private doctor"), 1, priv_hosp),
                       priv_prim = if_else(delivery_location %in% c("private doctor"), 0, priv_prim),
                       hospital = if_else(delivery_location %in% c("private doctor"), 1, hospital),
                       primary = if_else(delivery_location %in% c("private doctor"), 0, primary), #private doctor is in private primary, but should be in private hospital
                       delivery_location_mapped = if_else(delivery_location %in% c("private doctor"), "priv_hosp", delivery_location_mapped)) #private doctor is in private primary, but should be in private hospital
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_154897.csv", row.names = F)

data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_159617.csv")
data = data %>% mutate(priv_hosp = if_else(delivery_location %in% c("private doctor"), 1, priv_hosp),
                       priv_prim = if_else(delivery_location %in% c("private doctor"), 0, priv_prim),
                       hospital = if_else(delivery_location %in% c("private doctor"), 1, hospital),
                       primary = if_else(delivery_location %in% c("private doctor"), 0, primary),#private doctor is in private primary, but should be in private hospital
                       delivery_location_mapped = if_else(delivery_location %in% c("private doctor"), "priv_hosp", delivery_location_mapped)) #private doctor is in private primary, but should be in private hospital

write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_159617.csv", row.names = F)

data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_26842.csv")
data = data %>% mutate(priv_hosp = if_else(delivery_location %in% c("private doctor"), 1, priv_hosp),
                       priv_prim = if_else(delivery_location %in% c("private doctor"), 0, priv_prim),
                       hospital = if_else(delivery_location %in% c("private doctor"), 1, hospital),
                       primary = if_else(delivery_location %in% c("private doctor"), 0, primary), #private doctor is in private primary, but should be in private hospital
                       delivery_location_mapped = if_else(delivery_location %in% c("private doctor"), "priv_hosp", delivery_location_mapped)) #private doctor is in private primary, but should be in private hospital
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_26842.csv", row.names = F)

data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_19521.csv")
data = data %>% mutate(priv_hosp = if_else(delivery_location %in% c("private doctor"), 1, priv_hosp),
                       priv_prim = if_else(delivery_location %in% c("private doctor"), 0, priv_prim),
                       hospital = if_else(delivery_location %in% c("private doctor"), 1, hospital),
                       primary = if_else(delivery_location %in% c("private doctor"), 0, primary), #private doctor is in private primary, but should be in private hospital
                       delivery_location_mapped = if_else(delivery_location %in% c("private doctor"), "priv_hosp", delivery_location_mapped)) #private doctor is in private primary, but should be in private hospital
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_19521.csv", row.names = F)

#NID 538795 "public heatlh centre" reclassified from pub_hosp to pub_prim
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_538795.csv")
data = data %>% mutate(pub_prim = if_else(delivery_location %in% c("public health centre"), 1, pub_prim),
                       pub_hosp = if_else(delivery_location %in% c("public health centre"), 0, pub_hosp),
                       primary = if_else(delivery_location %in% c("public health centre"), 1, primary),
                       hospital = if_else(delivery_location %in% c("public health centre"), 0, hospital), #public health centre is in public_hosp, but should be in primary
                       delivery_location_mapped = if_else(delivery_location %in% c("public health centre"), "pub_prim", delivery_location_mapped)) #public health centre is in public_hosp, but should be in primary
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_538795.csv", row.names = F)


#9516, 76703, 151797 nigeria move private clinic to private hospital
# data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_9516.csv")
# data = data %>% mutate(priv_hosp = if_else(delivery_location %in% c("private clinic"), 1, priv_hosp),
#                        priv_prim = if_else(delivery_location %in% c("private clinic"), 0, priv_prim),
#                        hospital = if_else(delivery_location %in% c("private clinic"), 1, hospital),
#                        primary = if_else(delivery_location %in% c("private clinic"), 0, primary)) #private clinic is in private primary, but should be in private hospital
# write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_9516.csv", row.names = F)
# 
# data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_76703.csv")
# data = data %>% mutate(priv_hosp = if_else(delivery_location %in% c("private clinic"), 1, priv_hosp),
#                        priv_prim = if_else(delivery_location %in% c("private clinic"), 0, priv_prim),
#                        hospital = if_else(delivery_location %in% c("private clinic"), 1, hospital),
#                        primary = if_else(delivery_location %in% c("private clinic"), 0, primary)) #private clinic is in private primary, but should be in private hospital
# write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_76703.csv", row.names = F)
# 
# data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_151797.csv")
# data = data %>% mutate(priv_hosp = if_else(delivery_location %in% c("private clinic"), 1, priv_hosp),
#                        priv_prim = if_else(delivery_location %in% c("private clinic"), 0, priv_prim),
#                        hospital = if_else(delivery_location %in% c("private clinic"), 1, hospital),
#                        primary = if_else(delivery_location %in% c("private clinic"), 0, primary)) #private clinic is in private primary, but should be in private hospital
# write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_151797.csv", row.names = F)

#18834 move tirana maternity to public hospital
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_18834.csv")
data = data %>% mutate(pub_hosp = if_else(delivery_location %in% c("tirana maternity"), 1, pub_hosp),
                       hospital = if_else(delivery_location %in% c("tirana maternity"), 1, hospital),
                       pub_prim = if_else(delivery_location %in% c("tirana maternity"), 0, priv_hosp),
                       primary = if_else(delivery_location %in% c("tirana maternity"), 0, primary), #tirana maternity is in private primary, but should be in public hospital
                       delivery_location_mapped = if_else(delivery_location %in% c("tirana maternity"), "pub_hosp", delivery_location_mapped)) #tirana maternity is in private primary, but should be in public hospital
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2025-05-17/collapse_prep_18834.csv", row.names = F)
#20947 move university hospital to hospital, move health house and maternity house to pub_prim from prim
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_20947.csv")
data = data %>% mutate(pub_hosp = if_else(delivery_location %in% c("university hospital"), 1, pub_hosp),
                       hospital = if_else(delivery_location %in% c("university hospital"), 1, hospital),
                       no_prim = if_else(delivery_location %in% c("university hospital"), 0, priv_hosp),
                       delivery_location_mapped = if_else(delivery_location %in% c("university hospital"), "pub_hosp", delivery_location_mapped)) #university hospital is in private primary, but should be in public hospital
data = data %>% mutate(pub_prim = if_else(delivery_location %in% c("health house", "maternity house"), 1, pub_prim),
                       no_prim = if_else(delivery_location %in% c("health house", "maternity house"), 0, no_prim),
                       primary = if_else(delivery_location %in% c("health house", "maternity house"), 1, primary), #health house and maternity house are in private primary, but should be in public primary
                       delivery_location_mapped = if_else(delivery_location %in% c("health house", "maternity house"), "pub_prim", delivery_location_mapped)) #health house and maternity house are in private primary, but should be in public primary
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_20947.csv", row.names = F)

#20954 move maternity house, health house, health centre to public primary from no_prim
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2025-05-17/20954/collapse_prep_20954.csv")
data = data %>% mutate(pub_prim = if_else(delivery_location %in% c("maternity house", "health house", "health centre"), 1, pub_prim),
                       no_prim = if_else(delivery_location %in% c("maternity house", "health house", "health centre"), 0, no_prim),
                       primary = if_else(delivery_location %in% c("maternity house", "health house", "health centre"), 1, primary), #maternity house, health house, health centre are in no_prim, but should be in public primary
                       delivery_location_mapped = if_else(delivery_location %in% c("maternity house", "health house", "health centre"), "pub_prim", delivery_location_mapped)) #maternity house, health house, health centre are in no_prim, but should be in public primary
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2025-05-17/20954/collapse_prep_20954.csv", row.names = F)
#223669 move university hospital to pub_hosp from no_hosp, move maternity house and health house to pub_prim from no_prim
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_223669.csv")
data = data %>% mutate(pub_hosp = if_else(delivery_location %in% c("university hospital"), 1, pub_hosp),
                       no_hosp = if_else(delivery_location %in% c("university hospital"), 0, no_hosp),
                       hospital = if_else(delivery_location %in% c("university hospital"), 1, hospital), #university hospital is in no_hosp, but should be in public hospital
                       delivery_location_mapped = if_else(delivery_location %in% c("university hospital"), "pub_hosp", delivery_location_mapped)) #university hospital is in no_hosp, but should be in public hospital
data = data %>% mutate(pub_prim = if_else(delivery_location %in% c("maternity house", "health house"), 1, pub_prim),
                       no_prim = if_else(delivery_location %in% c("maternity house", "health house"), 0, no_prim),
                       primary = if_else(delivery_location %in% c("maternity house", "health house"), 1, primary), #maternity house and health house are in no_prim, but should be in public primary
                       delivery_location_mapped = if_else(delivery_location %in% c("maternity house", "health house"), "pub_prim", delivery_location_mapped)) #maternity house and health house are in no_prim, but should be in public primary
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_223669.csv", row.names = F)

#32421 move research hospital and university hospital to pub_hosp from no_hosp, move maternity house and health centre to pub_prim from no_prim
data = read.csv("/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2024-11-08/collapse_prep_32421.csv")
data = data %>% mutate(pub_hosp = if_else(delivery_location %in% c("research hospital", "university hospital"), 1, pub_hosp),
                       no_hosp = if_else(delivery_location %in% c("research hospital", "university hospital"), 0, no_hosp),
                       hospital = if_else(delivery_location %in% c("research hospital", "university hospital"), 1, hospital), #research hospital and university hospital are in no_hosp, but should be in public hospital
                       delivery_location_mapped = if_else(delivery_location %in% c("research hospital", "university hospital"), "pub_hosp", delivery_location_mapped)) #research hospital and university hospital are in no_hosp, but should be in public hospital
data = data %>% mutate(pub_prim = if_else(delivery_location %in% c("maternity house", "health centre"), 1, pub_prim),
                       no_prim = if_else(delivery_location %in% c("maternity house", "health centre"), 0, no_prim),
                       primary = if_else(delivery_location %in% c("maternity house", "health centre"), 1, primary), #maternity house and health centre are in no_prim, but should be in public primary
                       delivery_location_mapped = if_else(delivery_location %in% c("maternity house", "health centre"), "pub_prim", delivery_location_mapped)) #maternity house and health centre are in no_prim, but should be in public primary
write.csv(data, "/mnt/share/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep/2025-05-17/32421/collapse_prep_32421.csv", row.names = F)










