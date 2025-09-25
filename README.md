# Delivery Location Remapping

This project is part of the GBD intrapartum care and burden estimation project funded by the Gates Foundation. 

## Sub-Project Description

This sub-project is focused on mapping the `delivery_location` variable from microdata extractions. The goal is to create a new categorization of the `delivery_location_mapped` field that is more informative and useful for analysis, and which contains information on sector/ownership (namely, public vs. private vs. non-governmental organization) and facility level (hospital, primary care, or split).

## Sub-Project Contents

| Directory or File | Description |
| --- | --- |
| [`csection_collapse/`](https://stash.ihme.washington.edu/projects/HS/repos/pcp/browse/delivery_location_remapping/csection_collapse) | Contains code for reporting metrics like count of deliveries and csection prop by NID and delivery location |
| [`initial_categorization.py`](https://stash.ihme.washington.edu/projects/HS/repos/pcp/browse/delivery_location_remapping/initial_categorization.py) | This script creates an initial, regex-based categorization of `delivery_location` values from survey microdata for manual review. |
| [`README.md`](https://stash.ihme.washington.edu/projects/HS/repos/pcp/browse/delivery_location_remapping/README.md) | Provides an overview of the sub-project and its contents. |

In the `prep` folder, the `csection_collapse/csection_collapse_by_delivery_location.R` collapses extracted microdata on NID and delivery location (the raw values of delivery location reported from surveys), to show aggregate metrics such as proportion of deliveries for that location/year/NID and delivery location response value which were C-Sections. We found this collapsed dataset useful for identifying patterns in the data and for identifying potential errors in the mapping of delivery location values. For instance when unsure whether facility level should be mapped to "prim" or "hosp", C-section prop, for responses with enough total deliveries, can be used to determine if the facility is likely a hospital or primary care.

In addition, `initial_categorization.py` assigned an initial categorization to `delivery_location` values based on regex patterns. This initial categorization was used as a starting point for manual review of the `delivery_location` values.

That said, the majority of the re-mapping process involves manual review of specific `delivery_location` values from survey microdata, by looking up the NID that the value came from in GHDx and considering any documentation, including questionairres, in tandem with the survey year and location. 

The `pipeline` then goes through data collapse, processing prior to modeling, launching st-gpr, post-processing and figure creation.

## New Categorization

| Category Label | Category Name | Examples |
| --- | --- | --- |
| home | Home | "home", "at home", "others home" |
| other | Other | "in the road", "on the way to hospital" |
| unknown | Unknown | "missing", "dont know" |
| pub_hosp | Public hospital | "government maternity hospital", "central hospital" |
| pub_prim | Public primary care | "centre de sante public", "community clinic" |
| pub_split | Public split | "government hospital/health center/health post" |
| pub_none | Public no level | "autre public", "public facility" |
| priv_hosp | Private hospital | "hôpital privé", "private hospital" |
| priv_prim | Private primary care | "clinique privée", "doctor's pvt clinic" |
| priv_split | Private split | "clinica / hospital privado" |
| priv_none | Private no level | "autre prive medical" |
| ngo_hosp | NGO/mission hospital | "district hospital (religious/voluntary)", "umn/ red cross hospital" |
| ngo_prim | NGO/mission primary care | "cham / mission health center" |
| ngo_split | NGO/mission split | "relig org hosp /clin" |
| ngo_none | NGO/mission no level | "ngo", "marie stopes" |
| no_hosp | No sector hospital | "at a hospital" |
| no_prim | No sector primary care | "at a clinic", "clinic/office of physician" |
| no_split | No sector split | "hospital/clinic" |
| no_none | Unspecified facility | "other facility", "other medical facility" |
