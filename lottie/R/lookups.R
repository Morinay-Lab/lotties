## Filename : lookups.R
## Description : Define dataframes for lookup tables

other_species_df <- data.frame(
    code = c(
        "bt",
        "cc",
        "ch",
        "ct",
        "du",
        "gc",
        "gt",
        "nh",
        "rb",
        "sk",
        "tc",
        "ut",
        "wp",
        "wr",
        "ww"),
    description = c(
        "BlueTit (bt)",
        "Chiffchaff (cc)",
        "Chaffinch (ch)",
        "CoalTit (ct)",
        "Dunnock (du)",
        "GC (gc)",
        "GreatTit (gt)",
        "Nuthatch (nh)",
        "Robin (rb)",
        "Siskin (sk)",
        "Treecreeper (tc)",
        "UnknownTit (ut)",
        "Greater Spotted Woodpecker (wp)",
        "Wren (wr)",
        "WillowWarbler (ww)"
    )
)

rings_df <- data.frame(
    code = c("None",
        "B",
        "D",
        "F",
        "G",
        "M",
        "N",
        "O",
        "P",
        "R",
        "U",
        "W",
        "Y",
        "Sd",
        "Sg",
        "Sn",
        "Sp",
        "bm",
        "dy",
        "gd",
        "ng",
        "nr",
        "od",
        "on",
        "rd",
        "ry",
        "B*",
        "G*",
        "N*",
        "R*",
        "W*",
        "Y*"),
    description = c(
        NA,
        "light blue (B)",
        "dark blue (D)",
        "flamingo (/Old light pink) (F)",
        "green (G)",
        "mauve (M)",
        "black (N)",
        "orange (O)",
        "pink (P)",
        "red (R)",
        "umber (/brown) (U)",
        "white (W)",
        "yellow (Y)",
        "striped dark blue yellow (Sd)",
        "striped light green dark green (Sg)",
        "striped black white (Sn)",
        "striped black pink (Sp)",
        "split light blue mauve (bm)",
        "split dark blue yellow (dy)",
        "split green dark blue (gd)",
        "split black green (ng)",
        "split black red (nr)",
        "split orange dark blue (od)",
        "split orange black (on)",
        "split red dark blue (rd)",
        "split red yellow (ry)",
        "pit-tag light blue (B*)",
        "pit-tag green (G*)",
        "pit-tag black (N*)",
        "pit-tag red (R*)",
        "pit-tag white (W*)",
        "pit-tag yellow (Y*)"
    )
)

person_df <- data.frame(
    code = c("SB", "LN", "MJ", "ND"),
    forename = c("Sarah (SB)", "Luke (LN)", "Micko (MJ)", "Nina (ND)") ##,
    ## surname = character("", "", "", "")
)

section_df <- data.frame(
    code = c("RV",
             "BB",
             "NH",
             "FH"),
    description = c("Rivelin Valley (RV)",
                    "Black Brook (BB)",
                    "North Hagg (NH)",
                    "Fox Hagg (FH)"))

## Conditions
conditions_df <- data.frame(
    code = c("sunny", "partly cloudy", "cloudy/ grey", "foggy", "windy", "light rain", "really rainy"),
    description = c("Sunny", "Partly cloudy", "Cloudy/ grey", "Foggy", "Windy", "Light rain", "Really rainy")
)

## Visibility
visibility_df <- data.frame(
    code = c("perfect", "good", "average", "mediocre"),
    description = c("Perfect", "Good", "Average", "Mediocre")
)

## Interactions
interactions_df <- data.frame(
    code = c("foraging together", "a chasing b", "b chasing a", "being close but not interacting",
             "other - see comments"),
    description = c("Foraging together", "A chasing B", "B chasing A", "Near but not interacting", "Other (see notes)")
)

## Ring Combinations
colour_ring_df <- data.frame(
    code = c("None", "BTO L", "BTO R", "B*BL","B*BR","B*DL","B*DR","B*GL","B*GR","B*ML","B*NL","B*NR","B*OL","B*OR","B*PL",
             "B*RL","B*SdL","B*SdR","B*SgL","B*SgR","B*SnL","B*SnR","B*SpL","B*SpR","B*SrR",
             "B*SyL","B*SyR", "B*UL", "B*WR", "B*YL", "B*gdR", "B*ngL", "B*ngR", "BBR", "BMR", "BOL", "BR*R", "BRR",
    "BSnR", "BSpL", "BUR", "BYR", "BbmL", "BbmR", "BgdL", "BgdR", "BmbL", "BmbR", "BngL", "BngR", "ByrR", "DB*L",
    "DB*R", "DDL", "DDR", "DFL", "DG*R", "DGR", "DML", "DMR", "DN*R", "DNL", "DOL", "DOR", "DP*L", "DP*R", "DRL", "DRR",
    "DSdL", "DSdR", "DSgR", "DSnL", "DSnR", "DSpL", "DSpR", "DUL", "DW*L", "DWR", "DY*L", "DYL", "DYR", "DbmL", "DbmR",
    "DgdL", "DgdR", "DmbL", "DmbR", "DngL", "DngR", "DryR", "DsdR", "FB*L", "FB*R", "FDL", "FDR", "FFL", "FG*L", "FG*R",
    "FN*R", "FOR", "FP*L", "FP*R", "FR*L", "FSdL", "FSdR", "FSnL", "FSnR", "FSpL", "FSpR", "FY*L", "FYR", "FbmL",
    "FbmR", "FgdL", "FgdR", "FmbL", "FmbR", "FngL", "FngR", "G*DL", "G*DR", "G*FR", "G*GL", "G*GR", "G*ML", "G*MR",
    "G*OR", "G*PL", "G*PR", "G*R", "G*RL", "G*RR", "G*SdL", "G*SgL", "G*SgR", "G*SnL", "G*SnR", "G*SpL", "G*SpR",
    "G*SrL", "G*SrR", "G*SyL", "G*SyR", "G*UL", "G*drL", "G*gdR", "G*gnR", "G*mbL", "G*ngR", "GBL", "GBR", "GFL", "GML",
    "GNL", "GNR", "GOL", "GOR", "GPR", "GRL", "GSdR", "GSnR", "GSpL", "GSpR", "GUR", "GW*L", "GWL", "GWR", "GYL", "GYR",
    "GbmL", "GbmR", "GgdL", "GgdR", "GmbR", "GngL", "GngR", "GryR", "MB*L", "MB*R", "MBR", "MDL", "MDR", "MG*R", "MGR",
    "MML", "MMR", "MNL", "MNR", "MOL", "MP*L", "MP*R", "MR*L", "MR*R", "MSdL", "MSdR", "MSgL", "MSnL", "MSnR", "MSpL",
    "MSpR", "MUL", "MUR", "MW*L", "MWR", "MYL", "MYR", "MbmL", "MbmR", "MgdL", "MgdR", "MmbL", "MmbR", "MngL", "MngR",
    "MryR", "N*BL", "N*BR", "N*DR", "N*FL", "N*FR", "N*L", "N*MR", "N*PR", "N*R", "N*RR", "N*SdL", "N*SdR", "N*SgL",
    "N*SgR", "N*SnL", "N*SnR", "N*SpL", "N*SpR", "N*SrL", "N*SrR", "N*SyL", "N*SyR", "N*UL", "N*UR", "N*WL", "N*YR",
    "N*gdL", "N*gdR", "N*mbL", "N/A", "NDL", "NGL", "NGR", "NNL", "NNR", "NOL", "NOR", "NPL", "NPR", "NRL", "NSnR",
    "NSpL", "NSpR", "NW*R", "NWR", "NYL", "NYR", "NbmL", "NbmR", "NgdL", "NgdR", "NmbR", "NngL", "NngR", "OB*L", "OB*R",
    "OBL", "ODL", "ODR", "OFR", "OG*R", "OGL", "OML", "OMR", "ON*L", "ONR", "OOL", "OOR", "OPL", "OPR", "OR*L", "ORL",
    "ORR", "OSdL", "OSdR", "OSgR", "OSnL", "OSnR", "OSpL", "OSpR", "OUR", "OW*R", "OWL", "OY*L", "OY*R", "OYL", "OYR",
    "ObmL", "ObmR", "OgdL", "OgdR", "OmbL", "OmbR", "OngL", "OngR", "OryR", "P*DR", "P*FL", "P*GL", "P*GR", "P*MR",
    "P*NR", "P*OL", "P*OR", "P*PR", "P*RR", "P*SdL", "P*SdR", "P*SgL", "P*SnL", "P*SnR", "P*SpR", "P*SrL", "P*SrR",
    "P*SyL", "P*SyR", "P*UL", "P*UR", "P*YL", "P*gdR", "P*gnL", "P*mbL", "PBL", "PBR", "PDL", "PML", "PMR", "PNL",
    "POL", "POR", "PSpL", "PSpR", "PWL", "PYL", "PYR", "PbmL", "PbmR", "PgdL", "PgdR", "PmbR", "PngL", "PngR", "PryR",
    "R*DL", "R*FL", "R*FR", "R*L", "R*MR", "R*NL", "R*OR", "R*PR", "R*SdL", "R*SgL", "R*SgR", "R*SnL", "R*SnR", "R*SpR",
    "R*SrR", "R*SyL", "R*WL", "R*WR", "R*YR", "R*gnR", "R*mbL", "RBL", "RBR", "RDL", "RGL", "RGR", "RML", "RMR", "RNR",
    "ROL", "RPL", "RRL", "RRR", "RSdR", "RSnR", "RSpL", "RSpR", "RWL", "RYL", "RYR", "RbmL", "RbmR", "RdgL", "RdrL",
    "RgdL", "RmbR", "RngL", "RngR", "SdB*L", "SdFL", "SdG*L", "SdML", "SdN*L", "SdN*R", "SdOL", "SdP*L", "SdP*R",
    "SdR*L", "SdR*R", "SdRL", "SdUL", "SdW*L", "SdW*R", "SdY*L", "SdY*R", "SgB*L", "SgB*R", "SgG*L", "SgG*R", "SgN*L",
    "SgN*R", "SgP*L", "SgR*L", "SgR*R", "SgTL", "SgW*L", "SgW*R", "SnB*L", "SnBL", "SnBR", "SnDL", "SnDR", "SnFL",
    "SnFR", "SnG*L", "SnG*R", "SnGR", "SnML", "SnMR", "SnN*R", "SnNL", "SnOL", "SnOR", "SnP*L", "SnP*R", "SnPR",
    "SnR*L", "SnR*R", "SnUL", "SnUR", "SnW*L", "SnW*R", "SnWR", "SnY*L", "SnY*R", "SnYR", "SpB*L", "SpB*R", "SpBL",
    "SpDL", "SpDR", "SpFL", "SpFR", "SpG*L", "SpGR", "SpML", "SpMR", "SpN*R", "SpNL", "SpNR", "SpOL", "SpOR", "SpP*L",
    "SpP*R", "SpPL", "SpR*R", "SpRL", "SpRR", "SpSnL", "SpUL", "SpUR", "SpW*R", "SpWR", "SpY*L", "SpY*R", "SpYR",
    "SrB*L", "SrB*R", "SrG*L", "SrG*R", "SrN*L", "SrN*R", "SrP*L", "SrP*R", "SrR*L", "SrR*R", "SrW*L", "SrW*R", "SrY*L",
    "SrY*R", "SyB*L", "SyB*R", "SyG*L", "SyN*L", "SyP*L", "SyP*R", "SyR*L", "SyR*R", "SyW*L", "SyW*R", "SyY*L", "SyY*R",
    "TRL", "U", "UB*L", "UB*R", "UDL", "UDR", "UFL", "UFR", "UG*L", "UG*R", "UL", "UML", "UMR", "UN*R", "UNL", "UOL",
    "UOR", "UP*L", "UP*R", "UPR", "UR*L", "UR*R", "USdL", "USdR", "USnL", "USnR", "USpL", "USpR", "UUL", "UUR", "UW*R",
    "UWL", "UY*L", "UYL", "UYR", "UbmL", "UbmR", "UgdL", "UgdR", "UgnR", "UmbL", "UmbR", "UngL", "UngR", "W*BL", "W*FL",
    "W*FR", "W*GR", "W*MR", "W*NL", "W*OR", "W*PL", "W*PR", "W*RR", "W*SdL", "W*SdR", "W*SgL", "W*SnL", "W*SnR",
    "W*SpR", "W*SrR", "W*SyL", "W*SyR", "W*gdR", "W*gnL", "W*gnR", "W*mbL", "WBL", "WBR", "WDL", "WGL", "WML", "WNL",
    "WNR", "WPL", "WRL", "WSpL", "WUR", "WWL", "WWR", "WYL", "WYR", "WbmL", "WbmR", "WgdL", "WgdR", "WmbR", "WngL",
    "WngR", "WryR", "Y*DR", "Y*FL", "Y*NR", "Y*PL", "Y*RL", "Y*RR", "Y*SdL", "Y*SgL", "Y*SgR", "Y*SnL", "Y*SnR",
    "Y*SpL", "Y*SpR", "Y*SrR", "Y*SyL", "Y*SyR", "Y*UL", "Y*YR", "Y*gdR", "YBL", "YBR", "YDL", "YFL", "YFR", "YGL",
    "YML", "YMR", "YNL", "YNR", "YOL", "YOR", "YPL", "YSdL", "YUR", "YWL", "YWR", "YbmL", "YbmR", "YgdL", "YgdR",
    "YmbL", "YmbR", "YngL", "YngR", "YryR", "dg*PL", "dgB*L", "dgB*R", "dgG*L", "dgG*R", "dgN*R", "dgP*R", "dgR*L",
    "gdB*L", "gdB*R", "gdG*L", "gdG*R", "gdN*L", "gdN*R", "gdP*L", "gdP*R", "gnBL", "gnBR", "gnDL", "gnDR", "gnFL",
    "gnFR", "gnGL", "gnGR", "gnML", "gnMR", "gnNR", "gnOL", "gnOR", "gnP*R", "gnPL", "gnPR", "gnRL", "gnRR", "gnUL",
    "gnUR", "gnWL", "gnWR", "gnYR", "mbB*L", "mbG*L", "mbN*L", "mbP*R", "ngBL", "ngBR", "ngDL", "ngDR", "ngFL", "ngFR",
    "ngGL", "ngGR", "ngML", "ngMR", "ngNL", "ngNR", "ngOL", "ngOR", "ngPL", "ngPR", "ngRL", "ngRR", "ngUR", "ngWR",
    "noR*R", "ryBR", "ryDR", "ryFR", "ryGR"))
