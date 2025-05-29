Data dictionaries for the data files are provided below.

`coreidd/df9_articledata_0.7_public.csv`

* `article_id`: a unique identifier for each article
* `PY`: year of publication
* `PD`: month of publication, contains NAs
* `AG`: gender pairing of first and last authors of the article using `genderize.io`, `M` stands for man, `W` for woman, and `U` for unknown; first letter is first author, second letter is last author
* `ARbin_ethni`: race pairing of first and last authors of the article using `ethnicolr` and binned into White (`W`) and non-White (`N`); first letter is first author, second letter is last author
* `FA_genderize`: first author gender from `genderize.io`, `M` stands for man, `W` for woman, and `U` for unknown
* `LA_genderize`: last author gender from `genderize.io`, `M` stands for man, `W` for woman, and `U` for unknown
* `FA_ethni_race`: first author race from `ethnicolr`, `HL` stands for Hispanic, `W_NL` for non-Hispanic White, `B_NL` for non-Hispanic Black, `A` for Asian, and `U` for unknown
* `LA_ethni_race`: last author race from `ethnicolr`, `HL` stands for Hispanic, `W_NL` for non-Hispanic White, `B_NL` for non-Hispanic Black, `A` for Asian, and `U` for unknown
* `FA_binrace_ethni`: first author binary race from `ethnicolr`, `W` stands for White, `N` for non-White (including Hispanic, Asian, and Black non-Hispanic), `U` is unknown
* `LA_binrace_ethni`: last author binary race from `ethnicolr`, `W` stands for White, `N` for non-White (including Hispanic, Asian, and Black non-Hispanic), `U` is unknown
* `Country`: country of affiliation for last author of article (string)
* `global_north`: indicator variable for whether the last author's country of affiliation falls in the Global North (0 or 1)
* `has_single_author`: boolean variable for whether the article has only a single author

`coreidd/df9_consensus_authorship_public.csv`

This file contains the article ids and publication years (PY) for articles in the authorship dataset.

`coreidd/df9_consensus_citation_public.csv`

This file contains the articles in the citation dataset including which article they were cited by, and whether they should be included in the analysis depending on the threshold of inclusion.

* `article_id`: the article's unique identifier so it can be linked to the full dataset
* `in_bib_of_article_id`: article id of the citing article
* `PY`: publication year of cited article
* `include_95`: boolean for whether article should be included with 95th percentile consensus citation threshold
* `include_90`: boolean for whether article should be included with 90th percentile consensus citation threshold. The main analysis uses this threshold; see the paper for a more detailed description of the construction and interpretation of these thresholds
* `include_75`: boolean for whether article should be included with 75th percentile consensus citation threshold
* `include_50`: boolean for whether article should be included with 50th percentile consensus citation threshold
* `is_self_cite`: indicator for whether the citation is a self citation by the first or last author (0 or 1)

`input_jif_regression_nosf_20241212_public.csv`,

This file contains data necessary for the impact factor regression in the main text.

* `pair_gender`: gender pairing of first and last authors of the article using `genderize.io`, `M` stands for man, `W` for woman; first letter is first author, second letter is last author
* `pair_race`: race pairing of first and last authors of the article using `ethnicolr`, `W` stands for White, `N` for non-White; first letter is first author, second letter is last author
* `FA_gender`: first author gender from `genderize.io`, `M` stands for man, `W` for woman
* `LA_gender`: last author gender from `genderize.io`, `M` stands for man, `W` for woman
* `FA_race`: first author race from `ethincolr`, `W` stands for White, `N` for non-White
* `LA_race`: last author race from `ethincolr`, `W` stands for White, `N` for non-White
* `JIF`: 2023 Journal Impact Factor of journal where article was published
* `year`: year of publication
* `num_citations`: number of times article was cited by articles in authorship dataset
* `log_num_citations`: natural log of `num_citations` column
* `citation_rate`: number of citations divided by years since publication
* `log_citation_rate`: natural log of `citation_rate` column
* `WC`: Web of Science category
* `num_cited_refs`: number of references in the article's bibliography
* `WC_1st`: first entry in `WC`
* `Country`: country of affiliation for last author of article (string)
* `global_north`: indicator variable for whether the last author's country of affiliation falls in the Global North (0 or 1)
* `article_id`: article unique identifier