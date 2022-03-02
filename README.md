# sociology_word_use_acs1
## scripts/
  * crossref.R :: Code collects DOIs from 1 of 3 papers specified in the file from the open citation index and then uses those DOIs to extract the citation metadata from the Crossref API.
  * coci.R :: Code collecting metadata from 1 of 3 papers specified from https://opencitations.net
  * coci_accre.R :: Code that collections metadata that works on the ACCRE cluster at Vanderbilt
  * coci_random_accre.R :: Code that collections random sample of metadata that works on the ACCRE cluster at Vanderbilt
  * create_dataframe.R :: Clean, de-duplicate, and write out citation metadata for each paper
