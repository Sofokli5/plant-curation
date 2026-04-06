# Plant reference sequences curation process
This repository contains step-by-step markdown guides for curating plant DNA reference sequences. The goal is to produce high-quality, clean reference files suitable for use with the **nf-core/ampliseq** pipeline.

## Overview
The instructions describe how to collect, clean, and standardize plant sequence data from multiple public databases (such as **BOLD Systems**, ADD OTHERS). The curation process ensures that the final reference set is consistent, taxonomically valid, and ready for use in amplicon sequencing analyses.

## Installation

```bash
git clone https://github.com/npechl/plant-curation.git
```

> Please follow instruction in each database-related sub folder.

## Contents

- `environment.yml`: contains required software packages for processing
- each folder contains instructions for data retrieval, curation and processing of plant sequences to clean references (aimed to be used in `nf-core/ampliseq` pipeline).

## Expected Outputs

After following the documented steps, users will obtain:

* A curated **FASTA file** containing unique, high-quality reference sequences.
* A corresponding **taxonomy mapping file** with complete taxonomy information.
* Optionally, a metadata summary file for provenance and traceability.

## Intended Users
This repository is intended for researchers, bioinformaticians, and ecologists who need to build or refine custom plant reference databases for metabarcoding or amplicon-based analyses.

## License

This work, as a whole, is licensed under the [MIT license](https://opensource.org/license/mit).
