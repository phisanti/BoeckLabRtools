# Boeck Lab R Tools

## Introduction

Welcome to the Boeck Lab R Tools repository! This R package is designed to provide a collection of useful functions and tools to streamline and enhance the data analysis pipelines frequently used in the Boeck Lab. Our lab focuses on various aspects of microbiology and antimicrobial research, and this package aims to facilitate the analysis of data generated from our experiments, particularly those involving Antimicrobial Single-Cell Testing (ASCT).

## Purpose

The primary purpose of this package is to offer a suite of tools that can be easily integrated into the data analysis workflows of the Boeck Lab. These tools are specifically tailored to handle the unique requirements of our research, including quality control (QC) and kill curve kinetics analysis. By providing these functions in a centralized package, we aim to improve the efficiency, reproducibility, and accuracy of our data analysis processes.

## Features

### Quality Control Tools

Quality control is a critical step in any data analysis pipeline. Ensuring the accuracy and reliability of the data before proceeding with further analysis is essential. The Boeck Lab R Tools package includes several QC functions designed to help identify and correct potential issues in the data. Some of the key QC features include:

- **Object Class Distribution Plots**: Visualize the spatial distribution of object classes across the plate at the initial time point (t0). This helps in identifying patterns or anomalies in object distribution.
- **Growth Plot**: Provides an overview of the growth patterns observed across all wells throughout the experiment. The growth ratio is calculated for each well and log-transformed for better visualization.
- **Consistency Plot**: Demonstrates the consistency of classification in the bright field across different conditions. This plot is faceted by object class, allowing comparison of classification consistency across different types of objects.
- **PI Consistency Plot**: Visualizes the consistency of Propidium Iodide (PI) classification across different wells and time points, focusing on single-cell objects.
- **Time Delay Plot**: Illustrates the time delays observed for imaging in the first frame across the plate.

### Kill Curve Kinetics Tools

Kill curve kinetics analysis is a crucial aspect of our research, particularly in the context of antimicrobial testing. The Boeck Lab R Tools package includes functions to facilitate the analysis of kill curve data, allowing for a detailed understanding of antimicrobial efficacy. Key features include:

- **Interpolate Variable**: Interpolates a variable based on given time points and values, handling regular and irregular time steps, filling extreme values, and returning interpolated time points if desired.
- **Calculate AUC (Area Under the Curve)**: Calculates the AUC using the trapezoidal rule, with options for interpolation and normalization.
- **Enforce Monotonic Decrease**: Ensures that values in a numeric vector only decrease monotonically, which is particularly useful for live/death fraction data.
- **Extrapolate T0 Data**: Extrapolates the correct T0 given plate well data, accounting for time lags in imaging start times across wells.

## Installation

To install the Boeck Lab R Tools package, you can use the following commands in R:

```r
# Install remotes if not already installed
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install the Boeck Lab R Tools package from GitHub
remotes::install_github("phisanti/BoeckLabRtools")
```
### Dependencies

This package relies on `pandoc` and `pdftotext` to generate quality report outputs. You need to install these dependencies before using the report generation features.

#### Installing dependencies on macOS:

```bash
# Install Homebrew if not already installed
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

```bash
# Install pandoc
brew install pandoc
```

```bash
# Install basictex (which includes pdftotext)
brew install basictex
```

## Contributing
We welcome contributions from the community to help improve and expand the functionality of the Boeck Lab R Tools package. If you have any suggestions, bug reports, or would like to contribute code, please feel free to open an issue or submit a pull request on our GitHub repository.

## License
This package is licensed under the GPL (>= 3) license. See the LICENSE file for more details.

## Acknowledgements
We would like to thank all the members of the Boeck Lab for their valuable input and feedback during the development of this package. Special thanks to Santiago Cano-Muniz for leading the development efforts.

## Contact
For any questions or inquiries, please contact Santiago Cano-Muniz at santiago.cano-muniz@unibas.ch.

We hope you find the Boeck Lab R Tools package useful in your research and data analysis workflows. Happy analyzing!