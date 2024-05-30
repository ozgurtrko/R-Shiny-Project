# Song Recommendation System

This project is a Shiny-based application that allows users to get song recommendations using specific filters, analyze data with various graphs, and create their own playlists.

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)
- [Project Structure](#project-structure)
- [Contributing](#contributing)

## Installation

To run this project, follow these steps:

1. Clone the project:

    ```bash
    git clone https://github.com/username/song-recommendation-system.git
    cd song-recommendation-system
    ```

2. Install the required R packages:

    ```R
    install.packages(c("shiny", "shinydashboard", "shinythemes", "readr", "dplyr", "DT", "ggplot2"))
    ```

3. Place the `dataset.csv` file in the `~/desktop` directory for the application to function correctly.

## Usage

To start the application, run the following command:

```R
shiny::runApp()
```

## Application Tabs

*Recommendation:* Use filters to get song recommendations and view the results.
*Graphs:* Analyze data with different graphs (Top genres and top artists by selected genre).
*Playlist:* Create your own playlist, view summary charts, and see the number of songs per artist.

## Project Structure

*ui.R:* Defines the user interface components.
*server.R:* Defines the server functions and reactive processes.
*dataset.csv:* The dataset used by the project.

## Contributing

If you want to contribute, please submit a pull request (PR) or open an issue. All contributions are welcome!
