# NFL Performance Analytics

This project visualizes player performance in the NFL using bar graphs, spider charts, and scatter plots. By analyzing key performance and physical metrics, the visualizations offer insights into player strengths, position-specific trends, and the relationship between attributes and performance.

## My Contributions

- Led the overall project, including data preprocessing, visualization, and dashboard design
- Created and published an interactive HTML dashboard in R with visualizations including bar charts, spider charts, and scatter plots

## Dataset

Two main data sources were used:
- **nflreadr R package**: Provided physical metrics such as height, weight, shuttle time, and vertical jump.
- **nflstat website (as of Nov 29, 2024)**: Manual collection of top 10 player statistics in categories like passing, receiving, rushing, interceptions, and tackles.

## Data Processing

- Normalized all values to a 0–1 scale to avoid dominance of outlier metrics (e.g., broad jump).
- Removed missing (NA) values to ensure data integrity.
- Applied unique team color mapping for clarity in visuals.
- Organized performance categories by offensive and defensive roles.

## Visualizations

Three main types of visualizations were created to explore player metrics:

### 1. Bar Graphs
- Visualized top 10 players by category (e.g., passing yards, tackles).
- Color-coded by team and overlaid with player images.
- Clear comparison of player performance within and across teams.

### 2. Spider Charts
- Compared individual player metrics to the average of their position group.
- Focused on physical metrics: vertical, broad jump, bench, shuttle, three-cone, forty-yard dash.
- Highlighted strengths and improvement areas per player.

### 3. Scatter Plots
- Explored relationships between physical traits (e.g., weight, height) and performance metrics (e.g., 40-yard dash).
- Plotted by position group to show role-based patterns.

## Key Takeaways

- **Performance clustering by role**: Lighter players like CB and RB excel in agility-based metrics, while heavier players like DE and OG show power-oriented performance.
- **Spider charts offer role-based performance benchmarking**, helping to evaluate players against expected standards.
- **Visualization-driven insights** support scouting, training strategies, and performance comparisons.

## Tools Used

- Language: R
- Packages: `ggplot2`, `dplyr`, `nflreadr`

## Output

You can view the interactive NFL visualization dashboard here:  


👉 **[NFL Dashboard Output](https://ylee219.shinyapps.io/NFL_Analysis/)** 👈


Feel free to explore the dashboard by selecting different teams, positions, and players! ❤️
