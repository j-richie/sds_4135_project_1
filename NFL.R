library(dplyr)

#This part is to importing data into Rstudio. 

# Set the directory path
folder_path <- "/Users/shenghangao/Desktop/season_data/2025_backtest"
skill_file <- "/Users/shenghangao/Desktop/season_data/2025_skill.csv"

# Get a list of all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

# Initialize an empty list to store data frames
data_list <- list()

# Loop through each CSV file
for (file in csv_files) {
  # Extract the season from the file name (e.g., "season2016.csv" -> 2016)
  season <- as.numeric(gsub(".*season([0-9]+)\\.csv", "\\1", basename(file)))
  
  # Read the CSV, skip the first row, and assign column names
  temp_data <- read.csv(file, header = FALSE, skip = 1)
  colnames(temp_data) <- c("Week", "Day", "Date", "Time", "Winner", "At", "Loser", 
                           "Boxscore", "PtsW", "PtsL", "YdsW", "TOW", "YdsL", "TOL")
  
  # Remove the first two rows
  temp_data <- temp_data[-c(1, 2), ]
  
  # Add a Season column
  temp_data$Season <- season
  
  # Store the processed data in the list
  data_list[[file]] <- temp_data
}

# Combine all data frames into one
combined_data <- do.call(rbind, data_list)

#This part is to clear all useless rows. Like links, etc. 
combined_data <- combined_data[!is.na(combined_data$Winner) & combined_data$Winner != "", ]

combined_data <- combined_data %>%
  mutate(
    Winner = recode(Winner,
                    "St. Louis Rams" = "Los Angeles Rams",
                    "Oakland Raiders" = "Las Vegas Raiders",
                    "Washington Redskins" = "Washington Commanders",
                    "Washington Football Team" = "Washington Commanders",
                    "San Diego Chargers" = "Los Angeles Chargers",
                    .default = Winner),  # Preserve original value if not in mapping
    Loser = recode(Loser,
                   "St. Louis Rams" = "Los Angeles Rams",
                   "Oakland Raiders" = "Las Vegas Raiders",
                   "Washington Redskins" = "Washington Commanders",
                   "Washington Football Team" = "Washington Commanders",
                   "San Diego Chargers" = "Los Angeles Chargers",
                   .default = Loser)  # Preserve original value if not in mapping
  )

#write.csv(combined_data, '/Users/shenghangao/Desktop/Applied Stats/Combined_data.csv')




#Here we create the BT table:
# Load the data from the CSV file
#data <- read.csv("/Users/shenghangao/Desktop/Applied Stats/Combined_data.csv", stringsAsFactors = FALSE)

data <- combined_data
# Get all unique teams from Winner and Loser columns, sorted alphabetically
teams <- sort(unique(c(data$Winner, data$Loser)))

# Initialize a square matrix with zeros, named by teams
win_matrix <- matrix(0, nrow = length(teams), ncol = length(teams), 
                     dimnames = list(teams, teams))

# Loop through each row (game) and count wins (skip ties if any)
for (i in 1:nrow(data)) {
  winner <- data$Winner[i]
  loser <- data$Loser[i]
  pts_w <- as.numeric(data$PtsW[i])
  pts_l <- as.numeric(data$PtsL[i])
  
  # Only count if it's a win (PtsW > PtsL) and teams are different
  if (!is.na(pts_w) && !is.na(pts_l) && pts_w > pts_l && winner != loser) {
    win_matrix[winner, loser] <- win_matrix[winner, loser] + 1
  }
}

# Set the diagonal to "-" (no self-games)
diag(win_matrix) <- "-"

write.csv(win_matrix, file = '/Users/shenghangao/Desktop/Applied Stats/ResultMatrix.csv')

## ---- 1) Load the results matrix -------------------------------------------
## Expected CSV shape:
##   - Header row has team abbreviations/names for columns (B,C,...)
##   - First column is team names for the rows (A,B,...)
##   - Cell [i,j] = number of times row team i beat column team j
##   - Diagonal can be 0, NA, or "-"; we'll coerce to 0.

file <- "/Users/shenghangao/Desktop/Applied Stats/ResultMatrix.csv"   # adjust if needed
raw  <- read.csv(file, check.names = FALSE, stringsAsFactors = FALSE)

# If first column holds team names, move them to row names
if (!is.numeric(raw[[2]])) {
  # second column not numeric -> assume first col is team names
  team_names <- raw[[1]]
  M <- raw[ , -1, drop = FALSE]
  rownames(M) <- team_names
} else {
  # already numeric with rownames present
  M <- raw
  if (is.null(rownames(M))) stop("Row team names not found; ensure first column has team names.")
}

# Coerce everything numeric (turn dashes/NA into 0)
toNum <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.]", "", x)))
for (j in seq_len(ncol(M))) M[[j]] <- toNum(M[[j]])
M[is.na(as.matrix(M))] <- 0

M <- as.matrix(M)
colnames_ok <- identical(colnames(M), rownames(M))
if (!colnames_ok) stop("Row and column team orders do not match. Please align them so names are identical and in the same order.")

diag(M) <- 0  # no self-games

## ---- 2) Bradley–Terry (MM) iteration --------------------------------------
## Model: P(i beats j) = p_i / (p_i + p_j)
## Update: p_i^{new} = w_i / sum_j n_ij / (p_i + p_j)
## where w_i = sum_j w_ij   (row sums),  n_ij = w_ij + w_ji

n_teams <- nrow(M)
wins_i  <- rowSums(M)            # w_i
Nij     <- M + t(M)              # total meetings between i and j
diag(Nij) <- 0

# Initialize skill parameters
p <- rep(1, n_teams)
names(p) <- rownames(M)

# Iteration controls
tol      <- 1e-10
max_iter <- 10000
eps      <- 1e-15   # numerical guard

for (iter in 1:max_iter) {
  denom <- numeric(n_teams)
  
  # Compute denominators: sum_j n_ij / (p_i + p_j)
  for (i in 1:n_teams) {
    denom[i] <- sum( Nij[i, ] / (p[i] + p + eps) )
  }
  
  p_new <- wins_i / pmax(denom, eps)
  
  # Normalize (the Bradley–Terry scale is only relative; fix mean to 1)
  p_new <- p_new / mean(p_new)
  
  # Convergence check
  if (max(abs(p_new - p)) < tol) {
    p <- p_new
    break
  }
  p <- p_new
}

## ---- 3) Output data frame --------------------------------------------------
skill_df <- data.frame(
  Team  = names(p),
  Skill = as.numeric(p),
  row.names = NULL,
  check.names = FALSE
)

# Optional: sort by skill descending
skill_df <- skill_df[order(-skill_df$Skill), ]

# Show the result
print(skill_df)

write.csv(skill_df, file = skill_file)
