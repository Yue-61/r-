## Assignment 2 
## Team Members: YUE TANG S2737479; ZIGE FANG S2797028; DANNI ZHOU S286981

## GROUP COLLABORATION STATEMENT

## This SEIR model with social structure implementation was collaboratively
## developed by a team of three members. Below is the breakdown of individual
## contributions to this project:
##
## Team Member 1: YUE TANG
## - Primary responsibility: Core SEIR model implementation (nseir function)
## - Key contributions:
##   * Implemented the main nseir function with social structure
##   * Developed the state transition logic for S->E, E->I, I->R
##   * Implemented the triple infection mechanism (household, network, random)
##   * Optimized the infection probability calculations
##   * Added error handling and data validation
##
## Team Member 2: ZIGE FANG
## - Primary responsibility: Social network and household structure
## - Key contributions:
##   * Implemented the get.net function for social network generation
##   * Developed the household creation and assignment logic
##   * Designed the sociability-based connection probability system
##   * Ensured proper handling of household exclusions in network generation
##   * Implemented bidirectional link recording without duplication
##
## Team Member 3: DANNI ZHOU
## - Primary responsibility: Visualization and scenario analysis
## - Key contributions:
##   * Developed the plot_seir and plot_seir_comparison functions
##   * Created the multi-panel visualization system
##   * Implemented the four scenario comparison framework
##   * Added quantitative analysis and statistical reporting
##   * Designed the professional plotting aesthetics and legends
##
## Collaborative Process:
## - All team members participated in code review and debugging sessions
## - Regular meetings were held to ensure consistent coding standards
## - Git version control was used for collaborative development
## - Each member tested and validated other members' code components
## - The final integration and documentation were jointly completed
##
## This project represents truly collaborative work where all team members
## contributed significantly to both coding and conceptual development.


## Code to simulate an SEIR epidemic model with household and social network structure.
## This implementation extends the basic SEIR model by incorporating realistic social
## structures including households and social contact networks. The model allows
## investigation of how social heterogeneity affects epidemic dynamics.

## Set up population parameters and household structure
n <- 1000  ## total population size
## Create household assignments: divide n people into households of size 1-5
## Each household ID is repeated according to the household size sampled from 1:5
h <- rep(1:n, times = sample(1:5, n, replace = TRUE))[1:n]

get.net <- function(beta, h, nc = 15) {
  ## Generate a social contact network based on individual sociability parameters
  ## The network connects individuals with probability proportional to the product
  ## of their sociability parameters (beta values), excluding household members
  ## who are connected through the household transmission mechanism.
  ##
  ## Parameters:
  ##   beta: vector of length n, sociability parameters for each individual
  ##   h: vector of length n, household assignments for each individual  
  ##   nc: scalar, average number of contacts per person (default 15)
  ##
  ## Returns:
  ##   A list of length n, where the ith element contains the indices of 
  ##   individual i's non-household social contacts
  
  n <- length(beta)  ## get total population size
  mean_beta <- mean(beta)  ## compute mean sociability for normalization
  
  ## initialize empty list to store contacts for each individual
  alink <- vector("list", n)
  
  ## iterate over all possible pairs (i,j) where j > i to avoid duplicate links
  ## this ensures each potential connection is evaluated exactly once
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      ## skip if individuals are in the same household
      if (h[i] == h[j]) {
        next  ## proceed to next pair
      }
      
      ## calculate connection probability using sociability parameters
      prob_ij <- (nc * beta[i] * beta[j]) / (mean_beta^2 * (n-1))
      
      ## determine if connection exists using random number generation
      if (runif(1) < prob_ij) {
        ## add bidirectional connection: j to i's list and i to j's list
        alink[[i]] <- c(alink[[i]], j)
        alink[[j]] <- c(alink[[j]], i)
      }
    }
  }
  
  return(alink)
} ## get.net

nseir <- function(beta, h, alink, alpha = c(0.1, 0.01, 0.01), delta = 0.2, 
                  gamma = 0.4, nc = 15, nt = 100, pinf = 0.005) {
  ## Simulate SEIR epidemic dynamics with social structure over multiple days
  ## The model tracks individuals through four states: Susceptible (S), Exposed (E), 
  ## Infectious (I), and Recovered (R). Transmission occurs through three routes:
  ## household contacts, social network contacts, and random mixing.
  ##
  ## Parameters:
  ##   beta: vector of length n, individual sociability parameters
  ##   h: vector of length n, household assignments
  ##   alink: list of length n, social network contacts from get.net
  ##   alpha: vector of 3 transmission probabilities [household, network, random]
  ##   delta: scalar, daily probability of recovery (I -> R)
  ##   gamma: scalar, daily probability of becoming infectious (E -> I)  
  ##   nc: scalar, average number of contacts per person
  ##   nt: scalar, number of days to simulate
  ##   pinf: scalar, proportion of population initially infectious
  ##
  ## Returns:
  ##   A list with elements:
  ##     S, E, I, R: vectors of length nt with daily counts in each state
  ##     t: time vector 1:nt
  ##     beta: the input beta vector for reference
  
  n <- length(beta)  ## total population size
  mean_beta <- mean(beta)  ## mean sociability for probability calculations
  
  ## initialize state vector using integer coding: 0=S, 1=E, 2=I, 3=R
  x <- rep(0, n)  ## all individuals start as susceptible
  
  ## randomly select initial infectious individuals
  initial_infected <- sample(n, size = max(1, round(n * pinf)))
  x[initial_infected] <- 2  ## set selected individuals to infectious state
  
  ## initialize vectors to store daily state counts
  S <- E <- I <- R <- rep(0, nt)
  
  ## main simulation loop over days
  for (t in 1:nt) {
    ## generate uniform random numbers for all probability comparisons
    u <- runif(n)
    
    ## State transitions:
    ## I -> R: infectious individuals recover with daily probability delta
    x[x == 2 & u < delta] <- 3
    
    ## E -> I: exposed individuals become infectious with daily probability gamma  
    x[x == 1 & u < gamma] <- 2
    
    ## S -> E: infection of susceptible individuals
    ## identify all susceptible and infectious individuals
    s_indices <- which(x == 0)  ## indices of susceptible individuals
    i_indices <- which(x == 2)  ## indices of infectious individuals
    
    ## only proceed if there are both susceptibles and infectious individuals
    if (length(s_indices) > 0 && length(i_indices) > 0) {
      ## for each susceptible individual, calculate infection probability
      for (j in s_indices) {
        ## initialize probability of escaping infection from all infectious individuals
        not_infected <- 1
        
        ## consider infection risk from each infectious individual
        for (i in i_indices) {
          ## initialize probability of escaping infection from this particular infectious individual
          not_infected_by_i <- 1
          
          ## household transmission: higher probability if same household
          if (h[i] == h[j]) {
            not_infected_by_i <- not_infected_by_i * (1 - alpha[1])
          }
          
          ## network transmission: if j is in i's social network contacts
          if (j %in% alink[[i]]) {
            not_infected_by_i <- not_infected_by_i * (1 - alpha[2])
          }
          
          ## random mixing transmission: probability depends on sociability parameters
          random_prob <- (alpha[3] * nc * beta[i] * beta[j]) / (mean_beta^2 * (n - 1))
          not_infected_by_i <- not_infected_by_i * (1 - random_prob)
          
          ## update overall probability of escaping infection
          not_infected <- not_infected * not_infected_by_i
        }
        
        ## determine if infection occurs: if random number exceeds escape probability
        if (runif(1) > not_infected) {
          x[j] <- 1  ## transition from susceptible to exposed
        }
      }
    }
    
    ## record daily counts for each state
    S[t] <- sum(x == 0)  ## susceptible count
    E[t] <- sum(x == 1)  ## exposed count
    I[t] <- sum(x == 2)  ## infectious count
    R[t] <- sum(x == 3)  ## recovered count
  }
  
  return(list(S = S, E = E, I = I, R = R, t = 1:nt, beta = beta))
} ## nseir

plot_seir <- function(seir_results, main = "SEIR Model Dynamics") {
  ## Create a plot showing the dynamics of SEIR model simulation results
  ## Displays the temporal evolution of all four compartments (S, E, I, R)
  ## with distinct colors and a legend for easy interpretation.
  ##
  ## Parameters:
  ##   seir_results: list returned by nseir function containing S, E, I, R, t
  ##   main: character string, plot title (default "SEIR Model Dynamics")
  ##
  ## Returns:
  ##   None (creates a plot as side effect)
  
  ## extract time series data from results
  S <- seir_results$S
  E <- seir_results$E
  I <- seir_results$I
  R <- seir_results$R
  t <- seir_results$t
  
  ## save current graphics parameters and restore on function exit
  old_par <- par(no.readonly = TRUE) 
  on.exit(par(old_par))
  
  ## set plot margins: bottom, left, top, right (increased right margin for legend)
  par(mar = c(4, 4, 3, 6))
  
  ## determine y-axis range to encompass all compartments
  y_max <- max(c(S, E, I, R), na.rm = TRUE)
  y_min <- 0
  
  ## create base plot with susceptible trajectory
  plot(t, S, type = "l", col = "black", lwd = 2,
       ylim = c(y_min, y_max),
       xlab = "Day", ylab = "Population",
       main = main,
       frame.plot = FALSE)  ## remove frame around plot
  
  ## add trajectories for other compartments with distinct colors
  lines(t, E, col = "blue", lwd = 2)    ## exposed (blue)
  lines(t, I, col = "red", lwd = 2)     ## infectious (red)
  lines(t, R, col = "green", lwd = 2)   ## recovered (green)
  
  ## add legend in top-right corner with bordered box
  legend("topright", 
         legend = c("S", "E", "I", "R"),  ## compartment labels
         col = c("black", "blue", "red", "green"),  ## corresponding colors
         lty = 1,     ## line type
         lwd = 1.5,   ## line width in legend
         bty = "o",   ## draw box around legend
         box.lwd = 0.5,      ## box line width
         box.col = "darkgray",  ## box color
         bg = "white",        ## background color
         cex = 0.6,           ## character expansion (font size)
         inset = 0.02,        ## inset from plot edges
         x.intersp = 0.8,     ## horizontal spacing between elements
         y.intersp = 0.8,     ## vertical spacing between elements  
         seg.len = 1.5)       ## length of line segments in legend
  
  ## add grid lines for better readability
  grid()
} ## plot_seir

plot_seir_comparison <- function(results_list, scenario_names = NULL, 
                                 main = "SEIR Model Comparison") {
  ## Create a multi-panel comparison plot of multiple SEIR simulations
  ## Arranges individual SEIR plots in a grid layout to facilitate visual
  ## comparison of different scenarios or parameter settings.
  ##
  ## Parameters:
  ##   results_list: list of nseir results, one element per scenario
  ##   scenario_names: vector of character names for each scenario
  ##   main: character string, overall plot title
  ##
  ## Returns:
  ##   None (creates a multi-panel plot as side effect)
  
  n_scenarios <- length(results_list)  ## number of scenarios to compare
  
  ## set up multi-panel layout with automatic row/column calculation
  old_par <- par(no.readonly = TRUE)
  par(mfrow = c(ceiling(n_scenarios/2), 2),  ## automatic grid dimensions
      mar = c(4, 4, 2, 5),  ## panel margins: bottom, left, top, right
      oma = c(0, 0, 2, 0))  ## outer margins: bottom, left, top, right
  on.exit(par(old_par))
  
  ## generate default scenario names if not provided
  if (is.null(scenario_names)) {
    scenario_names <- paste("Scenario", 1:n_scenarios)
  }
  
  ## create individual plot for each scenario
  for (i in 1:n_scenarios) {
    plot_seir_single_panel(results_list[[i]], main = scenario_names[i])
  }
  
  ## add overall title in outer margin
  mtext(main, side = 3, outer = TRUE, cex = 1.2, font = 2)
} ## plot_seir_comparison

plot_seir_single_panel <- function(seir_results, main = "") {
  ## Create a single panel SEIR plot for use in multi-panel comparisons
  ## Simplified version with smaller elements suitable for multi-panel layout.
  ##
  ## Parameters:
  ##   seir_results: list returned by nseir function
  ##   main: character string, panel title
  ##
  ## Returns:
  ##   None (creates a single plot panel as side effect)
  
  ## extract time series data
  S <- seir_results$S
  E <- seir_results$E
  I <- seir_results$I
  R <- seir_results$R
  t <- seir_results$t
  
  ## determine y-axis range
  y_max <- max(c(S, E, I, R), na.rm = TRUE)
  
  ## create simplified plot with thinner lines for multi-panel display
  plot(t, S, type = "l", col = "black", lwd = 1.5,
       ylim = c(0, y_max),
       xlab = "Day", ylab = "Population",
       main = main)
  
  ## add other compartment trajectories
  lines(t, E, col = "blue", lwd = 1.5)
  lines(t, I, col = "red", lwd = 1.5)
  lines(t, R, col = "green", lwd = 1.5)
  
  ## add minimal legend only if panel has a title
  if (main != "") {
    legend("topright", 
           legend = c("S", "E", "I", "R"),
           col = c("black", "blue", "red", "green"),
           lty = 1, 
           lwd = 1,        ## thinner lines in legend
           bty = "o",      ## bordered legend
           box.lwd = 0.3,  ## thinner border
           box.col = "darkgray",  
           bg = "white",  
           cex = 0.5,      ## smaller font
           inset = 0.01,   ## smaller inset
           x.intersp = 0.6,  ## tighter horizontal spacing
           y.intersp = 0.6,  ## tighter vertical spacing
           seg.len = 1)      ## shorter line segments
  }
} ## plot_seir_single_panel

## MAIN SIMULATION: Compare 4 scenarios as specified in the document
## Set random seed for reproducible results
set.seed(123)

## Set beta to a vector of U(0,1) random variables as specified
## This creates individual sociability parameters drawn from uniform distribution
beta_random <- runif(n)

## Create social network using the U(0,1) beta values
alink <- get.net(beta = beta_random, h = h)

## SCENARIO 1: Full model with default parameters
scenario1 <- nseir(beta = beta_random, h = h, alink = alink,
                   alpha = c(0.1, 0.01, 0.01))  ## default alpha values

## SCENARIO 2: Remove household and network structure, keep same average contacts
## Set α_h = α_c = 0 and α_r = 0.04 to maintain similar average contact rates
scenario2 <- nseir(beta = beta_random, h = h, alink = alink, 
                   alpha = c(0, 0, 0.04))

## SCENARIO 3: Full model with constant beta (mean of random beta)
## Set beta vector to contain the average of previous beta vector for every element
beta_uniform <- rep(mean(beta_random), n)
alink_uniform <- get.net(beta_uniform, h)  ## regenerate network with uniform beta
scenario3 <- nseir(beta = beta_uniform, h = h, alink = alink_uniform,
                   alpha = c(0.1, 0.01, 0.01))  ## default alpha values

## SCENARIO 4: Combine constant beta with random mixing
scenario4 <- nseir(beta = beta_uniform, h = h, alink = alink_uniform, 
                   alpha = c(0, 0, 0.04))

## CREATE COMPARISON PLOT
plot_seir_comparison(
  list(scenario1, scenario2, scenario3, scenario4),
  c("Random beta + Social Structure", 
    "Random beta + Random Mixing", 
    "Constant beta + Social Structure", 
    "Constant beta + Random Mixing"),
  "Impact of Social Structure on Epidemic Dynamics"
)

## QUANTITATIVE ANALYSIS AND COMMENTS
cat("COMMENTS ON THE APPARENT EFFECT OF SOCIAL STRUCTURE\n")

## Calculate key epidemic metrics for each scenario
calculate_metrics <- function(results) {
  peak_infectious <- max(results$I)
  peak_time <- which.max(results$I)
  final_size <- results$R[length(results$R)]
  attack_rate <- final_size / n
  return(c(peak_infectious, peak_time, final_size, attack_rate))
}

metrics1 <- calculate_metrics(scenario1)
metrics2 <- calculate_metrics(scenario2)
metrics3 <- calculate_metrics(scenario3)
metrics4 <- calculate_metrics(scenario4)

## Display comparison table
cat("Epidemic Metrics Comparison:\n")
cat("Scenario                    | Peak I | Peak Day | Final R | Attack Rate\n")
cat(sprintf("Random β + Social Struct  | %6.0f | %8d | %7.0f | %11.3f\n", 
            metrics1[1], metrics1[2], metrics1[3], metrics1[4]))
cat(sprintf("Random β + Random Mixing  | %6.0f | %8d | %7.0f | %11.3f\n", 
            metrics2[1], metrics2[2], metrics2[3], metrics2[4]))
cat(sprintf("Constant β + Social Struct| %6.0f | %8d | %7.0f | %11.3f\n", 
            metrics3[1], metrics3[2], metrics3[3], metrics3[4]))
cat(sprintf("Constant β + Random Mixing| %6.0f | %8d | %7.0f | %11.3f\n", 
            metrics4[1], metrics4[2], metrics4[3], metrics4[4]))

## Beta distribution statistics
cat("\nBeta Distribution Statistics:\n")
cat(sprintf("Random beta - Mean: %.4f, SD: %.4f, CV: %.4f\n", 
            mean(beta_random), sd(beta_random), sd(beta_random)/mean(beta_random)))
cat(sprintf("Constant beta - All values: %.4f\n", mean(beta_random)))

## COMMENTS ON THE EFFECTS OF SOCIAL STRUCTURE
cat("COMMENTS ON THE APPARENT EFFECT OF SOCIAL STRUCTURE\n")

cat("1. COMPARISON: Random beta + Social Structure vs Random beta + Random Mixing\n")
cat("   (Scenario 1 vs Scenario 2)\n")
if (metrics1[1] < metrics2[1]) {
  cat("   - Social structure REDUCES peak infections by", 
      round((metrics2[1] - metrics1[1])/metrics2[1] * 100, 1), "%\n")
} else {
  cat("   - Social structure INCREASES peak infections by", 
      round((metrics1[1] - metrics2[1])/metrics2[1] * 100, 1), "%\n")
}

if (metrics1[3] < metrics2[3]) {
  cat("   - Social structure REDUCES final epidemic size by", 
      round((metrics2[3] - metrics1[3])/metrics2[3] * 100, 1), "%\n")
} else {
  cat("   - Social structure INCREASES final epidemic size by", 
      round((metrics1[3] - metrics2[3])/metrics2[3] * 100, 1), "%\n")
}

cat("   - Interpretation: Social structure creates 'firebreaks' in transmission\n")
cat("     by concentrating infections within households and social networks,\n")
cat("     potentially slowing overall spread but may lead to more localized outbreaks.\n\n")

cat("2. COMPARISON: Random beta + Social Structure vs Constant beta + Social Structure\n")
cat("   (Scenario 1 vs Scenario 3)\n")
if (metrics1[1] < metrics3[1]) {
  cat("   - Sociability variation REDUCES peak infections by", 
      round((metrics3[1] - metrics1[1])/metrics3[1] * 100, 1), "%\n")
} else {
  cat("   - Sociability variation INCREASES peak infections by", 
      round((metrics1[1] - metrics3[1])/metrics3[1] * 100, 1), "%\n")
}

cat("   - Interpretation: Individual variation in sociability creates 'super-spreaders'\n")
cat("     and 'low-spreaders', which can either amplify or dampen transmission\n")
cat("     depending on network structure and early infection patterns.\n\n")

cat("3. COMPARISON: Social Structure vs Random Mixing (with constant beta)\n")
cat("   (Scenario 3 vs Scenario 4)\n")
if (metrics3[1] < metrics4[1]) {
  cat("   - With uniform sociability, social structure REDUCES peak by", 
      round((metrics4[1] - metrics3[1])/metrics4[1] * 100, 1), "%\n")
} else {
  cat("   - With uniform sociability, social structure INCREASES peak by", 
      round((metrics3[1] - metrics4[1])/metrics4[1] * 100, 1), "%\n")
}

cat("   - Interpretation: Even without individual variation, social structure\n")
cat("     fundamentally alters transmission dynamics by creating non-random\n")
cat("     contact patterns that can either facilitate or hinder spread.\n\n")
