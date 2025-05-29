# Story-time: I was rock climbing with a friend of mine late Friday evening, where I came across a diabolical V3 course. An amateur, it looked like a fun project to me.
# "If I ace this", I thought, "I could try attempting a V4, finally", with the aim to finish. 
# I ended up struggling to finish the course, something that said "you aren't ready yet". 
# So, did I buck up and try attempting the V4? No, I chickened out and decided instead to make an indifference curve in my notebook.
# My friend, more experienced than I, viewed higher climbing levels as a challenge and potential for high reward - something I noticed was not the case with me and many amateurs.
# While not all climbers above my grade view risk as directly proportional to reward, higher levels of experience tends to come with better technique, and a lesser aversion to at least trying courses.
# I noticed that amatuer climbers generally tended to not climb over 2 grades above their comfort levels, while more experienced climbers (generally after V5) tend to have more leway (up to 3 grades above their levels)
# # I thought it would be best to imagine them climbing separately, both starting from V0, and to go as far as they can in grade levels. If they both started with their comfort level, there wouldn't be too much difference (unless?)
# The idea here is simple: to see how level of experience informs risk-taking behavior between two climbers. We compare the overall percieved utility for each climber.
# We do so simply by calculating percieved level of reward, minus penalty faced through risk. This informs our indifference curves through various simulation contexts, and finally a graph to see their overall utility per level climbed.
# The hope is that you may read this as either an amateur or experienced climber, and make me have to reassess my thoughts and simulations through taking more risk as an amateur, or be a very rigid experienced climber.

#Part 1
# Risk-taking behavior in bouldering

library(ggplot2)
library(dplyr)
library(tidyr)

# Defining climbing grades and our two climbers
# Bouldering grades are denoted generally by 'V'. V-scale for bouldering (V0 = easiest, V9 = very hard)
# While grades can go up to V18, we assess up to V9 for simplicity.

grades <- paste0("V", 0:9)
grade_difficulty <- 0:9  # Arranging our climbing grades through numeric value

# Let's meet our two climbers, Shim (climber 1) and Ty (climber 2)
climber1 <- list(
  name = "Climber 1 (Intermediate)",
  max_grade = 3,  # Usually climbs up to V3 (I'm trying to get better, really)
  risk_tolerance = 0.6  # Moderate risk tolerance; thinking of climbing anything past V4 for example is a silly notion.
)

climber2 <- list(
  name = "Climber 2 (Advanced)", 
  max_grade = 5,  # Can climb up to V5 (he enjoys challenging himself with V6s and sometimes, albeit rarely, V7s)
  risk_tolerance = 0.4  # Lower risk tolerance (more experienced = more technique, less aversion to risk)
)

print("STEP 1: Climber profiles")
print(paste("Climber 1: Max grade V", climber1$max_grade, ", Risk tolerance:", climber1$risk_tolerance))
print(paste("Climber 2: Max grade V", climber2$max_grade, ", Risk tolerance:", climber2$risk_tolerance))

#Part 2 
# Let us define our functions for probabilities for risk & success

# Calculating probability of success given grade difficulty

calc_success_prob <- function(grade_diff, max_grade) {
  # Success probability decreases as grade exceeds climber's max ability
  if (grade_diff <= max_grade) {
    # Assuming -> if grade is within ability: high chance of success
    return(0.9 - (grade_diff / max_grade) * 0.3)  # 90% to 60% success rate
  } else {
    # If grade beyond ability: decreasing chances of success
    excess_difficulty <- grade_diff - max_grade
    return(0.6 * exp(-excess_difficulty * 0.5))
  }
}

# Calculating perceived risk for each climber

calc_risk_perception <- function(grade_diff, max_grade) {
  # Risk perception increases with grade difficulty
  if (grade_diff <= max_grade) {
    return(0.1 + (grade_diff / max_grade) * 0.4)  # 10% to 50% risk
  } else {
    excess_difficulty <- grade_diff - max_grade
    return(0.5 + (1 - exp(-excess_difficulty * 0.3)) * 0.4)  # 50% to 90% risk
  }
}

print("\nSTEP 2: Risk and success functions")

#Part 3 
# Let us now calculate risk and success for each climber at each grade

results <- data.frame()

for (i in 1:length(grade_difficulty)) {
  grade <- grade_difficulty[i]
  
  # Climber 1:
  c1_success <- calc_success_prob(grade, climber1$max_grade)
  c1_risk <- calc_risk_perception(grade, climber1$max_grade)
  
  # Climber 2:  
  c2_success <- calc_success_prob(grade, climber2$max_grade)
  c2_risk <- calc_risk_perception(grade, climber2$max_grade)
  
  # Storing our results
  results <- rbind(results, data.frame(
    Grade = grades[i+1],
    Grade_Numeric = grade,
    Climber = "Climber 1",
    Success_Prob = c1_success,
    Risk_Perception = c1_risk,
    Risk_Tolerance = climber1$risk_tolerance
  ))
  
  results <- rbind(results, data.frame(
    Grade = grades[i+1],
    Grade_Numeric = grade,
    Climber = "Climber 2", 
    Success_Prob = c2_success,
    Risk_Perception = c2_risk,
    Risk_Tolerance = climber2$risk_tolerance
  ))
}

print("\nSTEP 3: Risk and success probabilities per climber")
print("Results Sample:")
print(head(results, 8))

#Part 4
# Calculating climbing utility to inform decision making

# Utility = (Probability of Success * Perceieved Reward) - (Risk Perception * Tolerance to Risk * Percieved Risk Penalty)
results$Utility <- (results$Success_Prob * 10) - (results$Risk_Perception * results$Risk_Tolerance * 8)
results$Will_Attempt <- results$Utility > 0

print("\nSTEP 4: Climb Utility (will attempt if Utility > 0)")

#Part 5
# Let us run some basic plots for comparison 

# Plot 1: Probability of Success vs Climbing Grade
p1 <- ggplot(results, aes(x = Grade_Numeric, y = Success_Prob, color = Climber)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 0:9, labels = grades) +
  labs(title = "Success Probability by Grade",
       x = "Bouldering Grade", 
       y = "Success Probability") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 2: Risk Perception vs Climbing Grade  
p2 <- ggplot(results, aes(x = Grade_Numeric, y = Risk_Perception, color = Climber)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 0:9, labels = grades) +
  labs(title = "Risk Perception by Grade",
       x = "Bouldering Grade",
       y = "Risk Perception") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)
print(p2)

print("\nSTEP 5: Basic comparison")

#Part 6 
# It is now time to create indifference curves for our two climbers
# These indifference curves show us combinations of risk and success that give out the same utility

# Noting our risk and success values
risk_range <- seq(0, 1, 0.02)
success_range <- seq(0, 1, 0.02)
grid <- expand.grid(Risk = risk_range, Success = success_range)

# Calculating utility for each climber across these bundles of percieved risk and success
grid$Utility_C1 <- (grid$Success * 10) - (grid$Risk * climber1$risk_tolerance * 8)
grid$Utility_C2 <- (grid$Success * 10) - (grid$Risk * climber2$risk_tolerance * 8)

# Creating our indifference curves
p3 <- ggplot(grid, aes(x = Risk, y = Success)) +
  # Climber 1 indifference curves (red)
  geom_contour(aes(z = Utility_C1), 
               breaks = c(0, 2, 4, 6, 8), 
               color = "red", size = 1) +
  # Climber 2 indifference curves (blue)  
  geom_contour(aes(z = Utility_C2), 
               breaks = c(0, 2, 4, 6, 8), 
               color = "blue", size = 1) +
  # Adding the data points for each climber
  geom_point(data = results[results$Climber == "Climber 1",], 
             aes(x = Risk_Perception, y = Success_Prob), 
             color = "red", size = 3, alpha = 0.7) +
  geom_point(data = results[results$Climber == "Climber 2",], 
             aes(x = Risk_Perception, y = Success_Prob), 
             color = "blue", size = 3, alpha = 0.7) +
  labs(title = "Indifference Curves for Both Climbers",
       subtitle = "Red = Climber 1 (higher risk tolerance), Blue = Climber 2 (lower risk tolerance)\nPoints show actual risk/success probabilities for each grade",
       x = "Risk Perception", 
       y = "Success Probability") +
  theme_minimal()

print(p3)

print("\nSTEP 6: Indifference Curves")

#Part 7 
# We are now going to compare our two climbers' decision-making choices

decision_summary <- results %>%
  group_by(Climber) %>%
  summarise(
    Grades_Will_Attempt = sum(Will_Attempt),
    Highest_Grade_Attempted = max(Grade_Numeric[Will_Attempt]),
    Avg_Risk_Taken = mean(Risk_Perception[Will_Attempt]),
    .groups = 'drop'
  )

print("\nSTEP 7: Decision Summary")
print(decision_summary)

# Let us now view our climbers' decisions for EACH grade, V0 to V9.
decision_detail <- results %>%
  select(Grade, Climber, Success_Prob, Risk_Perception, Utility, Will_Attempt) %>%
  arrange(Grade, Climber)

print("\nDecisions by grade:")
print(decision_detail)

#Part 8
# Decision visualization

# Plot 4: Comparing decision utility by grade
p4 <- ggplot(results, aes(x = Grade_Numeric, y = Utility, fill = Climber)) +
  geom_col(position = "dodge", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(breaks = 0:9, labels = grades) +
  scale_fill_manual(values = c("Climber 1" = "red", "Climber 2" = "blue")) +
  labs(title = "Utility by Grade for Each Climber",
       subtitle = "Positive utility = Will attempt, Negative utility = Will not attempt",
       x = "Bouldering Grade", 
       y = "Utility Score") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 5: Decision choice bundles (will attempt vs will not attempt)

decision_matrix <- results %>%
  select(Grade, Climber, Will_Attempt) %>%
  mutate(Decision = ifelse(Will_Attempt, "Will Attempt", "Will Not Attempt"))

p5 <- ggplot(decision_matrix, aes(x = Grade, y = Climber, fill = Decision)) +
  geom_tile(color = "white", size = 1) +
  scale_fill_manual(values = c("Will Attempt" = "green", "Will Not Attempt" = "red"),
                    name = "Decision") +
  labs(title = "Decision Matrix: Attempt vs Not Attempt",
       subtitle = "Green = Will attempt, Red = Will not attempt",
       x = "Bouldering Grade", 
       y = "Climber") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# Plot 6: Looking at our utility components
utility_breakdown <- results %>%
  mutate(
    Reward_Component = Success_Prob * 10,
    Risk_Penalty = Risk_Perception * Risk_Tolerance * 8
  ) %>%
  select(Grade, Climber, Reward_Component, Risk_Penalty) %>%
  pivot_longer(cols = c(Reward_Component, Risk_Penalty), 
               names_to = "Component", values_to = "Value") %>%
  mutate(Component = case_when(
    Component == "Reward_Component" ~ "Reward (Success × 10)",
    Component == "Risk_Penalty" ~ "Risk Penalty (Risk × Tolerance × 8)"
  ))

p6 <- ggplot(utility_breakdown, aes(x = Grade, y = Value, fill = Component)) +
  geom_col(position = "dodge", alpha = 0.7) +
  facet_wrap(~Climber, ncol = 1) +
  scale_fill_manual(values = c("Reward (Success × 10)" = "green", 
                               "Risk Penalty (Risk × Tolerance × 8)" = "orange")) +
  labs(title = "Utility Components Breakdown",
       subtitle = "Utility = Reward - Risk Penalty",
       x = "Bouldering Grade",
       y = "Component Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

print(p4)
print(p5) 
print(p6)

print("\nSTEP 8: Decision visualization plots")

cat("\n=== INTERPRETATION ===")
cat("\n1. Climber 1 (intermediate, higher risk tolerance) is more willing to attempt harder grades")
cat("\n2. Climber 2 (advanced, lower risk tolerance) is more selective despite higher ability") 
cat("\n3. In the indifference curve plot:")
cat("\n   - Steeper curves = lower risk tolerance (Climber 2)")
cat("\n   - Flatter curves = higher risk tolerance (Climber 1)")
cat("\n   - Points above curves = positive utility (will attempt)")
cat("\n   - Points below curves = negative utility (will not attempt)")
cat("\n4. The decision matrix clearly shows the 'crossover point' where each climber stops attempting")
cat("\n5. Utility breakdown shows how reward and risk penalty balance differently for each climber")