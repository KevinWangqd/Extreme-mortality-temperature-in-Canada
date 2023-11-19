# Pickands Dependence Function Plot
# Based on the output from "6_extreme_value_analysis.R"
library(ggplot2)
library(showtext)

# Function: Calculate the Pickands dependence value
pick_fun <- function(w, alpha){
  ans <- rep(NA, length(w))
  idx <- which((w<=0) | (w>1))
  if(length(idx)>0){
    w <- w[-idx]
    ans[-idx] <- ((1-w)^(1/alpha) + w^(1/alpha))^alpha
  }
  else ans <- ((1-w)^(1/alpha) + w^(1/alpha))^alpha
  return(ans)
}

w <- seq(0, 1, length.out=252)

## Age 0-44 & T10
alpha_1 <- (log(2-model_CART10044$chi))/log(2)
pickands_1 <- pick_fun(w, alpha_1)
pickands_1[1] <- 1
alpha_2 <- (log(2-model_NEAT10044$chi))/log(2)
pickands_2 <- pick_fun(w, alpha_2)
pickands_2[1] <- 1
alpha_3 <- (log(2-model_NEFT10044$chi))/log(2)
pickands_3 <- pick_fun(w, alpha_3)
pickands_3[1] <- 1
alpha_4 <- (log(2-model_NPLT10044$chi))/log(2)
pickands_4 <- pick_fun(w, alpha_4)
pickands_4[1] <- 1
alpha_5 <- (log(2-model_NWPT10044$chi))/log(2)
pickands_5 <- pick_fun(w, alpha_5)
pickands_5[1] <- 1
alpha_n1 <- model_National_10044$param[5]
pickands_n1 <- pick_fun(w, alpha_n1)
pickands_n1[1] <- 1

pick044T10 <- as.data.frame(cbind(w, pickands_1, pickands_2, pickands_3, pickands_4, pickands_5,pickands_n1))
# Draw the plot 
plot_pick044T10 <- ggplot(pick044T10, aes(x = w)) + 
  geom_line(aes(y = pickands_1, linetype = "CAR", colour = "CAR")) +
  geom_line(aes(y = pickands_2, linetype = "NEA", colour = "NEA")) +
  geom_line(aes(y = pickands_3, linetype = "NEF", colour = "NEF")) +
  geom_line(aes(y = pickands_4, linetype = "NPL", colour = "NPL")) +
  geom_line(aes(y = pickands_5, linetype = "NWP", colour = "NWP")) + 
  geom_line(aes(y = pickands_n1, linetype = "National", colour = "National")) + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0.5, 1)) +
  geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1, colour = "lightgrey")) +
  geom_segment(aes(x = 0, y = 1, xend = 0.5, yend = 0.5, colour = "lightgrey")) +
  geom_segment(aes(x = 0.5, y = 0.5, xend = 1, yend = 1, colour = "lightgrey")) +
  theme_minimal() +
  scale_color_manual(name = "Region", values = c("CAR" = "blue", "NEA" = "red", "NEF" = "purple", 
                                                 "NPL" = "orange", "NWP" = "brown", "National"="#2ca02c")) + 
  scale_linetype_manual(name = "Region", values = c("CAR" = "solid", "NEA" = "dashed", "NEF" = "dotted", 
                                   "NPL" = "twodash", "NWP" = "longdash","National"="dotdash" )) +
  ggtitle("T10: Age 0-44") + 
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  labs(x = expression(paste(omega)), y = expression(paste("A (", omega, ")")))

# Add the legend
linetype_labels <- c("CAR" = "solid", "NEA" = "dashed", "NEF" = "dotted", 
                     "NPL" = "twodash", "NWP" = "longdash", "National"="dotdash")
color_labels <- c("CAR" = "blue", "NEA" = "red", "NEF" = "purple", 
                  "NPL" = "orange", "NWP" = "brown", "National"="#2ca02c")
plot_pick044T10 <- plot_pick044T10 + 
  guides(linetype = guide_legend(override.aes = list(linetype = linetype_labels)))
plot_pick044T10




























jpeg("T10_044.jpeg", quality = 100, units = "in", width = 7, height = 6, res = 300)

# Print the plot or visualization
plot_pick044T10
# Close the jpeg device
dev.off()


#labs(x = "ω", y = "A(ω)")

## Age 0-44 & T90
alpha_6 <- (log(2-model_CART90044$chi))/log(2)
pickands_6 <- pick_fun(w, alpha_6)
pickands_6[1] <- 1
alpha_7 <- (log(2-model_NEAT90044$chi))/log(2)
pickands_7 <- pick_fun(w, alpha_7)
pickands_7[1] <- 1
alpha_8 <- (log(2-model_NEFT90044$chi))/log(2)
pickands_8 <- pick_fun(w, alpha_8)
pickands_8[1] <- 1
alpha_9 <- (log(2-model_NPLT90044$chi))/log(2)
pickands_9 <- pick_fun(w, alpha_9)
pickands_9[1] <- 1
alpha_10 <- (log(2-model_NWPT90044$chi))/log(2)
pickands_10 <- pick_fun(w, alpha_10)
pickands_10[1] <- 1
alpha_n2 <- model_National_90044$param[5]
pickands_n2 <- pick_fun(w, alpha_n2)
pickands_n2[1] <- 1

pick044T90 <- as.data.frame(cbind(w, pickands_6, pickands_7, pickands_8, pickands_9, pickands_10,pickands_n2))
# Draw the plot 
plot_pick044T90 <- ggplot(pick044T90, aes(x = w)) + 
  geom_line(aes(y = pickands_6, linetype = "CAR", colour = "CAR")) +
  geom_line(aes(y = pickands_7, linetype = "NEA", colour = "NEA")) +
  geom_line(aes(y = pickands_8, linetype = "NEF", colour = "NEF")) +
  geom_line(aes(y = pickands_9, linetype = "NPL", colour = "NPL")) +
  geom_line(aes(y = pickands_10, linetype = "NWP", colour = "NWP")) + 
  geom_line(aes(y = pickands_n2, linetype = "National", colour = "National")) + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0.5, 1)) +
  geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1, colour = "lightgrey")) +
  geom_segment(aes(x = 0, y = 1, xend = 0.5, yend = 0.5, colour = "lightgrey")) +
  geom_segment(aes(x = 0.5, y = 0.5, xend = 1, yend = 1, colour = "lightgrey")) +
  theme_minimal() +
  scale_color_manual(name = "Region", values = c("CAR" = "blue", "NEA" = "red", "NEF" = "purple", 
                                                 "NPL" = "orange", "NWP" = "brown", "National"="#2ca02c")) + 
  scale_linetype_manual(name = "Region", values = c("CAR" = "solid", "NEA" = "dashed", "NEF" = "dotted", 
                                                    "NPL" = "twodash", "NWP" = "longdash","National"="dotdash" )) +
  ggtitle("T90: Age 0-44") + 
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  labs(x = expression(paste(omega)), y = expression(paste("A (", omega, ")")))
# Add the legend
plot_pick044T90 <- plot_pick044T90 + 
  guides(linetype = guide_legend(override.aes = list(linetype = linetype_labels)))
plot_pick044T90


jpeg("T90_044.jpeg", quality = 100, units = "in", width = 7, height = 6, res = 300)

# Print the plot or visualization
plot_pick044T90
# Close the jpeg device
dev.off()



## Age 45-64 & T10
alpha_11 <- (log(2-model_CART1045$chi))/log(2)
pickands_11 <- pick_fun(w, alpha_11)
pickands_11[1] <- 1
alpha_12 <- (log(2-model_NEAT1045$chi))/log(2)
pickands_12 <- pick_fun(w, alpha_12)
pickands_12[1] <- 1
alpha_13 <- (log(2-model_NEFT1045$chi))/log(2)
pickands_13 <- pick_fun(w, alpha_13)
pickands_13[1] <- 1
alpha_14 <- (log(2-model_NPLT1045$chi))/log(2)
pickands_14 <- pick_fun(w, alpha_14)
pickands_14[1] <- 1
alpha_15 <- (log(2-model_NWPT1045$chi))/log(2)
pickands_15 <- pick_fun(w, alpha_15)
pickands_15[1] <- 1
alpha_n3 <- model_National_104564$param[5]
pickands_n3 <- pick_fun(w, alpha_n3)
pickands_n3[1] <- 1

pick45T10 <- as.data.frame(cbind(w, pickands_11, pickands_12, pickands_13, pickands_14, pickands_15, pickands_n3))
# Draw the plot 
plot_pick45T10 <- ggplot(pick45T10, aes(x = w)) + 
  geom_line(aes(y = pickands_11, linetype = "CAR", colour = "CAR"),size=1.2) +
  geom_line(aes(y = pickands_12, linetype = "NEA", colour = "NEA"),size=1.2) +
  geom_line(aes(y = pickands_13, linetype = "NEF", colour = "NEF"),size=1.2) +
  geom_line(aes(y = pickands_14, linetype = "NPL", colour = "NPL"),size=1.2) +
  geom_line(aes(y = pickands_15, linetype = "NWP", colour = "NWP"),size=1.2) +
  geom_line(aes(y = pickands_n3, linetype = "National", colour = "National"),size=1.2) + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0.5, 1)) +
  geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1, colour = "lightgrey")) +
  geom_segment(aes(x = 0, y = 1, xend = 0.5, yend = 0.5, colour = "lightgrey")) +
  geom_segment(aes(x = 0.5, y = 0.5, xend = 1, yend = 1, colour = "lightgrey")) +
  theme_minimal() +
  scale_color_manual(name = "Region", values = c("CAR" = "blue", "NEA" = "red", "NEF" = "purple", 
                                                 "NPL" = "orange", "NWP" = "brown", "National"="#2ca02c")) + 
  scale_linetype_manual(name = "Region", values = c("CAR" = "solid", "NEA" = "dashed", "NEF" = "dotted", 
                                                    "NPL" = "twodash", "NWP" = "longdash","National"="dotdash" )) +
  ggtitle("T10: Age 45-64") + 
  theme(plot.title = element_text(size = 20, hjust = 0.5), legend.key.size = unit(30,"pt")) +
  labs(x = expression(paste(omega)), y = expression(paste("A (", omega, ")")))


# Add the legend
plot_pick45T10 <- plot_pick45T10 + 
  guides(linetype = guide_legend(override.aes = list(linetype = linetype_labels)))
plot_pick45T10


jpeg("T10_4564.jpeg", quality = 100, units = "in", width = 7, height = 6, res = 300)

# Print the plot or visualization
plot_pick45T10
# Close the jpeg device
dev.off()


## Age 45-64 & T90
alpha_16 <- (log(2-model_CART9045$chi))/log(2)
pickands_16 <- pick_fun(w, alpha_16)
pickands_16[1] <- 1
alpha_17 <- (log(2-model_NEAT9045$chi))/log(2)
pickands_17 <- pick_fun(w, alpha_17)
pickands_17[1] <- 1
alpha_18 <- (log(2-model_NEFT9045$chi))/log(2)
pickands_18 <- pick_fun(w, alpha_18)
pickands_18[1] <- 1
alpha_19 <- (log(2-model_NPLT9045$chi))/log(2)
pickands_19 <- pick_fun(w, alpha_19)
pickands_19[1] <- 1
alpha_20 <- (log(2-model_NWPT9045$chi))/log(2)
pickands_20 <- pick_fun(w, alpha_20)
pickands_20[1] <- 1

alpha_n4 <- model_National_904564$param[5]
pickands_n4 <- pick_fun(w, alpha_n4)
pickands_n4[1] <- 1

pick45T90 <- as.data.frame(cbind(w, pickands_16, pickands_17, pickands_18, pickands_19, pickands_20, pickands_n4))
# Draw the plot 
plot_pick45T90 <- ggplot(pick45T90, aes(x = w)) + 
  geom_line(aes(y = pickands_16, linetype = "CAR", colour = "CAR"),size=1.2) +
  geom_line(aes(y = pickands_17, linetype = "NEA", colour = "NEA"),size=1.2) +
  geom_line(aes(y = pickands_18, linetype = "NEF", colour = "NEF"),size=1.2) +
  geom_line(aes(y = pickands_19, linetype = "NPL", colour = "NPL"),size=1.2) +
  geom_line(aes(y = pickands_20, linetype = "NWP", colour = "NWP"),size=1.2) + 
  geom_line(aes(y = pickands_n4, linetype = "National", colour = "National"),size=1.2) + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0.5, 1)) +
  geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1, colour = "lightgrey")) +
  geom_segment(aes(x = 0, y = 1, xend = 0.5, yend = 0.5, colour = "lightgrey")) +
  geom_segment(aes(x = 0.5, y = 0.5, xend = 1, yend = 1, colour = "lightgrey")) +
  theme_minimal() +
  scale_color_manual(name = "Region", values = c("CAR" = "blue", "NEA" = "red", "NEF" = "purple", 
                                                 "NPL" = "orange", "NWP" = "brown", "National"="#2ca02c")) + 
  scale_linetype_manual(name = "Region", values = c("CAR" = "solid", "NEA" = "dashed", "NEF" = "dotted", 
                                                    "NPL" = "twodash", "NWP" = "longdash","National"="dotdash" )) +
  ggtitle("T90: Age 45-64") + 
  theme(plot.title = element_text(size = 20, hjust = 0.5), legend.key.size = unit(30,"pt")) +
  labs(x = expression(paste(omega)), y = expression(paste("A (", omega, ")")))
# Add the legend
plot_pick45T90 <- plot_pick45T90 + 
  guides(linetype = guide_legend(override.aes = list(linetype = linetype_labels)))
plot_pick45T90


jpeg("T90_4564.jpeg", quality = 100, units = "in", width = 7, height = 6, res = 300)

# Print the plot or visualization
plot_pick45T90
# Close the jpeg device
dev.off()


## Age 65-84 & T10
alpha_21 <- (log(2-model_CART1065$chi))/log(2)
pickands_21 <- pick_fun(w, alpha_21)
pickands_21[1] <- 1
alpha_22 <- (log(2-model_NEAT1065$chi))/log(2)
pickands_22 <- pick_fun(w, alpha_22)
pickands_22[1] <- 1
alpha_23 <- (log(2-model_NEFT1065$chi))/log(2)
pickands_23 <- pick_fun(w, alpha_23)
pickands_23[1] <- 1
alpha_24 <- (log(2-model_NPLT1065$chi))/log(2)
pickands_24 <- pick_fun(w, alpha_24)
pickands_24[1] <- 1
alpha_25 <- (log(2-model_NWPT1065$chi))/log(2)
pickands_25 <- pick_fun(w, alpha_25)
pickands_25[1] <- 1
alpha_n5 <- model_National_106584$param[5]
pickands_n5 <- pick_fun(w, alpha_n5)
pickands_n5[1] <- 1
pick65T10 <- as.data.frame(cbind(w, pickands_21, pickands_22, pickands_23, pickands_24, pickands_25,pickands_n5))
# Draw the plot 
plot_pick65T10 <- ggplot(pick65T10, aes(x = w)) + 
  geom_line(aes(y = pickands_21, linetype = "CAR", colour = "CAR"),size=1.2) +
  geom_line(aes(y = pickands_22, linetype = "NEA", colour = "NEA"),size=1.2) +
  geom_line(aes(y = pickands_23, linetype = "NEF", colour = "NEF"),size=1.2) +
  geom_line(aes(y = pickands_24, linetype = "NPL", colour = "NPL"),size=1.2) +
  geom_line(aes(y = pickands_25, linetype = "NWP", colour = "NWP"),size=1.2) + 
  geom_line(aes(y = pickands_n5, linetype = "National", colour = "National"),size=1.2) + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0.5, 1)) +
  geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1, colour = "lightgrey")) +
  geom_segment(aes(x = 0, y = 1, xend = 0.5, yend = 0.5, colour = "lightgrey")) +
  geom_segment(aes(x = 0.5, y = 0.5, xend = 1, yend = 1, colour = "lightgrey")) +
  theme_minimal() +
  scale_color_manual(name = "Region", values = c("CAR" = "blue", "NEA" = "red", "NEF" = "purple", 
                                                 "NPL" = "orange", "NWP" = "brown", "National"="#2ca02c")) + 
  scale_linetype_manual(name = "Region", values = c("CAR" = "solid", "NEA" = "dashed", "NEF" = "dotted", 
                                                    "NPL" = "twodash", "NWP" = "longdash","National"="dotdash" )) +
  ggtitle("T10: Age 65-84") + 
  theme(plot.title = element_text(size = 20, hjust = 0.5), legend.key.size = unit(30,"pt")) +
  labs(x = expression(paste(omega)), y = expression(paste("A (", omega, ")")))
# Add the legend
plot_pick65T10 <- plot_pick65T10 + 
  guides(linetype = guide_legend(override.aes = list(linetype = linetype_labels)))
plot_pick65T10

jpeg("T10_6584.jpeg", quality = 100, units = "in", width = 7, height = 6, res = 300)

# Print the plot or visualization
plot_pick65T10
# Close the jpeg device
dev.off()


## Age 65-84 & T90
alpha_26 <- (log(2-model_CART9065$chi))/log(2)
pickands_26 <- pick_fun(w, alpha_26)
pickands_26[1] <- 1
alpha_27 <- (log(2-model_NEAT9065$chi))/log(2)
pickands_27 <- pick_fun(w, alpha_27)
pickands_27[1] <- 1
alpha_28 <- (log(2-model_NEFT9065$chi))/log(2)
pickands_28 <- pick_fun(w, alpha_28)
pickands_28[1] <- 1
alpha_29 <- (log(2-model_NPLT9065$chi))/log(2)
pickands_29 <- pick_fun(w, alpha_29)
pickands_29[1] <- 1
alpha_30 <- (log(2-model_NWPT9065$chi))/log(2)
pickands_30 <- pick_fun(w, alpha_30)
pickands_30[1] <- 1
alpha_n6 <- model_National_906584$param[5]
pickands_n6 <- pick_fun(w, alpha_n6)
pickands_n6[1] <- 1
pick65T90 <- as.data.frame(cbind(w, pickands_26, pickands_27, pickands_28, pickands_29, pickands_30))
# Draw the plot 
plot_pick65T90 <- ggplot(pick65T90, aes(x = w)) + 
  geom_line(aes(y = pickands_26, linetype = "CAR", colour = "CAR"),size=1.2) +
  geom_line(aes(y = pickands_27, linetype = "NEA", colour = "NEA"),size=1.2) +
  geom_line(aes(y = pickands_28, linetype = "NEF", colour = "NEF"),size=1.2) +
  geom_line(aes(y = pickands_29, linetype = "NPL", colour = "NPL"),size=1.2) +
  geom_line(aes(y = pickands_30, linetype = "NWP", colour = "NWP"),size=1.2) + 
  geom_line(aes(y = pickands_n6, linetype = "National", colour = "National"),size=1.2) + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0.5, 1)) +
  geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1, colour = "lightgrey")) +
  geom_segment(aes(x = 0, y = 1, xend = 0.5, yend = 0.5, colour = "lightgrey")) +
  geom_segment(aes(x = 0.5, y = 0.5, xend = 1, yend = 1, colour = "lightgrey")) +
  theme_minimal() +
  scale_color_manual(name = "Region", values = c("CAR" = "blue", "NEA" = "red", "NEF" = "purple", 
                                                 "NPL" = "orange", "NWP" = "brown", "National"="#2ca02c")) + 
  scale_linetype_manual(name = "Region", values = c("CAR" = "solid", "NEA" = "dashed", "NEF" = "dotted", 
                                                    "NPL" = "twodash", "NWP" = "longdash","National"="dotdash" )) +
  ggtitle("T90: Age 65-84") + 
  theme(plot.title = element_text(size = 20, hjust = 0.5), legend.key.size = unit(30,"pt")) +
  labs(x = expression(paste(omega)), y = expression(paste("A (", omega, ")")))
# Add the legend
plot_pick65T90 <- plot_pick65T90 + 
  guides(linetype = guide_legend(override.aes = list(linetype = linetype_labels)))
plot_pick65T90

jpeg("T90_6584.jpeg", quality = 100, units = "in", width = 7, height = 6, res = 300)

# Print the plot or visualization
plot_pick65T90
# Close the jpeg device
dev.off()

## Age 85+ & T10
alpha_31 <- (log(2-model_CART1085$chi))/log(2)
pickands_31 <- pick_fun(w, alpha_31)
pickands_31[1] <- 1
alpha_32 <- (log(2-model_NEAT1085$chi))/log(2)
pickands_32 <- pick_fun(w, alpha_32)
pickands_32[1] <- 1
alpha_33 <- (log(2-model_NEFT1085$chi))/log(2)
pickands_33 <- pick_fun(w, alpha_33)
pickands_33[1] <- 1
alpha_34 <- (log(2-model_NPLT1085$chi))/log(2)
pickands_34 <- pick_fun(w, alpha_34)
pickands_34[1] <- 1
alpha_35 <- (log(2-model_NWPT1085$chi))/log(2)
pickands_35 <- pick_fun(w, alpha_35)
pickands_35[1] <- 1
alpha_n7 <- model_National_1085$param[5]
pickands_n7 <- pick_fun(w, alpha_n7)
pickands_n7[1] <- 1
pick85T10 <- as.data.frame(cbind(w, pickands_31, pickands_32, pickands_33, pickands_34, pickands_35,pickands_n7))
# Draw the plot 
plot_pick85T10 <- ggplot(pick85T10, aes(x = w)) + 
  geom_line(aes(y = pickands_31, linetype = "CAR", colour = "CAR"),size=1.2) +
  geom_line(aes(y = pickands_32, linetype = "NEA", colour = "NEA"),size=1.2) +
  geom_line(aes(y = pickands_33, linetype = "NEF", colour = "NEF"),size=1.2) +
  geom_line(aes(y = pickands_34, linetype = "NPL", colour = "NPL"),size=1.2) +
  geom_line(aes(y = pickands_35, linetype = "NWP", colour = "NWP"),size=1.2) + 
  geom_line(aes(y = pickands_n7, linetype = "National", colour = "National"),size=1.2) + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0.5, 1)) +
  geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1, colour = "lightgrey")) +
  geom_segment(aes(x = 0, y = 1, xend = 0.5, yend = 0.5, colour = "lightgrey")) +
  geom_segment(aes(x = 0.5, y = 0.5, xend = 1, yend = 1, colour = "lightgrey")) +
  theme_minimal() +
  scale_color_manual(name = "Region", values = c("CAR" = "blue", "NEA" = "red", "NEF" = "purple", 
                                                 "NPL" = "orange", "NWP" = "brown", "National"="#2ca02c")) + 
  scale_linetype_manual(name = "Region", values = c("CAR" = "solid", "NEA" = "dashed", "NEF" = "dotted", 
                                                    "NPL" = "twodash", "NWP" = "longdash","National"="dotdash" )) +
  ggtitle("T10: Age 85+") + 
  theme(plot.title = element_text(size = 20, hjust = 0.5), legend.key.size = unit(30,"pt")) +
  labs(x = expression(paste(omega)), y = expression(paste("A (", omega, ")")))
# Add the legend
plot_pick85T10 <- plot_pick85T10 + 
  guides(linetype = guide_legend(override.aes = list(linetype = linetype_labels)))
plot_pick85T10


jpeg("T10_85.jpeg", quality = 100, units = "in", width = 7, height = 6, res = 300)

# Print the plot or visualization
plot_pick85T10
# Close the jpeg device
dev.off()




## Age 85+ & T90
alpha_36 <- (log(2-model_CART9085$chi))/log(2)
pickands_36 <- pick_fun(w, alpha_36)
pickands_36[1] <- 1
alpha_37 <- (log(2-model_NEAT9085$chi))/log(2)
pickands_37 <- pick_fun(w, alpha_37)
pickands_37[1] <- 1
alpha_38 <- (log(2-model_NEFT9085$chi))/log(2)
pickands_38 <- pick_fun(w, alpha_38)
pickands_38[1] <- 1
alpha_39 <- (log(2-model_NPLT9085$chi))/log(2)
pickands_39 <- pick_fun(w, alpha_39)
pickands_39[1] <- 1
alpha_40 <- (log(2-model_NWPT9085$chi))/log(2)
pickands_40 <- pick_fun(w, alpha_40)
pickands_40[1] <- 1
alpha_n8 <- model_National_9085$param[5]
pickands_n8 <- pick_fun(w, alpha_n8)
pickands_n8[1] <- 1
pick85T90 <- as.data.frame(cbind(w, pickands_36, pickands_37, pickands_38, pickands_39, pickands_30,pickands_n8))
# Draw the plot 
plot_pick85T90 <- ggplot(pick85T90, aes(x = w)) + 
  geom_line(aes(y = pickands_36, linetype = "CAR", colour = "CAR"),size=1.2) +
  geom_line(aes(y = pickands_37, linetype = "NEA", colour = "NEA"),size=1.2) +
  geom_line(aes(y = pickands_38, linetype = "NEF", colour = "NEF"),size=1.2) +
  geom_line(aes(y = pickands_39, linetype = "NPL", colour = "NPL"),size=1.2) +
  geom_line(aes(y = pickands_30, linetype = "NWP", colour = "NWP"),size=1.2) + 
  geom_line(aes(y = pickands_n8, linetype = "National", colour = "National"),size=1.2) + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0.5, 1)) +
  geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1, colour = "lightgrey")) +
  geom_segment(aes(x = 0, y = 1, xend = 0.5, yend = 0.5, colour = "lightgrey")) +
  geom_segment(aes(x = 0.5, y = 0.5, xend = 1, yend = 1, colour = "lightgrey")) +
  theme_minimal() +
  scale_color_manual(name = "Region", values = c("CAR" = "blue", "NEA" = "red", "NEF" = "purple", 
                                                 "NPL" = "orange", "NWP" = "brown", "National"="#2ca02c")) + 
  scale_linetype_manual(name = "Region", values = c("CAR" = "solid", "NEA" = "dashed", "NEF" = "dotted", 
                                                    "NPL" = "twodash", "NWP" = "longdash","National"="dotdash" )) +
  ggtitle("T90: Age 85+") + 
  theme(plot.title = element_text(size = 20, hjust = 0.5), legend.key.size = unit(30,"pt")) +
  labs(x = expression(paste(omega)), y = expression(paste("A (", omega, ")")))
# Add the legend
plot_pick85T90 <- plot_pick85T90 + 
  guides(linetype = guide_legend(override.aes = list(linetype = linetype_labels)))
plot_pick85T90

jpeg("T90_85.jpeg", quality = 100, units = "in", width = 7, height = 6, res = 300)

# Print the plot or visualization
plot_pick85T90
# Close the jpeg device
dev.off()

