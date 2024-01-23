# figures -----------------------------------------------------------------
## profile plots of lca models --------------------------------------------

# class-conditional probabilities
probs <- probs(models)

# profile plots
pplots <- pplots(probs)
print(pplots)

ggsave(
  paste(format(Sys.time(), "%Y.%m.%d.%H.%M"),
        "profile_plots.png",
        sep = "-"),
  device = png(),
  path = here("fig"),
  scale = 2.5,
  width = 1600,
  height = 900,
  units = "px",
  dpi = "retina"
)
## profile plot of best-fitting model -------------------------------------
# 
# # best / selected model, if not already set
# bestmodel.classes <- 4
# bestmodel <- which(classes == bestmodel.classes)
# 
# # descriptive labels for each class in best model
# class.labels <- laply(1:bestmodel.classes, function(x) {
#   labels <- c(" PTSD ", " CPTSD ", " DSO ", " Low ")
#   paste("Class ", x, labels[x], "(",
#         round(100 * table(models[[bestmodel]]$predclass)[x] /
#                 models[[bestmodel]]$N, 2),
#         "%)",
#         sep = ""
#   )
# })
# 
# # descriptive labels for indicator variables
# lca.vars.labels <- c(
#   "PTSD Dreams", "PTSD Flashbacks", "PTSD Av. Thoughts",
#   "PTSD Av. Behaviour", "PTSD Hypervigilance", "PTSD Startle",
#   "DSO Aff. Hyper.", "DSO Aff. Hypo.", "DSO Guilty",
#   "DSO Worthless", "DSO Distant", "DSO Detached"
# )
# 
# # import SE from mplus calc; seems more accurate than poLCA SE
# mplus4se <- as.data.frame(read_excel("mplus4se.xlsx"))
# 
# # class-conditional probabilities in best model + mplus SEs
# bestmodel.means <- mutate(models.plots$data[
#   models.plots$data$model == paste(c(bestmodel.classes, "-class"),
#                                    collapse = "") &
#     as.numeric(models.plots$data$class) <= bestmodel.classes,
# ],
# lower = mplus4se[, 3],
# upper = mplus4se[, 5]
# )
# 
# # profile plot of best model
# pplot2 <- function(j) {
#   ggplot(j, aes(x = var, y = prob)) +
#     geom_hline(yintercept = seq(0.1, 1, .1), color = "#939398") +
#     geom_line(aes(colour = class, group = class), size = 1.5) +
#     scale_color_manual(
#       labels = class.labels,
#       values = c("#e87d72", "#7ab582", "#56bcc2", "#ba9f56")
#     ) +
#     geom_ribbon(aes(ymin = lower, ymax = upper, fill = class, group = class),
#                 alpha = 0.175) +
#     coord_cartesian(ylim = c(0, 1)) +
#     geom_point(aes(color = class, shape = class, group = class), size = 4.5) +
#     scale_shape_manual(values = c(19, 19, 19, 19), labels = class.labels) +
#     scale_fill_manual(values = c("#e87d72", "#7ab582", "#56bcc2", "#ba9f56")) +
#     theme_bw() +
#     guides(shape = FALSE, fill = FALSE) +
#     scale_y_continuous(
#       breaks = seq(0, 1, .1), labels = c("0", seq(0.1, .9, 0.1), "1"),
#       expand = expansion(mult = c(.0025, .0025))
#     ) +
#     scale_x_discrete(labels = lca.vars.labels) +
#     geom_hline(yintercept = seq(0, 0), color = "#939398") +
#     theme(
#       axis.text.x = element_text(
#         angle = 45, vjust = 1,
#         hjust = 1, size = 10, color = "#000000"
#       ),
#       axis.text.y = element_text(size = 10, color = "#000000"),
#       panel.grid.minor.x = element_blank(),
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor.y = element_blank(),
#       panel.grid.major.y = element_blank(),
#       axis.title.x = element_blank(),
#       axis.title.y = element_blank(),
#       axis.ticks = element_blank(),
#       axis.ticks.y = element_blank(),
#       strip.background = element_blank(),
#       strip.text = element_blank(),
#       panel.border = element_blank(),
#       legend.title = element_blank(),
#       legend.key.width = unit(2, "cm"),
#       legend.text = element_text(size = 10),
#       plot.margin = unit(c(1, 1, 0.5, 1), "cm"),
#       legend.box.margin = margin(0, 0, 0, -1.1, unit = "cm"),
#       legend.position = "bottom",
#       text = element_text(family = "sans")
#     )
# }
# 
# bestmodel.plot <- pplot2(bestmodel.means)
# bestmodel.plot
# 
# # ggsave('figure1.p',bestmodel.plot,dev='pdf',scale=2,width=16.7,height=7,
# # dpi=300,units='cm')
# # ggsave('models.plots.pdf',models.plots,dev='pdf',scale=2.5,width=12,height=6,
# # units='cm')
