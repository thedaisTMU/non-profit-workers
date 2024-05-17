library(SFSTheme)
library(ggplot2)
library(data.table)



#########################################################
#Load in graph spread sheets
graph.data <- fread("Graphs_spreadsheet.csv")




#################################
#Figure 1 onwards


figure_1_data <- fread("Figure_1.csv")
figure.1 <- plot.column.sfs(figure_1_data, stat, Sector, order.bar="ascending", group.by=Occupation, label=FALSE,
                             y.axis= graph.data[graph.data$Figure_number=="Figure 1",Y_Axis],
                             caption = graph.data[graph.data$Figure_number=="Figure 1",Caption],
                             label = TRUE,
                             label.unit = graph.data[graph.data$Figure_number=="Figure 1",Y_Axis_Ticks],
                             export = TRUE,
                             export.name = "F1") +
  scale_x_discrete(guide = guide_axis(angle = 0))
figure_2_data <- fread("Figure_2.csv")
figure.2 <- plot.column.sfs(figure_2_data, stat, Sector, order.bar="ascending", group.by=Occupation, label=FALSE,
                             y.axis= graph.data[graph.data$Figure_number=="Figure 2",Y_Axis],
                             caption = graph.data[graph.data$Figure_number=="Figure 2",Caption],
                             label = TRUE,
                             label.unit = graph.data[graph.data$Figure_number=="Figure 2",Y_Axis_Ticks],
                             export = TRUE,
                             export.name = "F2") +
  scale_x_discrete(guide = guide_axis(angle = 0))

figure_3_data <- fread("Figure_3.csv")
figure.3 <- plot.column.sfs(figure_3_data, stat, Age, group.by=Sector, label=FALSE,
                             y.axis= graph.data[graph.data$Figure_number=="Figure 3",Y_Axis],
                             caption = graph.data[graph.data$Figure_number=="Figure 3",Caption],
                             label = TRUE,
                             label.unit = graph.data[graph.data$Figure_number=="Figure 3",Y_Axis_Ticks],
                             export = TRUE,
                             export.name = "F3") +
  scale_x_discrete(guide = guide_axis(angle = 0))

figure_4_data <- fread("Figure_4.csv") |>
  dplyr::filter(Occupation=="Tech Worker")
figure.4 <- plot.column.sfs(figure_4_data, stat, Age, group.by=Sector, label=FALSE,
                             y.axis= graph.data[graph.data$Figure_number=="Figure 4",Y_Axis],
                             caption = graph.data[graph.data$Figure_number=="Figure 4",Caption],
                             label = TRUE,
                             label.unit = graph.data[graph.data$Figure_number=="Figure 4",Y_Axis_Ticks],
                             export = TRUE,
                             export.name = "F4") +
  scale_x_discrete(guide = guide_axis(angle = 0))

figure_5_data <- fread("Figure_5.csv")
figure.5 <- plot.column.sfs(figure_5_data, stat, Gender, order.bar="ascending", group.by=Sector, label=FALSE,
                             y.axis= graph.data[graph.data$Figure_number=="Figure 5",Y_Axis],
                             caption = graph.data[graph.data$Figure_number=="Figure 5",Caption],
                             label = TRUE,
                             label.unit = graph.data[graph.data$Figure_number=="Figure 5",Y_Axis_Ticks],
                             export = TRUE,
                             export.name = "F5") +
  scale_x_discrete(guide = guide_axis(angle = 0))

figure_6_data <- fread("Figure_6.csv")
figure_6_data$Education <- factor(figure_6_data$Education, levels=unique(figure_6_data$Education))
figure.6 <- plot.column.sfs(figure_6_data, stat, Education, group.by=Sector, label=FALSE,
                             y.axis= graph.data[graph.data$Figure_number=="Figure 6",Y_Axis],
                             caption = graph.data[graph.data$Figure_number=="Figure 6",Caption],
                             label = TRUE,
                             label.unit = graph.data[graph.data$Figure_number=="Figure 6",Y_Axis_Ticks],
                             export = TRUE,
                             export.name = "F6") +
  scale_x_discrete(guide = guide_axis(angle = 0))

figure_7_data <- fread("Figure_7.csv") |>
  dplyr::filter(Occupation=="Tech Worker")
figure_7_data$Education <- factor(figure_7_data$Education, levels=unique(figure_7_data$Education))
figure.7 <- plot.column.sfs(figure_7_data, stat, Education, group.by=Sector, label=FALSE,
                             y.axis= graph.data[graph.data$Figure_number=="Figure 7",Y_Axis],
                             caption = graph.data[graph.data$Figure_number=="Figure 7",Caption],
                             label = TRUE,
                             label.unit = graph.data[graph.data$Figure_number=="Figure 7",Y_Axis_Ticks],
                             export = TRUE,
                             export.name = "F7") +
  scale_x_discrete(guide = guide_axis(angle = 0))

figure_8_data <- fread("Figure_8.csv")
figure.8 <- plot.column.sfs(figure_8_data, stat, Ethnicity, order.bar="ascending", group.by=Sector, label=FALSE,
                             y.axis= graph.data[graph.data$Figure_number=="Figure 8",Y_Axis],
                             caption = graph.data[graph.data$Figure_number=="Figure 8",Caption],
                             label = TRUE,
                             label.unit = graph.data[graph.data$Figure_number=="Figure 8",Y_Axis_Ticks],
                             export = TRUE,
                             export.name = "F8") +
  scale_x_discrete(guide = guide_axis(angle = 0))

figure_9_data <- fread("Figure_9.csv") |>
  dplyr::filter(Occupation=="Tech Worker")
figure.9 <- plot.column.sfs(figure_9_data, stat, Ethnicity, order.bar="ascending", group.by=Sector, label=FALSE,
                             y.axis= graph.data[graph.data$Figure_number=="Figure 9",Y_Axis],
                             caption = graph.data[graph.data$Figure_number=="Figure 9",Caption],
                             label = TRUE,
                             label.unit = graph.data[graph.data$Figure_number=="Figure 9",Y_Axis_Ticks],
                             export = TRUE,
                             export.name = "F9") +
  scale_x_discrete(guide = guide_axis(angle = 0))

figure_10_data <- fread("Figure_10.csv")
figure.10 <- plot.column.sfs(figure_10_data, stat, IndigenousIdentity, order.bar="ascending", group.by=Sector, label=FALSE,
                             y.axis= graph.data[graph.data$Figure_number=="Figure 10",Y_Axis],
                             caption = graph.data[graph.data$Figure_number=="Figure 10",Caption],
                             label.unit = graph.data[graph.data$Figure_number=="Figure 10",Y_Axis_Ticks],
                             export = TRUE,
                             export.name = "F10") +
  scale_x_discrete(guide = guide_axis(angle = 0))

figure_11_data <- fread("Figure_11.csv") |>
  dplyr::filter(Occupation=="Tech Worker")
figure.11 <- plot.column.sfs(figure_11_data, stat, IndigenousIdentity, order.bar="ascending", group.by=Sector, label=FALSE,
                             y.axis= graph.data[graph.data$Figure_number=="Figure 11",Y_Axis],
                             caption = graph.data[graph.data$Figure_number=="Figure 11",Caption],
                             label = TRUE,
                             label.unit = graph.data[graph.data$Figure_number=="Figure 11",Y_Axis_Ticks],
                             export = TRUE,
                             export.name = "F11") +
  scale_x_discrete(guide = guide_axis(angle = 0))
