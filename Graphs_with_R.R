
#installing necessary packages
#install.packages("readxl")
library("readxl")
#install.packages("Rtools")
#install.packages("magrittr")
#install.packages("dplyr")
library(magrittr)
library(dplyr) 
df<-read_excel('hotdog-contest-winners.xlsm')
head(df)
str(df)
tablenew<-table(df$Country)
tabledata<-data.frame(tablenew)
#bar graph with R
op <- par(mar = c(14,10,10,6),mgp = c(6.5, 1, 0))
barplot(sort(table(df$Country), decreasing = FALSE),horiz=TRUE, las=1,space=0.5,cex.axis=1.5,cex.main = 1.7, cex.lab = 1.6, width=10, xlab='Count',ylab = 'Winning Countries', main = 'Distribution of Victories among the Countries', col = "dimgray")
par(op)

#Pie chart
install.packages("plotly")
library(plotly)
tabledata
m = list(
  l = 100,
  r = 100,
  b = 120,
  t = 120,
  pad = 0
)

colors <- c( 'blue','orange', 'dimgray', "#56B4E9")

fig <- plot_ly(tabledata, labels = ~Var1, values = ~Freq, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF', size =18),
               insidetextorientation='radial',
               hoverinfo = 'text',
               text = ~paste('$', Freq, ' billions'),
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE)
fig <- fig %>% layout(title = 'Winners per country',autosize = F, width = 750, height = 750, margin = m,titlefont=list(size=30, font = 'bald'),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig

#donut chart
m = list(
  l = 100,
  r = 100,
  b = 120,
  t = 120,
  pad = 0
)

fig1 <- tabledata %>% plot_ly(labels = ~Var1, values = ~Freq, textfont = list(color = "white", size = 18), marker = list(colors = c('blue','orange', 'dimgray', "#56B4E9")))
fig1 <- fig1 %>% add_pie(hole = 0.5)
fig1 <- fig1 %>% layout(autosize = F, width = 600, height = 600, margin = m, title = "Winners per country\n",titlefont=list(size=27, font = 'bald'),showlegend = T,
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


fig1
#stacked bar graph

colors = c("#009E73", 
           "#E69F00")
names(df)[names(df) == "New record"] <- "new_record"
dat <- data.frame(table(df$Country,df$new_record))
names(dat) <- c("Country","new_record","Count")
dat$new_record <- factor(dat$new_record, levels=c(0,1), labels=c("Not a Record", "New Record"))

head(dat)
ggplot(data=dat, aes(x = reorder(Country, -Count), y=Count, fill=new_record))+labs(title="Records per Country") +   xlab("Countries") + geom_bar(stat="identity")+theme(plot.title = element_text(hjust = 0.5))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_rect(fill = "white"),
                                                                                                                                                                       panel.background = element_rect(fill = "white"), axis.line = element_line(color = "grey"))+  scale_fill_manual(values=c("#E69F00", "#999999"))+
  labs( fill = "Records")
