library(data.table)
library(ggplot2)

rm(list=ls())
gc()

t <- theme(plot.title = element_text(face="bold"),
           axis.text.x = element_text(size=12,color='#000000',angle=45,hjust=1),
           axis.text.y = element_text(size=12,color='#000000'),
           axis.title.x = element_text(face="bold", size=10,color='#000000'),
           axis.title.y = element_text(face="bold", size=10,color='#000000'),
           panel.background = element_rect(fill='#ffffff', color='#a5a5a5',size=0.5),
           panel.ontop = F,
           panel.grid.major = element_line(color='#a5a5a5', linetype='dashed',size=0.2),
           panel.grid.minor = element_line(color='#a5a5a5', linetype='dashed', size=0),
           legend.text = element_text(size=10,color='#000000'),
           legend.title = element_text(face='bold',size=10,color='#000000'),
           legend.box.background = element_rect(fill='#ffffff', color='#ffffff', size=1.5),
           strip.text = element_text(size=10,color='#000000', face='bold'),
           strip.background = element_rect(colour = NA, fill = '#ffffff'))

pal <- c('#E02128','#ff0072','#1B2C3F','#3e21aa','#2672A4','#43A39E','#EB9E0F','#333745','#8f8f8f','#515151','#000000')

dt <- data.table(read.csv('jobs.csv', na.strings='error', stringsAsFactors=F))
dt <- dt[!is.na(description)]

dt <- dt[,title := tolower(title)]

dt[,powerbi := grepl(pattern='(powerbi|power bi)',description,ignore.case=T)]
dt[,qlik := grepl(pattern='qlik',description,ignore.case=T)]
dt[,tableau := grepl(pattern='tableau',description,ignore.case=T)]

dt[,c('p1','p2','p3','p4','p5','p6') := tstrsplit(meta_footer,'-',fixed=T)]
dt[,c('p2','p3','p4','p5','p6', 'employer','meta_footer','meta_header','scrape_date') := NULL]


setnames(dt,'p1','company')
setcolorder(dt,c('company','title','powerbi','qlik','tableau','description'))

dt_all <- dt
dt <- dt[powerbi + qlik + tableau > 0]
dt_g1 <- dt[,lapply(.SD,sum), .SDcols=c('powerbi','qlik','tableau')]
dt_g1 <- dt_g1[,lapply(.SD,function(x) { x / nrow(dt) * 100})]
dt_g1 <- melt.data.table(dt_g1)

g1 <- ggplot(dt_g1,aes(x=variable,y=value, fill=variable)) + 
  geom_bar(stat='identity') + 
  labs(y='% of job offers',x='') +
  scale_fill_manual(values=pal,name='software') +
  t
g1
ggsave('1.png',g1,width=48.8,height=27.4, units='cm')
rm(g1, dt_g1)

write.csv(dt,'vis_jobs.csv')
