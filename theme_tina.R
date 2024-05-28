
theme_tina <- theme_minimal() +
   theme(legend.position="bottom") +
   theme(text=element_text(size=10)) +
   theme(axis.text=element_text(size=20)) +
   theme(axis.title.x = element_text(size=30, margin=margin(20,0,0,0))) +
   theme(axis.title.y = element_text(size=30, margin=margin(0,20,0,0))) +
   theme(legend.key.size = unit(3, 'mm'), #change legend key size
         legend.title = element_text(size=20), #change legend title font size
         legend.text = element_text(size=20), #change legend text font size
         legend.title.align = 0.5,
         legend.margin=margin(10,0,0,0)) +
   theme(plot.margin = margin(1,1,1.5,1.2,"cm"))