ggplot(prop_data, aes(x=Diet.Scav, y=proportion, col=toxicant.group))+
  geom_point()+facet_wrap(~toxicant.group)+theme(text=element_text(size=15),
                                                 panel.grid.major = element_blank(), 
                                                 panel.grid.minor = element_blank(),
                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("% scavenger diet")

ggplot(prop_data, aes(x=toxicant.group, y=proportion))+
  geom_boxplot()+geom_jitter()+scale_x_discrete(guide = guide_axis(angle = 45))+theme(text=element_text(size=15),
                                                 panel.grid.major = element_blank(), 
                                                 panel.grid.minor = element_blank(),
                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))

ARplot=prop_data%>%filter(toxicant.group=="anticoagulant rodenticides")

ar1=ggplot(ARplot, aes(x=Diet.Scav, y=proportion))+
  geom_point()+theme(text=element_text(size=15),
                                                 panel.grid.major = element_blank(), 
                                                 panel.grid.minor = element_blank(),
                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("% scavenger diet")+ggtitle("anticoagulant rodenticides")

ar2=ggplot(ARplot, aes(x=Diet.Inv, y=proportion))+
  geom_point()+theme(text=element_text(size=15),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("% invertebrate diet")

ar3=ggplot(ARplot, aes(x=BodyMass.Value, y=proportion))+
  geom_point()+theme(text=element_text(size=15),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("body mass(g)")

OCplot=prop_data%>%filter(toxicant.group=="organochlorine insecticides")

oc1=ggplot(OCplot, aes(x=Diet.Scav, y=proportion))+
  geom_point()+theme(text=element_text(size=15),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("% scavenger diet")+ggtitle("organochlorine insectides")

oc2=ggplot(OCplot, aes(x=Diet.Inv, y=proportion))+
  geom_point()+theme(text=element_text(size=15),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("% invertebrate diet")

oc3=ggplot(OCplot, aes(x=BodyMass.Value, y=proportion))+
  geom_point()+theme(text=element_text(size=15),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("body mass(g)")

ggarrange(ar1,ar2,ar3, ncol=1)

ggarrange(oc1,oc2,oc3, ncol=1)
