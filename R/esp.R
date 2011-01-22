


numSelected<-function(mod1,code){
	
	

if(mod1$regr.order==1){dd=mod1$Stepfin}
if(mod1$regr.order>1){dd=mod1$StepI}


cc<-dd$terms; first<-attr(cc,"term.labels")


options(warn=-1);ff<-first != code; options(warn=0);num<-first[ff]

#print("Centered Variables Are")
#print (num)

return(num)

}
