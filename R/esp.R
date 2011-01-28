


numSelected<-function(mod1,code){
	
	

if(mod1$regr.order==1){dd=mod1$Stepfin}
if(mod1$regr.order>1){dd=mod1$StepI}


cc<-dd$terms; first<-attr(cc,"term.labels")


num<-setdiff(first,code)

#print("Centered Variables Are")
#print (num)

return(num)

}
