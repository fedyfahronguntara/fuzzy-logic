data <- read.csv("DataTugas2.csv")
no<-data$No
pendapatan<- data$Pendapatan
hutang<- data$Hutang
nkt1=0
nkt2=0
nkt3=0
nkrr=0
nktt=0
nkr1=0
nkr2=0
nkr3=0
nkr4=0
nkr5=0
nkt4=0
nkr4=0
nkt5=0
nkt6=0
nkt7=0
fh1anggota=0
fh2anggota=0
fp1anggota=0
fp2anggota=0
rasio=array()
ystar=array()
sigmoidleft <- function(x,a,b,c){
  if (x<=a){
    hasil=0
    return (hasil)
  }
  else if (a<x && x<=b){
    hasil<-2*(((x-a)/(c-a))^2)
    return(hasil)
  }
  else if (b<x && x<c){
    a<-2*(((c-x)/(c-a))^2)
    hasil<- 1-a
    return(hasil)
  }
  else if (c<=x){
    hasil=1
    return(hasil)
  }
  
}
sigmoidright <- function(x,a,b,c){
  if (x<=a){
    hasil=1
    return(hasil)
  }
  else if (a<x && x<=b){
    a<-2*(((x-a)/(c-a))^2)
    hasil<- 1-a  
    return(hasil)
  }
  else if (b<x && x<c){
    hasil<-2*(((c-x)/(c-a))^2)
    return(hasil)
  }
  else if (x>=c){
    hasil=0
    return(hasil)
  }
}


trapesium<- function(x,a,b,c,d){
  if (x<=a || x>=d){
    hasil=0
    return(hasil)
  }
  else if (b<=x && x<=c){
    hasil=1
    return(hasil)
  }
  else if (c<x && x<=d){
    hasil<- (-1*(x-d))/(d-c) 
    return(hasil)
  }
  else if (a<x && x<b){
    hasil<-(x-a)/(b-a)
    return(hasil)
  }
  
}

segitigadown<- function(x,a,b){
  if (x<a){
    hasil=1
    return(hasil)
  }else {
    hasil<-(b-x)/(b-a)
    return(hasil)
  }
}
segitigaup<- function(x,a,b){
  if (x>b){
    hasil=1
    return(hasil)
  }else{
    hasil<-(x-a)/(b-a)
    return(hasil)
  }
}



for (i in 1:100){
  
  #grafik hutang
  if (hutang[i]>=0 && hutang[i]<=10){
    y=hutang[i]
    fh1anggota=segitigadown(y, 10, 30)
    fh2anggota=0
    nh1anggota="kecil"
    nh2anggota=""
  }

  else if (hutang[i]>10 && hutang[i]<30){
    y=hutang[i]
    fh1anggota=segitigadown(y, 10, 30)
    fh2anggota=trapesium(y, 10, 30, 40, 60)
    nh1anggota="kecil"
    nh2anggota="sedang"
    
    
  }
  else if (hutang[i]>=30 && hutang[i]<=40){
    y=hutang[i]
    fh1anggota=trapesium(y, 10, 30, 40, 60)
    fh2angggota=0
    nh1anggota="sedang"
    nh2anggota=""
    
  }
  else if (hutang[i]>40 && hutang[i]<60){
    y=hutang[i]
    fh1anggota=trapesium(y, 10, 30, 40, 60)
    fh2anggota=trapesium(y, 40, 60, 70, 120)
    nh1anggota="sedang"
    nh2anggota="besar"
    
  }
 else if (hutang[i]>=60 && hutang[i]<=70){
    y=hutang[i]
    fh1anggota=trapesium(y, 40, 60, 70, 120)
    fh2angggota=0
    nh1anggota="besar"
    nh2anggota=""
    
  }
  else if (hutang[i]> 70 && hutang[i] < 120){
    y=hutang[i]
    fh1anggota=trapesium(y, 40, 60, 70, 120)
    fh2anggota=segitigaup(y, 70, 120)
    nh1anggota="besar"
    nh2anggota="stbesar"
    
  }
  else if (hutang[i]>=120){
    y=hutang[i]
    fh1anggota=segitigaup(y, 70, 120)
    fh2angggota=0
    nh1anggota="stbesar"
    nh2anggota=""
    
  }
  
  


#grafik rasio pendapatan
persen=pendapatan[i]*0.4
bcicil=hutang[i]/persen
rasio[i]=bcicil/12
  
if (rasio[i]>=0 && rasio[i]<=10){
  x=rasio[i]
  fp1anggota=segitigadown(x, 10, 30)
  fp2anggota=0
  np1anggota="ideal"
  np2anggota=""
}

else if (rasio[i]>10 && rasio[i]<30){
  x=rasio[i]
  fp1anggota=segitigadown(x, 10, 30)
  fp2anggota=trapesium(x, 10, 30, 40, 60)
  np1anggota="ideal"
  np2anggota="cukup"
  
  
}
else if (rasio[i]>=30 && rasio[i]<=40){
  x=rasio[i]
  fp1anggota=trapesium(x, 10, 30, 40, 60)
  fh2angggota=0
  np1anggota="cukup"
  np2anggota=""
  
}
else if (rasio[i]>40 && rasio[i]<60){
  x=rasio[i]
  fp1anggota=trapesium(x, 10, 30, 40, 60)
  fp2anggota=segitigaup(x, 40, 60)
  np1anggota="cukup"
  np2anggota="tidak ideal"
  
}
else if (rasio[i]>=60){
  x=rasio[i]
  fp1anggota=segitigaup(x, 40, 60)
  fh2angggota=0
  np1anggota="tidak ideal"
  np2anggota=""
  
} 
  
    

#fuzzy rule
  if (np1anggota=="tidak ideal" && nh1anggota=="kecil"){
    nkr1=min(fp1anggota,fh1anggota)
    nkanggota="rendah"
  }
  else if (np1anggota=="tidak ideal" && nh2anggota=="kecil"){
    nkr1=min(fp1anggota,fh2anggota)
    nkanggota="rendah"
  }
  else if (np2anggota=="tidak ideal" && nh1anggota=="kecil"){
    nkr1=min(fp2anggota,fh1anggota)
    nkanggota="rendah"
  }
  else if (np2anggota=="tidak ideal" && nh2anggota=="kecil"){
    nkr1=min(fp2anggota,fh2anggota)
    nkanggota="rendah"
  }
  


  else if (np1anggota=="tidak ideal" && nh1anggota=="sedang"){
    nkt1=min(fp1anggota,fh1anggota)
    nkanggota="tinggi"
  }
  else if (np1anggota=="tidak ideal" && nh2anggota=="sedang"){
    nkt1=min(fp1anggota,fh2anggota)
    nkanggota="tinggi"
  }
  else if (np2anggota=="tidak ideal" && nh1anggota=="sedang"){
    nkt1=min(fp2anggota,fh1anggota)
    nkanggota="tinggi"
    
  }
  else if (np2anggota=="tidak ideal" && nh2anggota=="sedang"){
    nkt1=min(fp2anggota,fh2anggota)
    nkanggota="tinggi"
  }



  else if (np1anggota=="tidak ideal" && nh1anggota=="besar"){
    nkt2=min(fp1anggota,fh1anggota)
    nkanggota="tinggi"
  }
  else if (np1anggota=="tidak ideal" && nh2anggota=="besar"){
    nkt2=min(fp1anggota,fh2anggota)
    nkanggota="tinggi"
  }
  else if (np2anggota=="tidak ideal" && nh1anggota=="besar"){
    nkt2=min(fp2anggota,fh1anggota)
    nkanggota="tinggi"
  }
  else if (np2anggota=="tidak ideal" && nh2anggota=="besar"){
    nkt2=min(fp2anggota,fh2anggota)
    nkanggota="tinggi"
  }


  else if (np1anggota=="tidak ideal" && nh1anggota=="stbesar"){
    nkt3=min(fp1anggota,fh1anggota)
    nkanggota="tinggi"
  }
  else if (np1anggota=="tidak ideal" && nh2anggota=="stbesar"){
    nkt3=min(fp1anggota,fh2anggota)
    nkanggota="tinggi"
  }
  else if (np2anggota=="tidak ideal" && nh1anggota=="stbesar"){
    nkt3=min(fp2anggota,fh1anggota)
    nkanggota="tinggi"
  }
  else if (np2anggota=="tidak ideal" && nh2anggota=="stbesar"){
    nkt3=min(fp2anggota,fh2anggota)
    nkanggota="tinggi"
  }

#======================================================================
  if (np1anggota=="cukup" && nh1anggota=="kecil"){
    nkr2=min(fp1anggota,fh1anggota)
    nkanggota="rendah"
  }
  else if (np1anggota=="cukup" && nh2anggota=="kecil"){
    nkr2=min(fp1anggota,fh2anggota)
    nkanggota="rendah"
  }
  else if (np2anggota=="cukup" && nh1anggota=="kecil"){
    nkr2=min(fp2anggota,fh1anggota)
    nkanggota="rendah"
  }
  else if (np2anggota=="cukup" && nh2anggota=="kecil"){
    nkr2=min(fp2anggota,fh2anggota)
    nkanggota="rendah"
  }
  
  
  else if (np1anggota=="cukup" && nh1anggota=="sedang"){
    nkr3=min(fp1anggota,fh1anggota)
    nkanggota="rendah"
  }
  else if (np1anggota=="cukup" && nh2anggota=="sedang"){
    nkr3=min(fp1anggota,fh2anggota)
    nkanggota="rendah"
  }
  else if (np2anggota=="cukup" && nh1anggota=="sedang"){
    nkr3=min(fp2anggota,fh1anggota)
    nkanggota="rendah"
  }
  else if (np2anggota=="cukup" && nh2anggota=="sedang"){
    nkr3=min(fp2anggota,fh2anggota)
    nkanggota="rendah"
  }
  
  
  else if (np1anggota=="cukup" && nh1anggota=="besar"){
    nkt4=min(fp1anggota,fh1anggota)
    nkanggota="tinggi"
  }
  else if (np1anggota=="cukup" && nh2anggota=="besar"){
    nkt4=min(fp1anggota,fh2anggota)
    nkanggota="tinggi"
  }
  else if (np2anggota=="cukup" && nh1anggota=="besar"){
    nkt4=min(fp2anggota,fh1anggota)
    nkanggota="tinggi"
  }
  else if (np2anggota=="cukup" && nh2anggota=="besar"){
    nkt4=min(fp2anggota,fh2anggota)
    nkanggota="tinggi"
  }
  
  
  
  else if (np1anggota=="cukup" && nh1anggota=="stbesar"){
    nkt5=min(fp1anggota,fh1anggota)
    nkanggota="tinggi"
  }
  else if (np1anggota=="cukup" && nh2anggota=="stbesar"){
    nkt5=min(fp1anggota,fh2anggota)
    nkanggota="tinggi"
  }
  else if (np2anggota=="cukup" && nh1anggota=="stbesar"){
    nkt5=min(fp2anggota,fh1anggota)
    nkanggota="tinggi"
  }
  else if (np2anggota=="cukup" && nh2anggota=="stbesar"){
    nkt5=min(fp2anggota,fh2anggota)
    nkanggota="tinggi"
  }
  
  
  
  
#====================================================================
  if (np1anggota=="ideal" && nh1anggota=="kecil"){
    nkr4=min(fp1anggota,fh1anggota)
    nkanggota="rendah"
  }
  else if (np1anggota=="ideal" && nh2anggota=="kecil"){
    nkr4=min(fp1anggota,fh2anggota)
    nkanggota="rendah"
  }
  else if (np2anggota=="ideal" && nh1anggota=="kecil"){
    nkr4=min(fp2anggota,fh1anggota)
    nkanggota="rendah"
  }
  else if (np2anggota=="ideal" && nh2anggota=="kecil"){
    nkr4=min(fp2anggota,fh2anggota)
    nkanggota="rendah"
  }
  
  
  else if (np1anggota=="ideal" && nh1anggota=="sedang"){
    nkr5=min(fp1anggota,fh1anggota)
    nkanggota="rendah"
  }
  else if (np1anggota=="ideal" && nh2anggota=="sedang"){
    nkr5=min(fp1anggota,fh2anggota)
    nkanggota="rendah"
  }
  else if (np2anggota=="ideal" && nh1anggota=="sedang"){
    nkr5=min(fp2anggota,fh1anggota)
    nkanggota="rendah"
  }
  else if (np2anggota=="ideal" && nh2anggota=="sedang"){
    nkr5=min(fp2anggota,fh2anggota)
    nkanggota="rendah"
  }
  
  
  else if (np1anggota=="ideal" && nh1anggota=="besar"){
    nkt6=min(fp1anggota,fh1anggota)
    nkanggota="tinggi"
  }
  else if (np1anggota=="ideal" && nh2anggota=="besar"){
    nkt6=min(fp1anggota,fh2anggota)
    nkanggota="tinggi"
  }
  else if (np2anggota=="ideal" && nh1anggota=="besar"){
    nkt6=min(fp2anggota,fh1anggota)
    nkanggota="tinggi"
  }
  else if (np2anggota=="ideal" && nh2anggota=="besar"){
    nkt6=min(fp2anggota,fh2anggota)
    nkanggota="tinggi"
  }
  
  
  
  else if (np1anggota=="ideal" && nh1anggota=="stbesar"){
    nkt7=min(fp1anggota,fh1anggota)
    nkanggota="tinggi"
  }
  else if (np1anggota=="ideal" && nh2anggota=="stbesar"){
    nkt7=min(fp1anggota,fh2anggota)
    nkanggota="tinggi"
  }
  else if (np2anggota=="ideal" && nh1anggota=="stbesar"){
    nkt7=min(fp2anggota,fh1anggota)
    nkanggota="tinggi"
  }
  else if (np2anggota=="ideal" && nh2anggota=="stbesar"){
    nkt7=min(fp2anggota,fh2anggota)
    nkanggota="tinggi"
  }
  





#kesimpulan

  if (nkanggota=="tinggi"){
    nktt=max(nkt1,nkt2,nkt3,nkt4,nkt5,nkt6,nkt7)
  }
  else if (nkanggota=="rendah"){
    nkrr=max(nkr1,nkr2,nkr3,nkr4,nkr5)
  }


#y star

  ystar[i] =(nkrr*(390)+nktt*(595))/((nkrr*12)+(nktt*7))
  
  
  print(ystar[i])
  print("=========")

}



order(ystar)
plot(ystar)
sort(ystar)
datajadi=array()
datajadi=order(ystar)
blt=array()
k=1
l=100
for (k in 1:20) {
  blt[k]=datajadi[l]
  l=l-1
}
bltt<-data.frame(blt)
names(bltt)<-c("Nomor Keluarga")
  
  write.csv(bltt, file = "TebakanTugasAI.csv" )

