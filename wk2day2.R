library('dplyr')
library('grid')
library('ggplot2')
library('Rmisc')
##Regularization

set.seed(1); j=50; a=0.25
x=rnorm(j)
error = sqrt(1-a^2)*rnorm(j)
y = a*x + error

summary(lm(y~x-1))

cost =function(x,y,aEst,lambda,p){
  return(
    (mean((y-aEst*x)^2))^0.5+lambda*sum((abs(aEst))^p)
  )
}

reg_df = 
  expand.grid(
    lambda=sapply(1:10,function(x){
      2^(x-9)
    }),
    a= sapply(1:401,function(x){
      -0.1+0.001*(x-1)
    })
  )

## add two columns
reg_df[3] = costL1=0
reg_df[4] = costL2=0

##for loop to fill these columns
for (i in seq_len(nrow(reg_df))){
  reg_df[i,3]=cost(x,y,reg_df[i,2],reg_df[i,1],1)
}
for (i in seq_len(nrow(reg_df))){
  reg_df[i,4]=cost(x,y,reg_df[i,2],reg_df[i,1],2)
}


##making plots
graph_list_1=
  lapply(
    unique(reg_df[1]),
    function(w){
      df_temp = dplyr::filter(reg_df, lambda==w)
      return(qplot(x=df_temp[2],y=df_temp[3]))
    }
)

graph_list_2=
  lapply(
    unique(reg_df[1]),
    function(w){
      df_temp = dplyr::filter(reg_df, lambda==w)
      return(ggplot(data=df_temp) + geom_point(aes(x=df_temp[2],y=df_temp[4], color=sample(1:300,1))))
    }
  )

multiplot(plotlist=graph_list_1,cols=2)
qplot(graph_list_1)
