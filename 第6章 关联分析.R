install.packages("arules")
library ( arules )  

data("Groceries")                                
summary(Groceries)                                  
inspect(Groceries[1:10])                        

rules0=apriori(Groceries,parameter=list(support=0.001,confidence=0.5))
rules0                                           
inspect(rules0[1:10])                         

rules1=apriori(Groceries,parameter=list(support=0.005,confidence=0.5))
rules1             
rules2=apriori(Groceries,parameter=list(support=0.005,confidence=0.6))
rules2 
rules3=apriori(Groceries,parameter=list(support=0.005,confidence=0.64))
rules3
inspect(rules3)
                          
rules.sorted_sup = sort ( rules0, by="support" )   
inspect ( rules.sorted_sup [1:5] )                               
rules.sorted_con = sort ( rules0, by="confidence" )   
inspect ( rules.sorted_con [1:5] )    
rules.sorted_lift = sort ( rules0, by="lift" )   
inspect ( rules.sorted_lift [1:5] ) 

rules4=apriori(Groceries,parameter=list(maxlen=2,supp=0.001,conf=0.1),appearance=list(rhs="mustard",default="lhs"))
inspect ( rules4 )  
                            
itemsets_apr = apriori ( Groceries, parameter = list (supp=0.001,target = "frequent itemsets"),control=list(sort=-1)) 
itemsets_apr                                         
inspect(itemsets_apr[1:5])              
itemsets_ecl = eclat( Groceries, parameter = list ( minlen=1, maxlen=3,supp=0.001, target = "frequent itemsets"),control=list(sort=-1)) 
itemsets_ecl 
inspect(itemsets_ecl[1:5])

library ( arulesViz );library ( MASS );library ( scatterplot3d );library ( vcd );library ( grid )
library ( colorspace );library ( seriation );library ( cluster );library ( TSP );library ( gclus )

rules5 = apriori ( Groceries, parameter = list ( support=0.002, confidence=0.5 ) )       
rules5    
                                           
plot(rules5)                                      
plot(rules5,method="grouped") 
plot(rules5,interactive=TRUE)               
plot(rules5,shading="order", control=list(main = "Two©\key plot"))
plot(rules5[1:50], method="matrix", measure="lift")
plot(rules5[1:50], method="matrix3D", measure="lift")
plot(rules5[1:50], method="paracoord")




















