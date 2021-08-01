Sys.setenv(JAVA_HOME='C:/Arquivos de Programas/Java/jre1.8.0_211/') # for 64-bit version
#Sys.setenv(JAVA_HOME='C:/Program Files (x86)/Java/jre1.8.0_171') # for 32-bit version
install.packages("rJava")
install.packages("DBI")
options(java.parameters = "-Xmx16G")#Aumenta o espaço de memória do java
install.packages("RJDBC")
library("rJava")#Conexão com Oracle (auxiliar)
library("DBI")#Conexão com Oracle (auxiliar)
library("RJDBC")#Conexão com Oracle (principal)
#---------------------------------------------------------------------------------------------
#Conectando com o banco de dados DBAIH (Oracle)
#driver <- JDBC("oracle.jdbc.OracleDriver",
#               classPath="C:/app/alexandre.fonseca/product/11.2.0/client_1/odbc/lib/ojdbc8.jar"," ")
driver <- JDBC("oracle.jdbc.OracleDriver",
               classPath="C:/Users/mikael.lemos/Downloads/OJDBC-Full/ojdbc7.jar"," ")
con <-dbConnect(driver,"jdbc:oracle:thin:@exadf-scan.saude.gov:1521/vinculasus.saude.gov",
                    "CPF15635015387","evaristo1")
dbGetQuery(con, "select * from DBSIA.TB_ESPELHO_APAC")

d <- dbReadTable(con, "iris")

----------------------------------------------------------------------------------------------
