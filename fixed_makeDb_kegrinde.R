# get new sqldf functions
source('~/Documents/derfinder/R/fixed_read_csv_sql_kegrinde.R')

fixed_makeDb <- function (dbfile, textfile, tablename, sep = "\t", cutoff = 5) {
  cat(file = dbfile)
  column.names = names(read.table(textfile, sep = sep, nrows = 1, 
                                  header = TRUE))
  for (i in 2:length(column.names)) {
    if (i == 2) 
      where.statement = paste(gsub("\\.", "_", column.names[i]), 
                              ">", cutoff, "OR")
    else if (i == length(column.names)) 
      where.statement = paste(where.statement, gsub("\\.", 
                                                    "_", column.names[i]), ">", cutoff)
    else where.statement = paste(where.statement, gsub("\\.", 
                                                       "_", column.names[i]), ">", cutoff, "OR")
  }
  tablename.statement = paste("main", tablename, sep = ".")
  sql.statement = paste("create table", tablename.statement, 
                        "as select * from file where", where.statement)
  fixed_read.csv.sql(textfile, sql = sql.statement, dbname = dbfile, 
               sep = sep)
  print(paste("Wrote database file", dbfile, "containing table", 
              tablename))
}