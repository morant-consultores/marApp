#' pool
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

pool <- pool::dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = "db_gp",
  host = 'mysql.cduzrgqgkfht.us-east-2.rds.amazonaws.com',
  username = 'root',
  password = '9Blw33caY',
  port = 3306
)

shiny::onStop(function() {
  pool::poolClose(pool)
})

tbl_usuarios <- "mar_usuarios"
tbl_actores <- "mar_actores"
tbl_combinaciones <- "mar_combinaciones"
tbl_comparaciones <- "mar_comparaciones"
