#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny bs4Dash shinyWidgets
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      preloader = list(html = tagList(waiter::spin_fill(), p("Cargando...")), color = "#3c8dbc"),
      header = dashboardHeader(
        title = dashboardBrand(
          title = "Mapa de actores relevantes",
          color = "primary",
          href = NULL,
          image = NULL#"https://adminlte.io/themes/v3/dist/img/AdminLTELogo.png"
        )
      ),
      sidebar = dashboardSidebar(
        sidebarMenu(
          menuItem("Comparar actores",
                   tabName = "comparacion",
                   icon = icon("user-group")
          ),
          menuItem("Registrar actor",
                   tabName = "registrar",
                   icon = icon("plus")
          ),
          menuItem("Mapa de actores relevantes",
                   tabName = "mapa",
                   icon = icon("circle-nodes")
          )
        )
      ),
      body = dashboardBody(
        shinyjs::useShinyjs(),
        tabItems(
          tabItem(tabName = "comparacion",
            mod_comparacion_ui("comparacion_1")
          ),
          tabItem(tabName = "registrar",
                  mod_registrar_ui("registrar_1")
          ),
          tabItem(tabName = "mapa",
                  mod_mar_ui("mar_1")
          )
        )
      ),
      controlbar = dashboardControlbar(),
      title = "DashboardPage"
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "marApp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
