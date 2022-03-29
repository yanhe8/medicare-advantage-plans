library(rsconnect)
# rsconnect::configureApp("app", size="xlarge")
rsconnect::setAccountInfo(name='yanheupenn',
                          token='C31C823B0CB197134F1228D8E69507E4',
                          secret='2qlbFM/NQkJ2zDYcWUcG2xb87/0JosNXqG/OyNDn')
deployApp(account = "yanheupenn", appName = 'MA_plans')
rsconnect::deployApp()

# library(profvis)
# profvis(runApp())