# install.packages("leaflet")
library(leaflet)


total_beds <- data.frame(
    name = c("Lea Pessin","Amalia Ramirez Atiles", "Miguel Elias Purcell", "Zach Darr",
             "My Lan Do Nguyen", "Pierre Carbuccia", "Steffi Dipp", "Paola de Casas",
             "Dorian Castillo", "Joao Paulo", "Stephen Gaffney", "Luis Urraca", "David Gaffney",
             "Luis Alfonso", "Javier Cimentada", "Lauren Burrows", "Queralt Capsada", "Simon Venema",
             "Bram Hogendoorn", "Michiel Van Rijn", "Ansgar Hudde", "Roman Guerrero Goldberg",
             "Eduardo Sorribas",
             "Ava Hirori", "Reed Curtis", "Christine Salmen", "Sara Rutigliano", "Carlos Gil",
             "Roberta Rutigliano", "Gaby Kourkov", "Tia Mary", "Irene - Michi", "Paola de Casas",
             "Ava Hirori","Carina de Mondesert","Yana Tasueva", "Roberta Rutigliano"," My Lan Do Nguyen",
             "Joao Paulo", "Daniel de los Santos", "Joana S", "Nerea Hernaiz", "Michael Todemann",
             "Claudia Soñe"),
    
    email = c("lpessin@psu.edu", "amalia.ramirez.atiles@gmail.com", "michaeleliaspurcell@gmail.com",
              "Facebook", "mylando.nguyen@upf.edu", "pierrecarbuccia@gmail.com", "Facebook o Twitter",
              "decasasp@gmail.com", "doriarito@gmail.com", "joaotcf@gmail.com", "stephenjgaffney@gmail.com",
              "Facebook", "Facebook", "Facebook", "jcimentada@gmail.com", "laurenburrows@hotmail.co.uk",
              "queralt.capsada@upf.edu o Facebook", "sdvenema@gmail.com", "bramhog@gmail.com", "Facebook",
              "ansgar.hudde@gmx.de", "Facebook", "eduardo@sorribas.org", "avahirori@gmail.com",
              "reed.curtis@edu.su.se", "christine.salmen@univie.ac.at", "Facebook","carjgil@gmail.com",
              "roberta.rutigliano@upf.edu", "gabriellakourkov@gmail.com", "Facebook","Facebook",
              "decasasp@gmail.com", "avahirori@gmail.com", "Facebook", "yana.tasueva@gmail.com",
              "roberta.rutigliano@upf.edu", "mylando.nguyen@upf.edu", "joaotcf@gmail.com",
              "Facebook","joanarfsousa@gmail.com", "Nerea.Hernaiz@uv.es","michael.todemann@gmail.com",
              "Facebook"),
    
    lat = c(40.808207, 34.053182, 25.762211, 40.710770, 33.769698, 45.583985, 50.038682, 18.465593,
            18.578914, -22.909907, 53.270435, 53.349658, 51.506767, 51.511467, 53.480602, 50.822235,
            55.863725, 53.220246, 52.370189, 51.560810, 49.898904, 52.637971, 55.677055, 59.329175,
            59.362967, 48.208448, 41.902767, 43.769463, 41.099952, 47.498759, 43.363183,40.472440,
            41.401931, 41.402531, 41.382655, 41.375410, 41.403221, 41.381524,41.380912, 41.377637,
            41.561432, 43.361935, 50.936828, -33.870210),
    
    lon = c(-77.860975, -118.268945, -80.197360, -74.008224, -118.194564, -122.660151, -123.146060,
            -66.108126, -68.410209, -43.203210, -9.049786, -6.255323, -0.137997,-0.090276, -2.243843,
            -0.138977, -4.256843, 6.568527, 4.896553, 5.089843, 10.904419, 13.348963, 12.568734,
            18.066762, 18.058678, 16.371744, 12.495693, 11.256501, 16.830697, 19.049272, -5.849516,
            -3.733833, 2.150568, 2.169536, 2.147849, 2.134256, 2.153876, 2.191577, 2.177326, 2.152749,
            -8.396370, -8.411139, 6.954956, 151.209005)
)

total_beds$text <-paste(sep="<br/>",total_beds$name,total_beds$email)

m <- leaflet(total_beds) %>% addTiles() %>% addCircleMarkers(radius=6,
                                                             fillOpacity = 0.5,
                                                             color="green",
                                                             popup=~text) %>%
    addProviderTiles("Thunderforest.Pioneer") %>%
    setView(lat=34.851349, lng=-11.643972, zoom = 2)
