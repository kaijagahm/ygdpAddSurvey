# Define as global variables the names of data columns
data(uaGeom)
data(uaCols)
data(nsw)
data(carver)
data(anae)
data(counties)

utils::globalVariables(names(uaGeom))
utils::globalVariables(names(uaCols))
utils::globalVariables(names(nsw))
utils::globalVariables(names(carver))
utils::globalVariables(names(anae))
utils::globalVariables(names(counties))
