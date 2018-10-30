###############################################################################
######### EXTRAER LOS METADATOS DE LAS ENCUESTAS DE GOBIERNO DE INEGI #########
###############################################################################
##################### AUTOR: JOSÉ SICABÍ CRUZ SALINAS #########################
###################### # FECHA: 12 DE SEPTIEMBRE DE 2018 ###################### 
############## DERECHOS: USO, REPRODUCCIÓN Y MODIFICACIÓN LIBRE ###############
# 1. Creamos la función con los argumentos que recibirá: a) la dirección URL
# donde se encuentra el archivo de metadatos y b) el nombre del archivo de 
# metadatos. Ambos como un vector de caracteres sin extensión predefinida.
metadatos_gobierno_INEGI <-
        function(url = character(), archivo = character()) {
                # 2.Cargamos los paquetes para procesar documentos en
                # formato de eXtensible Markup Language(XML).
                library(XML)
                library(xml2)
                # 3. Una vez que hayamos ubicado MANUALMENTE (es decir, en la 
                # pantalla del explorador de internet que usemos) la dirección 
                # URL del archivo DDI que contiene los metadata de la encuesta,
                # así como el nombre del archivo completo (con extensión), baja
                # mos su contenido a nuestro directorio de trabajo. Le pedimos a
                # la función CURL que muestre el progreso de la descarga.
                curl::curl_download(url, archivo, quiet = FALSE)
                # 3. Leemos el archivo XML y analizamos su estructura para 
                # ubicar los principales nodos que contienen información sobre
                # las variables y los archivos. Usamos la función CURL.
                doc <- xmlTreeParse(file = archivo, useInternalNodes = FALSE)
                nodeset <- xml_children(read_xml(archivo))
                # 4. Los metadatos de las encuestas que cumplen el formato DDI
                # están en el nodo "dataDscr". Pedimos que busque el número de  
                # nodo correspondiente a 'dataDscr' y lo asigne a una variable.
                data_node <- which(xml_name(nodeset) == "dataDscr")
                # 5. Calculamos el número total de variables de la base de datos
                # para poder especificar después el número de veces que se
                # repetirán las operaciones de extracción de datos.
                rootnode <- xmlRoot(doc)
                rootsize <- xmlSize(rootnode[[data_node]])
                # 6. Estraemos la información que hay en el nodo <fileDscr>. 
                # Para poder clasificar las variables de acuerdo a su origen,
                # extraemos información sobre la clave que identifica a los 
                # archivos (ID), su nombre (Name) y su descripción (fileCont),
                # el número de variables que hay por archivos, (varQnty), así
                # como el orden en el que fueron registrados los cuestionarios.
                # 6.1 Empezamos por extraer los nombres de los archivos.
                files_name <- xmlApply(rootnode, xmlAttrs, name = "ID")
                files_name <-
                        files_name[names(files_name) == "fileDscr"]
                files_name <- lapply(files_name, "[", -1)
                files_name <-
                        lapply(files_name, function(x)
                                sub(".*Name=", "", x))
                files_name <- unname(unlist(files_name))
                # 6.2 Extraemos el ID de los archivos
                files_id <- xmlApply(rootnode, xmlAttrs, name = "ID")
                files_id <-
                        files_id[names(files_id) == "fileDscr"]
                files_id <- lapply(files_id, "[", -2)
                files_id <-
                        lapply(files_id, function(x)
                                sub(".*Name=", "", x))
                files_id <- unname(unlist(files_id))
                # 6.3 Extraemos la descripción del contenido de los archivos.
                files_content <-
                        xmlChildren(rootnode)[names(rootnode) == "fileDscr"]
                files_content <- lapply(files_content, "[[", "fileTxt")
                files_content <- lapply(files_content, "[[", "fileCont")
                files_content <- trimws(unname(unlist(
                                        lapply(files_content,
                                                function(x)
                                                xmlSApply(x, xmlValue))
                                        )),"both")
                # 6.4 Extraemos la cantidad de variables que hay por archivo.
                files_variables_quantity <-
                        xmlChildren(rootnode)[names(rootnode) == "fileDscr"]
                files_variables_quantity <- lapply(files_variables_quantity,
                                                  "[[", "fileTxt")
                files_variables_quantity <- lapply(files_variables_quantity,
                                                  "[[", "dimensns")
                files_variables_quantity <- lapply(files_variables_quantity,
                                                  "[[", "varQnty")
                files_variables_quantity <- trimws(unname(unlist(
                        lapply(files_variables_quantity,
                               function(x)
                                       xmlSApply(x, xmlValue))
                )),"both")
                # 6.5 Extraemos el orden en el que fueron registrados los 
                # cuestionarios.
                files_order <- seq_along(files_id)
                # 6.6 Guardamos la descripción del contenido de los archivos en 
                # dos variables diferentes. Una va a contener la descripción y 
                # el ID y la otra va a contener la descripción y el nombre del
                # archivo. Esto facilitará más adelante la creación de la
                # variable de nombre de archivo de origen en nuestro 
                # data.frame final.
                files_content_id <- files_content
                files_content_name <- files_content
                names(files_content_id) <- files_id
                names(files_content_name) <- files_name
                # 7. Extraemos la información que hay en el nodo <dataDscr>.
                # Creamos la lista con los metadatos para poder extraer 
                # los metadatos sobre las preguntas y respuestas de la encuesta
                # en un formato diferente a XML.
                metadata <- xmlToList(rootnode[[data_node]])
                # 7.1 Creamos las variables que identifican a los metadatos
                # del identificador del archivo de origen en este nodo.
                variable_file_id <-
                        xmlApply(rootnode[[data_node]], xmlGetAttr, "files")
                variable_file_id <- unname(unlist(variable_file_id))
                # 7.2 Luego la del identificador de la variable.
                variable_id <- xmlApply(rootnode[[data_node]], xmlGetAttr, "ID")
                variable_id <- unname(unlist(variable_id))
                variable_id <- sub(variable_id, pattern = "V", replacement = "")
                # 7.3 La del nombre con el que es identificada la variable.
                variable_name <- xmlApply(rootnode[[data_node]], 
                                          xmlGetAttr, "name")
                variable_name <- unname(unlist(variable_name))
                # 7.4 La del contenido de la variable.
                variable_label <-
                        trimws(as.character(lapply(metadata, "[[", "labl")), 
                               "both")
                # 7.5 La de la especificación de la variable.
                variable_description <-
                        trimws(as.character(lapply(metadata, "[[", "txt")), 
                               "both")
                # 7.6 La de la pregunta realizada en el cuestionario.
                variable_question <- lapply(metadata, "[[", "qstn")
                variable_question <-
                        trimws(as.character(lapply(variable_question, 
                                                   "[[", "qstnLit")),
                               "both")
                # 7.7 La de la condición para hacer la pregunta.
                variable_question_condition <- lapply(metadata, "[[", "qstn")
                variable_question_condition <-
                        trimws(as.character(lapply(variable_question_condition,
                                                   "[[", 
                                                   "preQTxt")), 
                               "both")
                variable_question_condition[variable_question_condition == "NULL"] <- "Ninguna"
                # 7.8 La del rango de respuestas de la variable
                variable_answers_range <- character()
                for (i in 1:rootsize) {
                        if (is.null(rootnode[[data_node]][[i]][["valrng"]][["range"]])) {
                                variable_answers_range[[i]] <- "No especificado"
                        } else {
                                variable_answers_range[[i]] <-
                                        paste(as.character(unlist(
                                                xmlToList(rootnode[[data_node]][[i]][["valrng"]][["range"]])
                                        )),
                                        collapse = " a ")
                        }
                }
                variable_answers_range <- sub(variable_answers_range, 
                                      pattern =  "REAL a ", 
                                      replacement =  "De ")
                # 7.9 La del catálogo de respuestas de la variable. Tenemos que 
                # realizar un bucle de repetición anidado para poder acceder a
                # las categorías de respuestas porque el número de categorías 
                # cambia de una variable a otra. Sugerencias de cómo mejorar
                # esta parte del código son especialmente bienvenidas.
                variable_answers <-
                        lapply(metadata, function(x)
                                x[names(x) == "catgry"])
                answers_values <- list()
                for (i in 1:rootsize) {
                        tmp_label <- character()
                        if (length(variable_answers[[i]]) == 0)
                                answers_values[[i]] <- "No especificado"
                        else
                                for (j in 1:length(variable_answers[[i]])) {
                                        tmp_label <-
                                                c(tmp_label,
                                                  paste(
                                                          trimws(variable_answers[[i]][[j]][["catValu"]],
                                                                 which = "both"),
                                                          ":",
                                                          sep = ""
                                                  ))
                                        tmp_label <-
                                                c(tmp_label,
                                                  paste(
                                                          trimws(variable_answers[[i]][[j]][["labl"]],
                                                                 which = "both"),
                                                          ". ",
                                                          sep = ""
                                                  ))
                                        answers_values[[i]] <-
                                                paste(tmp_label, collapse = "")
                                }
                }
                answers_values <- as.character(answers_values)
                answers_values <- sub(answers_values, pattern = "Sysmiss.*", 
                                      replacement = "")
                variable_answers <- trimws(answers_values, which = "right")
                # 7.10 De la condición de acuerdo a la respuesta de la pregunta
                variable_answer_condition <- lapply(metadata, "[[", "qstn")
                variable_answer_condition <- trimws(as.character(lapply(variable_answer_condition,
                                                             "[[",
                                                             "postQTxt")),
                                         "both")
                variable_answer_condition[variable_answer_condition == "NULL"] <- "Ninguna"
                # 8. Construimos el data.frame que contendrá los metadatos.
                # 8.1 Primero, creamos un data.frame con las variables extraidas 
                # del nodo <dataDscr>
                codebook <- data.frame(
                        cbind(
                                variable_file_id,
                                variable_id,
                                variable_name,
                                variable_label,
                                variable_description,
                                variable_question,
                                variable_question_condition,
                                variable_answers_range,
                                variable_answers,
                                variable_answer_condition
                        )
                )
                # 8.2 Luego, creamos un data.frame con la variables extraidas de 
                #los nodos <fileDscr>.
                files_description <-
                        data.frame(
                                variable_file_id = names(files_content_id),
                                variable_file_order = files_order,
                                variable_file_name = names(files_content_name),
                                variables_per_file = files_variables_quantity,
                                variable_file_description = files_content,
                                row.names = NULL,
                                stringsAsFactors = FALSE
                        )
                # 8.3 Combinamos ambos data.frames a partir de la variable de iden
                # tificador de archivo de origen. Después, eliminamos la primera
                # variable para que no aparezca en nuestro data.frame final, 
                # porque es redundante con la variable de orden del archivo
                # de origen, aunque era necesaria para combinar los data.frames.
                codebook <-    merge(files_description, 
                               codebook, 
                               by = "variable_file_id"
                               )
                codebook[1] <- NULL
                # 8.4 Cambiamos los nombres de las columnas de nuestro 
                # data.frame para que refleje adecuadamente a qué se refire cada 
                # variable creada.
                colnames(codebook) <-
                        c(
                                "Número del archivo de origen",
                                "Nombre del archivo de origen",
                                "Número de variables por archivo",
                                "Descripción del archivo de origen",
                                "Identificador de la variable",
                                "Nombre de la variable",
                                "Contenido de la variable",
                                "Especificación de la variable",
                                "Pregunta realizada",
                                "Condición para la pregunta realizada",
                                "Rango de las respuestas",
                                "Catálogo de respuestas",
                                "Condición para la respuesta emitida"
                        )
                # 9. Exportamos el data.frame creado a diferentes formatos.
                # 9.1 Primero, exportamos el data.frame en formato de hoja de 
                # cálculo separado por comas a nuestro directorio de trabajo. 
                # El nombre será el mismo que el del archivo XML original con la
                # etiqueta '_diccionario' añaadida.
                write.csv(codebook, file = paste(sub("*.xml", "", archivo),
                                                    "_diccionario.csv",
                                                    sep = ""))
                # 9.2 Exportamos el data.frame al ambiente global de R para que
                # este sea el resultado final de la función.
                diccionario <<- codebook
        }