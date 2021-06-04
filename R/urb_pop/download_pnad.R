my_getpnadc <- function(year,link_name, dataname,link_def, link_dici,
                        quarter = NULL, interview = NULL, topic = NULL,
                        vars = NULL, defyear = NULL, defperiod = NULL, labels = TRUE,
                        deflator = TRUE, design = TRUE, savedir = tempdir())
{
  if (is.null(quarter) & is.null(interview) & is.null(topic)) {
    stop("Quarter number or interview number or topic number must be provided.")
  }
  if ((!is.null(quarter) & !is.null(interview)) |
      (!is.null(quarter) & !is.null(topic)) |
      (!is.null(interview) & !is.null(topic)) |
      (!is.null(quarter) & !is.null(interview) & !is.null(topic))) {
    stop("Must be provided only one between quarter number, interview number and topic number.")
  }
  if (year < 2012) {
    stop("Year must be greater or equal to 2012.")
  }
  if (year > timeDate::getRmetricsOptions("currentYear")) {
    stop("Year cannot be greater than current year.")
  }
  if (!dir.exists(savedir)) {
    savedir <- tempdir()
    warning(paste0("The directory provided does not exist, so the directory was set to '",
                   tempdir()), "'.")
  }
  if (substr(savedir, nchar(savedir), nchar(savedir)) == "/" |
      substr(savedir, nchar(savedir), nchar(savedir)) == "\\") {
    savedir <- substr(savedir, 1, nchar(savedir) - 1)
  }
  if (!is.null(quarter)) {
    if (quarter < 1 | quarter > 4) {
      stop("Quarter number must be an integer from 1 to 4.")
    }
    ftpdir <- ("http://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/")
    ftpdata <- paste0(ftpdir, year, "/")
    #datayear <- unlist(strsplit(gsub("\r\n", "\n",
    #                                 RCurl::getURL(ftpdata,dirlistonly = TRUE)),
    #                            "\n"))
    #dataname <- datayear[which(startsWith(datayear, paste0("PNADC_0",
    #                                                       quarter, year)))]
    #if (length(dataname) == 0) {
    #  stop("Data unavailable for selected quarter and year.")
    #}
    # docfiles <- unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(paste0(ftpdir,
    #                                                                     "Documentacao/"),
    #                                                              dirlistonly = TRUE)), "\n"))
    # inputzip <- docfiles[which(startsWith(docfiles, "Dicionario_e_input"))]
    # defzip <- docfiles[which(startsWith(docfiles, "Deflatores"))]
    utils::download.file(url = link_dici,
                         destfile = paste0(savedir, "/Dicionario_e_input.zip"),
                         mode = "wb")
    utils::unzip(zipfile = paste0(savedir, "/Dicionario_e_input.zip"),
                 exdir = savedir)
    utils::download.file(url = link_name,
                         destfile = paste0(savedir, "/", dataname), mode = "wb")
    utils::unzip(zipfile = paste0(savedir, "/", dataname),
                 exdir = savedir)
    microdataname <- dir(savedir, pattern = paste0("^PNADC_0",
                                                   quarter, year, ".*\\.txt$"), ignore.case = FALSE)
    microdatafile <- paste0(savedir, "/", microdataname)
    microdatafile <- rownames(file.info(microdatafile)[order(file.info(microdatafile)$ctime),
    ])[length(microdatafile)]
    inputname <- dir(savedir, pattern = paste0("^input_PNADC_trimestral.*\\.txt$"),
                     ignore.case = FALSE)
    inputfile <- paste0(savedir, "/", inputname)
    inputfile <- rownames(file.info(inputfile)[order(file.info(inputfile)$ctime),
    ])[length(inputfile)]
    data_pnadc <- PNADcIBGE::read_pnadc(microdata = microdatafile,
                                        input_txt = inputfile, vars = vars)
    if (labels == TRUE) {
      if (exists("pnadc_labeller", where = "package:PNADcIBGE",
                 mode = "function")) {
        dicname <- dir(savedir, pattern = paste0("^dicionario_PNADC_microdados_trimestral.*\\.xls$"),
                       ignore.case = FALSE)
        dicfile <- paste0(savedir, "/", dicname)
        dicfile <- rownames(file.info(dicfile)[order(file.info(dicfile)$ctime),
        ])[length(dicfile)]
        data_pnadc <- PNADcIBGE::pnadc_labeller(data_pnadc = data_pnadc,
                                                dictionary.file = dicfile)
      }
      else {
        warning("Labeller function is unavailable in package PNADcIBGE.")
      }
    }
    if (deflator == TRUE) {
      if (exists("pnadc_deflator", where = "package:PNADcIBGE",
                 mode = "function")) {
        utils::download.file(url = link_def,
                             destfile = paste0(savedir, "/Deflatores.zip"),
                             mode = "wb")
        utils::unzip(zipfile = paste0(savedir, "/Deflatores.zip"),
                     exdir = savedir)
        defname <- dir(savedir, pattern = paste0("^deflator_PNADC_.*\\_trimestral_.*\\.xls$"),
                       ignore.case = FALSE)
        deffile <- paste0(savedir, "/", defname)
        deffile <- rownames(file.info(deffile)[order(file.info(deffile)$ctime),
        ])[length(deffile)]
        data_pnadc <- PNADcIBGE::pnadc_deflator(data_pnadc = data_pnadc,
                                                deflator.file = deffile)
      }
      else {
        warning("Deflator function is unavailable in package PNADcIBGE.")
      }
    }
  }
  if (!is.null(interview)) {
    if (interview < 1 | interview > 5) {
      stop("Interview number must be a integer from 1 to 5.")
    }
    ftpdir <- ("http://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/")
    ftpdata <- paste0(ftpdir, "Visita_", interview, "/Dados/")
    datayear <- unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(ftpdata,
                                                                 dirlistonly = TRUE)), "\n"))
    yrint_list <- regmatches(datayear, gregexpr("[[:digit:]]+",
                                                datayear))
    dataname <- NULL
    for (i in 1:length(datayear)) {
      if (as.numeric(yrint_list[[i]])[1] == year & as.numeric(yrint_list[[i]])[2] ==
          interview) {
        dataname <- datayear[i]
      }
    }
    if (length(dataname) == 0) {
      stop("Data unavailable for selected interview and year.")
    }
    docfiles <- unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(paste0(ftpdir,
                                                                        "Visita_", interview, "/Documentacao/"), dirlistonly = TRUE)),
                                "\n"))
    if (year < 2015) {
      inputpre <- docfiles[which(startsWith(docfiles,
                                            paste0("input_PNADC_2012_a_2014_visita", interview)))]
      dicpre <- docfiles[which(startsWith(docfiles, paste0("dicionario_PNADC_microdados_2012_a_2014_visita",
                                                           interview)))]
    }
    else {
      inputpre <- docfiles[which(startsWith(docfiles,
                                            paste0("input_PNADC_", year, "_visita", interview)))]
      dicpre <- docfiles[which(startsWith(docfiles, paste0("dicionario_PNADC_microdados_",
                                                           year, "_visita", interview)))]
    }
    utils::download.file(url = paste0(ftpdata, dataname),
                         destfile = paste0(savedir, "/", dataname), mode = "wb")
    utils::download.file(url = paste0(ftpdir, "Visita_",
                                      interview, "/Documentacao/", inputpre), destfile = paste0(savedir,
                                                                                                "/", inputpre), mode = "wb")
    utils::unzip(zipfile = paste0(savedir, "/", dataname),
                 exdir = savedir)
    microdataname <- dir(savedir, pattern = paste0("^PNADC_",
                                                   year, "_visita", interview, ".*\\.txt$"), ignore.case = FALSE)
    microdatafile <- paste0(savedir, "/", microdataname)
    microdatafile <- rownames(file.info(microdatafile)[order(file.info(microdatafile)$ctime),
    ])[length(microdatafile)]
    inputname <- dir(savedir, pattern = paste0("^input_PNADC_.*\\_visita",
                                               interview, ".*\\.txt$"), ignore.case = FALSE)
    inputfile <- paste0(savedir, "/", inputname)
    inputfile <- rownames(file.info(inputfile)[order(file.info(inputfile)$ctime),
    ])[length(inputfile)]
    data_pnadc <- PNADcIBGE::read_pnadc(microdata = microdatafile,
                                        input_txt = inputfile, vars = vars)
    if (labels == TRUE) {
      if (exists("pnadc_labeller", where = "package:PNADcIBGE",
                 mode = "function")) {
        utils::download.file(url = paste0(ftpdir, "Visita_",
                                          interview, "/Documentacao/", dicpre), destfile = paste0(savedir,
                                                                                                  "/", dicpre), mode = "wb")
        dicname <- dir(savedir, pattern = paste0("^dicionario_PNADC_microdados_.*\\_visita",
                                                 interview, ".*\\.xls$"), ignore.case = FALSE)
        dicfile <- paste0(savedir, "/", dicname)
        dicfile <- rownames(file.info(dicfile)[order(file.info(dicfile)$ctime),
        ])[length(dicfile)]
        data_pnadc <- PNADcIBGE::pnadc_labeller(data_pnadc = data_pnadc,
                                                dictionary.file = dicfile)
      }
      else {
        warning("Labeller function is unavailable in package PNADcIBGE.")
      }
    }
    if (deflator == TRUE) {
      if (exists("pnadc_deflator", where = "package:PNADcIBGE",
                 mode = "function")) {
        arcfiles <- unlist(strsplit(gsub("\r\n", "\n",
                                         RCurl::getURL(paste0(ftpdir, "Documentacao_Geral/"),
                                                       dirlistonly = TRUE)), "\n"))
        if (is.null(defyear)) {
          defyear <- timeDate::getRmetricsOptions("currentYear") -
            1
          warning(paste0("Deflator year was not provided, so deflator year was set to ",
                         defyear, "."))
        }
        if (defyear < year) {
          defyear <- year
          warning(paste0("Deflator year must be greater or equal to microdata year, so deflator year was changed to ",
                         defyear, "."))
        }
        if (defyear < 2017 | defyear >= timeDate::getRmetricsOptions("currentYear")) {
          defyear <- timeDate::getRmetricsOptions("currentYear") -
            1
          warning(paste0("Deflator year must be greater or equal to 2017 and cannot be greater or equal than current year, so deflator year was changed to ",
                         defyear, "."))
        }
        if (identical(arcfiles[which(startsWith(arcfiles,
                                                paste0("deflator_PNADC_", defyear)))], character(0))) {
          defyear <- defyear - 1
          warning(paste0("Deflator data unavailable for selected year, so deflator year was changed to ",
                         defyear, "."))
        }
        defpre <- arcfiles[which(startsWith(arcfiles,
                                            paste0("deflator_PNADC_", defyear)))]
        utils::download.file(url = paste0(ftpdir, "Documentacao_Geral/",
                                          defpre), destfile = paste0(savedir, "/", defpre),
                             mode = "wb")
        defname <- dir(savedir, pattern = paste0("^deflator_PNADC_",
                                                 defyear, ".*\\.xls$"), ignore.case = FALSE)
        deffile <- paste0(savedir, "/", defname)
        deffile <- rownames(file.info(deffile)[order(file.info(deffile)$ctime),
        ])[length(deffile)]
        data_pnadc <- PNADcIBGE::pnadc_deflator(data_pnadc = data_pnadc,
                                                deflator.file = deffile)
      }
      else {
        warning("Deflator function is unavailable in package PNADcIBGE.")
      }
    }
  }
  if (!is.null(topic)) {
    if (topic < 1 | topic > 4) {
      stop("Topic number must be a integer from 1 to 4.")
    }
    ftpdir <- ("http://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Trimestre/")
    ftpdata <- paste0(ftpdir, "Trimestre_", topic, "/Dados/")
    datayear <- unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(ftpdata,
                                                                 dirlistonly = TRUE)), "\n"))
    yrint_list <- regmatches(datayear, gregexpr("[[:digit:]]+",
                                                datayear))
    dataname <- NULL
    for (i in 1:length(datayear)) {
      if (as.numeric(yrint_list[[i]])[1] == year & as.numeric(yrint_list[[i]])[2] ==
          topic) {
        dataname <- datayear[i]
      }
    }
    if (length(dataname) == 0) {
      stop("Data unavailable for selected topic and year.")
    }
    docfiles <- unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(paste0(ftpdir,
                                                                        "Trimestre_", topic, "/Documentacao/"), dirlistonly = TRUE)),
                                "\n"))
    inputpre <- docfiles[which(startsWith(docfiles, paste0("input_PNADC_trimestre",
                                                           topic)))]
    dicpre <- docfiles[which(startsWith(docfiles, paste0("dicionario_PNADC_microdados_trimestre",
                                                         topic)))]
    utils::download.file(url = paste0(ftpdata, dataname),
                         destfile = paste0(savedir, "/", dataname), mode = "wb")
    utils::download.file(url = paste0(ftpdir, "Trimestre_",
                                      topic, "/Documentacao/", inputpre), destfile = paste0(savedir,
                                                                                            "/", inputpre), mode = "wb")
    utils::unzip(zipfile = paste0(savedir, "/", dataname),
                 exdir = savedir)
    microdataname <- dir(savedir, pattern = paste0("^PNADC_",
                                                   year, "_trimestre", topic, ".*\\.txt$"), ignore.case = FALSE)
    microdatafile <- paste0(savedir, "/", microdataname)
    microdatafile <- rownames(file.info(microdatafile)[order(file.info(microdatafile)$ctime),
    ])[length(microdatafile)]
    inputname <- dir(savedir, pattern = paste0("^input_PNADC_trimestre",
                                               topic, ".*\\.txt$"), ignore.case = FALSE)
    inputfile <- paste0(savedir, "/", inputname)
    inputfile <- rownames(file.info(inputfile)[order(file.info(inputfile)$ctime),
    ])[length(inputfile)]
    data_pnadc <- PNADcIBGE::read_pnadc(microdata = microdatafile,
                                        input_txt = inputfile, vars = vars)
    if (labels == TRUE) {
      if (exists("pnadc_labeller", where = "package:PNADcIBGE",
                 mode = "function")) {
        utils::download.file(url = paste0(ftpdir, "Trimestre_",
                                          topic, "/Documentacao/", dicpre), destfile = paste0(savedir,
                                                                                              "/", dicpre), mode = "wb")
        dicname <- dir(savedir, pattern = paste0("^dicionario_PNADC_microdados_trimestre",
                                                 topic, ".*\\.xls$"), ignore.case = FALSE)
        dicfile <- paste0(savedir, "/", dicname)
        dicfile <- rownames(file.info(dicfile)[order(file.info(dicfile)$ctime),
        ])[length(dicfile)]
        data_pnadc <- PNADcIBGE::pnadc_labeller(data_pnadc = data_pnadc,
                                                dictionary.file = dicfile)
      }
      else {
        warning("Labeller function is unavailable in package PNADcIBGE.")
      }
    }
    if (deflator == TRUE) {
      if (exists("pnadc_deflator", where = "package:PNADcIBGE",
                 mode = "function")) {
        arcfiles <- unlist(strsplit(gsub("\r\n", "\n",
                                         RCurl::getURL(paste0(ftpdir, "Documentacao_Geral/"),
                                                       dirlistonly = TRUE)), "\n"))
        if (is.null(defyear) | is.null(defperiod)) {
          defyear <- year
          defperiod <- topic
          warning(paste0("Deflator year or period was not provided, so deflator year was set to ",
                         defyear, " and period was set to ", defperiod,
                         "."))
        }
        if (defyear < year) {
          defyear <- year
          warning(paste0("Deflator year must be greater or equal to microdata year, so deflator year was changed to ",
                         defyear, "."))
        }
        if (defyear == 2016) {
          defyear <- 2017
          warning(paste0("There is no Deflator data for 2016, so deflator year was changed to ",
                         defyear, "."))
        }
        if (defyear < 2017 | defyear > timeDate::getRmetricsOptions("currentYear")) {
          defyear <- year
          warning(paste0("Deflator year must be greater or equal to 2017 and cannot be greater than current year, so deflator year was changed to ",
                         defyear, "."))
        }
        if (defyear == year & defperiod < topic) {
          defperiod <- topic
          warning(paste0("For ", defyear, ", deflator period must be greater or equal to microdata topic, so deflator period was changed to ",
                         defperiod, "."))
        }
        if (defperiod < 1 | defperiod > 4) {
          defperiod <- topic
          warning(paste0("Deflator period must be greater or equal to 1 and cannot be greater than 4, so deflator period was changed to ",
                         defperiod, "."))
        }
        perfiles <- unlist(strsplit(gsub("\r\n", "\n",
                                         RCurl::getURL(paste0(ftpdir, "Trimestre_",
                                                              defperiod, "/Documentacao/"), dirlistonly = TRUE)),
                                    "\n"))
        if (identical(perfiles[which(startsWith(perfiles,
                                                paste0("deflator_PNADC_", defyear, "_trimestre",
                                                       defperiod)))], character(0))) {
          defyear <- year
          defperiod <- topic
          warning(paste0("Deflator data unavailable for selected year and period, so deflator year was changed to ",
                         defyear, " and period was changed to ",
                         defperiod, "."))
        }
        defpre <- perfiles[which(startsWith(perfiles,
                                            paste0("deflator_PNADC_", defyear, "_trimestre",
                                                   defperiod)))]
        utils::download.file(url = paste0(ftpdir, "Trimestre_",
                                          defperiod, "/Documentacao/", defpre), destfile = paste0(savedir,
                                                                                                  "/", defpre), mode = "wb")
        defname <- dir(savedir, pattern = paste0("^deflator_PNADC_",
                                                 defyear, "_trimestre", defperiod, ".*\\.xls$"),
                       ignore.case = FALSE)
        deffile <- paste0(savedir, "/", defname)
        deffile <- rownames(file.info(deffile)[order(file.info(deffile)$ctime),
        ])[length(deffile)]
        data_pnadc <- PNADcIBGE::pnadc_deflator(data_pnadc = data_pnadc,
                                                deflator.file = deffile)
      }
      else {
        warning("Deflator function is unavailable in package PNADcIBGE.")
      }
    }
  }
  if (design == TRUE) {
    if (exists("pnadc_design", where = "package:PNADcIBGE",
               mode = "function")) {
      data_pnadc <- PNADcIBGE::pnadc_design(data_pnadc = data_pnadc)
    }
    else {
      warning("Sample design function is unavailable in package PNADcIBGE.")
    }
  }
  return(data_pnadc)
}
