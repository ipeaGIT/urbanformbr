rm(list=ls())
library(ggplot2)
library(magrittr)
library(data.table)
library(tabulizer)




car <- tabulizer::extract_tables(file = "../../data-raw/frota_denatran/classificacao_veic_raw.pdf"
                                 ,pages = 1:60)




tmp_car <- lapply(X = 1:(60),FUN = function(i){ # i = 78
  if(i == 1){
    return(car[[i]][,1])
  }else{
    return(car[[i]][,2])
  }
}) %>% unlist() %>% as.vector() %>% unique()

tmp_car_p61 <- c( "155663 I/VOLVO S60 T4"
                  ,"244100 I/VOLVO XC90 T6 AWD"
                  ,"244128 I/VOLVO XC90 T6 INSCRIPT"
                  ,"244116 I/VOLVO XC60 T6 AWD"
                  ,"244125 I/VOLVO XC60 2.0 T5 KIN"
                  ,"244126 I/VOLVO XC90 T6 1EDITION"
                  , "244127 I/VOLVO XC90 T6 MOMENTUM"
                  , "244123 I/VOLVO XC60 2.0 T6 INS"
                  , "244122 I/VOLVO XC60 2.0 T5 MOM"
                  , "244121 I/VOLVO XC60 2.4 D5 MOM"
                  , "244117 I/VOLVO XC60 T5"
                  , "244131 I/VOLVO XC60 2.0 T5 INS"
                  , "244136 I/VOLVO XC60 D5 MOMENTUM"
                  , "244137 I/VOLVO XC60 D5 KINETIC"
                  , "244140 I/VOLVO XC90 T8 INSCRIPT"
                  , "244101 I/VOLVO XC90 2.5T AWD"
                  , "244213 I/VOLVO XC90 2.5T"
                  , "244124 I/VOLVO XC60 T6 R-DESIGN"
                  , "244115 I/VOLVO XC60 2.0T5 R-DES"
                  , "244114 I/VOLVO XC60 2.0T"
                  , "244113 I/VOLVO XC60 3.2L"
                  , "244112 I/VOLVO XC60 2.0 T5 COMF"
                  , "244111 I/VOLVO XC60 2.0 T5 DYNA"
                  , "244110 I/VOLVO XC60 T6"
                  , "244109 I/VOLVO XC60 3.0T R-DESI"
                  , "244108 I/VOLVO XC60 3.0T TOP"
                  , "244107 I/VOLVO XC60 3.0TCOMFORT"
                  , "244106 I/VOLVO XC60 3.0TDYNAMIC"
                  , "244105 I/VOLVO XC90"
                  , "244104 I/VOLVO XC60 3.0T AWD"
                  , "244146 I/VOLVO XC60 AWD"
                  , "ZNA"
                  , "210300 I/ZNA OTING SUV 4X4 STD"
                  , "ZXAUTO"
                  , "245804 I/ZXAUTO LANDMARK"
                  , "245803 I/ZXAUTO GRANDTIGER LXCD"
                  , "245805 I/ZXAUTO GRANDTIGER")


tmp_car <- gsub("(^[0-9]+ )", "", c(tmp_car,tmp_car_p61))



# tmp mptp ---------
moto <- tabulizer::extract_tables(file = "../../data-raw/frota_denatran/classificacao_veic_raw.pdf"
                                  ,pages = 88:119)



tmp_moto <- sapply(2:(length(87:118)-2),function(i){ # i = 78
  if(i == 2){
    return(moto[[i]][,1])
  }else{
    return(moto[[i]][,2])
  }
}) %>% unlist() %>% as.vector() %>% unique()

tmp_moto_p32 <- c("036618 I/LINGYU MOTORINO 50S"
                  ,"ADVENTURE"
                  ,"032500 ADVENTURE/TORNADO 1.8A01"
                  ,"AGRALE"
                  ,"000201 AGRALE/DAKAR 30.0"
                  ,"000401 AGRALE/AGRALE RXT 16.5"
                  ,"022903 AGRALE/SMC TCHAU 50"
                  ,"ALEX"
                  ,"037501 I/ALEX MOTOSTAR FIGHTER"
                  ,"037504 I/ALEX MOTOSTAR VIP"
                  ,"037502 I/ALEX MOTOSTAR FUEGO"
                  ,"037500 I/ALEX MOTOSTAR FIERA"
                  ,"037503 I/ALEX MOTOSTAR STRAIL"
                  ,"AME"
                  ,"000999 AME/CHOPPER"
                  ,"AMERICAN"
                  ,"005305 I/AMERICAN CHOPPER R T"
                  ,"005306 I/AMERICAN CHOPPER"
                  ,"APOLLO"
                  ,"033602 I/APOLLO ORION AGB31 WR1"
                  ,"033603 I/APOLLO ORION AGB30 AR1"
                  ,"033601 I/APOLLO ORION AGB30 WR2"
                  ,"APRILIA"
                  ,"017936 I/APRILIA RSV 4 FACTORY"
                  ,"017921 I/APRILIA RSV 1000 R"
                  ,"017903 IMP/APRILIA PEGASO 650"
                  ,"017923 I/APRILIA SCARABEO 200"
                  ,"017924 I/APRILIA MANA 850"
                  ,"017927 I/APRILIA SCARABEO 150"
                  ,"017928 I/APRILIA SCARABEO 300 S"
                  ,"017929 I/APRILIA RSV 4"
                  ,"017932 I/APRILIA RSV4R 1000CC"
                  ,"017922 I/APRILIA SCARABEO 500"
                  ,"017935 I/APRILIA SR MOTARD 125"
                  ,"ARD"
                  ,"039600 I/ARD EXIM TUCTUC VAN"
                  ,"ARIEL"
                  ,"001001 I/ARIEL"
                  ,"ASA"
                  ,"031900 I/ASA 150T2"
                  ,"ASIA"
                  ,"034203 ASIAMOTOS/RS 125"
                  ,"034204 ASIAMOTOS/CHALLENGER 250"
                  ,"ATALA"
                  ,"025301 ATALA/CALIFFONE PIU"
                  ,"025302 ATALA/MASTER LF.2M"
                  ,"025303 ATALA/HACKER"
                  ,"025304 ATALA/SKEGGIA"
                  ,"ATMAN"
                  ,"037401 ATMAN/FALCON CHOPPERS 01"
                  ,"037400 ATMAN/FALCON 01"
                  , "BAODIAO"
                  , "036901 I/BAODIAO BD 110-A"
                  , "036900 I/BAODIAO BD 125-11A"
                  , "BAOTIAN"
                  , "037704 BAOTIAN BT49QT 20C4"
                  , "037702 I/BAOTIAN ALANMOTORSCOLT"
                  , "037700 I/BAOTIAN BT125T 12"
                  , "037701 I/BAOTIAN BT49QT"
                  , "BASHAN"
                  , "035219 I/BASHAN BRAZIL CG 150"
                  , "035219 I/BASHAN BRAZIL CG 150"
                  , "035218 I/BASHAN HE48Q 2B"
                  , "035210 I/BASHAN KELT QUASAR"
                  , "035217 I/BASHAN BS 150ZH-C"
                  , "035216 I/BASHAN BS 150ZK-8"
                  ,"030203 I/ZHONGYU WAKE 110S"
                  ,"030202 I/ZHONGYU ZY 150 TA"
                  ,"ZONGSHEN"
                  ,"028512 I/ZONGSHEN ZS125-50"
                  ,"028500 I/ZONGSHEN ZS125-17"
                  ,"028501 I/ZONGSHEN ZS100-16"
                  ,"028502 I/ZONGSHEN ZS125-GY"
                  ,"028506 I/ZONGSHEN ZS125T 30"
                  , "028507 I/ZONGSHEN ZS250 GS"
                  , "028532 I/ZONGSHEN JUNIOR 50"
                  , "028511 I/ZONGSHEN WH150ZH-3A"
                  , "028531 I/ZONGSHEN REV 250 POPIN"
                  , "028513 I/ZONGSHEN ZS110-60"
                  , "028514 I/ZONGSHEN ZS150GY-10"
                  , "028515 I/ZONGSHEN ZS150-70"
                  , "028517 I/ZONGSHEN ZS150GY-10C"
                  , "028518 I/ZONGSHEN ZS150T-80 A"
                  , "028523 I/ZONGSHEN WIN 125"
                  , "028510 I/ZONGSHEN ZS125ZH YUMBO"
                  , "034100 I/ZONGSHEN PIAGGIO FLY" )

tmp_moto <- c(tmp_moto,tmp_moto_p32)

tmp_moto <- gsub("(^[0-9]+ )", "", tmp_moto)


export_list <- list(tmp_moto,tmp_car)
readr::write_rds(x = export_list,file = "../../data/urbanformbr/denatran/list_cars_motos.rds")


