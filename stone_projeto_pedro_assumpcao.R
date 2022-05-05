library(pacman)
p_load(data.table)
p_load(lubridate)
p_load(stringi)
p_load(ggplot2)
p_load(plotly)
p_load(caret)
p_load(ggrepel)
p_load(DataExplorer)
p_load(GGally)
p_load(stringr)



caminho_arquivos = 'Downloads\\STONE_CHALLENGE\\dados'



portfolio_clientes = fread(paste0(caminho_arquivos,'\\portfolio_clientes.csv'), encoding = "UTF-8")
portfolio_comunicados = fread(paste0(caminho_arquivos,'\\portfolio_comunicados.csv'), encoding = "UTF-8")
portfolio_geral = fread(paste0(caminho_arquivos,'\\portfolio_geral.csv'), encoding = "UTF-8")
portfolio_tpv = fread(paste0(caminho_arquivos,'\\portfolio_tpv.csv'), encoding = "UTF-8")






#limpeza das variaveis categoricas


get_char_cols = function(DT){
  num_cols = DT[, sapply(.SD, is.character)]
  return(names(DT)[num_cols])
}



clean_char = function(DT){
  char_cols = get_char_cols(DT)
  DT[, (char_cols):=lapply(.SD, stri_trans_general,id = "Latin-ASCII"), .SDcols = char_cols]
  
}


clean_char(portfolio_clientes)


#ordenando os datasets


portfolio_geral = portfolio_geral[order(nr_documento,contrato_id,dt_ref_portfolio)]
portfolio_comunicados = portfolio_comunicados[order(contrato_id,data_acao)]
portfolio_tpv = portfolio_tpv[order(nr_documento,dt_transacao)]
portfolio_clientes = portfolio_clientes[order(nr_documento)]



####ANALISE DAS VARIAVEIS####

#tab clientes

str(portfolio_clientes)
summary(portfolio_clientes)

#tab geral

str(portfolio_geral)
summary(portfolio_geral)
#converte flag para categorico
# portfolio_geral[,flag_transacao:=as.factor(flag_transacao)]
portfolio_geral[,safra:=lubridate::ym(safra)]

#tab tpv

str(portfolio_tpv)
summary(portfolio_tpv)
portfolio_tpv[,dt_transacao:=lubridate::ymd(dt_transacao)]
#ha valores negaticos inconsistentes



#tab clientes

str(portfolio_clientes)
summary(portfolio_clientes)




####VERIFICAO DE VALORES MISSING####



portfolio_clientes[, lapply(.SD, function(x) sum(is.na(x)))]
portfolio_geral[, lapply(.SD, function(x) sum(is.na(x)))]
portfolio_tpv[, lapply(.SD, function(x) sum(is.na(x)))]
portfolio_clientes[, lapply(.SD, function(x) sum(is.na(x)))]


#VERIFICACAO DE VALORES DISTINTOS 



unique_count = function(DT) {
  
  
  classe_DT=DT[, lapply(.SD, class)]
  setDT(classe_DT)
  
  
  if (nrow(classe_DT)>1) {  classe_DT = classe_DT[1]}
  
  distinto_DT = DT[,lapply(.SD,uniqueN)]
  
  classe_distinto_DT = rbindlist(list(classe_DT,distinto_DT))
  # rm(classe_DT,distinto_DT)
  
  classe_distinto_DT = data.table(classe_distinto_DT, id=c(1,2))
  classe_distinto_DT =  melt(classe_distinto_DT, id.vars = 'id')
  
  
  classe_distinto_DT = dcast(classe_distinto_DT, variable ~ id, value.var = 'value')
  setnames(classe_distinto_DT, c('variavel','tipo','unico'))
  classe_distinto_DT[,unico:=as.numeric(unico)]
  classe_distinto_DT = classe_distinto_DT[order(-unico)]
  print(classe_distinto_DT)
  
}


unique_count(portfolio_clientes)
unique_count(portfolio_geral)
unique_count(portfolio_tpv)
unique_count(portfolio_comunicados)





#nao temos valores misssing em nenhuma tabela


####JUNCAO DAS TABELAS####


#verificao das chaves primeiro join
str(portfolio_geral$nr_documento)
length(unique(portfolio_geral[,nr_documento]))

str(portfolio_clientes$nr_documento)
length(unique(portfolio_clientes[,nr_documento]))

#execucao primeiro join
# portfolio_total = merge(x = portfolio_geral, y = portfolio_clientes, by.x = 'nr_documento', by.y  = 'nr_documento' )






####ANALISE EXPLORATORIA PARA VARIAVEIS NUMERICAS #####

get_num_cols = function(DT){
  num_cols = DT[, sapply(.SD, is.numeric)]
  return(names(DT)[num_cols])
}


portfolio_geral[,var_created_contratos_por_cliente:=uniqueN(contrato_id), by = nr_documento]


portfolio_geral[,var_created_situacao_inadimplencia:=fcase(
  
  dspp == 0 & dsp == 0, "ok",
  dspp > 0 & dsp  > 0, "inadimplente_inativo",
  dspp > 0 & dsp  == 0, "inadimplente_ativo",
  default = "erro"
  
)]

contratos_com_problema_dsp = unique(portfolio_geral[var_created_situacao_inadimplencia=='erro',contrato_id])


#calculo das reguas padroes

portfolio_geral[,regua_dsp:=fcase(
  
  dsp >=5 & dsp <10, 1,
  dsp >= 10 & dsp <15, 2,
  dsp >= 15 & dsp <30, 3,
  dsp >= 30 & dsp < 60, 4,
  dsp >= 60 & dsp < 90, 5,
  dsp >= 90, 5,
  default = 0
  
  
)]

portfolio_geral[,regua_dspp:=fcase(
  
  dspp >=15 & dspp <30, 1,
  dspp >=30 & dspp <45, 2,
  dspp >= 45, 3,
  default = 0
  
  
)]


#Transforacao para o formato wide
comunicados_wide = dcast(portfolio_comunicados,formula = contrato_id + dt_ref_portfolio + data_acao + acao ~ tipo_acao, value.var = 'status')



comunicados_wide[,EMAIL_EFETIVO:=fcase(
  
  EMAIL=='LIDO', 'OK',
  EMAIL=='RESPONDIDO','OK',
  EMAIL=='NAO ENTREGUE','FALHA',
  default = 'NAO_CONCLUSIVO'
  
  
)]


comunicados_wide[,HSM_EFETIVO:=fcase(
  
  HSM=='LIDO', 'OK',
  HSM=='RESPONDIDO','OK',
  HSM=='NAO ENTREGUE','FALHA',
  default = 'NAO_CONCLUSIVO'
  
  
)]


comunicados_wide = comunicados_wide[!(EMAIL=='NAO ENTREGUE' & HSM == 'NAO ENTREGUE')]


#limpa variaveis redundantes da tab comunicacao
comunicados_wide[,EMAIL:=NULL]
comunicados_wide[,HSM:=NULL]


#vetor de contratos questionados
contratos_questionados = unique(comunicados_wide[,contrato_id])

#FILTRO DE CLIENTES INADIMPLENTES QUE FORAM CONTACTADOS

portifolio_analisado = copy(portfolio_geral[contrato_id %in% contratos_questionados])



target_intermed = merge(portifolio_analisado[,.(contrato_id,dt_ref_portfolio,dsp,dspp)],comunicados_wide[,.(contrato_id,dt_ref_portfolio,data_acao,acao)], by = c('contrato_id','dt_ref_portfolio'), all.x = T )
target_intermed[, contatos_por_dia:=.N, by=.(contrato_id,dt_ref_portfolio)]


target_intermed[,codificao_acao_comunicado:=fcase(
  
  dsp==5  & acao == 'campanhaobservacao',1,
  dsp==10 & acao ==  'campanhaparcelamento',2,
  dsp==15 & acao == 'campanhaboletoquitado',3,
  dsp==30 & acao == 'campanhaprenegativacao',4,
  dsp==60 & acao ==  'campanhanegativacao',5,
  dsp==90 & acao == 'campanhaboletoquitado',6,
  dspp==15 & acao == 'campanhaobservacao',1,
  dspp==30 & acao ==  'campanhaparcelamento',2,
  dspp==45 & acao == 'campanhaboletoquitado',3,
  default = NA
  
)]



#vamos considerar que a acao mais drastica

target_intermed[,acao_mais_drastica:=max(codificao_acao_comunicado), by=.(contrato_id,dt_ref_portfolio)]

teste = target_intermed[contatos_por_dia>1]

target_intermed = target_intermed[contatos_por_dia==1 | (codificao_acao_comunicado==acao_mais_drastica)]

target_intermed[, conta_chave:=.N, by=.(contrato_id,dt_ref_portfolio)]
table(target_intermed[, conta_chave])
target_intermed[, conta_chave:=NULL]

target_intermed = target_intermed[!is.na(acao_mais_drastica)]

target_intermed = unique(target_intermed[,.(contrato_id,data_acao,contatos_por_dia,acao_mais_drastica)])


#juncao com a tabela de comunicados pela chave correta
target = merge(portifolio_analisado,target_intermed, by.x = c('contrato_id','dt_ref_portfolio'),by.y = c('contrato_id','data_acao') , all.x = T, suffixes = c('_geral','_comunicados') )


#left join com a target com a portfolio_tpv

target = merge(target, portfolio_tpv, by.x = c('nr_documento','dt_ref_portfolio'), by.y = c('nr_documento','dt_transacao'),all.x = T, suffixes = c("_target","_tpv") )

#ordenando a target
target = target[order(contrato_id,dt_ref_portfolio)]



#Determinacao dos regimes/ciclos de divida
target[,var_created_pagamento_efetuado:=ifelse(dsp==0,1,0)]
target[,regime_ativo:=rleid(var_created_pagamento_efetuado), by = contrato_id]
target[,regime_ativo:=ifelse(var_created_pagamento_efetuado==0 & shift(var_created_pagamento_efetuado, type = 'lead')!=0,regime_ativo, shift(regime_ativo, type = 'lead') )]


#agregamento da tabela target

#Fazendo uma copia da tabela target
  
preditivas_agregadas = copy(target)
str(target)

var_unicas = c('contrato_id','regime_ativo','var_created_situacao_inadimplencia')

#ajuste de variáveis
preditivas_agregadas[,var_agg_dt_ref_portfolio_min:=min(dt_ref_portfolio), by = .(regime_ativo,contrato_id) ]
preditivas_agregadas[,var_agg_dt_ref_portfolio_max:=max(dt_ref_portfolio), by = .(regime_ativo,contrato_id) ]
preditivas_agregadas[,var_agg_vlr_pgto_realizado_sum:=sum(vlr_pgto_realizado, na.rm = T),by = .(regime_ativo,contrato_id)]
preditivas_agregadas[,var_agg_vlr_saldo_devedor_min:=min(vlr_saldo_devedor), by = .(regime_ativo,contrato_id) ]
preditivas_agregadas[,var_agg_vlr_saldo_devedor_max:=max(vlr_saldo_devedor), by = .(regime_ativo,contrato_id) ]
preditivas_agregadas[,var_agg_vlr_saldo_devedor_esperado_min:=min(vlr_saldo_devedor_esperado), by = .(regime_ativo,contrato_id) ]
preditivas_agregadas[,var_agg_vlr_saldo_devedor_esperado_max:=max(vlr_saldo_devedor_esperado), by = .(regime_ativo,contrato_id) ]
preditivas_agregadas[,var_agg_dsp_max:=max(dsp), by = .(regime_ativo,contrato_id) ]
preditivas_agregadas[,var_agg_flag_transacao_med:=mean(flag_transacao), by = .(regime_ativo,contrato_id) ]
preditivas_agregadas[,var_agg_qtd_transacoes_sum:=sum(qtd_transacoes, na.rm = T),by = .(regime_ativo,contrato_id)]
preditivas_agregadas[,var_agg_vlr_tpv_sum:=sum(vlr_tpv, na.rm = T),by = .(regime_ativo,contrato_id)]

#selecao da variavel de situação de inadimplencia para o inicio do regime

preditivas_agregadas[,var_created_situacao_inadimplencia:=ifelse(dsp==0,var_created_situacao_inadimplencia,NA)]

preditivas_agregadas = preditivas_agregadas[!is.na(var_created_situacao_inadimplencia)]


variveis_agregadas = stringr::str_subset(colnames(preditivas_agregadas),'var_agg_')

#Aplicando o filtro de colunas e executando o distinct

filtro_colunas = c(var_unicas,variveis_agregadas)
preditivas_agregadas = unique(preditivas_agregadas[ ,colnames(preditivas_agregadas) %in% filtro_colunas, with=FALSE])
setcolorder(preditivas_agregadas,filtro_colunas)

preditivas_agregadas[,conta:=.N, by=.(regime_ativo,contrato_id)]

head(preditivas_agregadas[conta>1])

#relacionar campanha com regimes

campanha_regime = unique(copy(target[!is.na(acao_mais_drastica),.(contrato_id,regime_ativo,acao_mais_drastica,dsp)]))
campanha_regime[,numero_dias_contactados_pagamento:=.N, by=.(contrato_id,regime_ativo)]
campanha_regime[,max_dsp:=max(dsp), by = .(contrato_id,regime_ativo)]
campanha_regime[,max_campanha:=max(acao_mais_drastica), by = .(contrato_id,regime_ativo)]


#vamos filtrar os casos o ultimo caso informado ao cliente e a campanha mais grave do ciclo
campanha_regime = campanha_regime[dsp==max_dsp | acao_mais_drastica==max_campanha]

campanha_regime[dsp==max_dsp ,ultima_campanha_informada:=acao_mais_drastica]
campanha_regime[acao_mais_drastica==max_campanha ,principal_campanha_informada:=acao_mais_drastica]

campanha_regime[dsp==max_dsp ,dsp_ultima_campanha_informada:=dsp]
campanha_regime[acao_mais_drastica==max_campanha ,dsp_principal_campanha_informada:=dsp]

#ajuste dos valores missing
campanha_regime[,ultima_campanha_informada:=max(ultima_campanha_informada, na.rm = T), by = .(contrato_id,regime_ativo)]
campanha_regime[,principal_campanha_informada:=max(principal_campanha_informada, na.rm = T), by = .(contrato_id,regime_ativo)]
campanha_regime[,dsp_ultima_campanha_informada:=max(dsp_ultima_campanha_informada, na.rm = T), by = .(contrato_id,regime_ativo)]
campanha_regime[,dsp_principal_campanha_informada:=max(dsp_principal_campanha_informada, na.rm = T), by = .(contrato_id,regime_ativo)]



campanha_regime = unique(campanha_regime[,.(contrato_id,regime_ativo,dsp_ultima_campanha_informada,ultima_campanha_informada,dsp_principal_campanha_informada,principal_campanha_informada)])
