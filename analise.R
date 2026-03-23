## analise.R - Versão 1.0
# Script de análise de dados municipais
# Autor: [Murilo Rocha Souto Maior]
# Data: 2026-03-23

# ============================================
# 1. CONFIGURAÇÃO INICIAL
# ============================================

# Limpar ambiente
rm(list = ls())
cat("\014")  # Limpar console

# Definir seed para reprodutibilidade
set.seed(123)

# Mensagem de início
cat("========================================\n")
cat("INICIANDO ANÁLISE DE DADOS MUNICIPAIS\n")
cat("========================================\n\n")

# ============================================
# 2. CARREGAR BIBLIOTECAS
# ============================================

# Verificar e instalar pacotes se necessário
pacotes <- c("dplyr", "ggplot2", "tidyr", "knitr")

for(pacote in pacotes) {
  if(!require(pacote, character.only = TRUE)) {
    install.packages(pacote)
    library(pacote, character.only = TRUE)
  }
}

cat("✓ Bibliotecas carregadas com sucesso\n\n")

# ============================================
# 3. CRIAR DATASET DE EXEMPLO
# ============================================

cat("Criando dataset de exemplo...\n")

# Lista de municípios brasileiros (amostra)
municipios <- c(
  "São Paulo", "Rio de Janeiro", "Brasília", "Salvador", "Fortaleza",
  "Belo Horizonte", "Manaus", "Curitiba", "Recife", "Porto Alegre",
  "Belém", "Goiânia", "Campinas", "São Luís", "Maceió",
  "Natal", "Teresina", "Campo Grande", "João Pessoa", "Aracaju"
)

# Gerar dados
dados <- data.frame(
  municipio = municipios,
  populacao = round(runif(20, 50000, 12000000), 0),
  area_km2 = round(runif(20, 100, 8000), 0),
  idh = round(runif(20, 0.6, 0.85), 3),
  pib_per_capita = round(runif(20, 8000, 60000), 0),
  regiao = sample(c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"), 
                  20, replace = TRUE),
  stringsAsFactors = FALSE
)

# Calcular densidade demográfica
dados$densidade <- round(dados$populacao / dados$area_km2, 2)

cat("✓ Dataset criado com", nrow(dados), "municípios\n\n")

# ============================================
# 4. ESTATÍSTICAS DESCRITIVAS BÁSICAS
# ============================================

cat("========================================\n")
cat("ESTATÍSTICAS DESCRITIVAS\n")
cat("========================================\n\n")

# Resumo das variáveis numéricas
cat("Resumo da População:\n")
print(summary(dados$populacao))

cat("\nResumo do IDH:\n")
print(summary(dados$idh))

cat("\nResumo do PIB per capita:\n")
print(summary(dados$pib_per_capita))

cat("\nResumo da Densidade Demográfica (hab/km²):\n")
print(summary(dados$densidade))

# ============================================
# 5. ANÁLISE POR REGIÃO
# ============================================

cat("\n========================================\n")
cat("ANÁLISE POR REGIÃO\n")
cat("========================================\n\n")

analise_regiao <- dados %>%
  group_by(regiao) %>%
  summarise(
    n_municipios = n(),
    populacao_media = mean(populacao),
    populacao_total = sum(populacao),
    idh_medio = mean(idh),
    pib_medio = mean(pib_per_capita),
    densidade_media = mean(densidade),
    .groups = 'drop'
  ) %>%
  arrange(desc(populacao_total))

print(analise_regiao)

# ============================================
# 6. TOP 10 MUNICÍPIOS
# ============================================

cat("\n========================================\n")
cat("TOP 10 MUNICÍPIOS\n")
cat("========================================\n\n")

# Top 10 por população
top10_pop <- dados %>%
  arrange(desc(populacao)) %>%
  select(municipio, regiao, populacao, idh, pib_per_capita) %>%
  head(10)

cat("TOP 10 MUNICÍPIOS POR POPULAÇÃO:\n")
print(top10_pop)

# Top 10 por IDH
top10_idh <- dados %>%
  arrange(desc(idh)) %>%
  select(municipio, regiao, idh, populacao, pib_per_capita) %>%
  head(10)

cat("\nTOP 10 MUNICÍPIOS POR IDH:\n")
print(top10_idh)

# ============================================
# 7. CORRELAÇÕES
# ============================================

cat("\n========================================\n")
cat("CORRELAÇÕES\n")
cat("========================================\n\n")

correlacoes <- cor(dados[, c("populacao", "idh", "pib_per_capita", "densidade")])
print(round(correlacoes, 3))

# ============================================
# 8. MENSAGEM FINAL
# ============================================

cat("\n========================================\n")
cat("✓ ANÁLISE CONCLUÍDA COM SUCESSO!\n")
cat("========================================\n")
cat("Total de municípios analisados:", nrow(dados), "\n")
cat("Total de regiões:", length(unique(dados$regiao)), "\n")
cat("\nPróximos passos:\n")
cat("- Criar visualizações gráficas\n")
cat("- Exportar resultados para CSV\n")
cat("- Realizar análises mais avançadas\n")