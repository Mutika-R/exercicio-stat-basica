# analise.R
# Script simples de análise de dados para demonstração de versionamento no GitHub

# Carregar bibliotecas necessárias
library(dplyr)
library(ggplot2)

# Criar um dataset de exemplo
set.seed(123)  # Para reprodutibilidade

dados <- data.frame(
  municipio = paste0("Município_", 1:50),
  populacao = round(runif(50, 5000, 500000), 0),
  pib_per_capita = round(runif(50, 5000, 50000), 0),
  regiao = sample(c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"), 
                  50, replace = TRUE)
)

# Estatísticas descritivas
cat("=== ESTATÍSTICAS DESCRITIVAS ===\n")
cat("\nPopulação:\n")
print(summary(dados$populacao))

cat("\nPIB per capita:\n")
print(summary(dados$pib_per_capita))

# Resumo por região
resumo_regiao <- dados %>%
  group_by(regiao) %>%
  summarise(
    populacao_media = mean(populacao),
    pib_per_capita_medio = mean(pib_per_capita),
    n_municipios = n()
  ) %>%
  arrange(desc(populacao_media))

cat("\n=== RESUMO POR REGIÃO ===\n")
print(resumo_regiao)

# Criar gráfico de barras
ggplot(dados, aes(x = reorder(regiao, -populacao), y = populacao)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "População Média por Região",
    x = "Região",
    y = "População Média"
  )

# Salvar gráfico
ggsave("populacao_por_regiao.png", width = 8, height = 6, dpi = 300)

# Criar gráfico de dispersão
ggplot(dados, aes(x = populacao, y = pib_per_capita, color = regiao)) +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Relação entre População e PIB per capita",
    x = "População",
    y = "PIB per capita (R$)",
    color = "Região"
  )

# Salvar gráfico
ggsave("pib_vs_populacao.png", width = 8, height = 6, dpi = 300)

# Mensagem final
cat("\n=== ANÁLISE CONCLUÍDA ===\n")
cat("Gráficos salvos como:\n")
cat("- populacao_por_regiao.png\n")
cat("- pib_vs_populacao.png\n")
cat("\nTotal de municípios analisados:", nrow(dados))
