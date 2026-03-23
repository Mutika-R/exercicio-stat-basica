# Versão modificada - Usando mediana em vez de média
# No trecho de análise por região, altere:

# Versão original:
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
  )

# Versão modificada (usando mediana):
analise_regiao <- dados %>%
  group_by(regiao) %>%
  summarise(
    n_municipios = n(),
    populacao_mediana = median(populacao),  # Alterado de mean para median
    populacao_total = sum(populacao),
    idh_mediano = median(idh),              # Alterado de mean para median
    pib_mediano = median(pib_per_capita),   # Alterado de mean para median
    densidade_mediana = median(densidade),  # Alterado de mean para median
    .groups = 'drop'
  )
# Adicionar um novo gráfico de pizza
# Inserir este código após a criação dos outros gráficos:

# Gráfico 5: Distribuição de municípios por região (Pizza)
dados_regiao <- dados %>%
  group_by(regiao) %>%
  summarise(quantidade = n())

p5 <- ggplot(dados_regiao, aes(x = "", y = quantidade, fill = regiao)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(regiao, "\n", quantidade, " (", 
                               round(quantidade/sum(quantidade)*100, 1), "%)")),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  labs(title = "Distribuição de Municípios por Região") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("graficos/distribuicao_regiao_pizza.png", p5, width = 8, height = 6, dpi = 300)
cat("✓ Gráfico 5 salvo: distribuicao_regiao_pizza.png\n")
# Adicionar teste ANOVA para comparar IDH entre regiões
cat("\n========================================\n")
cat("TESTE ESTATÍSTICO - ANOVA\n")
cat("========================================\n\n")

# Teste ANOVA para verificar diferenças significativas
anova_idh <- aov(idh ~ regiao, data = dados)
cat("ANOVA - Diferença de IDH entre regiões:\n")
print(summary(anova_idh))

# Teste post-hoc Tukey
tukey_result <- TukeyHSD(anova_idh)
cat("\nTeste post-hoc Tukey:\n")
print(tukey_result)

# Salvar resultados
sink("resultados/teste_anova.txt")
cat("ANÁLISE DE VARIÂNCIA (ANOVA)\n")
cat("========================================\n\n")
cat("Hipótese: Existe diferença significativa no IDH entre as regiões?\n\n")
print(summary(anova_idh))
cat("\n\nTESTE POST-HOC TUKEY:\n")
print(tukey_result)
sink()
cat("✓ Resultados ANOVA exportados: resultados/teste_anova.txt\n")