# %%
import pandas as pd

# %%
# Carregando o DataFrame lucro_liquido
lucro_liquido = pd.read_excel(r'C:\Users\User\Empresas Canadá\ROA_cia_Canadá.xlsx', header=0, sheet_name='lucro_liquido')

# Limpeza e conversão das colunas para float
for col in lucro_liquido.columns[1:]:  # Exceto pela primeira coluna, assumindo que é 'DENOM_CIA'
    lucro_liquido[col] = lucro_liquido[col].astype(str).str.replace(' ', '').str.replace(',', '.')
    lucro_liquido[col] = pd.to_numeric(lucro_liquido[col], errors='coerce')

# Convertendo todos os nomes de colunas para strings
lucro_liquido.columns = lucro_liquido.columns.map(str)

# Identificando colunas que representam os anos
colunas_anos = [col for col in lucro_liquido.columns if col.isdigit()]

# Transformando o DataFrame 'lucro_liquido' de formato largo para longo
lucro_liquido_long = pd.melt(lucro_liquido, id_vars=['DENOM_CIA '], value_vars=colunas_anos, var_name='ano', value_name='lucro_liquido')

# Exibindo o resultado da transformação
print(lucro_liquido_long.head(30))


# %%
# Carregando o arquivo Excel
ativos_totais = pd.read_excel(r'C:\Users\User\Empresas Canadá\ROA_cia_Canadá.xlsx', header=0, sheet_name='ativos_totais')

# Eliminando a segunda coluna
ativos_totais = ativos_totais.drop(ativos_totais.columns[1], axis=1)
# Limpeza e conversão para float
for col in ativos_totais.columns[1:]:
    ativos_totais[col] = ativos_totais[col].astype(str).str.replace(' ', '').str.replace(',', '.')
    ativos_totais[col] = pd.to_numeric(ativos_totais[col], errors='coerce')
# Identificando as colunas que representam os anos
colunas_anos = [col for col in ativos_totais.columns[1:] if str(col).isdigit()]

# Transformando o DataFrame de ativos totais de formato largo para longo
ativos_totais_long = pd.melt(ativos_totais, id_vars=['DENOM_CIA '], value_vars=colunas_anos, var_name='ano', value_name='ativos_totais')
# Exibindo o resultado da transformação
print(ativos_totais_long.head(30))

# %%
# Renomear a coluna 'DENOM_CIA ' para 'DENOM_CIA'
lucro_liquido_long['DENOM_CIA'] = lucro_liquido_long['DENOM_CIA ']

# Agora podemos descartar a coluna 'DENOM_CIA ' se não for mais necessária
lucro_liquido_long.drop(columns=['DENOM_CIA '], inplace=True)

# Renomear a coluna 'DENOM_CIA ' para 'DENOM_CIA'
ativos_totais_long['DENOM_CIA'] = ativos_totais_long['DENOM_CIA ']

# Agora podemos descartar a coluna 'DENOM_CIA ' se não for mais necessária
ativos_totais_long.drop(columns=['DENOM_CIA '], inplace=True)

# %%
lucro_liquido_long['ano'] = lucro_liquido_long['ano'].astype(int)
ativos_totais_long['ano'] = ativos_totais_long['ano'].astype(int)

# %%
# Mesclar os dataframes usando a coluna 'DENOM_CIA' e 'ano_lucro' como chaves
ROA = pd.merge(lucro_liquido_long, ativos_totais_long, on=['DENOM_CIA', 'ano'])

# %%
# Reordenando as colunas
ROA = ROA[['DENOM_CIA', 'ano', 'lucro_liquido', 'ativos_totais']]
# Calculando a coluna ROA
ROA['ROA'] = (ROA['lucro_liquido'] / ROA['ativos_totais']) * 100

ROA['pais'] = 3


# %%
ROA.head(30)

# %%
caminho_salvar = r'C:\Users\User\Empresas Canadá\ROA_Canada.csv'

ROA.to_csv(caminho_salvar, index = False, encoding= 'utf-8-sig')

print(f'O arquivo foi salvo em {caminho_salvar}')


