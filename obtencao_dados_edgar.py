# %% [markdown]
# Bibliotecas

# %%
!pip install pandas
!pip install requests
!pip install numpy
!pip install natsort

# %%
import requests
import pandas as pd
import numpy as np
from natsort import natsorted
import os

# %% [markdown]
# Diretórios

# %%
dir_in_empresas_SP500 = r'C:\Users\User\Documents\GitHub\TCC---MBA_USP_-DSA\Arquivos TCC\empresas_cvm_tcc_csv-\empresas_cvm_tcc_csv\empresas_S_P500.csv'
dir_empresas_edgar = r'C:\Users\User\Documents\GitHub\TCC---MBA_USP_-DSA\Arquivos TCC\empresas_cvm_tcc_csv-\empresas_cvm_tcc_csv\dfs_edgar'
dir_dados = r'C:\Users\User\Documents\GitHub\TCC---MBA_USP_-DSA\Arquivos TCC\empresas_cvm_tcc_csv-\empresas_cvm_tcc_csv'

# %%
headers = {'User-agent': "xxx@yyy.com.br"} #adicionar o email para acesso à API EDGAR

# %%
company_tickers = requests.get(
    "https://www.sec.gov/files/company_tickers.json",
    headers = headers
)

#dictinary to dataframe
company_data = pd.DataFrame.from_dict(company_tickers.json(), orient= 'index')

# %%
company_data

# %%
#add leading zeros to CIK (is necessary to acess te company at API)
company_data['cik_str'] = company_data['cik_str'].astype(str).str.zfill(10)

# %%
#cik = company_data[0:1].cik_str[0]

# %%
company_data

# %%
#cik = company_data.iloc[1].cik_str
cik = '0001142790'

# %%
#get company specific filing metadata
filing_metadata = requests.get(
    f'https://data.sec.gov/submissions/CIK{cik}.json',
    headers= headers
)

#dictionary to Data Frame
all_forms = pd.DataFrame.from_dict(filing_metadata.json()['filings']['recent'])
#all_forms[['accessionNumber', 'reportDate', 'form']].head(20)
all_forms_10k = all_forms[all_forms['form'] == '10-K'][['accessionNumber', 'reportDate', 'form','fileNumber']]
all_forms_10k

# %%
#get company facts data
company_facts = requests.get(
    f'https://data.sec.gov/api/xbrl/companyfacts/CIK{cik}.json',
    headers = headers
)

company_facts_us_gaap = pd.DataFrame.from_dict(company_facts.json()['facts']['us-gaap'].keys())


# %%
company_facts_us_gaap

# %%
patrimonio_liquido = pd.DataFrame(company_facts.json()['facts']['us-gaap']['StockholdersEquity']['units']['USD'])
patrimonio_liquido
patrimonio_liquido_10K = patrimonio_liquido[(patrimonio_liquido['form'] == '10-K') & (patrimonio_liquido['fp'] == 'FY')]
patrimonio_liquido_10K_ok = patrimonio_liquido_10K[patrimonio_liquido_10K['end'].str.slice(0, 4) == patrimonio_liquido_10K['fy'].astype(str)]

# %%
ativo_circulante = pd.DataFrame(company_facts.json()['facts']['us-gaap']['AssetsCurrent']['units']['USD'])
ativo_circulante_10K = ativo_circulante[(ativo_circulante['form'] == '10-K') & (ativo_circulante['fp'] == 'FY')]
ativo_circulante_10K_ok = ativo_circulante_10K[ativo_circulante_10K['end'].str.slice(0, 4) == ativo_circulante_10K['fy'].astype(str)]

# %%
ativo_total = pd.DataFrame(company_facts.json()['facts']['us-gaap']['Assets']['units']['USD'])
ativo_total_10K = ativo_total[(ativo_total['form'] == '10-K') & (ativo_total['fp'] == 'FY')]
ativo_total_10K_ok = ativo_total_10K[ativo_total_10K['end'].str.slice(0, 4) == ativo_total_10K['fy'].astype(str)]

# %%
passivo_circulante = pd.DataFrame(company_facts.json()['facts']['us-gaap']['LiabilitiesCurrent']['units']['USD'])
passivo_circulante_10K = passivo_circulante[(passivo_circulante['form'] == '10-K') & (passivo_circulante['fp'] == 'FY')]
passivo_circulante_10K_ok = passivo_circulante_10K[passivo_circulante_10K['end'].str.slice(0, 4) == passivo_circulante_10K['fy'].astype(str)]

# %%
exigivel_total  = pd.DataFrame(company_facts.json()['facts']['us-gaap']['Liabilities']['units']['USD'])
exigivel_total_10K = exigivel_total[(exigivel_total['form'] == '10-K') & (exigivel_total['fp'] == 'FY')]
exigivel_total_10K_ok = exigivel_total_10K[exigivel_total_10K['end'].str.slice(0, 4) == exigivel_total_10K['fy'].astype(str)]

# %%
receita_liquida = pd.DataFrame(company_facts.json()['facts']['us-gaap']['Revenues']['units']['USD'])
receita_liquida_10K = receita_liquida[(receita_liquida['form'] == '10-K') & (receita_liquida['fp'] == 'FY')]
receita_liquida_10K_ok = receita_liquida_10K[receita_liquida_10K['end'].str.slice(0, 4).isin(receita_liquida_10K['start'].str.slice(0, 4))]
receita_liquida_10K_ok = receita_liquida_10K_ok.drop_duplicates(subset='start')


# %%
# Converte a coluna 'start' para o tipo datetime
receita_liquida_10K_ok['start'] = pd.to_datetime(receita_liquida_10K_ok['start'])

# Agrupa por 'fy' e soma 'val'
receita_liquida_cons = (
    receita_liquida_10K_ok.groupby('fy')['val']
    .sum()
    .reset_index(name='RL')
)


# %%
lucro_liquido = pd.DataFrame(company_facts.json()['facts']['us-gaap']['NetIncomeLoss']['units']['USD'])
lucro_liquido_10K = lucro_liquido[(lucro_liquido['form'] == '10-K')&(lucro_liquido['fp'] == 'FY')]
lucro_liquido_10K_ok = lucro_liquido_10K[lucro_liquido_10K['end'].str.slice(0, 4).isin(lucro_liquido_10K['start'].str.slice(0, 4))]
lucro_liquido_10K_ok = lucro_liquido_10K_ok.drop_duplicates(subset='start')

# %%
# Converte a coluna 'start' para o tipo datetime
lucro_liquido_10K_ok['start'] = pd.to_datetime(lucro_liquido_10K_ok['start'])

# Agrupa por 'fy' e soma 'val'
lucro_liquido_10K_ok_cons = (
    lucro_liquido_10K_ok.groupby('fy')['val']
    .sum()
    .reset_index(name='LL')
)

lucro_liquido_10K_ok_cons

# %%
ativo_total_10K_ok

# %% [markdown]
# Ebitida

# %%
lucro_operacional = pd.DataFrame(company_facts.json()['facts']['us-gaap']['OperatingIncomeLoss']['units']['USD'])
lucro_operacional_10K = lucro_operacional[(lucro_operacional['form'] == '10-K') & (lucro_operacional['fp'] == 'FY')]
lucro_operacional_10K_ok = lucro_operacional_10K[lucro_operacional_10K['end'].str.slice(0, 4).isin(lucro_operacional_10K['start'].str.slice(0, 4))]
lucro_operacional_10K_ok = lucro_operacional_10K_ok.drop_duplicates(subset='start')

# %%
lucro_operacional_10K_ok

# %%
depreciacao_amortizacao = pd.DataFrame(company_facts.json()['facts']['us-gaap']['DepreciationAndAmortization']['units']['USD'])
depreciacao_amortizacao_10K = depreciacao_amortizacao[(depreciacao_amortizacao['form'] == '10-K') & (depreciacao_amortizacao['fp'] == 'FY')]
depreciacao_amortizacao_10K_ok = depreciacao_amortizacao_10K[depreciacao_amortizacao_10K['end'].str.slice(0, 4) == depreciacao_amortizacao_10K['fy'].astype(str)]


# %%
# Criar um novo DataFrame com a estrutura de lucro_operacional
lucro_operacional_10K_ok['start'] = pd.to_datetime(lucro_operacional_10K_ok['start'])
lucro_operacional_cons = (
    lucro_operacional_10K_ok.groupby('fy')['val']
    .sum()
    .reset_index(name = 'ebit')
)



# %%
# Merge nos DataFrames usando a coluna 'fy'
merged_df = pd.merge(lucro_operacional_cons, depreciacao_amortizacao_10K_ok, how='inner', on='fy')

# Crie o novo DataFrame ebitda_df
ebitda_df = pd.DataFrame()

# Atribua a coluna 'ebitda' como a soma de 'ebit' e 'val'
ebitda_df['ebitda'] = merged_df['ebit'] + merged_df['val']
ebitda_df['Ano'] = merged_df['fy']

# %% [markdown]
# Liquidez corrente = ativo_circulante/passivo_circulante

# %%
# Criando o DataFrame 'liquidez_corrente'
liquidez_corrente = pd.DataFrame()

# Merge dos DataFrames 'ativo_circulante_10K_ok' e 'passivo_circulante_10K_ok' com base na coluna 'fy'
merged_df = pd.merge(ativo_circulante_10K_ok, passivo_circulante_10K_ok, on='fy', suffixes=('_ativo', '_passivo'))

# Preenchendo as colunas 'Ano' e 'LC' diretamente no DataFrame 'liquidez_corrente'
liquidez_corrente['Ano'] = merged_df['fy']
liquidez_corrente['LC'] = merged_df['val_ativo'] / merged_df['val_passivo']

# Obtendo o valor de 'CIK' diretamente do DataFrame 'company_data'
cik_value = cik
liquidez_corrente['CIK'] = cik_value

# Mapeando os títulos com base no 'CIK' usando o DataFrame 'company_data'
def get_title(cik):
    matches = company_data.loc[company_data['cik_str'] == cik, 'title']
    return matches.iloc[0] if not matches.empty else None


liquidez_corrente['DENOM_CIA'] = liquidez_corrente['CIK'].apply(get_title)
# Exibindo o DataFrame resultante
print(liquidez_corrente)

# %% [markdown]
# CTPT = exigivel_total/patrimonio_liquido

# %%
# Criando o DataFrame 'liquidez_corrente'
CTPT = pd.DataFrame()

# Merge dos DataFrames 'ativo_circulante_10K_ok' e 'passivo_circulante_10K_ok' com base na coluna 'fy'
merged_df = pd.merge(exigivel_total_10K_ok, patrimonio_liquido_10K_ok, on='fy', suffixes=('_ativo', '_passivo'))

# Preenchendo as colunas 'Ano' e 'LC' diretamente no DataFrame 'liquidez_corrente'
CTPT['Ano'] = merged_df['fy']
CTPT['CTPT'] = merged_df['val_ativo'] / merged_df['val_passivo']

# Obtendo o valor de 'CIK' diretamente do DataFrame 'company_data'
cik_value = cik
CTPT['CIK'] = cik_value

# Mapeando os títulos com base no 'CIK' usando o DataFrame 'company_data'
def get_title(cik):
    matches = company_data.loc[company_data['cik_str'] == cik, 'title']
    return matches.iloc[0] if not matches.empty else None


CTPT['DENOM_CIA'] = CTPT['CIK'].apply(get_title)


# Exibindo o DataFrame resultante
print(CTPT)

# %% [markdown]
# margem_liquida = lucro_liquido/receita_liquida

# %%
lucro_liquido_10K_ok['start'] = pd.to_datetime(lucro_liquido_10K_ok['start'])

# Agrupa por 'fy' e soma 'val'
lucro_liquido_cons = (
    lucro_liquido_10K_ok.groupby('fy')['val']
    .sum()
    .reset_index(name='LL')
)
lucro_liquido_cons= lucro_liquido_cons.rename(columns={'fy': 'Ano'})


# %%
receita_liquida_10K_ok['start'] = pd.to_datetime(receita_liquida_10K_ok['start'])

# Agrupa por 'fy' e soma 'val'
receita_liquida_cons = (
    receita_liquida_10K_ok.groupby('fy')['val']
    .sum()
    .reset_index(name='RL')
)
receita_liquida_cons= receita_liquida_cons.rename(columns={'fy': 'Ano'})

# %%
margem_liquida = pd.DataFrame()

# Merge dos DataFrames por ano
merged_df = pd.merge(lucro_liquido_cons, receita_liquida_cons, on='Ano')
margem_liquida['ML'] = merged_df['LL'] / merged_df['RL']

# Obtendo o valor de 'CIK' diretamente do DataFrame 'company_data'
cik_value = cik
margem_liquida['CIK'] = cik_value

# Mapeando os títulos com base no 'CIK' usando o DataFrame 'company_data'
def get_title(cik):
    matches = company_data.loc[company_data['cik_str'] == cik, 'title']
    return matches.iloc[0] if not matches.empty else None


margem_liquida['DENOM_CIA'] = margem_liquida['CIK'].apply(get_title)

# %% [markdown]
# margem_ebtida = EBTIDA/vendas_liquidas

# %%
ebitda = pd.DataFrame({'Ano': ebitda_df['Ano'], 'ebitda': ebitda_df['ebitda'].values})
margem_ebitda = pd.DataFrame()

# Convertendo as séries em DataFrames com uma coluna 'Ano'
receita_liquida_df = pd.DataFrame({'Ano': receita_liquida_cons['Ano'], 'RL': receita_liquida_cons['RL'].values})

# Convertendo a coluna 'Ano' para o mesmo tipo em ambos os DataFrames (se necessário)
ebitda['Ano'] = ebitda['Ano'].fillna(0).astype(int)
receita_liquida_df['Ano'] = receita_liquida_df['Ano'].astype(int)


# Merge dos DataFrames por ano
merged_df = pd.merge(ebitda, receita_liquida_df, on='Ano')
margem_ebitda['ME'] = merged_df['ebitda']/ merged_df['RL']


# Obtendo o valor de 'CIK' diretamente do DataFrame 'company_data'
cik_value = cik
margem_ebitda['CIK'] = cik_value

# Mapeando os títulos com base no 'CIK' usando o DataFrame 'company_data'
def get_title(cik):
    matches = company_data.loc[company_data['cik_str'] == cik, 'title']
    return matches.iloc[0] if not matches.empty else None


margem_ebitda['DENOM_CIA'] = margem_ebitda['CIK'].apply(get_title)

# %%
caminho_arq = dir_in_empresas_SP500
empresas_sp500 = pd.read_csv(caminho_arq)
#add leading zeros to CIK (is necessary to acess te company at API)
empresas_sp500['CIK'] = empresas_sp500['CIK'].astype(str).str.zfill(10)

# %% [markdown]
# Para todas as empresas

# %%
# Pegar todos os CIKs
#ciks_tot = company_data['cik_str'].tolist()
# Defina o número desejado de CIKs aleatórios
#numero_ciks_aleatorios = 3000
# Gere uma lista de CIKs aleatórios
#ciks_3000 = random.sample(ciks_tot, numero_ciks_aleatorios)
ciks_3000 = company_data['cik_str']

resultados_por_cik = []
# Iterar sobre cada CIK em company_data['cik_str']
for cik in ciks_3000:
    try:
                # Obter dados da empresa
        company_facts = requests.get(
            f'https://data.sec.gov/api/xbrl/companyfacts/CIK{cik}.json',
            headers=headers
        )

        patrimonio_liquido = pd.DataFrame(company_facts.json()['facts']['us-gaap']['StockholdersEquity']['units']['USD'])
        patrimonio_liquido_10K = patrimonio_liquido[(patrimonio_liquido['form'] == '10-K') & (patrimonio_liquido['fp'] == 'FY')]
        patrimonio_liquido_10K_ok = patrimonio_liquido_10K[patrimonio_liquido_10K['end'].str.slice(0, 4) == patrimonio_liquido_10K['fy'].astype(str)]

        ativo_circulante = pd.DataFrame(company_facts.json()['facts']['us-gaap']['AssetsCurrent']['units']['USD'])
        ativo_circulante_10K = ativo_circulante[(ativo_circulante['form'] == '10-K') & (ativo_circulante['fp'] == 'FY')]
        ativo_circulante_10K_ok = ativo_circulante_10K[ativo_circulante_10K['end'].str.slice(0, 4) == ativo_circulante_10K['fy'].astype(str)]

        passivo_circulante = pd.DataFrame(company_facts.json()['facts']['us-gaap']['LiabilitiesCurrent']['units']['USD'])
        passivo_circulante_10K = passivo_circulante[(passivo_circulante['form'] == '10-K') & (passivo_circulante['fp'] == 'FY')]
        passivo_circulante_10K_ok = passivo_circulante_10K[passivo_circulante_10K['end'].str.slice(0, 4) == passivo_circulante_10K['fy'].astype(str)]

        exigivel_total  = pd.DataFrame(company_facts.json()['facts']['us-gaap']['Liabilities']['units']['USD'])
        exigivel_total_10K = exigivel_total[(exigivel_total['form'] == '10-K') & (exigivel_total['fp'] == 'FY')]
        exigivel_total_10K_ok = exigivel_total_10K[exigivel_total_10K['end'].str.slice(0, 4) == exigivel_total_10K['fy'].astype(str)]

        receita_liquida = pd.DataFrame(company_facts.json()['facts']['us-gaap']['Revenues']['units']['USD'])
        receita_liquida_10K = receita_liquida[(receita_liquida['form'] == '10-K') & (receita_liquida['fp'] == 'FY')]
        receita_liquida_10K_ok = receita_liquida_10K[receita_liquida_10K['end'].str.slice(0, 4).isin(receita_liquida_10K['start'].str.slice(0, 4))]
        receita_liquida_10K_ok = receita_liquida_10K_ok.drop_duplicates(subset='start')

        lucro_liquido = pd.DataFrame(company_facts.json()['facts']['us-gaap']['NetIncomeLoss']['units']['USD'])
        lucro_liquido_10K = lucro_liquido[(lucro_liquido['form'] == '10-K')&(lucro_liquido['fp'] == 'FY')]
        lucro_liquido_10K_ok = lucro_liquido_10K[lucro_liquido_10K['end'].str.slice(0, 4).isin(lucro_liquido_10K['start'].str.slice(0, 4))]
        lucro_liquido_10K_ok = lucro_liquido_10K_ok.drop_duplicates(subset='start')

        ativo_total = pd.DataFrame(company_facts.json()['facts']['us-gaap']['Assets']['units']['USD'])
        ativo_total_10K = ativo_total[(ativo_total['form'] == '10-K') & (ativo_total['fp'] == 'FY')]
        ativo_total_10K_ok = ativo_total_10K[ativo_total_10K['end'].str.slice(0, 4) == ativo_total_10K['fy'].astype(str)]

        # Ebitda
        lucro_operacional = pd.DataFrame(company_facts.json()['facts']['us-gaap']['OperatingIncomeLoss']['units']['USD'])
        lucro_operacional_10K = lucro_operacional[(lucro_operacional['form'] == '10-K') & (lucro_operacional['fp'] == 'FY')]
        lucro_operacional_10K_ok = lucro_operacional_10K[lucro_operacional_10K['end'].str.slice(0, 4).isin(lucro_operacional_10K['start'].str.slice(0, 4))]
        lucro_operacional_10K_ok = lucro_operacional_10K_ok.drop_duplicates(subset='start')

        depreciacao_amortizacao = pd.DataFrame(company_facts.json()['facts']['us-gaap']['DepreciationAndAmortization']['units']['USD'])
        depreciacao_amortizacao_10K = depreciacao_amortizacao[(depreciacao_amortizacao['form'] == '10-K') & (depreciacao_amortizacao['fp'] == 'FY')]
        depreciacao_amortizacao_10K_ok = depreciacao_amortizacao_10K[depreciacao_amortizacao_10K['end'].str.slice(0, 4) == depreciacao_amortizacao_10K['fy'].astype(str)]

        lucro_operacional_10K_ok['start'] = pd.to_datetime(lucro_operacional_10K_ok['start'])
        lucro_operacional_cons = (
            lucro_operacional_10K_ok.groupby('fy')['val']
            .sum()
            .reset_index(name='ebit')
        )

        merged_df = pd.merge(lucro_operacional_cons, depreciacao_amortizacao_10K_ok, how='inner', on='fy')
        ebitda_df = pd.DataFrame()
        ebitda_df['ebitda'] = merged_df['ebit'] + merged_df['val']
        ebitda_df['Ano'] = merged_df['fy']

        # Váriaveis do modelo

        # Liquidez corrente
        liquidez_corrente = pd.DataFrame()
        merged_df = pd.merge(ativo_circulante_10K_ok, passivo_circulante_10K_ok, on='fy', suffixes=('_ativo', '_passivo'))
        liquidez_corrente['Ano'] = merged_df['fy']
        liquidez_corrente['LC'] = merged_df['val_ativo'] / merged_df['val_passivo']
        cik_value = cik
        liquidez_corrente['CIK'] = cik_value

        def get_title(cik):
            matches = company_data.loc[company_data['cik_str'] == cik, 'title']
            return matches.iloc[0] if not matches.empty else None

        liquidez_corrente['DENOM_CIA'] = liquidez_corrente['CIK'].apply(get_title)

        # CTPT
        CTPT = pd.DataFrame()
        merged_df = pd.merge(exigivel_total_10K_ok, patrimonio_liquido_10K_ok, on='fy', suffixes=('_ativo', '_passivo'))
        CTPT['Ano'] = merged_df['fy']
        CTPT['CTPT'] = merged_df['val_ativo'] / merged_df['val_passivo']
        cik_value = cik
        CTPT['CIK'] = cik_value

        CTPT['DENOM_CIA'] = CTPT['CIK'].apply(get_title)

        # Margem líquida
        lucro_liquido_10K_ok['start'] = pd.to_datetime(lucro_liquido_10K_ok['start'])
        lucro_liquido_cons = (
            lucro_liquido_10K_ok.groupby('fy')['val']
            .sum()
            .reset_index(name='LL')
        )
        lucro_liquido_cons = lucro_liquido_cons.rename(columns={'fy': 'Ano'})

        receita_liquida_10K_ok['start'] = pd.to_datetime(receita_liquida_10K_ok['start'])

        receita_liquida_cons = (
            receita_liquida_10K_ok.groupby('fy')['val']
            .sum()
            .reset_index(name='RL')
        )
        receita_liquida_cons = receita_liquida_cons.rename(columns={'fy': 'Ano'})

        margem_liquida = pd.DataFrame()
        merged_df = pd.merge(lucro_liquido_cons, receita_liquida_cons, on='Ano')
        margem_liquida['ML'] = merged_df['LL'] / merged_df['RL']
        cik_value = cik
        margem_liquida['CIK'] = cik_value
        margem_liquida['Ano'] = merged_df['Ano']
        margem_liquida['DENOM_CIA'] = margem_liquida['CIK'].apply(get_title)

        # Margem Ebitda
        ebitda = pd.DataFrame({'Ano': ebitda_df['Ano'], 'ebitda': ebitda_df['ebitda'].values})
        margem_ebitda = pd.DataFrame()
        receita_liquida_df = pd.DataFrame({'Ano': receita_liquida_cons['Ano'], 'RL': receita_liquida_cons['RL'].values})
        ebitda['Ano'] = ebitda['Ano'].fillna(0).astype(int)
        receita_liquida_df['Ano'] = receita_liquida_df['Ano'].astype(int)
        merged_df = pd.merge(ebitda, receita_liquida_df, on='Ano')
        margem_ebitda['ME'] = merged_df['ebitda'] / merged_df['RL']
        cik_value = cik
        margem_ebitda['CIK'] = cik_value
        margem_ebitda['Ano'] = merged_df['Ano']
        margem_ebitda['DENOM_CIA'] = margem_ebitda['CIK'].apply(get_title)

        # Junção dos DataFrames
        merged_df = liquidez_corrente.merge(CTPT, on=['Ano', 'CIK', 'DENOM_CIA'], how='outer')
        merged_df = merged_df.merge(margem_liquida, on=['Ano', 'CIK', 'DENOM_CIA'], how='outer')
        merged_df = merged_df.merge(margem_ebitda, on=['Ano', 'CIK', 'DENOM_CIA'], how='outer')

        for _, row in merged_df.iterrows():
            resultados_por_cik.append({
                'CIK': row['CIK'],
                'Ano': row['Ano'],
                'LC': row['LC'],
                'CTPT': row['CTPT'],
                'ML': row['ML'],
                'ME': row['ME']
            })

    except Exception as e:
        # Lidar com a exceção de forma genérica
        print(f"Erro para CIK {cik}: {e}")
        # Continuar para o próximo CIK
        continue

# Criar o DataFrame final 'empresas_edgar_tcc'
empresas_edgar_tcc_test = pd.DataFrame(resultados_por_cik)
#salvando
caminho_salvar = os.path.join(dir_empresas_edgar, 'empresas_edgar_tcc_LL_ok.csv')
empresas_edgar_tcc_test.to_csv(caminho_salvar, index=False)

# %%
empresas_edgar_tcc_test.nunique()

# %%
# Caminho para o arquivo CSV
caminho_arquivo = os.path.join(dir_empresas_edgar, 'empresas_edgar_tcc_LL_ok.csv')

# Carregar o DataFrame a partir do arquivo CSV
empresas_edgar_tcc = pd.read_csv(caminho_arquivo, sep=',')



# %%
empresas_edgar_tcc['CIK'].nunique()

# %%
valor_lc = empresas_edgar_tcc.loc[empresas_edgar_tcc['CIK'] == 1045810, 'LC'].values
valor_lc

# %%
#Função para verificar se o valor é numérico para remoção de observações que não estão formatadas
def is_numeric(value):
    try:
        float(value)
        return True
    except (ValueError, TypeError):
        return False

# Verificar e remover linhas com valores não numéricos nas colunas especificadas
for coluna in ['LC', 'CTPT', 'ML', 'ME']:
    empresas_edgar_tcc = empresas_edgar_tcc[empresas_edgar_tcc[coluna].apply(is_numeric)]



# %%
#filtrando o df para que contenham apenas ciks que possuam todas as observações válidas para pelo menos 2 anos
# Excluindo linhas com pelo menos um NaN
empresas_edgar_sem_nan = empresas_edgar_tcc.dropna()

# Verificando se há pelo menos dois anos únicos para cada CIK
ciks_com_pelo_menos_dois_anos = empresas_edgar_sem_nan.groupby('CIK')['Ano'].nunique()
ciks_com_pelo_menos_dois_anos = ciks_com_pelo_menos_dois_anos[ciks_com_pelo_menos_dois_anos >= 2].index

# Filtrando o DataFrame original com base nos CIKs que têm pelo menos dois anos únicos
empresas_final = empresas_edgar_tcc[empresas_edgar_tcc['CIK'].isin(ciks_com_pelo_menos_dois_anos)]

cik_filtrado = empresas_final['CIK'].unique()
cik_filtrado

# Filtrando o DataFrame original com base nos CIKs desejados
empresas_edgar_tcc = empresas_edgar_tcc[empresas_edgar_tcc['CIK'].isin(cik_filtrado)]




# %%
# Calculo do ROA
ROA = empresas_edgar_tcc.drop(['LC', 'ML', 'ME'], axis = 1).copy()

# %%
cik_filtrado = empresas_final['CIK'].astype(str).str.zfill(10).unique()
ciks_3000 = cik_filtrado
resultados_por_cik = []

for cik in ciks_3000:
    try:
        # Obter dados da empresa
        company_facts = requests.get(
            f'https://data.sec.gov/api/xbrl/companyfacts/CIK{cik}.json',
            headers=headers
        )

        # Processar ativo total
        ativo_total = pd.DataFrame(company_facts.json()['facts']['us-gaap']['Assets']['units']['USD'])
        ativo_total_10K = ativo_total[(ativo_total['form'] == '10-K') & (ativo_total['fp'] == 'FY')]
        ativo_total_10K_ok = ativo_total_10K[ativo_total_10K['end'].str.slice(0, 4) == ativo_total_10K['fy'].astype(str)]

        # Adicionar resultados à lista
        for index, row in ativo_total_10K_ok.iterrows():
            resultados_por_cik.append({
                'CIK': cik,
                'Ano': row['fy'],
                'AT': row['val'],
            })
    except Exception as e:
        # Lidar com a exceção de forma genérica
        print(f"Erro para CIK {cik}: {e}")
        # Continuar para o próximo CIK
        continue

# Criar o DataFrame final 'empresas_edgar_LL_AT'
empresas_edgar_AT = pd.DataFrame(resultados_por_cik)

# %%
cik_filtrado = empresas_final['CIK'].astype(str).str.zfill(10).unique()
ciks_3000 = cik_filtrado
resultados_por_cik = []

for cik in ciks_3000:
    try:
        # Obter dados da empresa
        company_facts = requests.get(
            f'https://data.sec.gov/api/xbrl/companyfacts/CIK{cik}.json',
            headers=headers
        )

        lucro_liquido = pd.DataFrame(company_facts.json()['facts']['us-gaap']['NetIncomeLoss']['units']['USD'])
        lucro_liquido_10K = lucro_liquido[(lucro_liquido['form'] == '10-K') & (lucro_liquido['fp'] == 'FY')]
        lucro_liquido_10K_ok = lucro_liquido_10K[lucro_liquido_10K['end'].str.slice(0, 4).isin(lucro_liquido_10K['start'].str.slice(0, 4))]
        lucro_liquido_10K_ok = lucro_liquido_10K_ok.drop_duplicates(subset='start')

        # Convertendo a coluna 'start' para o tipo datetime, se ainda não for
        lucro_liquido_10K_ok['start'] = pd.to_datetime(lucro_liquido_10K_ok['start'])

        lucro_liquido_cons = (
            lucro_liquido_10K_ok.groupby('fy')['val']
            .sum()
            .reset_index(name='LL')
            )
        lucro_liquido_cons = lucro_liquido_cons.rename(columns={'fy':'Ano'})

        # Adicionar resultados à lista
        for index, row in lucro_liquido_cons.iterrows():
            resultados_por_cik.append({
                'CIK': cik,
                'Ano': row['Ano'],
                'LL': row['LL'],
            })
    except Exception as e:
        # Lidar com a exceção de forma genérica
        print(f"Erro para CIK {cik}: {e}")
        # Continuar para o próximo CIK
        continue

# Criar o DataFrame final 'empresas_edgar_LL_AT'
empresas_edgar_LL = pd.DataFrame(resultados_por_cik)

# %%
#Merge usando CIK e Ano como chaves
merged_df = pd.merge(empresas_edgar_AT, empresas_edgar_LL, on=['CIK', 'Ano'])
# Adicionar coluna ROA
merged_df['ROA'] = merged_df['LL'] / merged_df['AT']
ROA = merged_df

# %%
#adicionando 0s necessários ao CIK
empresas_edgar_tcc['CIK'] = empresas_edgar_tcc['CIK'].astype(str).str.zfill(10)

# Convertendo as colunas 'CIK' e 'Ano' para o tipo str
empresas_edgar_tcc['CIK'] = empresas_edgar_tcc['CIK'].astype(str)
empresas_edgar_tcc['Ano'] = empresas_edgar_tcc['Ano'].astype(int)

ROA['CIK'] = ROA['CIK'].astype(str)
ROA['Ano'] = ROA['Ano'].astype(int)

# Merge após a conversão
merged_df = pd.merge(empresas_edgar_tcc, ROA, on=['CIK', 'Ano'])
empresas_edgar_tcc = merged_df.drop(['AT', 'LL'], axis = 1)

# %%
# Adicionar a coluna 'empresa' com base no agrupamento do CIK
empresas_edgar_tcc['Empresa'] = empresas_edgar_tcc.groupby('CIK').ngroup() + 1

# %%
#Salvando:
nome_do_arquivo = os.path.join(dir_empresas_edgar, 'empresas_ind_edgar_tcc.csv')

# Salvando o DataFrame em um arquivo CSV
empresas_edgar_tcc.to_csv(nome_do_arquivo, index=False)

print(f'DataFrame salvo em: {nome_do_arquivo}')

# %% [markdown]
# Merge dos dfs cvm e edgar

# %%
caminho_arq = os.path.join(dir_dados, 'dados_tcc_conc.csv')
dados = pd.read_csv(caminho_arq, sep = ',')

# %%
caminho_arq = os.path.join(dir_empresas_edgar, 'empresas_ind_edgar_tcc.csv')
dados_edgar = pd.read_csv(caminho_arq)

# %%
# Filtrando os anos de 2012 a 2022
dados_edgar = dados_edgar[
    (dados_edgar['Ano'] >= 2012) & (dados_edgar['Ano'] <= 2022)
]


# %%
#add leading zeros to CIK (is necessary to acess te company at API)
company_data['cik_str'] = company_data['cik_str'].astype(str).str.zfill(10)

# %%
# adicionando a coluna CNPJ_CIA em edgar:
# Filtrando as linhas onde cik_str está contido em cik_3000
filtered_data = company_data[company_data['cik_str'].isin(cik_filtrado)]

# Criando uma lista de títulos correspondentes
titles_correspondentes = filtered_data['title'].tolist()

# %%
filtered_data = filtered_data.drop(['ticker'], axis = 1)
filtered_data = filtered_data.rename(columns = {'cik_str': 'CIK', 'title':'DENOM_CIA'})

# %%
#adicionando 0s necessários ao CIK
dados_edgar['CIK'] = dados_edgar['CIK'].astype(str).str.zfill(10)
dados_edgar['CIK'] = dados_edgar['CIK'].astype(str)
dados_edgar = pd.merge(dados_edgar, filtered_data[['CIK', 'DENOM_CIA']], on='CIK', how='left')
dados_edgar = dados_edgar.rename(columns = {'CIK':'CNPJ_CIA'})

# %%
# Criando uma função para extrair os valores numéricos de uma string
def extrair_numeros(s):
    return ''.join(filter(str.isdigit, str(s)))

# Aplicando a função de extração para criar uma coluna 'CNPJ_CIA_numerico'
dados_edgar['CNPJ_CIA_numerico'] = dados_edgar['CNPJ_CIA'].apply(extrair_numeros)

# Tratando casos onde a extração resulta em uma string vazia
dados_edgar['CNPJ_CIA_numerico'] = dados_edgar['CNPJ_CIA_numerico'].replace('', '0')

# Convertendo a coluna 'CNPJ_CIA_numerico' para o tipo de dado inteiro
dados_edgar['CNPJ_CIA_numerico'] = dados_edgar['CNPJ_CIA_numerico'].astype(int)

# Obtendo a ordem natural dos CNPJ_CIA
ordenacao_natural = natsorted(dados_edgar['CNPJ_CIA_numerico'].unique())

# Criando uma coluna 'Empresa' com a ordenação numérica, começando do número 169 por grupo
dados_edgar['Empresa'] = dados_edgar['CNPJ_CIA_numerico'].apply(lambda x: ordenacao_natural.index(x) + 169)

# Removendo colunas temporárias utilizadas para a ordenação
dados_edgar = dados_edgar.drop(columns=['CNPJ_CIA_numerico'])



# %%
# Adicionando a coluna 'Pais_k' com todos os valores preenchidos com 2
dados_edgar['Pais_k'] = 2

# Criando uma coluna 'Ano_numerico' para extrair os valores numéricos do ano
dados_edgar['Ano_numerico'] = dados_edgar['Ano'].astype(int)

# Obtendo a ordem natural dos anos
ordenacao_natural_ano = natsorted(dados_edgar['Ano_numerico'].unique())

# Criando uma coluna 'Ano_t' com uma sequência de acordo com o ano, começando do número 1 por grupo
dados_edgar['Ano_t'] = dados_edgar['Ano_numerico'].apply(lambda x: ordenacao_natural_ano.index(x) + 1)

# Removendo coluna temporária utilizada para a ordenação
dados_edgar = dados_edgar.drop(columns=['Ano_numerico'])

# %%
# Salvado:
caminho_arquivo = os.path.join(dir_empresas_edgar, 'dados_edgar.csv')

# Salvar o DataFrame em um arquivo CSV
dados_edgar.to_csv(caminho_arquivo, index=False)

print(f'Dados salvos em: {caminho_arquivo}')

# %%
dados['ANO'] = dados['ANO'].astype(int).astype(str)
dados = dados.rename(columns = {'ANO':'Ano'})
dados_edgar['Ano'] = dados_edgar['Ano'].astype(str)



# %%
dados['Pais_k'] = dados['Pais_k'].astype(int)
dados['Ano_t'] = dados['Ano_t'].astype(int)
dados['Empresa'] = dados['Empresa'].astype(int)
dados_edgar['Pais_k'] = dados_edgar['Pais_k'].astype(int)
dados_edgar['Ano_t'] = dados_edgar['Ano_t'].astype(int)
dados_edgar['Empresa'] = dados_edgar['Empresa'].astype(int)
#concatenado os dfs
dados = pd.concat([dados, dados_edgar], ignore_index=True)

# %%
# Salvado:
caminho_arquivo = os.path.join(dir_empresas_edgar, 'dados_edgar_conc.csv')

# Salvar o DataFrame em um arquivo CSV
dados.to_csv(caminho_arquivo, index=False)

print(f'Dados salvos em: {caminho_arquivo}')


