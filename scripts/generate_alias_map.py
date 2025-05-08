import pandas as pd
from collections import defaultdict

biomart_results = pd.read_csv("../data/biomart_results.txt", header=0, sep="\t")

# Generate set of valid symbols
idt_exome_list = pd.read_csv("../data/idt_exome_gene_list.csv", header=0)
valid_idt_exome = set(idt_exome_list.symbol)

problematic_genes = set(["AZFA", "MIP-2A", "ODCP"])

# Iterate through all rows of results and group by HGNC gene symbol
grouped_aliases = defaultdict(list)
for i, row in biomart_results.iterrows():
    approved = row["Approved symbol"]
    alias = row["Alias symbol"]
    previous = row["Previous symbol"]
    if not pd.isna(alias) and alias.upper() not in grouped_aliases[approved.upper()]:
        grouped_aliases[approved.upper()].append(alias.upper())
    if not pd.isna(previous) and previous.upper() not in grouped_aliases[approved.upper()]:
        grouped_aliases[approved.upper()].append(previous.upper())

# For each alias in grouped_aliases, identify the alias in idt_exome_list
not_in_idt_exome_list = []
alias_dict = {}
for canonical, aliases in grouped_aliases.items():
    symbol_list = [canonical] + aliases
    idt_match = None
    for symbol in symbol_list:
        if symbol in valid_idt_exome:
            idt_match = symbol
            break
    if idt_match is not None:
        for symbol in symbol_list:
            if symbol != idt_match:
                alias_dict[symbol] = idt_match
    else:
        not_in_idt_exome_list += symbol_list

# Write outputs
with open("./valid_idt_exome.txt", "w") as outfile:
    for symbol in sorted(list(valid_idt_exome)):
        outfile.write(f'"{symbol.upper()}",\n')

with open("./not_in_idt_exome.txt", "w") as outfile:
    for symbol in not_in_idt_exome_list:
        outfile.write(f'"{symbol.upper()}",\n')

with open("./alias_dict.txt", "w") as outfile:
    for alias, matched in alias_dict.items():
        outfile.write(f'("{alias.upper()}", "{matched}"),\n')



# Identify duplicates / problem genes
#all_gene_symbols = []
#for canonical, aliases in grouped_aliases.items():
#    all_gene_symbols.append(canonical)
#    all_gene_symbols += aliases
#all_gene_symbols.sort()
#
#for symA, symB in zip(all_gene_symbols, all_gene_symbols[1:]):
#    if symA != symB and symA.upper() == symB.upper():
#    print(symA, symB, alias_dict.get(symA), alias_dict.get(symB))
