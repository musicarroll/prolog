from pyswip import Prolog

# Initialize Prolog
prolog = Prolog()

# Load the Prolog family knowledge base
prolog.consult("family.pl")

# Query for all grandparent relationships
grandparents = list(prolog.query("grandparent(Grandparent, Grandchild)"))

# Print the results
for relationship in grandparents:
    print(f"{relationship['Grandparent']} is the grandparent of {relationship['Grandchild']}")
