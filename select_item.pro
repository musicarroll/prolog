select_item(1,X,X).



test_select_item(BitVector,Set,Subset):- length(BitVector,M),length(Set,M),
    include(select_item,BitVector,Set,Subset).