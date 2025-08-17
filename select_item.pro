% select_item/3 - defines the select item predicate.
select_item(1,X,X).



% test_select_item/3 - defines the test select item predicate.
test_select_item(BitVector,Set,Subset):- length(BitVector,M),length(Set,M),
% include/4 - defines the include predicate.
    include(select_item,BitVector,Set,Subset).
