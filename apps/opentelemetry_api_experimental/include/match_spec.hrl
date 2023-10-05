-ifndef(MATCH_SPEC_TYPES_DEFINED).

-type match_var() :: '_' | '$1' | '$2' | '$3' | '$4' | '$5' | '$6' | '$7' | '$8' | '$9'.
-type match_spec(A) :: A | match_var() | {const, A}.

-define(MATCH_SPEC_TYPES_DEFINED, true).

-endif.
