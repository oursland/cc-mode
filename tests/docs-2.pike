#define DataList mixed
//! @decl typedef mixed DataList;
//!
//! Denotes the type of a list of @[Data] values.
//!
//! @seealso
//! @[SymbolHandler.empty_data_list] and
//! @[SymbolHandler.append_value].

#define LaxRxType object(RxNode)|string|array|multiset|mapping
//! @decl typedef RxNode|string|array(LaxRxType)|@
//!               multiset(LaxRxType)|mapping(LaxRxType:LaxRxType) LaxRxType;
//!
//! The type that the user may use anywhere a subnode in a regexp tree
//! is expected when it's built. @[int] is not part of this type.
