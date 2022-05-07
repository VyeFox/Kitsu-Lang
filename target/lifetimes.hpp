#include <variant>
#include <tuple>
#include <concepts>
/*
*   Compile time scope objects used to validate the lifetime of objects
*/

namespace kitsu {

    namespace details {

        template<typename T>
        constexpr bool _is_tuple_of_scope = false;

        template<typename T>
        constexpr bool _is_scope = requires {
            {_is_tuple_of_scope<typename T::captures>} -> std::same_as<const bool&>;
            requires _is_tuple_of_scope<typename T::captures>;
        };

        template<typename... Ts>
        constexpr bool _is_tuple_of_scope<std::tuple<Ts...>> =
            (... && _is_scope<Ts>);

        template<typename T, typename S>
        constexpr bool _is_tuple_of_scope_with_capture = false;

        template<typename T, typename S>
        constexpr bool _is_scope_with_capture = requires {
            requires _is_scope<T>;
            requires _is_scope<S>;
            {_is_tuple_of_scope_with_capture<typename T::captures, S>} -> std::same_as<const bool&>;
            requires std::same_as<T, S> || _is_tuple_of_scope_with_capture<typename T::captures, S>;
        };
        
        template<typename S, typename... Ts>
        constexpr bool _is_tuple_of_scope_with_capture<std::tuple<Ts...>, S> =
            (... || _is_scope_with_capture<Ts, S>);

    } using namespace details;

    template<typename T>
    concept scope = _is_scope<T>;

    template<typename T, typename... Ss>
    concept captures =
        (... && _is_scope_with_capture<T, Ss>);

};

