//#############################################################################
// Eggs.Storage
//
// Copyright Agustin K-ballo Berge, Fusion Fenix 2019
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef EGGS_STORAGE_HPP
#define EGGS_STORAGE_HPP

#include <cstddef>
#include <initializer_list>
#include <new>
#include <type_traits>
#include <utility>

#if __cplusplus < 201103L
#  if !defined(_MSC_VER) || _MSC_VER < 1900
#    error Eggs.Storage requires compiler and library support for the ISO C++ 2011 standard.
#  endif
#endif

namespace eggs
{
    namespace detail
    {
        ///////////////////////////////////////////////////////////////////////
#if __cpp_lib_byte >= 201603L
        using std::byte;
#else
        using byte = unsigned char;
#endif

        ///////////////////////////////////////////////////////////////////////
#if __cpp_lib_launder >= 201606L
        using std::launder;
#else
        template <typename T>
        static constexpr T* launder(T* p) noexcept
        {
            return p;
        }
#endif

        ///////////////////////////////////////////////////////////////////////
        template <bool C>
        struct conditionally_copyable
        {};

        template <>
        struct conditionally_copyable<false>
        {
            conditionally_copyable() noexcept = default;
            conditionally_copyable(conditionally_copyable const&) = delete;
            conditionally_copyable& operator=(conditionally_copyable const&) = delete;
        };

        ///////////////////////////////////////////////////////////////////////
        template <std::size_t X>
        struct ispow2
          : std::integral_constant<bool, (X & (X - 1)) == 0>
        {};

        template <>
        struct ispow2<0u>
          : std::false_type
        {};

        ///////////////////////////////////////////////////////////////////////
        template <std::size_t Len>
        struct next2
          : std::integral_constant<std::size_t, (next2<(Len >> 1)>::value << 1)>
        {};

        template <>
        struct next2<0u>
          : std::integral_constant<std::size_t, 1>
        {};

        ///////////////////////////////////////////////////////////////////////
        template <std::size_t Len>
        struct floor2
          : std::integral_constant<std::size_t, (next2<Len>::value >> 1)>
        {};

        template <>
        struct floor2<0u>
          : std::integral_constant<std::size_t, 0>
        {};

        ///////////////////////////////////////////////////////////////////////
        template <std::size_t Len>
        struct max_align
          : std::conditional<
                Len < alignof(std::max_align_t), floor2<Len>,
                std::integral_constant<std::size_t,  alignof(std::max_align_t)>
            >::type
        {};

        template <>
        struct max_align<0u>
          : std::integral_constant<std::size_t, 1>
        {};

        ///////////////////////////////////////////////////////////////////////
        template <std::size_t... Vs>
        struct static_max;

        template <>
        struct static_max<>
          : std::integral_constant<std::size_t, 0>
        {};

        template <std::size_t V>
        struct static_max<V>
          : std::integral_constant<std::size_t, V>
        {};

        template <std::size_t V0, std::size_t V1, std::size_t... Vs>
        struct static_max<V0, V1, Vs...>
          : static_max<(V0 < V1 ? V1 : V0), Vs...>::type
        {};

        ///////////////////////////////////////////////////////////////////////
        template <bool... Vs>
        struct static_and;

        template <>
        struct static_and<>
          : std::integral_constant<bool, true>
        {};

        template <bool V>
        struct static_and<V>
          : std::integral_constant<bool, V>
        {};

        template <bool V0, bool V1, bool... Vs>
        struct static_and<V0, V1, Vs...>
          : static_and<(V0 && V1), Vs...>::type
        {};
    }

    ///////////////////////////////////////////////////////////////////////////
    //! template <std::size_t Len, std::size_t Align = default-alignment,
    //!   bool Copyable = false>
    //! class storage;
    //!
    //! The `storage` class template provides storage suitable for use as
    //!  uninitialized storage for any object whose size is at most `Len` and
    //!  whose alignment is a divisor of `Align`. The value of
    //!  _default-alignment_ shall be the most stringent alignment requirement
    //!  for any C++ object type whose size is no greater than `Len`.
    //!
    //! \mandates `Align` is equal to `alignof(T)` for some type `T` or to
    //!  _default-alignment_.
    //!
    //! \remarks Each specialization of `storage` is a standard-layout class
    //!  type. Each specialization of `storage` for which `Copyable` is `true`
    //!  is a trivially copyable class type.
    template <
        std::size_t Len,
        std::size_t Align = detail::max_align<Len>::value,
        bool Copyable = false>
    class storage
    {
        static_assert(
            detail::ispow2<Align>::value,
            "Align shall be a power of 2");

        template <typename T>
        struct _assert_constraints
        {
            static_assert(
                std::is_object<T>::value,
                "T shall be an object type");
            static_assert(
                sizeof(T) <= Len,
                "Storage size not suitable for T");
            static_assert(
                Align % alignof(T) == 0,
                "Storage alignment not suitable for T");
            using type = T;
        };

    public:
        //! storage() = default;
        //!
        //! \remarks This constructor is trivial.
        storage() = default;

        //! storage(storage const&) = default;
        //!
        //! \remarks This constructor shall be defined as deleted unless
        //!  `Copyable` is `true`. If `Copyable` is `true`, this constructor
        //!  is trivial.
        storage(storage const&) = default;

        //! storage& operator=(storage const&) = default;
        //!
        //! \remarks This operator shall be defined as deleted unless
        //!  `Copyable` is `true`. If `Copyable` is `true`, this operator
        //!  is trivial.
        //!
        //! \returns `*this`.
        storage& operator=(storage const&) = default;

        //! template <class T, class... Args>
        //! T* construct(Args&&... args);
        //!
        //! \mandates The provided storage is suitable for an object of type
        //!  `T`. `std::is_constructible_v<T, Args&&...>` is `true`.
        //!
        //! \effects Initializes an object nested within `*this` as if
        //!  direct-non-list-initializing an object of type `T` at `target()`
        //!  with the arguments `std::forward<Args>(args)...`.
        //!
        //! \remarks The destructor for any object that is located in the
        //!  provided storage will not be implicitly called.
        //!
        //! \returns A pointer to the object located at `target()`.
        template <typename T, typename... Args,
            typename = typename _assert_constraints<T>::type>
        T* construct(Args&&... args) noexcept(
            std::is_nothrow_constructible<T, Args...>::value)
        {
            return ::new (target()) T(std::forward<Args>(args)...);
        }

        //! template <class T, class U, class... Args>
        //! T* construct(std::initializer_list<U>&& il, Args&&... args);
        //!
        //! \effects Equivalent to `return construct<T>(il,
        //!  std::forward<Args>(args)...)`.
        template <typename T, typename U, typename... Args,
            typename = typename _assert_constraints<T>::type>
        T* construct(std::initializer_list<U>&& il, Args&&... args) noexcept(
            noexcept(construct<T>(il, std::forward<Args>(args)...)))
        {
            return construct<T>(il, std::forward<Args>(args)...);
        }

        //! template <class T>
        //! void destroy();
        //!
        //! \mandates The provided storage is suitable for an object of type
        //!  `T`. `std::is_destructible_v<T>` is `true`.
        //!
        //! \expects An object that is within its lifetime and whose type is
        //!  similar to `T` is located at `target()`.
        //!
        //! \effects Destroys the object located at `target()` as if by
        //!  `target<T>()->~T()`.
        template <typename T,
            typename = typename _assert_constraints<T>::type>
        void destroy() noexcept(
            std::is_nothrow_destructible<T>::value)
        {
            target<T>()->~T();
        }

        //! void* target() noexcept;
        //! void const* target() const noexcept
        //!
        //! \returns A pointer to the first byte of the provided storage.
        void* target() noexcept
        {
            return static_cast<void*>(&_buffer[0]);
        }
        void const* target() const noexcept
        {
            return static_cast<void const*>(&_buffer[0]);
        }

        //! template <class T>
        //! T* target() noexcept;
        //! template <class T>
        //! T const* target() const noexcept
        //!
        //! \mandates The provided storage is suitable for an object of type
        //!  `T`.
        //!
        //! \expects An object that is within its lifetime and whose type is
        //!  similar to `T` is located at `target()`.
        //!
        //! \returns A pointer to the object located at `target()`.
        template <typename T,
            typename = typename _assert_constraints<T>::type>
        T* target() noexcept
        {
            return detail::launder(static_cast<T*>(target()));
        }
        template <typename T,
            typename = typename _assert_constraints<T>::type>
        T const* target() const noexcept
        {
            return detail::launder(static_cast<T const*>(target()));
        }

    private:
        union
        {
            alignas(Align) detail::byte _buffer[Len != 0 ? Len : 1];
            detail::conditionally_copyable<Copyable> _copyable;
        };
    };

    //! template <std::size_t Len, std::size_t Align = default-alignment>
    //! using copyable_storage = storage<Len, Align, true>;
    template <
        std::size_t Len,
        std::size_t Align = detail::max_align<Len>::value>
    using copyable_storage = storage<Len, Align, /*Copyable=*/true>;

    //! template <typename... Types>
    //! using storage_for = storage<see-below>;
    //!
    //! Let `Len` be the largest size of all types in `Types`. Let `Align` be
    //!  the strictest alignment of all types in `Types`. Let `Copyable` be
    //!  `true` if all types in `Types` are trivially copyable, `false`
    //!  otherwise.
    //!
    //! The type `storage_for<Types...>` denotes the type `storage<Len, Align,
    //!  Copyable>`.
    template <typename... Types>
    using storage_for = storage<
        detail::static_max<sizeof(Types)...>::value,
        detail::static_max<1, alignof(Types)...>::value,
        /*Copyable=*/detail::static_and<
            std::is_trivially_copyable<Types>::value...>::value>;

    //! template <typename... Types>
    //! using copyable_storage_for = copyable_storage<see-below>;
    //!
    //! Let `Len` be the largest size of all types in `Types`. Let `Align` be
    //!  the strictest alignment of all types in `Types`.
    //!
    //! The type `copyable_storage_for<Types...>` denotes the type
    //!  `copyable_storage<Len, Align>`.
    template <typename... Types>
    using copyable_storage_for = copyable_storage<
        detail::static_max<sizeof(Types)...>::value,
        detail::static_max<1, alignof(Types)...>::value>;

    ///////////////////////////////////////////////////////////////////////////
    //! template <std::size_t Len, std::size_t Align = default-alignment>
    //! struct aligned_storage;
    //!
    //! The value of _default-alignment_ shall be the most stringent alignment
    //!  requirement for any C++ object type whose size is no greater than
    //!  `Len`. The member typedef `type` shall be a trivial standard-layout
    //!  type suitable for use as uninitialized storage for any object whose
    //!  size is at most `Len` and whose alignment is a divisor of `Align`.
    //!
    //! \requires `Len` shall not be zero. `Align` shall be equal to
    //!  `alignof(T)` for some type `T` or to _default-alignment_.
    template <
        std::size_t Len,
        std::size_t Align = detail::max_align<Len>::value>
    struct aligned_storage
    {
        static_assert(
            Len != 0,
            "Len shall not be zero");
        static_assert(
            detail::ispow2<Align>::value,
            "Align shall be a power of 2");

        using type = storage<Len, Align, /*Copyable=*/true>;
    };

    //! template <std::size_t Len, std::size_t Align = default-alignment>
    //! using aligned_storage_t = typename aligned_storage<Len, Align>::type;
    template <
        std::size_t Len,
        std::size_t Align = detail::max_align<Len>::value>
    using aligned_storage_t = typename aligned_storage<Len, Align>::type;

    ///////////////////////////////////////////////////////////////////////////
    //! template <std::size_t Len, class... Types>
    //! struct aligned_union;
    //!
    //! The member typedef `type` shall be a trivial standard-layout type
    //!  suitable for use as uninitialized storage for any object whose type
    //!  is listed in `Types`; its size shall be at least `Len`. The static
    //!  member `alignment_value` shall be an integral constant of type
    //!  `std::size_t` whose value is the strictest alignment of all types
    //!  listed in `Types`.
    //!
    //! \requires At least one type is provided. Each type in the template
    //!  parameter pack `Types` shall be a complete object type.
    template <
        std::size_t Len,
        typename... Types>
    struct aligned_union
    {
        static_assert(
            sizeof...(Types) > 0,
            "At least one type shall be provided.");

        static constexpr std::size_t _length =
           detail::static_max<Len, sizeof(Types)...>::value;
        static constexpr std::size_t alignment_value =
          detail::static_max<alignof(Types)...>::value;

        using type = storage<_length, alignment_value, /*Copyable=*/true>;
    };

    //! template <std::size_t Len, class... Types>
    //! using aligned_union_t = typename aligned_union<Len, Types...>::type;
    template <
        std::size_t Len,
        typename... Types>
    using aligned_union_t = typename aligned_union<Len, Types...>::type;
}

#endif /*EGGS_STORAGE_HPP*/
