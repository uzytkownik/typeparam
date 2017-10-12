//! TypeParam allows to write argument parsing in typesafe manner.
//!
//! TypeParam is a macro taking an annotated structure and generating a structure
//! and implementation for parsing. Internally it uses
//! [clap](https://crates.io/crates/clap).
//!
//! Please note that it is in very early stage of development and not all features
//! are implemented. The interface and defaults are subject to change.
//!
//! # Example
//!
//! ```
//! #[macro_use]
//! extern crate typeparam;
//! extern crate clap;
//! typeparam!{
//!     struct Params [@app test ::std::string::ParseError] {
//!         quiet: bool [QUIET: -q],
//!         verbose: bool [VERBOSE: -v (value: flag)],
//!         cfg: String [CFG: -c (value: default String::from("---"))],
//!         path: String [PATH: -p (value: required)],
//!         foo: Option<String> [FOO: --foo (value: optional)],
//!         n: Option<u32> [N: -n (value: option map (|_v: Option<&str>| Some(3)))],
//!         x: u32 [X: -x (value: map (|_| 4))],
//!         command: @SUBCOMMANDS<Commands> [list => List(List),
//!                                          get => Get2(Get),
//!                                          foo => Foo: (default = Default)]
//!     }
//!     struct List [@subcommand ::std::string::ParseError];
//!     struct Get [@subcommand ::std::string::ParseError];
//! }
//!
//! # // TODO: Autogenerate it
//! # impl ::std::fmt::Debug for List {
//! #    fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
//! #        fmt.debug_struct("List").finish()
//! #    }
//! # }
//! # impl ::std::fmt::Debug for Get {
//! #    fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
//! #        fmt.debug_struct("Get").finish()
//! #    }
//! # }
//! #
//! fn main() {
//!     use typeparam::Command;
//!     let params = Params::parse(["simple", "-v", "--foo", "bar", "-p", "path", "-x", "X"].iter()).unwrap();
//!     assert!(!params.quiet);
//!     assert!(params.verbose);
//!     assert_eq!(params.cfg, "---");
//!     assert_eq!(params.path, "path");
//!     assert_eq!(params.foo, Some(String::from("bar")));
//!     assert_eq!(params.n, Some(3));
//!     assert_eq!(params.x, 4);
//!     match params.command {
//!         Commands::Default => {},
//!         Commands::List(_) | Commands::Get2(_) | Commands::Foo => {
//!             panic!("params.commands != Commands::Default")
//!         }
//!     }
//! }
//! ```
//!
//! In following example it created an parsing structure for application named
//! `test`. Application takes two required arguments - `-p` and `-x` - and
//! three optional ones - `--cfg`, `--foo` and `-n`. In addition it accepts two
//! flags - `-q` anf `-v`.
//!
//! It accepts optionally one of three commands - `list`, `get` or `foo`.
//!
//! After successful parsing it returns a structure containing parsed commands.
//!
//! # Structures and Enums
//!
//! Currently two types of structures are accepted - apps and subcommands.
//! Both have the same syntax with exception of the square brackets after type
//! name.
//!
//! Apps need to have `@app` as first token appearing in square bracket followed
//! by app name and type of error parsing may return.
//!
//! Subcommands need to have `@subcommand` as first token followed by type of
//! error parsing may return.
//!
//! # Parameters and Options
//!
//! Parameters and options are specified as normal fields in struct followed by
//! square brackets containing their name. Optionally after the name there can
//! be colon followed by any number of arguments. Arguments can be specified in
//! any order.
//!
//! To denote a short option a dash with a letter should be specified as argument
//! (`-l`) while long double dash with an identifier (`--foo`). They are not
//! mutually exclusive.
//!
//! ```
//! #[macro_use]
//! extern crate typeparam;
//! extern crate clap;
//! typeparam!{
//!     struct Params [@app test ::std::string::ParseError] {
//!         foo: bool [FOO: --foo],
//!         bar: bool [BAR: -b],
//!         foobar: bool [FOOBAR: -f --foobar]
//!     }
//! }
//!
//! fn main() {
//!     use typeparam::Command;
//!     let params = Params::parse(["simple", "-f", "--foo"].iter()).unwrap();
//!     assert_eq!(params.foo, true);
//!     assert_eq!(params.bar, false);
//!     assert_eq!(params.foobar, true);
//! }
//! ```
//!
//! If either option is omitted the field denote a positional argument.
//!
//! ```
//! #[macro_use]
//! extern crate typeparam;
//! extern crate clap;
//! typeparam!{
//!     struct Params [@app test ::std::string::ParseError] {
//!         foo: String [FOO: (value: required)],
//!         bar: String [BAR: (value: required)],
//!         foobar: bool [FOOBAR: -f]
//!     }
//! }
//!
//! fn main() {
//!     use typeparam::Command;
//!     let params = Params::parse(["simple", "foo", "-f", "bar"].iter()).unwrap();
//!     assert_eq!(params.foo.as_str(), "foo");
//!     assert_eq!(params.bar.as_str(), "bar");
//!     assert_eq!(params.foobar, true);
//! }
//! ```
//!
//! Each option can also take a `value` setting. Currently there are 5 valid
//! settings:
//!
//!   - `value: flag` is default and denotes a single flag - in other words no
//!     argument. Value returned is [`bool`](bool).
//!   - `value: required` denotes an required argument. If user does not passes
//!     it, an error is returned. Otherwise [`fromStr`](std::str::FromStr::from_str)
//!     is called on value passed by user.
//!   - `value: optional` denotes an optional argument. If user does not passes
//!     it [`None`](std::option::Option::None) is returned. Otherwise [`fromStr`](std::str::FromStr::from_str)
//!     is called on value and wrapped by [`Some`](std::option::Option::Some).
//!   - `value: map callback` denotes an required argument, just as `value: required`,
//!      however it allows to supply arbitrary function. Both functions returning value
//!      directly as well as [`Result`](std::result::Result) are accepted.
//!   - `value: option map callback` denotes an optional argument, just as
//!     `value: optional`. However it allows to supply an arbitrary function.
//!      Both functions returning value directly as well as
//!      [`Result`](std::result::Result) are accepted.
//!
//! # Subcommands
//! Subcommands functions as nested apps inside the command. This style has been
//! popularized by (git)[https://git-scm.com/].
//!
//! For application (or recursivly subcommand) to have subcommands it is necessary
//! to add field of type `@SUBCOMMANDS<NameOfEnum>`. There can be at most one such
//! field in struct.
//!
//! ```compile_fail
//! #[macro_use]
//! extern crate typeparam;
//! extern crate clap;
//! typeparam!{
//!     struct IllegalParams [@app illegal ::std::string::ParseError] {
//!         illegal1: @SUBCOMMANDS<Illegal1> [foo => Foo],
//!         illegal2: @SUBCOMMANDS<Illegal2> [foo => Bar]
//!     }
//! }
//! # fn main() {}
//! ```
//!
//! Afterwards the subcommands are specified in square brackets. There are
//! two forms of subcommand specification - including type
//! (`subcommand => Identifier(Type)`) and not (`subcommand => Identifier`).
//! In the first form a type must be a subcommand and the description of fields
//! is taken from there. The second form makes a subcommand not to get any
//! additional fields.
//! In both cases the `Identifier` is added to the enum specified after `@SUBCOMMAND`
//! - with single argument or without any arguments respectivly.
//!
//! ```
//! #[macro_use]
//! extern crate typeparam;
//! extern crate clap;
//! typeparam!{
//!     struct Params [@app test ::std::string::ParseError] {
//!         command: @SUBCOMMANDS<Commands> [list => List, find => Find(Find)],
//!         debug: bool [DEBUG: -d (value: flag)]
//!     }
//! }
//! typeparam!{
//!     struct Find [@subcommand ::std::string::ParseError] {
//!         name: String [NAME: (value: required)],
//!         case_insensitive: bool [CASE_INSENSITIVE: -i (value: flag)]
//!     }
//! }
//!
//! # // TODO: Autogenerate it
//! # impl ::std::fmt::Debug for Find {
//! #    fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
//! #        fmt.debug_struct("Find").field("name", &self.name).field("case_insensitive", &self.case_insensitive).finish()
//! #    }
//! # }
//! #
//! fn main() {
//!    use typeparam::Command;
//!    let list = Params::parse(["simple", "-d", "list"].iter()).unwrap();
//!    assert_eq!(list.debug, true);
//!    match list.command {
//!        Commands::List => {},
//!        _ => panic!("Expected Commands::List")
//!    }
//!    let find = Params::parse(["simple", "-d", "find", "-i", "something"].iter()).unwrap();
//!    assert_eq!(find.debug, true);
//!    match find.command {
//!        Commands::Find(find_cmd) => {
//!            assert_eq!(find_cmd.case_insensitive, true);
//!            assert_eq!(find_cmd.name.as_str(), "something");
//!        },
//!        _ => panic!("Expected Commands::Find(_)")
//!    }
//! }
//! ```
//!
//! Optionally they can be followed by colon and any number of arguments passed.
//! Currently there is only one argument supported - `(default = Value)`. If it is
//! specified `Value` becomes a value of enum when no command is given. Otherwise
//! passing a command is required.
//!
//! ```
//! #[macro_use]
//! extern crate typeparam;
//! extern crate clap;
//! typeparam!{
//!     struct Params [@app test ::std::string::ParseError] {
//!         command: @SUBCOMMANDS<Commands> [list => List, find => Find(Find)],
//!         debug: bool [DEBUG: -d (value: flag)]
//!     }
//!     struct Find [@subcommand ::std::string::ParseError] {
//!         name: String [NAME: (value: required)],
//!         case_insensitive: bool [CASE_INSENSITIVE: -i (value: flag)]
//!     }
//! }
//!
//! # // TODO: Autogenerate it
//! # impl ::std::fmt::Debug for Find {
//! #    fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
//! #        fmt.debug_struct("Find").field("name", &self.name).field("case_insensitive", &self.case_insensitive).finish()
//! #    }
//! # }
//! #
//! fn main() {
//!    use typeparam::Command;
//!    assert!(Params::parse(["simple"].iter()).is_err());
//! }
//! ```

extern crate clap;

#[doc(hidden)]
pub mod clap_export {
    pub use ::clap::{App,AppSettings,Arg,ArgMatches,SubCommand};
}

#[doc(hidden)]
#[macro_export]
macro_rules! typeparam_sanatize_struct_param {
    ({$($data:tt)*} {{} {$($long:tt)*} {$($value:tt)*}} -$s:ident $($tail:tt)*) => {
        typeparam_sanatize_struct_param!{
            {$($data)*}
            {{$s} {$($long)*} {$($value)*}}
            $($tail)*
        }
    };
    ({$($data:tt)*} {{$($short:tt)*} {} {$($value:tt)*}} --$l:ident $($tail:tt)*) => {
        typeparam_sanatize_struct_param!{
            {$($data)*}
            {{$($short)*} {$l} {$($value)*}}
            $($tail)*
        }
    };
    ({$($data:tt)*} {{$($short:tt)*} {$($long:tt)*} {}} (value: flag) $($tail:tt)*) => {
        typeparam_sanatize_struct_param!{
            {$($data)*}
            {{$($short)*} {$($long)*} {flag}}
            $($tail)*
        }
    };
    ({$($data:tt)*} {{$($short:tt)*} {$($long:tt)*} {}} (value: default $E:expr) $($tail:tt)*) => {
        typeparam_sanatize_struct_param!{
            {$($data)*}
            {{$($short)*} {$($long)*} {default $E}}
            $($tail)*
        }
    };
    ({$($data:tt)*} {{$($short:tt)*} {$($long:tt)*} {}} (value: required) $($tail:tt)*) => {
        typeparam_sanatize_struct_param!{
            {$($data)*}
            {{$($short)*} {$($long)*} {required}}
            $($tail)*
        }
    };
    ({$($data:tt)*} {{$($short:tt)*} {$($long:tt)*} {}} (value: optional) $($tail:tt)*) => {
        typeparam_sanatize_struct_param!{
            {$($data)*}
            {{$($short)*} {$($long)*} {optional}}
            $($tail)*
        }
    };
    ({$($data:tt)*} {{$($short:tt)*} {$($long:tt)*} {}} (value: option map $E:expr) $($tail:tt)*) => {
        typeparam_sanatize_struct_param!{
            {$($data)*}
            {{$($short)*} {$($long)*} {option map $E}}
            $($tail)*
        }
    };
    ({$($data:tt)*} {{$($short:tt)*} {$($long:tt)*} {}} (value: map $E:expr) $($tail:tt)*) => {
        typeparam_sanatize_struct_param!{
            {$($data)*}
            {{$($short)*} {$($long)*} {map $E}}
            $($tail)*
        }
    };
    ({$($data:tt)*} {{$($short:tt)*} {$($long:tt)*} {}}) => {
        typeparam_sanatize_struct_param_finish!{$($data)* [{$($short)*} {$($long)*} {flag}]}
    };
    ({$($data:tt)*} {{$($short:tt)*} {$($long:tt)*} {$($value:tt)*}}) => {
        typeparam_sanatize_struct_param_finish!{$($data)* [{$($short)*} {$($long)*} {$($value)*}]}
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! typeparam_sanatize_struct_param_finish {
    (
        [$acc:ident struct $N:ident [$($md:tt)*] {
            $($tail:tt)*
        }]
        [$($prefix:tt)*]
        [$($suffix:tt)*]
        $facc:ident $F:ident $n:ident $T:ty
        {$($params:tt)*}
        [{$($short:tt)*} {$($long:tt)*} {$($value:tt)*}]
    ) => {
        typeparam_sanatize_struct!{
            $acc struct $N [$($md)*] {
                $($tail)*
            } [
                $($prefix)*
                params => {$($params)* ($facc $F $n $T {$($short)*} {$($long)*} {$($value)*})}
                $($suffix)*
            ]
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! typeparam_sanatize_struct_subcommand {
    ({$($data:tt)*} {{} {$($params:tt)*}} $param:ident => $en:ident, $($tail:tt)*) => {
        typeparam_sanatize_struct_subcommand!{{$($data)*} {{} {$($params)* (@subcmd $param ($en) ($crate::EmptySubcommand))}} $($tail)*}
    };
    ({$($data:tt)*} {{} {$($params:tt)*}} $param:ident => $en:ident($typ:ty), $($tail:tt)*) => {
        typeparam_sanatize_struct_subcommand!{{$($data)*} {{} {$($params)* (@subcmd $param ($en($typ)) ($typ))}} $($tail)*}
    };
    ({$($data:tt)*} {{} {$($params:tt)*}} $param:ident => $en:ident: $($tail:tt)*) => {
        typeparam_sanatize_struct_subcommand!{{$($data)*} {{} {$($params)* (@subcmd $param ($en) ($crate::EmptySubcommand))}} : $($tail)*}
    };
    ({$($data:tt)*} {{} {$($params:tt)*}} $param:ident => $en:ident($typ:ty): $($tail:tt)*) => {
        typeparam_sanatize_struct_subcommand!{{$($data)*} {{} {$($params)* (@subcmd $param ($en($typ)) ($typ))}} : $($tail)*}
    };
    ({$($data:tt)*} {{} {$($params:tt)*}} $param:ident => $en:ident) => {
        typeparam_sanatize_struct_subcommand!{{$($data)*} {{} {$($params)* (@subcmd $param ($en) ($crate::EmptySubcommand))}}}
    };
    ({$($data:tt)*} {{} {$($params:tt)*}} $param:ident => $en:ident($typ:ty)) => {
        typeparam_sanatize_struct_subcommand!{{$($data)*} {{} {$($params)* (@subcmd $param ($en($typ)) ($typ))}}}
    };
    ({$($data:tt)*} {{} {$($params:tt)*}} : (default = $def:ident) $($tail:tt)*) => {
        typeparam_sanatize_struct_subcommand!{{$($data)*} {{$def} {$($params)*}} : $($tail)*}
    };
    ({$($data:tt)*} {{$($def:tt)*} {$($params:tt)*}}) => {
        typeparam_sanatize_struct_subcommand!{{$($data)*} {{$($def)*} {$($params)*}} :}
    };
    (
        {
            [$acc:ident struct $N:ident [$($md:tt)*] {
                    $($tail:tt)*
            }]
            [$($prefix:tt)*]
            [$($suffix:tt)*]
            $facc:ident $F:ident $T:ident
        }
        {{$($def:tt)*} {$($params:tt)*}}
        :
    ) => {
        typeparam_sanatize_struct!{
            $acc struct $N [$($md)*] {
                $($tail)*
            } [
                $($prefix)*
                subcommands => {$facc $F $T {$($def)*} {$($params)*}}
                $($suffix)*
            ]
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! typeparam_sanatize_struct {
    ($acc:ident struct $N:ident [$($md:tt)*] { } [$($map:tt)*]) => {
        typeparam_gen_struct!{$acc struct $N [$($md)*] [$($map)*]}
    };
    ($acc:ident struct $N:ident [$($md:tt)*] { , } [$($map:tt)*]) => {
        typeparam_sanatize_struct!($acc struct $N [$($md)*] { } [$($map)*]);
    };
    (
        $acc:ident struct $N:ident [$($md:tt)*] {
            $F:ident : $T:ty [$n:ident: $($settings:tt)*],
            $($tail:tt)*
        } [
            subcommands => { $($subcommands:tt)* }
            params => { $($params:tt)* }
            fields => { $($fields:tt)* }
        ]
    ) => {
        typeparam_sanatize_struct_param!{
            {
                [$acc struct $N [$($md)*] {
                    $($tail)*
                }]
                [subcommands => { $($subcommands)* }]
                [fields => {$($fields)* $F: $T,}]
                PRIV $F $n $T
                {$($params)*}
            }
            { {} {} {} }
            $($settings)*
        }
    };
    (
        $acc:ident struct $N:ident [$($md:tt)*] {
            pub $F:ident : $T:ty [$n:ident: $($settings:tt)*],
            $($tail:tt)*
        } [
            subcommands => { $($subcommands:tt)* }
            params => { $($params:tt)* }
            fields => { $($fields:tt)* }
        ]
    ) => {
        typeparam_sanatize_struct_param!{
            {
                [$acc struct $N [$($md)*] {
                    $($tail)*
                }]
                [subcommands => { $($subcommands)* }]
                [fields => {$($fields)* pub $F: $T,}]
                PUB $F $n $T
                {$($params)*}
            }
            { {} {} {} }
            $($settings)*
        }
    };
    (
        $acc:ident struct $N:ident [$($md:tt)*] {
            $F:ident : @SUBCOMMANDS<$T:ident> [$($settings:tt)*],
            $($tail:tt)*
        } [
            subcommands => { }
            params => { $($params:tt)* }
            fields => { $($fields:tt)* }
        ]
    ) => {
        typeparam_sanatize_struct_subcommand!{
            {
                [$acc struct $N [$($md)*] {
                    $($tail)*
                }]
                []
                [params => { $($params)* } fields => {$($fields)* $F: $T,}]
                PRIV $F $T
            }
            {{} {}}
            $($settings)*
        }
    };
    (
        $acc:ident struct $N:ident [$($md:tt)*] {
            $F:ident : @SUBCOMMANDS<$T:ident> [$($settings:tt)*],
            $($tail:tt)*
        } [
            subcommands => { }
            params => { $($params:tt)* }
            fields => { $($fields:tt)* }
        ]
    ) => {
        typeparam_sanatize_struct_subcommand!{
            {
                [$acc struct $N [$($md)*] {
                    $($tail)*
                }]
                []
                [params => { $($params)* } fields => {$($fields)* pub $F: $T,}]
                PUB $F $T
            }
            {{} {}}
            $($settings)*
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! typeparam_gen_enum {
    () => {};
    (PRIV $F:ident $T:ident { $def:ident } {$((@subcmd $param:ident ($($el:tt)*) ($typ:ty)))*}) => {
        #[derive(Debug)]
        enum $T {
            $def,
            $($($el)*),*
        }
    };
    (PRIV $F:ident $T:ident { } {$((@subcmd $param:ident ($($el:tt)*) ($typ:ty)))*}) => {
        #[derive(Debug)]
        enum $T {
            $($($el)*),*
        }
    };
    (PUB $F:ident $T:ident { $def:ident } {$((@subcmd $param:ident ($($el:tt)*) ($typ:ty)))*}) => {
        #[derive(Debug)]
        pub enum $T {
            $def,
            $($($el)*),*
        }
    };
    (PUB $F:ident $T:ident { } {$((@subcmd $param:ident ($($el:tt)*) ($typ:ty)))*}) => {
        #[derive(Debug)]
        pub $facc enum $T {
            $($($el)*),*
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! typeparam_gen_struct_hlp {
    (PRIV struct $N:ident {$($fields:tt)*}) => {
        struct $N {
            $($fields)*
        }
    };
    (PUB struct $N:ident {$($fields:tt)*}) => {
        pub struct $N {
            $($fields)*
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! typeparam_gen_commands {
    (($app:expr)) => {$app};
    (($app:expr) $facc:ident $F:ident $T:ident { $($def:tt)+ } { }) => {$app};
    (($app:expr) $facc:ident $F:ident $T:ident { } { }) => {
        $app.setting($crate::clap_export::AppSettings::SubcommandRequired)
    };
    (($app:expr) $facc:ident $F:ident $T:ident { $($def:tt)* } {(@subcmd $subcmd:ident ($($fld:tt)*) ($t:ty)) $($tail:tt)*}) => {{
        fn subcommand<T : $crate::SubCommand>(name: &'static str) -> $crate::clap_export::App<'static, 'static> {
            T::subcommand(name)
        }
        let app = $app.subcommand(subcommand::<$t>(stringify!($subcmd)));
        typeparam_gen_commands!((app) $facc $F $T { $($def)* } {$($tail)*})
    }}
}

#[doc(hidden)]
#[macro_export]
macro_rules! typeparam_gen_params_flg {
    (($arg:expr) {$short:ident} {$($long:tt)*}) => {{
        let arg = $arg.short(stringify!($short));
        typeparam_gen_params_flg!((arg) {} {$($long)*})
    }};
    (($arg:expr) {} {$long:ident}) => {{
        let arg = $arg.long(stringify!($long));
        typeparam_gen_params_flg!((arg) {} {})
    }};
    (($arg:expr) {} {}) => {
        $arg
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! typeparam_gen_params {
    (($app:expr) ($facc:ident $F:ident $n:ident $T:ty {$($short:tt)*} {$($long:tt)*} {flag}) $($tail:tt)*) => {{
        let arg = typeparam_gen_params_flg!((::clap::Arg::with_name(stringify!($n))) {$($short)*} {$($long)*});
        typeparam_gen_params!(($app.arg(arg.takes_value(false).required(false))) $($tail)*)
    }};
    (($app:expr) ($facc:ident $F:ident $n:ident $T:ty {$($short:tt)*} {$($long:tt)*} {default $E:expr}) $($tail:tt)*) => {{
        let arg = typeparam_gen_params_flg!((::clap::Arg::with_name(stringify!($n))) {$($short)*} {$($long)*});
        typeparam_gen_params!(($app.arg(arg.takes_value(true).required(false))) $($tail)*)
    }};
    (($app:expr) ($facc:ident $F:ident $n:ident $T:ty {$($short:tt)*} {$($long:tt)*} {optional}) $($tail:tt)*) => {{
        let arg = typeparam_gen_params_flg!((::clap::Arg::with_name(stringify!($n))) {$($short)*} {$($long)*});
        typeparam_gen_params!(($app.arg(arg.takes_value(true).required(false))) $($tail)*)
    }};
    (($app:expr) ($facc:ident $F:ident $n:ident $T:ty {$($short:tt)*} {$($long:tt)*} {required}) $($tail:tt)*) => {{
        let arg = typeparam_gen_params_flg!((::clap::Arg::with_name(stringify!($n))) {$($short)*} {$($long)*});
        typeparam_gen_params!(($app.arg(arg.takes_value(true).required(true))) $($tail)*)
    }};
    (($app:expr) ($facc:ident $F:ident $n:ident $T:ty {$($short:tt)*} {$($long:tt)*} {option map $E:expr}) $($tail:tt)*) => {{
        let arg = typeparam_gen_params_flg!((::clap::Arg::with_name(stringify!($n))) {$($short)*} {$($long)*});
        typeparam_gen_params!(($app.arg(arg.takes_value(true).required(false))) $($tail)*)
    }};
    (($app:expr) ($facc:ident $F:ident $n:ident $T:ty {$($short:tt)*} {$($long:tt)*} {map $E:expr}) $($tail:tt)*) => {{
        let arg = typeparam_gen_params_flg!((::clap::Arg::with_name(stringify!($n))) {$($short)*} {$($long)*});
        typeparam_gen_params!(($app.arg(arg.takes_value(true).required(true))) $($tail)*)
    }};
    (($app:expr)) => {$app};
}

#[derive(Copy, Clone, Debug)]
#[doc(hidden)]
pub struct Res<T, E>(pub Result<T, E>);

impl<T, E> From<T> for Res<T, E> {
    fn from(t: T) -> Res<T, E> {
        Res(Ok(t))
    }
}

impl<T, E2, E : Into<E2>> From<Result<T, E>> for Res<T, E2> {
    fn from(r: Result<T, E>) -> Res<T, E2> {
        match r {
            Ok(t) => Res(Ok(t)),
            Err(e) => Res(Err(E::into(e)))
        }
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! typeparam_gen_new_param_get {
    (($match:expr) ($err:ty) $facc:ident $F:ident $n:ident $T:ty {$($short:tt)*} {$($long:tt)*} {flag}) => {
        $match.is_present(stringify!($n))
    };
    (($match:expr) ($err:ty) $facc:ident $F:ident $n:ident $T:ty {$($short:tt)*} {$($long:tt)*} {default $E:expr}) => {
        $match.value_of(stringify!($n)).map_or_else(|| Ok($E), |val| val.parse::<$T>(),)?
    };
    (($match:expr) ($err:ty) $facc:ident $F:ident $n:ident $T:ty {$($short:tt)*} {$($long:tt)*} {required}) => {
        $match.value_of(stringify!($n)).map(|val| val.parse::<$T>()).unwrap()?
    };
    (($match:expr) ($err:ty) $facc:ident $F:ident $n:ident $T:ty {$($short:tt)*} {$($long:tt)*} {optional}) => {{
        use std::str::FromStr;
        trait OptionParse where Self : Sized {
            type Error;
            fn parse(string: Option<&str>) -> Result<Self, Self::Error>;
        }
        impl<T : FromStr> OptionParse for Option<T> {
            type Error = T::Err;
            fn parse(string: Option<&str>) -> Result<Self, Self::Error> {
                match string {
                    Some(ref val) => Ok(Some(val.parse::<T>()?)),
                    None => Ok(None)
                }
            }
        }
        fn parse<T : OptionParse>(val: Option<&str>) -> Result<T, T::Error> {
            T::parse(val)
        }
        parse::<$T>($match.value_of(stringify!($n)))?
    }};
    (($match:expr) ($err:ty) $facc:ident $F:ident $n:ident $T:ty {$($short:tt)*} {$($long:tt)*} {option map $E:expr}) => {{
        fn coerce<R : Into<$crate::Res<$T, $err>>, F : FnOnce(Option<&str>) -> R>(f: F, val: Option<&str>) -> Result<$T, $err> {
            R::into(f(val)).0
        }
        coerce($E, $match.value_of(stringify!($n)))?
    }};
    (($match:expr) ($err:ty) $facc:ident $F:ident $n:ident $T:ty {$($short:tt)*} {$($long:tt)*} {map $E:expr}) => {{
        fn coerce<R : Into<$crate::Res<$T, $err>>, F : FnOnce(&str) -> R>(f: F, val: &str) -> Result<$T, $err> {
            R::into(f(val)).0
        }
        coerce($E, $match.value_of(stringify!($n)).unwrap())?
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! typeparam_gen_new_command_get {
    (($match:expr) $T:ident { $($def:tt)* } { (@subcmd $subcmd:ident ($fld:ident($typ:ty)) ( $typ2:ty)) $($tail:tt)* }) => {{
        let mch = $match;
        if let Some(submatch) = mch.subcommand_matches(stringify!($subcmd)) {
            fn gen_subcmd<T : $crate::SubCommand>(am: &$crate::clap_export::ArgMatches) -> Result<T, T::Error> {
                T::new(am)
            }
            $T::$fld(gen_subcmd::<$typ>(submatch)?) 
        } else {
            typeparam_gen_new_command_get!((mch) $T { $($def)* } { $($tail)* })
        }
    }};
    (($match:expr) $T:ident { $($def:tt)* } { (@subcmd $subcmd:ident ($fld:ident) ( $typ2:ty)) $($tail:tt)* }) => {{
        let mch = $match;
        if mch.subcommand_name() == Some(stringify!($subcmd)) {
            $T::$fld
        } else {
            typeparam_gen_new_command_get!((mch) $T { $($def)* } { $($tail)* })
        }
    }};
    (($match:expr) $T:ident { $def:ident } { }) => {
        $T::$def
    };
    (($match:expr) $T:ident { } { }) => {
        panic!("No command given")
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! typeparam_gen_new {
    (($match:expr) $N:ident $err:ty {$($gen:tt)*} [subcommands => {$facc:ident $F:ident $T:ident { $($def:tt)* } { $($subcmd:tt)*} } params => {$($params:tt)*}]) => {{
        let mch = $match;
        let val: $T = typeparam_gen_new_command_get!((mch) $T { $($def)* } { $($subcmd)*});
        typeparam_gen_new!((mch) $N $err {$($gen)* $F: val,} [params => { $($params)* }])
    }};
    (($match:expr) $N:ident $err:ty {$($gen:tt)*} [subcommands => {} params => {$($params:tt)*}]) => {
        typeparam_gen_new!(($match) $N $err {$($gen)*} [params => { $($params)* }])
    };
    (($match:expr) $N:ident $err:ty {$($gen:tt)*} [params => { ($facc:ident $F:ident $n:ident $T:ty {$($short:tt)*} {$($long:tt)*} {$($value:tt)*}) $($params:tt)*}]) => {{
        let mch = $match;
        let val: $T = typeparam_gen_new_param_get!((mch) ($err) $facc $F $n $T {$($short)*} {$($long)*} {$($value)*});
        typeparam_gen_new!((mch) $N $err {$($gen)* $F: val,} [params => { $($params)* }])
    }};
    (($match:expr) $N:ident $err:ty {$($gen:tt)*} [params => {}]) => {{
        Ok($N {
            $($gen)*
        })
    }}
}

#[doc(hidden)]
#[macro_export]
macro_rules! typeparam_gen_impl {
    ($acc:ident struct $N:ident [@app $app:ident $err:ty] [subcommands => { $($subcmd:tt)* } params => { $($params:tt)* }]) => {
        impl $crate::Command for $N {
            type Error = $err;
            fn command() -> $crate::clap_export::App<'static, 'static> {
                let app = $crate::clap_export::App::new(stringify!($app));
                let app = app.settings(&[
                    $crate::clap_export::AppSettings::StrictUtf8
                ]);
                let app = typeparam_gen_commands!((app) $($subcmd)*);
                let app = typeparam_gen_params!((app) $($params)*);
                app
            }
            fn new(matches: &$crate::clap_export::ArgMatches) -> Result<Self, Self::Error> {
                typeparam_gen_new!((&matches) $N $err {} [subcommands => { $($subcmd)* } params => { $($params) * }])
            }
        }
    };
    ($acc:ident struct $N:ident [@subcommand $err:ty] [subcommands => { $($subcmd:tt)* } params => { $($params:tt)* }]) => {
        impl $crate::SubCommand for $N {
            type Error = $err;
            fn subcommand(name: &'static str) -> $crate::clap_export::App<'static, 'static> {
                let app = $crate::clap_export::SubCommand::with_name(name);
                let app = typeparam_gen_commands!((app) $($subcmd)*);
                let app = typeparam_gen_params!((app) $($params)*);
                app
            }
            fn new(_am: &$crate::clap_export::ArgMatches) -> Result<Self, Self::Error> {
                typeparam_gen_new!((&_am) $N $err {} [subcommands => { $($subcmd)* } params => { $($params) * }])
            }
        } 
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! typeparam_gen_struct {
    ($acc:ident struct $N:ident [$($md:tt)*] [subcommands => { $($subcmd:tt)* } params => { $($params:tt)* } fields => { $($fields:tt)* }]) => {
        typeparam_gen_enum!($($subcmd)*);
        typeparam_gen_struct_hlp!($acc struct $N { $($fields)* });
        typeparam_gen_impl!($acc struct $N [$($md)*] [subcommands => { $($subcmd)* } params => { $($params)* }]);
    };
}

#[macro_export]
macro_rules! typeparam {
    (struct $N:ident [$($md:tt)*] { $($struct:tt)+ } $($tail:tt)*) => {
        typeparam_sanatize_struct!{
            PRIV struct $N [$($md)*] {
                $($struct)*,
            } [
                subcommands => { }
                params => { }
                fields => { }
            ]
        }
        typeparam!{$($tail)*}
    };
    (struct $N:ident [$($md:tt)*]; $($tail:tt)*) => {
        typeparam_sanatize_struct!{
            PRIV struct $N [$($md)*] { } [
                subcommands => { }
                params => { }
                fields => { }
            ]
        }
        typeparam!{$($tail)*}
    };
    () => {};
}

#[derive(Debug)]
pub enum Error<T> {
    Parse(clap::Error),
    Command(T)
}

/// Commands are main result of [`typeparam!`](typeparam!) macro.
pub trait Command where Self : Sized {
    /// Error that may be returned by the user part of the process.
    type Error;
    /// Starts the automated parsing process with passed iterator.
    ///
    /// **NOTE** `--help` and `--version` are returned as errors.
    ///
    /// **NOTE** Note that first element of iteration is considered name of
    /// binary.
    fn parse<I : IntoIterator<Item = T>, T : Into<std::ffi::OsString> + Clone>(itr: I) -> Result<Self, Error<Self::Error>> {
        Self::new(&Self::command().get_matches_from_safe(itr).map_err(|e| Error::Parse(e))?).map_err(|e| Error::Command(e))
    }
    /// Starts the automated parsing process with commandline arguments.
    ///
    /// **NOTE** `--help` and `--version` are returned as errors.
    ///
    /// **NOTE** Note that command name is considered just a binary name.
    fn parse_args() -> Result<Self, Error<Self::Error>> {
        Self::parse(std::env::args())
    }
    /// Starts the automated parsing process with passed iterator.
    ///
    /// **NOTE** `--help` and `--version` are returned as errors.
    ///
    /// **NOTE** Note that first element of iteration is considered name of
    /// binary.
    fn parse_any<I : IntoIterator<Item = T>, T : Into<std::ffi::OsString> + Clone, E : From<Self::Error> + From<clap::Error>>(itr: I) -> Result<Self, E> {
        match Self::parse(itr) {
            Ok(res) => Ok(res),
            Err(Error::Parse(e)) => Err(E::from(e)),
            Err(Error::Command(e)) => Err(E::from(e))
        }
    }
    /// Starts the automated parsing process with commandline arguments.
    ///
    /// **NOTE** `--help` and `--version` are returned as errors.
    ///
    /// **NOTE** Note that command name is considered just a binary name.
    fn parse_any_args<E : From<Self::Error> + From<clap::Error>>() -> Result<Self, E> {
        Self::parse_any(std::env::args())
    }
    /// Starts the automated parsing process with passed iterator. If parsing
    /// fails an error is printed and process exits.
    ///
    /// **NOTE** Note that command name is considered just a binary name.
    fn parse_or_exit<I : IntoIterator<Item = T>, T : Into<std::ffi::OsString> + Clone>(itr: I) -> Self where Self::Error : std::fmt::Display {
        let mut app = Self::command();
        let res = app.get_matches_from_safe_borrow(itr).map_err(|e| Error::Parse(e)).and_then(|mch| {
            Self::new(&mch).map_err(|e| Error::Command(e))
        });
        match res {
            Ok(res) => res,
            Err(Error::Parse(e)) => e.exit(),
            Err(Error::Command(e)) => {
                use std::io::Write;
                let stderr = std::io::stderr();
                writeln!(&mut stderr.lock(), "{:}", e).unwrap();
                std::process::exit(1)
            }
        }
    }
    /// Starts the automated parsing process with passed iterator. If parsing
    /// fails an error is printed and process exits.
    ///
    /// **NOTE** Note that command name is considered just a binary name.
    fn parse_args_or_exit() -> Self where Self::Error : std::fmt::Display {
        Self::parse_or_exit(std::env::args())
    }
    /// Creates an [`App`](clap::App) based on specified command
    fn command() -> clap::App<'static, 'static>;
    /// Creates structure based on matched arguments.
    fn new(mch: &clap::ArgMatches) -> Result<Self, Self::Error>;
}

#[doc(hidden)]
pub trait SubCommand where Self : Sized {
    type Error;
    fn subcommand(name: &'static str) -> clap::App<'static, 'static>;
    fn new(mch: &clap::ArgMatches) -> Result<Self, Self::Error>;
}

#[derive(Copy, Clone, Debug)]
#[doc(hidden)]
pub enum EmptySubcommand {}

impl SubCommand for EmptySubcommand {
    type Error = std::string::ParseError;
    fn subcommand(name: &'static str) -> clap::App<'static, 'static> {
        clap::SubCommand::with_name(name)
    }
    fn new(_: &clap::ArgMatches) -> Result<Self, Self::Error> {
        panic!("EmptySubcommand::new should never be called");
    }
}

