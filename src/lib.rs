//! Typeparam allows to write argument parsing in typesafe manner 


extern crate clap;

#[allow(unused_macros)]
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

#[allow(unused_macros)]
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

#[allow(unused_macros)]
macro_rules! typeparam_sanatize_struct_subcommand {
    ({$($data:tt)*} {{} {$($params:tt)*}} $param:ident => $en:ident, $($tail:tt)*) => {
        typeparam_sanatize_struct_subcommand!{{$($data)*} {{} {$($params)* (@subcmd $param ($en) ($crate::EmptySubcommand))}} # $($tail)*}
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

#[allow(unused_macros)]
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

#[allow(unused_macros)]
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

#[allow(unused_macros)]
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

#[allow(unused_macros)]
macro_rules! typeparam_gen_commands {
    (($app:expr)) => {$app};
    (($app:expr) $facc:ident $F:ident $T:ident { $($def:tt)+ } { }) => {$app};
    (($app:expr) $facc:ident $F:ident $T:ident { } { }) => {
        $app.setting($crate::clap::AppSettings::SubcommandRequired)
    };
    (($app:expr) $facc:ident $F:ident $T:ident { $($def:tt)* } {(@subcmd $subcmd:ident ($($fld:tt)*) ($t:ty)) $($tail:tt)*}) => {{
        fn subcommand<T : $crate::SubCommand>(name: &'static str) -> $crate::clap::App<'static, 'static> {
            T::subcommand(name)
        }
        let app = $app.subcommand(subcommand::<$t>(stringify!($subcmd)));
        typeparam_gen_commands!((app) $facc $F $T { $($def)* } {$($tail)*})
    }}
}

#[allow(unused_macros)]
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

#[allow(unused_macros)]
macro_rules! typeparam_gen_params {
    (($app:expr) ($facc:ident $F:ident $n:ident $T:ty {$($short:tt)*} {$($long:tt)*} {flag}) $($tail:tt)*) => {{
        let arg = typeparam_gen_params_flg!((::clap::Arg::with_name(stringify!($n))) {$($short)*} {$($long)*});
        typeparam_gen_params!(($app.arg(arg)) $($tail)*)
    }};
    (($app:expr) ($facc:ident $F:ident $n:ident $T:ty {$($short:tt)*} {$($long:tt)*} {default $E:expr}) $($tail:tt)*) => {{
        let arg = typeparam_gen_params_flg!((::clap::Arg::with_name(stringify!($n))) {$($short)*} {$($long)*});
        typeparam_gen_params!(($app.arg(arg.takes_value(true))) $($tail)*)
    }};
    (($app:expr) ($facc:ident $F:ident $n:ident $T:ty {$($short:tt)*} {$($long:tt)*} {optional}) $($tail:tt)*) => {{
        let arg = typeparam_gen_params_flg!((::clap::Arg::with_name(stringify!($n))) {$($short)*} {$($long)*});
        typeparam_gen_params!(($app.arg(arg.takes_value(true))) $($tail)*)
    }};
    (($app:expr) ($facc:ident $F:ident $n:ident $T:ty {$($short:tt)*} {$($long:tt)*} {required}) $($tail:tt)*) => {{
        let arg = typeparam_gen_params_flg!((::clap::Arg::with_name(stringify!($n))) {$($short)*} {$($long)*});
        typeparam_gen_params!(($app.arg(arg.takes_value(true).required(true))) $($tail)*)
    }};
    (($app:expr) ($facc:ident $F:ident $n:ident $T:ty {$($short:tt)*} {$($long:tt)*} {option map $E:expr}) $($tail:tt)*) => {{
        let arg = typeparam_gen_params_flg!((::clap::Arg::with_name(stringify!($n))) {$($short)*} {$($long)*});
        typeparam_gen_params!(($app.arg(arg.takes_value(true))) $($tail)*)
    }};
    (($app:expr) ($facc:ident $F:ident $n:ident $T:ty {$($short:tt)*} {$($long:tt)*} {map $E:expr}) $($tail:tt)*) => {{
        let arg = typeparam_gen_params_flg!((::clap::Arg::with_name(stringify!($n))) {$($short)*} {$($long)*});
        typeparam_gen_params!(($app.arg(arg.takes_value(true).required(true))) $($tail)*)
    }};
    (($app:expr)) => {$app};
}

#[derive(Copy, Clone, Debug)]
struct Res<T, E>(Result<T, E>);

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

#[allow(unused_macros)]
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

#[allow(unused_macros)]
macro_rules! typeparam_gen_new_command_get {
    (($match:expr) $T:ident { $($def:tt)* } { (@subcmd $subcmd:ident ($fld:ident($typ:ty)) ( $typ2:ty)) $($tail:tt)* }) => {{
        let mch = $match;
        if let Some(submatch) = mch.subcommand_matches(stringify!($subcmd)) {
            fn gen_subcmd<T : $crate::SubCommand>(am: &$crate::clap::ArgMatches) -> Result<T, T::Error> {
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
    (($match:expr, $stack: expr) $T:ident { } { }) => {
        panic!("No command given") as $T
    };
}


#[allow(unused_macros)]
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

#[allow(unused_macros)]
macro_rules! typeparam_gen_impl {
    ($acc:ident struct $N:ident [@app $app:ident $err:ty] [subcommands => { $($subcmd:tt)* } params => { $($params:tt)* }]) => {
        impl $crate::Command for $N {
            type Error = $err;
            fn command() -> $crate::clap::App<'static, 'static> {
                let app = $crate::clap::App::new(stringify!($app));
                let app = app.settings(&[
                    $crate::clap::AppSettings::StrictUtf8
                ]);
                let app = typeparam_gen_commands!((app) $($subcmd)*);
                let app = typeparam_gen_params!((app) $($params)*);
                app
            }
            fn new(matches: &$crate::clap::ArgMatches) -> Result<Self, Self::Error> {
                typeparam_gen_new!((&matches) $N $err {} [subcommands => { $($subcmd)* } params => { $($params) * }])
            }
        }
    };
    ($acc:ident struct $N:ident [@subcommand $err:ty] [subcommands => { $($subcmd:tt)* } params => { $($params:tt)* }]) => {
        impl $crate::SubCommand for $N {
            type Error = $err;
            fn subcommand(name: &'static str) -> $crate::clap::App<'static, 'static> {
                let app = $crate::clap::SubCommand::with_name(name);
                let app = typeparam_gen_commands!((app) $($subcmd)*);
                let app = typeparam_gen_params!((app) $($params)*);
                app
            }
            fn new(_am: &$crate::clap::ArgMatches) -> Result<Self, Self::Error> {
                typeparam_gen_new!((&_am) $N $err {} [subcommands => { $($subcmd)* } params => { $($params) * }])
            }
        } 
    };
}

#[allow(unused_macros)]
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

pub trait Command where Self : Sized {
    type Error;
    fn command() -> clap::App<'static, 'static>;
    fn new(mch: &clap::ArgMatches) -> Result<Self, Self::Error>;
    fn parse<I : IntoIterator<Item = T>, T : Into<std::ffi::OsString> + Clone>(itr: I) -> Result<Self, Error<Self::Error>> {
        Self::new(&Self::command().get_matches_from_safe(itr).map_err(|e| Error::Parse(e))?).map_err(|e| Error::Command(e))
    }
    fn parse_args() -> Result<Self, Error<Self::Error>> {
        Self::parse(std::env::args())
    }
    fn parse_any<I : IntoIterator<Item = T>, T : Into<std::ffi::OsString> + Clone, E : From<Self::Error> + From<clap::Error>>(itr: I) -> Result<Self, E> {
        match Self::parse(itr) {
            Ok(res) => Ok(res),
            Err(Error::Parse(e)) => Err(E::from(e)),
            Err(Error::Command(e)) => Err(E::from(e))
        }
    }
    fn parse_any_args<E : From<Self::Error> + From<clap::Error>>() -> Result<Self, E> {
        Self::parse_any(std::env::args())
    }
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
}

pub trait SubCommand where Self : Sized {
    type Error;
    fn subcommand(name: &'static str) -> clap::App<'static, 'static>;
    fn new(mch: &clap::ArgMatches) -> Result<Self, Self::Error>;
}

#[derive(Copy, Clone, Debug)]
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

#[cfg(test)]
mod tests {
    use ::Command;

    // Compile-time test
    typeparam!{
        struct Params [@app test ::std::string::ParseError] {
            quiet: bool [QUIET: -q],
            verbose: bool [VERBOSE: -v (value: flag)],
            cfg: String [CFG: -c (value: default String::from("---"))],
            path: String [PATH: -p (value: required)],
            foo: Option<String> [FOO: --foo (value: optional)],
            n: Option<u32> [N: -n (value: option map (|_v| Some(3)))],
            x: u32 [X: -x (value: map (|_| 4))],
            command: @SUBCOMMANDS<Commands> [list => List(List), get => Get(Get), foo => Foo: (default = Default)]
        }
        struct List [@subcommand ::std::string::ParseError];
        struct Get [@subcommand ::std::string::ParseError];
    }

    // TODO: Autogenerate it
    impl ::std::fmt::Debug for List {
        fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
            fmt.debug_struct("List").finish()
        }
    }

    impl ::std::fmt::Debug for Get {
        fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
            fmt.debug_struct("Get").finish()
        }
    }

    #[test]
    fn simple1() {
        let params = Params::parse(["simple", "-v", "--foo", "bar", "-p", "path", "-x", "X"].iter()).unwrap();
        assert!(!params.quiet);
        assert!(params.verbose);
        assert_eq!(params.cfg, "---");
        assert_eq!(params.path, "path");
        assert_eq!(params.foo, Some(String::from("bar")));
        assert_eq!(params.n, Some(3));
        assert_eq!(params.x, 4);
        match params.command {
            Commands::Default => {},
            _ => panic!("params.commands != Commands::Default")
        }
    }

    #[test]
    fn simple2() {
        let params = Params::parse(["simple", "-c", "cfg", "-q", "-p", "path", "-x", "X", "foo"].iter()).unwrap();
        assert!(params.quiet);
        assert!(!params.verbose);
        assert_eq!(params.cfg, "cfg");
        assert_eq!(params.path, "path");
        assert_eq!(params.foo, None);
        assert_eq!(params.n, Some(3));
        assert_eq!(params.x, 4);
        match params.command {
            Commands::Foo => {},
            _ => panic!("params.commands != Commands::Foo")
        }
    }
}


