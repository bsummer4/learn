main :: IO ()
main = putStrLn "Hello World"

-- Hello World in Haskell.
--
-- The type `main :: IO ()` means that `main` is an "IO Machine" that may
-- perform some side effects and then must output an empty tuple.
--
-- `putStrLn "Hello World"` might not be intuitive for some people. This is
-- a function call. In most conventional languages, you would write
-- `putStrLn("Hello World")`. However, Haskell functions always take exactly
-- one argument, so we don't need the parenthesis and can simply juxtapose
-- the function and it's argument.
--
-- Similarly, `IO ()` is just Haskell's syntax for Generics. In conventional
-- languages, this would be written as `IO<Void>`. However, Haskell generic types
-- also always take one parameter, so we can omit the pointy-brackets and
-- simply juxtapose the generic type with it's parameter.
--
-- Internally, `IO` machines are capable of invoking C procedures (through
-- the FFI) and transforming data using (pure) Haskell functions. For example,
-- `putStrLn` is implemented through a combination of Haskell functions and
-- invocations of low-level IO routines from libc.
--
-- `IO` machines can also be combined (chained together) in various ways that
-- I'll discuss in the next section. For now, let's just get something
-- running.
--
-- One more thing to notice before we move on, is that the top-level entry
-- point into a Haskell program is always an `IO` machine.
