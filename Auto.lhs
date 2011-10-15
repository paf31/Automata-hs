> module Automata where

> import Data.Maybe
> import Data.Char

The type of finite deterministic automata taking input values of type i and
returning values of type a.

> data Auto i a = Success a | Failure | Wait (i -> Auto i a)

The functor instance maps successful computations to successful computations,
failures to failures, and nonterminal states to nonterminal states.

> instance Functor (Auto i) where
>   fmap f (Success a) = Success (f a)
>   fmap f Failure = Failure
>   fmap f (Wait t) = Wait (\i -> fmap f (t i))

In addition to being functorial in its second argument, Auto is also cofunctorial in
its first argument:

> pullback f (Success a) = Success a
> pullback f Failure = Failure
> pullback f (Wait t) = Wait (\i -> pullback f (t (f i)))

The type (Auto i) is also a monad. In fact, it is a free monad, whose bind operation
substitutes a new automaton at the end of a successful match.

> instance Monad (Auto i) where
>   return = Success
>   (>>=) (Success a) f = f a
>   (>>=) Failure f = Failure
>   (>>=) (Wait t) f = Wait (\i -> (t i) >>= f)

Given an automaton a, step a is the automaton which throws away its first input and
then continues with a.

> step = Wait . const

Given an automaton a, we can filter its acceptable input values based on the generated output.

> filterOutput p (Success a) = if (p a) then (Success a) else Failure
> filterOutput p Failure = Failure
> filterOutput p (Wait t) = Wait (\i -> filterOutput p (t i))

We can also filter input values directly

> filterInput p (Success a) = Success a
> filterInput p Failure = Failure
> filterInput p (Wait t) = Wait (\i -> if (p i) then filterInput p (t i) else Failure)

This automaton simply returns its input

> echo = Wait Success

This automaton matches a single input matching a predicate

> only = flip filterInput echo

This automaton matches a single value only

> match :: (Eq a) => a -> Auto a a
> match = only . (==)

This automaton matches an array of values one-by-one

> matchMany :: (Eq a) => [a] -> Auto a [a]
> matchMany = foldr (\a -> \b -> a >>= \x -> b >>= \xs -> return (x:xs)) (Success []) . fmap match

This operator continues with the second automaton if the first fails

> (??) Failure a = a
> (??) (Wait t1) (Wait t2) = Wait (\i -> case (t1 i) of
>   Failure -> t2 i
>   a -> a)
> (??) a _  = a

The accept method deconstructs a finite deterministic automaton by applying a list of input values.

> accepts (Success a) [] = Just a
> accepts (Wait t) (x:xs) = accepts (t x) xs
> accepts _ _ = Nothing

Matches a character which represents a digit and returns that digit as an integer.

> matchDigit = fmap digitToInt (only isDigit)

Matches a string of digits terminated by a period and returns their sum.

> sumDigits = (matchDigit >>= \d -> sumDigits >>= \s -> return (s + d)) ?? ((match '.') >> Success 0)