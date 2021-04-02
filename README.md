# monoid-filter

### A monoid for modifying or consuming a value

Functions with the type `(a -> m (Maybe a))` are endomorphisms in the
Kleisli category of `MaybeT m`. This means they form a monoid and can be
chained together. The most common use for these is to create a series of
(effectful) filters which may modify their input, consume their input, or
pass their input along unmodified.

A good example of this pattern is `iptables` from the Linux networking tool
suite. An administrator can set up a series of filters, and each one lets a
packet pass on to the next filter (with or without an effect), or stops its
proliferation through the filters. Another example is a stack of open windows
in a GUI, with the topmost one getting priority for input events and either
consuming the input or passing it on to the next window down.
