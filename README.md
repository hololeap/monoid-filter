# monoid-filter

### A monoid for modifying or consuming a value

Functions with the type `(a -> m (Maybe a))` are endomorphisms in the
Kleisli category of `MaybeT m`. This means they form a monoid and can be
chained together. The most common use for these is to create a series of
(effectful) filters which may modify their input, consume their input, or
pass their input along unmodified.
