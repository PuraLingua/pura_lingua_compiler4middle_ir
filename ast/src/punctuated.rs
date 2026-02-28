use std::fmt::Debug;

use fmt_with_src::DebugWithSrc;

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Punctuated<TValue, TSeparator> {
    pub inner: Vec<(TValue, TSeparator)>,
    pub last: Option<TValue>,
}

#[doc(hidden)]
pub struct PunctuatedDbg<'a, TValue, TSeparator>(
    &'a [(TValue, TSeparator)],
    &'a Option<TValue>,
    &'a str,
);

impl<'a, TValue: DebugWithSrc, TSeparator: DebugWithSrc> Debug
    for PunctuatedDbg<'a, TValue, TSeparator>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Punctuated")
            .field_with("inner", |ff| {
                let mut d = {
                    super let mut d = ff.debug_list();
                    &mut d
                };

                for (v, s) in self.0 {
                    d = d.entry(&v.debug(self.2));
                    d = d.entry(&s.debug(self.2));
                }

                if let Some(l) = self.1.as_ref() {
                    d = d.entry(&l.debug(self.2));
                }

                d.finish()
            })
            .finish()
    }
}

impl<TValue: DebugWithSrc + 'static, TSeparator: DebugWithSrc + 'static> DebugWithSrc
    for Punctuated<TValue, TSeparator>
{
    type Debug<'a> = PunctuatedDbg<'a, TValue, TSeparator>;
    fn debug<'a>(&'a self, src: &'a str) -> Self::Debug<'a> {
        PunctuatedDbg(&self.inner, &self.last, src)
    }
}

impl<TValue, TSeparator> Punctuated<TValue, TSeparator> {
    pub fn new(inner: Vec<(TValue, TSeparator)>, last: Option<TValue>) -> Self {
        Self { inner, last }
    }
    pub const fn len(&self) -> usize {
        self.inner.len() + if self.last.is_some() { 1 } else { 0 }
    }
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }
    pub fn iter(&self) -> impl Iterator<Item = &TValue> {
        self.inner.iter().map(|(x, _)| x).chain(self.last.as_ref())
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut TValue> {
        self.inner
            .iter_mut()
            .map(|(x, _)| x)
            .chain(self.last.as_mut())
    }

    /// Appends a syntax tree node onto the end of this punctuated sequence. The
    /// sequence must already have a trailing separator, or be empty.
    ///
    /// Use [`push`] instead if the punctuated sequence may or may not already
    /// have trailing separator.
    ///
    /// [`push`]: Punctuated::push
    ///
    /// # Panics
    ///
    /// Panics if the sequence is nonempty and does not already have a trailing
    /// separator.
    pub fn push_value(&mut self, val: TValue) {
        if !self.empty_or_trailing() {
            assert!(
                self.empty_or_trailing(),
                "Punctuated::push_value: cannot push value if Punctuated is missing trailing separator",
            );
        };
        assert!(
            self.empty_or_trailing(),
            "Punctuated::push_value: cannot push value if Punctuated is missing trailing separator",
        );

        self.last = Some(val);
    }

    /// Appends a trailing separator onto the end of this punctuated sequence.
    /// The sequence must be non-empty and must not already have trailing
    /// separator.
    ///
    /// # Panics
    ///
    /// Panics if the sequence is empty or already has a trailing separator.
    pub fn push_separator(&mut self, sep: TSeparator) {
        assert!(
            self.last.is_some(),
            "Punctuated::push_punct: cannot push separator if Punctuated is empty or already has trailing separator",
        );

        let last = self.last.take().unwrap();
        self.inner.push((last, sep));
    }

    /// Returns true if either this `Punctuated` is empty, or it has a trailing
    /// separator.
    ///
    /// Equivalent to `punctuated.is_empty() || punctuated.trailing_punct()`.
    pub fn empty_or_trailing(&self) -> bool {
        self.last.is_none()
    }

    /// Appends a syntax tree node onto the end of this punctuated sequence.
    ///
    /// If there is not a trailing separator in this sequence when this method
    /// is called, the default value of separator type `P` is inserted before
    /// the given value of type `T`.
    pub fn push(&mut self, value: TValue)
    where
        TSeparator: Default,
    {
        if !self.empty_or_trailing() {
            self.push_separator(Default::default());
        }
        self.push_value(value);
    }
}

impl<TValue, TSeparator> IntoIterator for Punctuated<TValue, TSeparator> {
    type Item = TValue;
    type IntoIter = impl Iterator<Item = TValue>;
    fn into_iter(self) -> <Self as IntoIterator>::IntoIter {
        self.inner.into_iter().map(|x| x.0).chain(self.last)
    }
}

impl<'a, TValue, TSeparator> IntoIterator for &'a Punctuated<TValue, TSeparator> {
    type Item = &'a TValue;
    type IntoIter = impl Iterator<Item = &'a TValue>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, TValue, TSeparator> IntoIterator for &'a mut Punctuated<TValue, TSeparator> {
    type Item = &'a mut TValue;
    type IntoIter = impl Iterator<Item = &'a mut TValue>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}
