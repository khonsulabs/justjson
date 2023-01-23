use core::ops::Deref;

/// A Cow-like type that only offers an `Owned` variant with `#[cfg(feature =
/// "alloc")]`.
#[derive(Debug, Clone)]
pub enum CowStr<'a> {
    /// An owned String.
    #[cfg(feature = "alloc")]
    Owned(alloc::string::String),
    /// A borrowed string slice.
    Borrowed(&'a str),
}

impl<'a> AsRef<str> for CowStr<'a> {
    fn as_ref(&self) -> &str {
        match self {
            #[cfg(feature = "alloc")]
            Self::Owned(str) => str,
            Self::Borrowed(str) => str,
        }
    }
}

impl<'a> Eq for CowStr<'a> {}

impl<'a, 'b> PartialEq<CowStr<'b>> for CowStr<'a> {
    fn eq(&self, other: &CowStr<'b>) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl<'a, 'b> PartialEq<&'b str> for CowStr<'a> {
    fn eq(&self, other: &&'b str) -> bool {
        self == *other
    }
}

impl<'a> PartialEq<str> for CowStr<'a> {
    fn eq(&self, other: &str) -> bool {
        self.as_ref() == other
    }
}

impl<'a> Deref for CowStr<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}
