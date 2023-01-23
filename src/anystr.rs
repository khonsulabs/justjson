use core::ops::Deref;

/// A string that can be either Owned or Borrowed.
///
/// This type is similar to the standard library's Cow, but the `Owned` variant
/// is only available when the `alloc` or `std` features are enabled.
#[derive(Debug, Clone)]
pub enum AnyStr<'a> {
    /// An owned String.
    #[cfg(feature = "alloc")]
    Owned(alloc::string::String),
    /// A borrowed string slice.
    Borrowed(&'a str),
}

impl<'a> AsRef<str> for AnyStr<'a> {
    fn as_ref(&self) -> &str {
        match self {
            #[cfg(feature = "alloc")]
            Self::Owned(str) => str,
            Self::Borrowed(str) => str,
        }
    }
}

impl<'a> Eq for AnyStr<'a> {}

impl<'a, 'b> PartialEq<AnyStr<'b>> for AnyStr<'a> {
    fn eq(&self, other: &AnyStr<'b>) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl<'a, 'b> PartialEq<&'b str> for AnyStr<'a> {
    fn eq(&self, other: &&'b str) -> bool {
        self == *other
    }
}

impl<'a> PartialEq<str> for AnyStr<'a> {
    fn eq(&self, other: &str) -> bool {
        self.as_ref() == other
    }
}

impl<'a> Deref for AnyStr<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}
