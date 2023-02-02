#[cfg(feature = "alloc")]
use alloc::string::String;
use core::borrow::Borrow;
use core::hash::Hash;
use core::ops::Deref;

/// A string that can be either Owned or Borrowed.
///
/// This type is similar to the standard library's Cow, but the `Owned` variant
/// is only available when the `alloc` or `std` features are enabled.
#[derive(Debug, Clone)]
pub enum AnyStr<'a> {
    /// An owned String.
    #[cfg(feature = "alloc")]
    Owned(String),
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

impl<'a> Borrow<str> for AnyStr<'a> {
    fn borrow(&self) -> &str {
        self.as_ref()
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

impl<'a, 'b> PartialOrd<AnyStr<'b>> for AnyStr<'a> {
    fn partial_cmp(&self, other: &AnyStr<'b>) -> Option<core::cmp::Ordering> {
        self.partial_cmp(other.as_ref())
    }
}

impl<'a, 'b> PartialOrd<&'b str> for AnyStr<'a> {
    fn partial_cmp(&self, other: &&'b str) -> Option<core::cmp::Ordering> {
        self.partial_cmp(*other)
    }
}
impl<'a> PartialOrd<str> for AnyStr<'a> {
    fn partial_cmp(&self, other: &str) -> Option<core::cmp::Ordering> {
        self.as_ref().partial_cmp(other)
    }
}

impl<'a> Deref for AnyStr<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<'a> Hash for AnyStr<'a> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state);
    }
}

impl<'a> From<&'a str> for AnyStr<'a> {
    fn from(value: &'a str) -> Self {
        Self::Borrowed(value)
    }
}

#[cfg(feature = "alloc")]
impl<'a> From<String> for AnyStr<'a> {
    fn from(value: String) -> Self {
        Self::Owned(value)
    }
}

#[test]
#[cfg(feature = "std")]
fn hash() {
    let mut set = std::collections::HashSet::new();
    set.insert(AnyStr::from(String::from("hello")));
    assert!(set.contains(&AnyStr::from("hello")));
    assert!(set.contains("hello"));
}

#[test]
fn ord() {
    assert!(AnyStr::Borrowed("a") < "b");
    assert!(AnyStr::Borrowed("a") < AnyStr::Borrowed("b"));
}
