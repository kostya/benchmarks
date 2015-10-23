extern crate serde;

use serde::{Deserialize, Deserializer, de};

// A simple type which deserializes by silently consuming all its input, allowing a field to be
// skipped.
pub struct Skip;
impl Deserialize for Skip {
    fn deserialize<D>(deserializer: &mut D) -> Result<Self, D::Error> where D: Deserializer {
        deserializer.visit(Skip)
    }
}

impl de::Visitor for Skip {
    type Value = Skip;

    fn visit_map<V>(&mut self, mut visitor: V) -> Result<Skip, V::Error> where V: de::MapVisitor {
        while try!(visitor.visit::<Skip, Skip>()).is_some() {}
        try!(visitor.end());
        Ok(Skip)
    }

    fn visit_seq<V>(&mut self, mut visitor: V) -> Result<Skip, V::Error> where V: de::SeqVisitor {
        while try!(visitor.visit::<Skip>()).is_some() {}
        try!(visitor.end());
        Ok(Skip)
    }

    fn visit_str<E>(&mut self, _v: &str) -> Result<Skip, E> where E: de::Error { Ok(Skip) }
    fn visit_u64<E>(&mut self, _v: u64) -> Result<Skip, E> where E: de::Error { Ok(Skip) }
    fn visit_bool<E>(&mut self, _v: bool) -> Result<Self::Value, E> where E: de::Error { Ok(Skip) }
}
