pub mod url {
    use url::Url;
    use serde::ser::{Serializer};
    use serde::de::{Deserializer, Deserialize};

    #[allow(dead_code)]
    pub fn serialize<S>(val: &Url, ser: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        ser.serialize_str(&val.clone().into_string())
    }

    #[allow(dead_code)]
    pub fn deserialize<'de, D>(des: D) -> Result<Url, D::Error>
        where D: Deserializer<'de>
    {
        let val: String = Deserialize::deserialize(des)?;
        Ok(Url::parse(&val).expect("error parsing url"))
    }
}

pub mod url_opt {
    use url::Url;
    use serde::ser::{Serializer};
    use serde::de::{Deserializer, Deserialize};

    pub fn serialize<S>(val: &Option<Url>, ser: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        match val {
            Some(x) => ser.serialize_str(&x.clone().into_string()),
            None => ser.serialize_none(),
        }
    }

    pub fn deserialize<'de, D>(des: D) -> Result<Option<Url>, D::Error>
        where D: Deserializer<'de>
    {
        let val_maybe: Option<String> = Deserialize::deserialize(des)?;
        match val_maybe {
            Some(val) => Ok(Some(Url::parse(&val).expect("error parsing url"))),
            None => Ok(None),
        }
    }
}

pub mod url_vec {
    use url::Url;
    use serde::ser::{Serializer, SerializeSeq};
    use serde::de::{Deserializer, Deserialize};

    pub fn serialize<S>(val: &Vec<Url>, ser: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        let mut seq = ser.serialize_seq(Some(val.len()))?;
        for v in val {
            seq.serialize_element(&v.clone().into_string()).expect("error serializing url string");
        }
        seq.end()
    }

    pub fn deserialize<'de, D>(des: D) -> Result<Vec<Url>, D::Error>
        where D: Deserializer<'de>
    {
        let vals: Vec<String> = Deserialize::deserialize(des)?;
        let vals = vals.into_iter()
            .map(|x| Url::parse(&x).expect("error parsing url"))
            .collect::<Vec<_>>();
        Ok(vals)
    }
}

pub mod datetime {
    use chrono::prelude::*;
    use serde::ser::{Serializer};
    use serde::de::{Deserializer, Deserialize};

    #[allow(dead_code)]
    pub fn serialize<S>(val: &DateTime<Utc>, ser: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        let datestr = val.to_rfc3339();
        ser.serialize_str(&datestr)
    }

    #[allow(dead_code)]
    pub fn deserialize<'de, D>(des: D) -> Result<DateTime<Utc>, D::Error>
        where D: Deserializer<'de>
    {
        let val: String = Deserialize::deserialize(des)?;
        Ok(val.parse::<DateTime<Utc>>().expect("error parsing date"))
    }
}

pub mod datetime_opt {
    use chrono::prelude::*;
    use serde::ser::{Serializer};
    use serde::de::{Deserializer, Deserialize};

    pub fn serialize<S>(val: &Option<DateTime<Utc>>, ser: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        match val {
            Some(x) => ser.serialize_str(&x.to_rfc3339()),
            None => ser.serialize_none(),
        }
    }

    pub fn deserialize<'de, D>(des: D) -> Result<Option<DateTime<Utc>>, D::Error>
        where D: Deserializer<'de>
    {
        let val_maybe: Option<String> = Deserialize::deserialize(des)?;
        match val_maybe {
            Some(val) => Ok(Some(val.parse::<DateTime<Utc>>().expect("error parsing url"))),
            None => Ok(None),
        }
    }
}

