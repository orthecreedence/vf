use std::env;
use std::fs::{self, File};
use std::io::Write;
use std::path::PathBuf;
use std::collections::HashMap;
use heck::{SnakeCase, CamelCase};
use serde_derive::Deserialize;
use url::Url;

static SCHEMA_LOCATION: &'static str = "./schema/schemas/";

#[derive(Deserialize, Debug)]
enum SpecType {
    #[serde(rename = "object")]
    Object,
    #[serde(rename = "array")]
    Array,
    #[serde(rename = "boolean")]
    Boolean,
    #[serde(rename = "integer")]
    Integer,
    #[serde(rename = "number")]
    Number,
    #[serde(rename = "string")]
    String,
}

#[derive(Deserialize, Debug)]
enum Format {
    #[serde(rename = "uri")]
    Uri,
    #[serde(rename = "date-time")]
    DateTime,
}

#[derive(Deserialize, Debug)]
struct Property {
    #[serde(rename = "type")]
    ty: Option<SpecType>,
    description: Option<String>,
    #[serde(rename = "enum")]
    enum_vals: Option<Vec<String>>,
    format: Option<Format>,
    #[serde(rename = "$ref")]
    reftype: Option<String>,
    items: Option<Box<Property>>,
}

#[derive(Deserialize, Debug)]
struct Schema {
    #[serde(rename = "$id")]
    id: String,
    #[serde(rename = "type")]
    ty: SpecType,
    title: String,
    description: Option<String>,
    properties: HashMap<String, Property>,
    required: Option<Vec<String>>,
}

struct PropSpec {
    ty: String,
    meta: Option<String>,
    enumdef: Option<String>,
}

impl PropSpec {
    fn new(ty: String, meta: Option<String>, enumdef: Option<String>) -> Self {
        Self { ty, meta, enumdef }
    }
}

fn prop_to_type(classname: &str, name: &str, prop: &Property) -> PropSpec {
    match prop.reftype {
        Some(ref reftype) if reftype.ends_with(".json") => {
            let url: Url = Url::parse(reftype).expect("prop_to_type() -- error parsing ref url");
            let ty = url.path_segments().unwrap().last().unwrap().trim_end_matches(".json");
            PropSpec::new(format!("Box<{}>", ty), None, None)
        }
        Some(ref reftype) => {
            panic!("prop_to_type() -- found a reftype that doesn't point to a JSON file: {}", reftype);
        }
        _ => {
            // if we don't have a $ref field (`reftype`) then we have `ty`
            match prop.ty.as_ref().unwrap() {
                SpecType::Object => panic!("prop_to_type() -- `object` type not implemented for properties"),
                SpecType::Array => {
                    let type_prop =  prop.items.as_ref().expect("prop_to_type() -- `array` type is missing `items` sibling. curious.");
                    let PropSpec { ty, meta, enumdef } = prop_to_type(classname, name, type_prop);
                    PropSpec::new(format!("Vec<{}>", ty), meta.map(|x| format!("{}_vec", x)), enumdef)
                }
                SpecType::Boolean => PropSpec::new(String::from("bool"), None, None),
                SpecType::Integer => PropSpec::new(String::from("i64"), None, None),
                SpecType::Number => PropSpec::new(String::from("f64"), None, None),
                SpecType::String => {
                    if prop.enum_vals.is_some() {
                        let enumtype = format!("{}_{}", classname, name).to_camel_case();
                        let mut enum_out = String::new();
                        enum_out.push_str(&format!("#[derive(Serialize, Deserialize, Debug, PartialEq)]\n"));
                        enum_out.push_str(&format!("pub enum {} {{\n", enumtype));
                        for enumval in prop.enum_vals.as_ref().unwrap() {
                            enum_out.push_str(&format!(r#"    #[serde(rename="{}")]"#, enumval));
                            enum_out.push_str("\n");
                            enum_out.push_str(&format!("    {},\n", enumval.to_camel_case()));
                        }
                        enum_out.push_str(&format!("}}"));
                        PropSpec::new(enumtype.into(), None, Some(enum_out))
                    } else {
                        match &prop.format {
                            Some(Format::Uri) => PropSpec::new("Url".into(), Some("crate::ser::url".into()), None),
                            Some(Format::DateTime) => PropSpec::new("DateTime<Utc>".into(), Some("crate::ser::datetime".into()), None),
                            None => PropSpec::new("String".into(), None, None),
                        }
                    }
                }
            }
        }
    }
}

/// Given a (parsed) schema, generate a rust struct that represents the schema
/// (including references to other types in the overall schema).
fn schema_to_class(schema: Schema) -> String {
    // our main write-as-you-go output
    let mut out = String::new();
    // a special output that gets prepended to the main output and consists of
    // any enum values we generated while processing the properties/fields
    let mut enum_out = String::new();

    // easy line maker
    let mut line = |contents: &str| {
        out.push_str(contents);
        out.push_str("\n");
    };

    // create our doc comments from the description
    if let Some(ref desc) = schema.description {
        line(&format!("/// {}", desc));
        line("///");
    }
    line(&format!("/// ID: {}", schema.id));
    // start the struct
    line("#[derive(Serialize, Deserialize, Debug, PartialEq)]");
    line(&format!("pub struct {} {{", schema.title));
    // loop over a sorted list of properties/fields
    let mut names = schema.properties.keys().collect::<Vec<_>>();
    names.sort();
    for name in names {
        // for each field, make sure we generate a correct type, and if needed
        // create our corresponding enums. the bulk of this work is in the
        // prop_to_type() function.
        let prop = schema.properties.get(name).unwrap();
        if let Some(ref desc) = prop.description {
            line(&format!("    /// {}", desc));
        }
        // is this field required?
        let required = match schema.required {
            Some(ref x) => x.contains(name),
            None => false,
        };
        // NOTE: this bool determines if the type references itself, but is
        // currently unused because we wrap ALL type references in Box<> for now
        // to simplify/standardize the implementation
        //let recursive = prop.reftype.as_ref().map(|x| x == &schema.id).unwrap_or(false);

        // parse our property and turn it into data we can use to make a field
        let PropSpec { ty: prop_type, meta, enumdef } = prop_to_type(&schema.title, &name, prop);
        // if this field requires an enum field, output it
        if let Some(enumdef) = enumdef {
            enum_out.push_str(&format!("{}\n\n", enumdef));
        }
        // if required, do not wrap in Option<>
        let (prop_type, meta) = if required {
            (prop_type, meta)
        } else {
            (
                if prop_type.contains("Vec") {
                    prop_type
                } else {
                    format!("Option<{}>", prop_type)
                },
                meta.map(|x| {
                    if x.ends_with("_vec") {
                        x
                    } else {
                        format!("{}_opt", x)
                    }
                }),
            )
        };
        // if we have meta, output it. this is mainly for #[serde(with="")] junk
        if let Some(meta) = meta {
            line(&format!(r#"    #[serde(with="{}")]"#, meta));
        }
        line(&format!("    {}: {},", name.to_snake_case(), prop_type));
    }
    line("}");
    line("");
    if enum_out == "" {
        out
    } else {
        format!("{}{}", enum_out, out)
    }
}

/// Generate the main schema. This loads all of our heroic json schema files and
/// turns them into rust code.
fn gen_schema() -> String {
    // we'll save output to a string
    let mut out = String::new();

    // grab a sorted list of schema files
    let mut files = fs::read_dir(SCHEMA_LOCATION).expect("Error finding schema files")
        .map(|f| f.unwrap().path())
        .collect::<Vec<_>>();
    files.sort();

    // loop over our files, grab the contents, and generate a class for each
    for path in files {
        let path_str = path.as_path().to_str().expect("cannot convert path to str ='(");
        let name = path.as_path().file_stem().unwrap().to_str().unwrap();
        if !path_str.ends_with(".json") { continue; }

        let contents = fs::read_to_string(&path).expect("Error reading file");
        let schema: Schema = match serde_json::from_str(&contents) {
            Ok(x) => x,
            Err(e) => panic!("error parsing schema: {}: {}", name, e),
        };
        let gen = schema_to_class(schema);
        out.push_str(&gen);
    }
    out
}

fn gen_header() -> String {
    let mut header = String::new();
    header.push_str("use chrono::prelude::*;\n");
    header.push_str("use serde_derive::{Serialize, Deserialize};\n");
    header.push_str("use url::Url;\n");
    header
}

/// Given a dump of generated code, save it to src/gen.rs
fn save(contents: String) {
    // write it all out to our src/gen.rs file, included by lib
    let out_dir = env::var("OUT_DIR").unwrap();
    let mut dest_path = PathBuf::from(&out_dir);
    dest_path.push("vf_gen.rs");
    let mut f = File::create(&dest_path).unwrap();
    f.write_all(contents.as_bytes()).unwrap();
}

fn main() {
    let header = gen_header();
    let contents = gen_schema();
    save(format!("{}\n{}", header, contents));
}

