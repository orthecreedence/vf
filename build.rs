use std::env;
use std::fs::{self, File};
use std::io::{Read, Write, BufReader};
use std::path::PathBuf;
use std::collections::HashMap;
use heck::{SnakeCase, CamelCase};
use rio_api::{
    parser::TriplesParser,
    model::{
        Triple,
        Term,
        NamedOrBlankNode,
        NamedNode,
        BlankNode,
        Literal,
    },
};
use rio_turtle::{self, TurtleParser, TurtleError};
use serde::Deserialize;
use serde_json;
use url::Url;

static SCHEMA_LOCATION: &'static str = "./schema/vf.ttl";

/*
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
    // props can be recursive, lol. especially arrays and objects
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
    fn new(ty: &str, meta: Option<String>, enumdef: Option<String>) -> Self {
        Self { ty: ty.into(), meta, enumdef }
    }
}

/// Given a property (and some other junk) return a struct that describes how
/// the resulting field should be formatted.
fn prop_to_type(classname: &str, name: &str, prop: &Property, indent: &str) -> PropSpec {
    match prop.reftype {
        Some(ref reftype) if reftype.ends_with(".json") => {
            let url: Url = Url::parse(reftype).expect("prop_to_type() -- error parsing ref url");
            let ty = url.path_segments().unwrap().last().unwrap().trim_end_matches(".json");
            PropSpec::new(&format!("Box<{}::{}>", ty.to_snake_case(), ty), None, None)
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
                    let PropSpec { ty, meta, enumdef } = prop_to_type(classname, name, type_prop, indent);
                    PropSpec::new(&format!("Vec<{}>", ty), meta.map(|x| format!("{}_vec", x)), enumdef)
                }
                SpecType::Boolean => PropSpec::new("bool", None, None),
                SpecType::Integer => PropSpec::new("i64", None, None),
                SpecType::Number => PropSpec::new("f64", None, None),
                SpecType::String => {
                    if prop.enum_vals.is_some() {
                        let enumtype = name.to_camel_case();
                        let mut enum_out = String::new();
                        enum_out.push_str(&format!("{}#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]\n", indent));
                        enum_out.push_str(&format!("{}pub enum {} {{\n", indent, enumtype));
                        for enumval in prop.enum_vals.as_ref().unwrap() {
                            enum_out.push_str(&format!(r#"{}    #[serde(rename="{}")]"#, indent, enumval));
                            enum_out.push_str("\n");
                            enum_out.push_str(&format!("{}    {},\n", indent, enumval.to_camel_case()));
                        }
                        enum_out.push_str(&format!("{}}}", indent));
                        PropSpec::new(&enumtype, None, Some(enum_out))
                    } else {
                        match &prop.format {
                            Some(Format::Uri) => PropSpec::new("Url", Some("crate::ser::url".into()), None),
                            Some(Format::DateTime) => PropSpec::new("DateTime<Utc>", Some("crate::ser::datetime".into()), None),
                            None => PropSpec::new("String", None, None),
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
    let mut out = String::new();
    let indent = "    ";
    // easy line maker
    macro_rules! line {
        ($contents:expr, $indent:expr) => {
            out.push_str($indent);
            out.push_str($contents);
            out.push_str("\n");
        };
        ($contents:expr) => { line!($contents, indent); }
    }

    // our main write-as-you-go output
    let mut struct_out: Vec<String> = Vec::new();
    // a special output that gets prepended to the main output and consists of
    // any enum values we generated while processing the properties/fields
    let mut enum_out: Vec<String> = Vec::new();
    // an output for our builder impl
    let mut builder_out: Vec<String> = Vec::new();

    // loop over a sorted list of properties/fields
    let mut names = schema.properties.keys().map(|x| x.clone()).collect::<Vec<_>>();
    names.sort();
    for name in &names {
        let name_snake = name.to_snake_case();
        // for each field, make sure we generate a correct type, and if needed
        // create our corresponding enums. the bulk of this work is in the
        // prop_to_type() function.
        let prop = schema.properties.get(name).unwrap();
        if let Some(ref desc) = prop.description {
            struct_out.push(format!("    /// {}", desc));
        }
        // is this field required?
        let required = match schema.required {
            Some(ref x) => x.contains(name),
            None => false,
        };

        // parse our property and turn it into data we can use to make a field
        let PropSpec { ty: prop_type, meta, enumdef } = prop_to_type(&schema.title, &name, prop, indent);
        // if this field requires an enum field, output it
        if let Some(enumdef) = enumdef {
            enum_out.push(format!("{}\n", enumdef));
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
            struct_out.push(format!(r#"    #[serde(with="{}")]"#, meta));
        }
        if prop_type.contains("Option") {
            struct_out.push(r#"    #[serde(skip_serializing_if = "Option::is_none")]"#.into());
            struct_out.push("    #[builder(setter(into, strip_option), default)]".into());
        }
        struct_out.push(format!("    {}: {},", name_snake, prop_type));
        let builder_line = if prop_type.contains("Option") {
            format!("match {0} {{ Some(x) => builder.{0}(x), None => builder, }}", name_snake)
        } else {
            format!("builder.{0}({0})", name_snake)
        };
        builder_out.push(format!("        builder = {};", builder_line));
    }
    line!(&format!("pub mod {} {{", schema.title.to_snake_case()), "");
    line!("use super::*;");
    line!("");
    if enum_out.len() > 0 {
        line!(&enum_out.join("\n"), "");
    }
    // create our doc comments from the description
    if let Some(ref desc) = schema.description {
        line!(&format!("/// {}", desc));
        line!("///");
    }
    line!(&format!("/// ID: <{}>", schema.id));
    // start the struct
    line!("#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Builder, Getters)]");
    #[cfg(feature = "getset_setters")]
    line!("#[derive(Setters)]");
    #[cfg(feature = "getset_getmut")]
    line!("#[derive(MutGetters)]");
    line!(r#"#[builder(pattern = "owned")]"#);
    line!(&format!(
        r#"#[getset(get = "pub"{}{})]"#,
        if cfg!(feature = "getset_setters") { r#", set = "pub""# } else { "" },
        if cfg!(feature = "getset_getmut") { r#", get_mut = "pub""# } else { "" },
    ));
    line!(&format!("pub struct {} {{", schema.title));
    for field in struct_out {
        line!(&field);
    }
    line!("}");
    line!("");
    line!(&format!("impl {} {{", schema.title));
    line!(&format!("    /// Turns {} into {}Builder", schema.title, schema.title));
    line!(&format!("    pub fn into_builder(self) -> {}Builder {{", schema.title));
    let fields = names.into_iter()
        .map(|x| x.clone().to_snake_case())
        .collect::<Vec<_>>()
        .join(", ");
    line!(&format!("        let {} {{ {} }} = self;", schema.title, fields));
    line!(&format!("        let mut builder = {}Builder::default();", schema.title));
    for buildfield in builder_out {
        line!(&buildfield);
    }
    line!("        builder");
    line!("    }");
    line!("}");
    line!("}", "");
    line!("", "");
    out
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

*/

struct StructType {
    // Vec<(field name, type)>
    fields: Vec<(String, String)>
}

struct EnumType {
    // list of enum vals
    vals: Vec<String>,
    // field name, enum type, string enum val
    impls: HashMap<String, Vec<(String, String)>>
}

#[derive(Debug, PartialEq, Deserialize)]
enum DataType {
    #[serde(rename = "http://www.w3.org/2001/XMLSchema#boolean")]
    Boolean,
    #[serde(rename = "http://www.w3.org/2001/XMLSchema#double")]
    Double,
    #[serde(rename = "http://www.w3.org/2001/XMLSchema#string")]
    String,
    #[serde(rename = "http://www.w3.org/2001/XMLSchema#anyURI")]
    Url,
    #[serde(rename = "http://www.w3.org/2001/XMLSchema#dateTimeStamp")]
    DateTime,
    // catch-all type, mainly for things like om2 and stuff
    Literal(String),
}

#[derive(Debug, PartialEq, Deserialize)]
enum NodeType {
    #[serde(rename = "http://www.w3.org/2002/07/owl#Ontology")]
    Ontology,
    #[serde(rename = "http://www.w3.org/2002/07/owl#Class")]
    StructOrEnum,
    #[serde(rename = "http://www.w3.org/2002/07/owl#ObjectProperty")]
    Field,
    #[serde(rename = "http://www.w3.org/2002/07/owl#NamedIndividual")]
    EnumVal,
    #[serde(rename = "http://www.w3.org/2002/07/owl#DatatypeProperty")]
    DataType,
    // for values we can't classify on the first round of parsing. in the case
    // of enums, a second type with a #vf:* id signifies the parent, which would
    // have the same effect as using `domain`
    Literal(String),
}

#[derive(Debug, PartialEq, Deserialize)]
// aka predicate
enum Relationship {
    #[serde(rename = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")]
    Type,
    #[serde(rename = "http://www.w3.org/2000/01/rdf-schema#domain")]
    Domain,
    #[serde(rename = "http://www.w3.org/2000/01/rdf-schema#range")]
    Range,
    #[serde(rename = "http://www.w3.org/2000/01/rdf-schema#label")]
    Label,
    #[serde(rename = "http://www.w3.org/2000/01/rdf-schema#comment")]
    Comment,
    #[serde(rename = "http://www.w3.org/2003/06/sw-vocab-status/ns#term_status")]
    Status,
    #[serde(rename = "http://www.w3.org/2002/07/owl#unionOf")]
    Union,
    #[serde(rename = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first")]
    First,
    #[serde(rename = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest")]
    Rest,
    // for values we can't classify on the first round of parsing mainly other
    // vf:* types that aren't in the class space yet
    Literal(String),
}

#[derive(Debug, PartialEq, Deserialize)]
enum Object {
}

#[derive(Debug, Default, PartialEq)]
struct Node {
    id: Option<String>,
    ty: Option<NodeType>,
    name: Option<String>,
    comment: Option<String>,
    status: Option<String>,
    // vec<node id> (good for processing unions)
    parents: Vec<String>,
}

macro_rules! to_enum {
    ($enumty:ty, $val:expr) => {
        match serde_json::from_str::<$enumty>(&format!(r#""{}""#, $val)) {
            Ok(x) => x,
            Err(e) => <$enumty>::Literal($val.into())
        }
    }
}

fn gen_schema() -> String {
    let file = fs::File::open(SCHEMA_LOCATION).expect("error opening schema file");
    let mut bufread = BufReader::new(file);

    // node id -> node index
    let mut nodemap: HashMap<String, Node> = HashMap::new();

    // allows looking up an enum by its entry name
    let mut enum_entry_lookup: HashMap<String, Vec<EnumType>> = HashMap::new();
    // stores BlankNode id -> Union lookups
    let mut union_lookup: HashMap<String, &Vec<Vec<Node>>> = HashMap::new();

    let mut out = String::new();

    let mut cur_node = Node::default();
    let mut ignore = false;
    let schema = TurtleParser::new(bufread, "file:vf.ttl").unwrap().parse_all(&mut |t| -> Result<(), TurtleError> {
        let Triple { subject, predicate: predicate_named, object } = t;
        let NamedNode { iri: predicate } = predicate_named;
        let (id, blank): (String, bool) = match subject {
            NamedOrBlankNode::NamedNode(NamedNode { iri }) => (iri.into(), false),
            NamedOrBlankNode::BlankNode(BlankNode { id }) => (id.into(), true),
        };
        let (obj_id, obj_val, obj_blank): (Option<String>, Option<String>, bool) = match object.clone() {
            Term::Literal(Literal::Simple { value: string }) => (None, Some(string.into()), false),
            Term::NamedNode(NamedNode { iri }) => (Some(iri.into()), None, false),
            Term::BlankNode(BlankNode { id }) => (Some(id.into()), None, true),
            _ => panic!("unknown `Object` combo: {:?}", object),
        };
        // we are done with the current node, so save it and start a new one
        if cur_node.id.is_some() && Some(&id) != cur_node.id.as_ref() && !blank {
            // TODO: save node
            cur_node = Default::default();
            cur_node.id = Some(id);
            ignore = false;
        } else if cur_node.id.is_none() {
            cur_node.id = Some(id);
        }
        let rel = to_enum!(Relationship, predicate);

        // ignore the top-level ontology classification.
        if rel == Relationship::Type && obj_id == Some("http://www.w3.org/2002/07/owl#Ontology".into()) {
            ignore = true;
        }
        if ignore { return Ok(()) };

        match rel {
            Relationship::Type => {
                let ty = to_enum!(NodeType, obj_id.as_ref().unwrap());
                cur_node.ty = Some(ty);
            }
            _ => {}
        }
        out.push_str(&format!("zing: {:?} -- {:?}\n", rel, (obj_id, obj_val)));
        Ok(())
    });
    out
}

fn gen_header() -> String {
    let mut header = String::new();
    header.push_str("use chrono::prelude::*;\n");
    header.push_str("use derive_builder::Builder;\n");
    header.push_str("use getset::Getters;\n");
    #[cfg(feature = "getset_setters")]
    header.push_str("use getset::Setters;\n");
    #[cfg(feature = "getset_getmut")]
    header.push_str("use getset::MutGetters;\n");
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

