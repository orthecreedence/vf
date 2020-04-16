use std::env;
use std::fs::{self, File};
use std::io::{Read, Write, BufReader};
use std::path::PathBuf;
use std::collections::HashMap;
use heck::{SnakeCase, CamelCase, KebabCase};
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
use serde::{Serialize, Deserialize};
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

#[derive(Debug, PartialEq, Clone)]
struct StructType {
    // Vec<(field name, type)>
    fields: Vec<(String, String)>
}

#[derive(Debug, PartialEq, Clone)]
struct EnumType {
    // list of enum vals
    vals: Vec<String>,
    // field name, enum type, string enum val
    impls: HashMap<String, Vec<(String, String)>>
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
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
    #[serde(rename = "http://www.ontology-of-units-of-measure.org/resource/om-2/Measure")]
    OmMeasure,
    #[serde(rename = "http://www.ontology-of-units-of-measure.org/resource/om-2/Unit")]
    OmUnit,
    // catch-all type, mainly for things like om2 and stuff
    Literal(String),
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
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

#[derive(Debug, PartialEq, Clone, Deserialize)]
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
    #[serde(rename = "http://www.w3.org/2002/07/owl#propertyChainAxiom")]
    PropertyChainAxiom,
    // for values we can't classify on the first round of parsing mainly other
    // vf:* types that aren't in the class space yet
    Literal(String),
}

#[derive(Debug, Default, PartialEq, Clone, Serialize)]
struct Node {
    id: Option<String>,
    ty: Option<NodeType>,
    label: Option<String>,
    comment: Option<String>,
    status: Option<String>,
    // vec<node id> (good for processing unions)
    domain: Vec<String>,
    range: Option<DataType>,
    rel_pairs: Vec<(String, String)>,
    // filled in on our second pass
    subnodes: Vec<Box<Node>>,
    // (TypeName, Namespace)
    custom: Option<(String, String)>,
}

impl Node {
    fn as_ref<'a>(&'a self) -> &'a Self {
        self
    }

    fn fieldname(&self) -> String {
        let parsed: Url = self.id.as_ref().map(|x| x.parse().expect("error parsing node id url")).unwrap();
        let field = self.label.as_ref()
            .map(|x| x.as_str())
            .or_else(|| parsed.fragment())
            .or_else(|| parsed.path_segments().map(|x| x.last()).unwrap());
        match field {
            Some(x) => x.to_snake_case(),
            None => panic!("Node.fieldname() -- could not derive field from {:?}", self.id),
        }
    }

    fn typename(&self) -> String {
        if let Some((ty, _)) = self.custom.as_ref() {
            ty.to_camel_case()
        } else {
            let id_ty = self.id.as_ref()
                .filter(|x| x.starts_with("https://w3id.org/valueflows#"))
                .map(|x| x.trim_start_matches("https://w3id.org/valueflows#").to_camel_case());
            let ty = self.label.as_ref()
                .filter(|x| x.starts_with("vf:"))
                .map(|x| x.trim_start_matches("vf:").to_camel_case())
                .or(id_ty);
            match ty {
                Some(x) => x,
                None => panic!("could not generate type for: {:?}", self),
            }
        }
    }

    fn namespace(&self) -> String {
        match self.custom.as_ref() {
            Some((_, ns)) => ns.clone(),
            None => "vf".to_string()
        }
    }

    fn is_enum(&self) -> bool {
        if self.ty != Some(NodeType::StructOrEnum) { return false; }
        let mut has_enum_vals = false;
        for sub in &self.subnodes {
            if sub.ty == Some(NodeType::EnumVal) {
                has_enum_vals = true;
                break;
            }
        }
        has_enum_vals
    }

    fn is_not_applicable(&self) -> bool {
        self.id == Some("https://w3id.org/valueflows#notApplicable".to_string())
    }
}

macro_rules! to_enum {
    ($enumty:ty, $val:expr) => {
        match serde_json::from_str::<$enumty>(&format!(r#""{}""#, $val)) {
            Ok(x) => x,
            Err(e) => <$enumty>::Literal($val.into())
        }
    }
}

struct StringWriter {
    string: String,
    indent: usize,
}

impl StringWriter {
    fn new() -> Self {
        Self { string: String::from(""), indent: 0 }
    }

    fn write(&mut self, val: &str) {
        self.string.push_str(val);
    }

    fn line(&mut self, val: &str) {
        let indent: String = (0..(self.indent * 4)).map(|_| " ").collect::<Vec<_>>().concat();
        self.string.push_str(&indent);
        self.string.push_str(val);
        self.string.push_str("\n");
    }

    fn line_noindent(&mut self, val: &str) {
        self.string.push_str(val);
        self.string.push_str("\n");
    }

    fn set_indent(&mut self, indent: usize) {
        self.indent = indent;
    }

    fn inc_indent(&mut self) {
        self.indent += 1;
    }

    fn dec_indent(&mut self) {
        if self.indent > 0 { self.indent -= 1; }
    }

    fn to_string(self) -> String {
        let Self { string: val, .. } = self;
        val
    }
}

/// Converts a node to an enum.
///
/// TODO: instead of relying on node.subnodes[].rel_pairs[] to build the impl
/// list, it makes more sense to use the non-enum object properties of the node.
///
/// NOTE that I'm not super happy about this implementation, specifically about
/// hardcoding logic around PairsWith and NotApplicable. It seems to me after
/// building this out that NotApplicable should essentially just represent an
/// exclusion and any ObjectProperties on enum values (ie, inputOutput,
/// pairsWith, resourceEffect) that don't include *all* values of that enum as
/// candidates for return would be modeled as an Option<T> instead of a T. Then
/// PairsWith AND NotApplicable could be completely removed from the spec and
/// nothing would need "special" treatment.
fn node_to_enum(out: &mut StringWriter, node: Node, nodemap: &HashMap<String, Node>) {
    // PairsWith is "special" in that it acts as a proxy between NotApplicable
    // and Action, while pointing to Action's items.
    if node.id == Some("https://w3id.org/valueflows#PairsWith".to_string()) {
        return;
    }

    if let Some(comment) = node.comment.as_ref() {
        out.line(&format!("/// {}", comment));
    }
    out.line("#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]");
    out.line(&format!("pub enum {} {{", node.typename()));
    out.inc_indent();

    for field in &node.subnodes {
        if field.ty != Some(NodeType::EnumVal) { continue; }
        // NotApplicable is kind of special. although we *could* parse it as an
        // enum val, this would make the situation with PairsWith pretty strange
        // (and it's already an "interesting" item) so instead we keep PairsWith
        // a strict subset of Action but make anything that can return a
        // NotApplicable *an Option* which is somewhat more idiomatic rust
        // anyway.
        if field.is_not_applicable() { continue; }

        let id = field.id.as_ref().unwrap().clone();
        let typename = field.typename();

        let label = field.label.as_ref().expect("enum val missing label");
        if let Some(comment) = field.comment.as_ref() {
            out.line(&format!("/// {}", comment));
        }
        out.line(&format!(r#"#[serde(rename = "{}")]"#, label.to_kebab_case()));
        out.line(&format!("{},", typename));
    }

    out.dec_indent();
    out.line("}");

    // map ParentType -> EnumNode
    let mut impl_map: HashMap<String, Vec<(Node, Node)>> = HashMap::new();
    for field in &node.subnodes {
        for (parent, nodeid) in &field.rel_pairs {
            let node = nodemap.get(nodeid).unwrap();
            let entry = impl_map.entry(parent.clone()).or_insert(Vec::new());
            (*entry).push((field.as_ref().clone(), node.clone()));
        }
    }
    if impl_map.len() > 0 {
        out.write("\n");
        out.line(&format!("impl {} {{", node.typename()));
        out.inc_indent();
        let mut keys = impl_map.keys().collect::<Vec<_>>();
        keys.sort();
        for implid in keys {
            let vals = impl_map.get(implid).unwrap();
            let implnode = nodemap.get(implid).unwrap().clone();
            let fnname = implnode.typename().to_snake_case();
            let retnode = if implid == "https://w3id.org/valueflows#pairsWith" {
                nodemap.get("https://w3id.org/valueflows#Action").unwrap().clone()
            } else {
                implnode.clone()
            };
            let has_not_applicable = vals.iter()
                .filter(|(_, val)| val.is_not_applicable())
                .collect::<Vec<_>>()
                .len() > 0;
            let rettype = if has_not_applicable {
                format!("Option<{}>", retnode.typename())
            } else {
                retnode.typename()
            };
            out.line(&format!("pub fn {}(&self) -> {} {{", fnname, rettype));
            out.inc_indent();
            out.line("match self {");
            out.inc_indent();
            for (val, ret) in vals {
                let retval = if has_not_applicable {
                    if ret.is_not_applicable() {
                        "None".to_string()
                    } else {
                        format!("Some({}::{})", retnode.typename(), ret.typename())
                    }
                } else {
                    format!("{}::{}", retnode.typename(), ret.typename())
                };
                out.line(&format!("Self::{} => {},", val.typename(), retval));
            }
            out.dec_indent();
            out.line("}");
            out.dec_indent();
            out.line("}");
            out.write("\n");
        }
        out.dec_indent();
        out.line("}");
    }

    out.write("\n");
}

/// Converts a node into a struct
fn node_to_struct(out: &mut StringWriter, node: Node, nodemap: &HashMap<String, Node>) {
    // start the struct
    if let Some(comment) = node.comment.as_ref() {
        out.line(&format!("/// {}", comment));
    }
    out.line("#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Builder, Getters)]");
    #[cfg(feature = "getset_setters")]
    out.line("#[derive(Setters)]");
    #[cfg(feature = "getset_getmut")]
    out.line("#[derive(MutGetters)]");
    out.line(r#"#[builder(pattern = "owned")]"#);
    out.line(&format!(
        r#"#[getset(get = "pub"{}{})]"#,
        if cfg!(feature = "getset_setters") { r#", set = "pub""# } else { "" },
        if cfg!(feature = "getset_getmut") { r#", get_mut = "pub""# } else { "" },
    ));
    out.line(&format!("pub struct {} {{", node.typename()));
    out.inc_indent();
    for sub in node.subnodes {
        out.line(&format!("pub {}: {},", sub.fieldname(), "String"));
    }
    out.dec_indent();
    out.line("}");
}

fn gen_schema() -> String {
    let mut out = StringWriter::new();

    let file = fs::File::open(SCHEMA_LOCATION).expect("error opening schema file");
    let mut bufread = BufReader::new(file);

    // our saved nodes from the first round of parsing
    let mut nodes: Vec<String> = vec![];
    let mut nodemap: HashMap<String, Node> = HashMap::new();

    let mut save_node = |node: Node| {
        let node_id = node.id.as_ref().unwrap().clone();
        // this preservs order and is what we loop on after this pass
        nodes.push(node_id.clone());
        // this maps id -> node for easier graph-building later
        nodemap.insert(node_id, node);
    };

    // first loop! 
    let mut cur_node = Node::default();
    let mut ignore = false;
    let mut cur_list_id: Option<String> = None;
    let mut cur_list: Vec<String> = vec![];
    let schema = TurtleParser::new(bufread, "file:vf.ttl").unwrap().parse_all(&mut |t| -> Result<(), TurtleError> {
        let Triple { subject, predicate: predicate_named, object } = t;
        let NamedNode { iri: predicate } = predicate_named;
        let (id, blank): (String, bool) = match subject {
            NamedOrBlankNode::NamedNode(NamedNode { iri }) => (iri.into(), false),
            NamedOrBlankNode::BlankNode(BlankNode { id }) => (id.into(), true),
        };
        let blank_id: Option<String> = if id != "" && blank { Some(id.clone()) } else { None };
        let (obj_id, obj_val, obj_blank): (Option<String>, Option<String>, bool) = match object.clone() {
            Term::Literal(Literal::Simple { value: string }) => (None, Some(string.into()), false),
            Term::NamedNode(NamedNode { iri }) => (Some(iri.into()), None, false),
            Term::BlankNode(BlankNode { id }) => (Some(id.into()), None, true),
            _ => panic!("unknown `Object` combo: {:?}", object),
        };
        // we are done with the current node, so save it and start a new one
        if cur_node.id.is_some() && Some(&id) != cur_node.id.as_ref() && !blank {
            // we're starting a new node, so save the existing one
            if !ignore {
                save_node(cur_node.clone());
            }
            cur_node = Default::default();
            if !blank {
                cur_node.id = Some(id);
            }
            // NOTE: do NOT clear our cur_list[_id] here! it's possible that a
            // new item (looking at you, skos:note) starts with a union, meaning
            // that if we clean out cur_list, that domain data will be lost
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
                if cur_node.ty.is_some() {
                    cur_node.domain.push(obj_id.unwrap());
                } else {
                    cur_node.ty = Some(ty);
                }
            }
            Relationship::Domain => {
                if obj_id.is_some() && obj_id == cur_list_id {
                    // really ties the list together
                    cur_node.domain = cur_list.clone();
                    cur_list = vec![];
                } else if let Some(type_id) = obj_id {
                    cur_node.domain = vec![type_id];
                }
            }
            Relationship::Range => { cur_node.range = Some(to_enum!(DataType, obj_id.as_ref().unwrap())); }
            Relationship::Label => { cur_node.label = obj_val; }
            Relationship::Comment => { cur_node.comment = obj_val; }
            Relationship::Status => { cur_node.status = obj_val; }
            Relationship::Union => {
                cur_list_id = blank_id;
            }
            Relationship::First => { cur_list.push(obj_id.unwrap()); }
            // note that we *could* implement "correct" first/rest parsing, but
            // because our triples are *in order* we don't really need to. so,
            // fuck off, Rest...
            Relationship::Rest => {}
            Relationship::PropertyChainAxiom => {}
            Relationship::Literal(val) => {
                cur_node.rel_pairs.push((val, obj_id.unwrap()));
            }
        }
        Ok(())
    });
    if !ignore {
        save_node(cur_node.clone());
    }

    // helps us make some hardcoded top-level types we want to "import" by
    // "hand" "so to" "speak."
    macro_rules! custom_type {
        ($id:expr, $typename:expr, $namespace:expr, $comment:expr) => {
            let mut custom_node = Node::default();
            custom_node.id = Some($id.into());
            custom_node.ty = Some(NodeType::StructOrEnum);
            custom_node.comment = Some($comment.into());
            custom_node.custom = Some(($typename.into(), $namespace.into()));
            save_node(custom_node);
        }
    }
    // foaf:Agent
    custom_type!("http://xmlns.com/foaf/0.1/Agent", "Agent", "foaf", "A person or group or organization with economic agency.");
    // geo:SpatialThing
    custom_type!("http://www.w3.org/2003/01/geo/wgs84_pos#SpatialThing", "SpatialThing", "geo", "A mappable location.");

    let mut finished: HashMap<String, Node> = HashMap::new();
    let nodemap_clone = nodemap.clone();
    for node_id in nodes {
        let node = nodemap.remove(&node_id).unwrap();
        match node.ty.clone() {
            Some(NodeType::StructOrEnum) => {
                finished.insert(node_id, node);
            }
            Some(NodeType::Field) | Some(NodeType::EnumVal) | Some(NodeType::DataType) | None  => {
                for domain in &node.domain {
                    let parentnode = nodemap.get_mut(domain)
                        .or_else(|| finished.get_mut(domain));
                    match parentnode {
                        Some(x) => x.subnodes.push(Box::new(node.clone())),
                        None => {
                            // this is all things we probably don't need
                        }
                    }
                }
            }
            // "at this point we should have no literal types," he said, with a
            // boyish grin
            Some(NodeType::Literal(val)) => {}
            // none of these either
            Some(NodeType::Ontology) => {}
        }
    }

    // sort by ns asc, key asc
    let mut keys = finished.keys()
        .map(|x| {
            let ns = finished.get(x).unwrap().namespace();
            (ns, x.clone())
        })
        .collect::<Vec<_>>();
    keys.sort();

    // ok, build everything
    let mut cur_ns = "".to_string();
    for (ns, key) in keys {
        let node = finished.remove(&key).unwrap();
        if ns != cur_ns {
            out.set_indent(0);
            if cur_ns != "" { out.line("}"); }
            out.write("\n");
            out.line(&format!("mod {} {{", ns));
            out.inc_indent();
            out.line("use super::*;");
            out.write("\n");
            cur_ns = ns;
        }
        if node.is_enum() {
            node_to_enum(&mut out, node, &nodemap_clone);
        } else {
            node_to_struct(&mut out, node, &nodemap_clone);
        }
    }
    out.line_noindent("}");
    out.to_string()
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

/// Given a dump of generated code, save it to the output dir
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

