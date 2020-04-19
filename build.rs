use std::env;
use std::fs::{self, File};
use std::io::{Write, BufReader};
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
use serde_json::{self, Value};
use url::Url;

static SCHEMA_LOCATION: &'static str = "./schema/vf.ttl";

// -----------------------------------------------------------------------------
// Utils
// -----------------------------------------------------------------------------

/// Makes it eas(ier) to write out code
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
        self.write(&indent);
        self.write(val);
        self.nl();
    }

    fn nl(&mut self) {
        self.write("\n");
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

/// Returns a sorted vec of keys from a hash table
fn sorted_keys<T, X>(hash: &HashMap<T, X>) -> Vec<T>
    where T: Ord + Clone
{
    let mut keys = hash.keys().map(|x| x.clone()).collect::<Vec<_>>();
    keys.sort();
    keys
}

// -----------------------------------------------------------------------------
// Parsing enums
// -----------------------------------------------------------------------------

/// This (very important) enum translates between string ids and rust types, but
/// also has a number of implementation functions that help us along the way.
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
enum DataType {
    #[serde(rename = "bool")]
    #[serde(alias = "http://www.w3.org/2001/XMLSchema#boolean")]
    Boolean,
    #[serde(rename = "f64")]
    #[serde(alias = "http://www.w3.org/2001/XMLSchema#double")]
    Double,
    #[serde(alias = "http://www.w3.org/2001/XMLSchema#string")]
    String,
    #[serde(alias = "http://www.w3.org/2001/XMLSchema#anyURI")]
    #[serde(alias = "http://www.w3.org/2002/07/owl#Thing")]
    Url,
    #[serde(rename = "DateTime<Utc>")]
    #[serde(alias = "http://www.w3.org/2001/XMLSchema#dateTimeStamp")]
    #[serde(alias = "http://purl.org/dc/terms/created")]
    DateTime,
    #[serde(alias = "http://www.w3.org/2006/time#hasDuration")]
    Duration,
    #[serde(rename = "om2::Measure")]
    #[serde(alias = "http://www.ontology-of-units-of-measure.org/resource/om-2/Measure")]
    Measure,
    #[serde(rename = "om2::Unit")]
    #[serde(alias = "http://www.ontology-of-units-of-measure.org/resource/om-2/Unit")]
    Unit,
    #[serde(rename = "geo::SpatialThing")]
    #[serde(alias = "http://www.w3.org/2003/01/geo/wgs84_pos#SpatialThing")]
    SpatialThing,
    #[serde(alias = "http://xmlns.com/foaf/0.1/Agent")]
    Agent,
    #[serde(rename = "dfc::ProductBatch")]
    #[serde(alias = "http://www.virtual-assembly.org/DataFoodConsortium/BusinessOntology#ProductBatch")]
    ProductBatch,
    #[serde(rename = "String")]
    #[serde(alias = "http://www.w3.org/2004/02/skos/core#note")]
    Note,
    // catch-all type, mainly for things like om2 and stuff
    Literal(String),
    // used for post-processing mainly
    RangeEnum(String),
}

impl DataType {
    /// Coverts this type into a string to be included in rust code.
    fn to_string(&self, box_vf: bool) -> String {
        match self {
            DataType::Literal(ref id) => {
                let should_box = box_vf && id.starts_with("https://w3id.org/valueflows");
                let typename = Node::new(&id).fieldname().to_camel_case();
                if should_box {
                    format!("Box<{}>", typename)
                } else {
                    typename
                }
            }
            DataType::RangeEnum(ref name) => {
                let typename = name.clone();
                if box_vf {
                    format!("Box<{}>", typename)
                } else {
                    typename
                }
            }
            _ => {
                let jval = serde_json::to_value(self).unwrap();
                if let Value::String(val) = jval {
                    if self == &DataType::Agent && box_vf {
                        format!("Box<{}>", val)
                    } else {
                        val
                    }
                } else {
                    panic!("failed to deserialized type {:?} -- {:?}", self, jval);
                }
            }
        }
    }

    /// Determines if, when outputing this type, any meta is required (such as
    /// custom serialization directives).
    fn meta(&self, is_vector: bool, is_required: bool) -> Vec<String> {
        match self {
            DataType::Url => {
                let serval = if is_vector {
                    "url_vec"
                } else if !is_required {
                    "url_opt"
                } else {
                    "url"
                };
                let ser = format!(r#"#[serde(with = "crate::ser::{}")]"#, serval);
                vec![ser]
            }
            DataType::DateTime => {
                let serval = if is_vector {
                    "datetime_vec"
                } else if !is_required {
                    "datetime_opt"
                } else {
                    "datetime"
                };
                let ser = format!(r#"#[serde(with = "crate::ser::{}")]"#, serval);
                vec![ser]
            }
            _ => vec![],
        }
    }

    /// For some of our types, we want fields that are defined in remote specs
    /// and not in our local spec (even though our local spec can "add on"
    /// fields to the remote types. This is where we define our extra fields on
    /// a per-type basis.
    fn extra_fields(&self) -> Vec<Field> {
        match self {
            DataType::SpatialThing => {
                vec![
                    Field::new(
                            "http://www.w3.org/2003/01/geo/wgs84_pos#lat",
                            "lat",
                            &DataType::Double,
                            Some("The WGS84 latitude of a SpatialThing (decimal degrees)."),
                            Some(false),
                            Some(false),
                    ),
                    Field::new(
                            "http://www.w3.org/2003/01/geo/wgs84_pos#long",
                            "long",
                            &DataType::Double,
                            Some("The WGS84 longitude of a SpatialThing (decimal degrees)."),
                            Some(false),
                            Some(false),
                    ),
                    Field::new(
                            "http://www.w3.org/2003/01/geo/wgs84_pos#alt",
                            "alt",
                            &DataType::Double,
                            Some("The WGS84 altitude of a SpatialThing (decimal meters above the local reference ellipsoid)."),
                            Some(false),
                            Some(false),
                    ),
                ]
            }
            DataType::ProductBatch => {
                vec![
                    Field::new(
                            "http://www.virtual-assembly.org/DataFoodConsortium/BusinessOntology#batchNumber",
                            "batch_number",
                            &DataType::String,
                            None,
                            Some(true),
                            Some(false),
                    ),
                    Field::new(
                            "http://www.virtual-assembly.org/DataFoodConsortium/BusinessOntology#expiryDate",
                            "expiry_date",
                            &DataType::DateTime,
                            None,
                            Some(false),
                            Some(false),
                    ),
                    Field::new(
                            "http://www.virtual-assembly.org/DataFoodConsortium/BusinessOntology#productionDate",
                            "production_date",
                            &DataType::DateTime,
                            None,
                            Some(false),
                            Some(false),
                    ),
                ]
            }
            DataType::Measure => {
                vec![
                    Field::new(
                            "http://www.ontology-of-units-of-measure.org/resource/om-2/Measure#hasNumericValue",
                            "has_numerical_value",
                            &DataType::Double,
                            None,
                            Some(true),
                            Some(false),
                    ),
                    Field::new(
                            "http://www.ontology-of-units-of-measure.org/resource/om-2/Measure#hasUnit",
                            "has_unit",
                            &DataType::Unit,
                            None,
                            Some(true),
                            Some(false),
                    ),
                ]
            }
            DataType::Unit => {
                vec![
                    Field::new(
                            "http://www.ontology-of-units-of-measure.org/resource/om-2/Unit#label",
                            "label",
                            &DataType::String,
                            None,
                            Some(true),
                            Some(false),
                    ),
                    Field::new(
                            "http://www.ontology-of-units-of-measure.org/resource/om-2/Unit#symbol",
                            "symbol",
                            &DataType::String,
                            None,
                            Some(true),
                            Some(false),
                    ),
                ]
            }
            _ => vec![]
        }
    }
}

/// Helps us parse out what type of node we're dealing with when looping over
/// our triples.
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

/// Encodes the various relationships used between our RDF nodes
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

/// Helps us convert a string to an enum, as long as the enum has the Literal
/// value to act as a catch-all. Maybe this could be written as a function, but
/// I will be damned if I write a bunch of stupid traits/impls to avoid a four-
/// line macro.
macro_rules! to_enum {
    ($enumty:ty, $val:expr) => {
        match serde_json::from_str::<$enumty>(&format!(r#""{}""#, $val)) {
            Ok(x) => x,
            Err(_) => <$enumty>::Literal($val.into())
        }
    }
}

// -----------------------------------------------------------------------------
// Node struct for storing and grouping parsed values
// -----------------------------------------------------------------------------

#[derive(Debug, Default, PartialEq, Clone, Serialize)]
struct Node {
    id: Option<String>,
    ty: Option<NodeType>,
    label: Option<String>,
    comment: Option<String>,
    status: Option<String>,
    // vec<node id> (good for processing unions)
    domain: Vec<String>,
    range: Vec<String>,
    rel_pairs: Vec<(String, String)>,
    // filled in on our second pass
    subnodes: Vec<Box<Node>>,
    // (TypeName, Namespace)
    custom: Option<(String, String)>,
}

impl Node {
    fn new(id: &str) -> Self {
        let mut node = Self::default();
        node.id = Some(id.to_string());
        node
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
                .and_then(|x| {
                    if  x.starts_with("https://w3id.org/valueflows#") {
                        Some(x.trim_start_matches("https://w3id.org/valueflows#").to_camel_case())
                    } else {
                        match x.as_str() {
                            "http://www.w3.org/2004/02/skos/core#note" => Some("String".to_string()),
                            "http://purl.org/dc/terms/created" => Some("DateTime<Utc>".to_string()),
                            "http://www.w3.org/2006/time#hasDuration" => Some("crate::time::Duration".to_string()),
                            _ => None,
                        }
                    }
                });
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
        let default = "vf".to_string();
        match self.custom.as_ref() {
            Some((_, ns)) => ns.clone(),
            None => {
                match self.id.as_ref() {
                    Some(x) => {
                        if x.starts_with("https://w3id.org/valueflows") {
                            default
                        } else if x.starts_with("http://www.w3.org/2004/02/skos/core#note") {
                            "skos".into()
                        } else if x.starts_with("http://www.w3.org/2003/01/geo") {
                            "geo".into()
                        } else if x.starts_with("http://www.w3.org/2006/time") {
                            "".into()
                        } else if x.starts_with("http://purl.org/dc/terms") {
                            "".into()
                        } else if x.starts_with("http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue") {
                            "".into()
                        } else {
                            panic!("missing namespace {:?}", self);
                        }
                    }
                    None => default,
                }
            }
        }
    }

    /// Determines if this node has a range enum (ie, a range union) and if so
    /// returns the namespace of that union and the union name
    fn range_enum(&self) -> Option<(String, Vec<DataType>, String)> {
        if self.range.len() <= 1 { return None; }
        let mut rangekeys = self.range.clone();
        rangekeys.sort();
        let enums = rangekeys.iter()
            .map(|x| {
                let mut tmpnode = Node::default();
                tmpnode.id = Some(x.clone());
                (
                    Node::namespace(&tmpnode),
                    to_enum!(DataType, x),
                    Node::fieldname(&tmpnode).to_camel_case(),
                )
            })
            .collect::<Vec<_>>();
        Some((
            enums[0].0.clone(),
            enums.iter().map(|x| x.1.clone()).collect::<Vec<_>>(),
            enums.iter().map(|x| x.2.clone()).collect::<Vec<_>>().join("") + "Union",
        ))
    }
}

// -----------------------------------------------------------------------------
// Final schema definition (what we parse the rdf into)
// -----------------------------------------------------------------------------

#[derive(Debug, PartialEq, Clone, Serialize)]
struct RangeUnion {
    types: Vec<DataType>,
}

impl RangeUnion {
    fn new(types: Vec<DataType>) -> Self {
        Self { types }
    }

    fn type_to_name(ty: &DataType, box_vf: bool) -> String {
        ty.to_string(box_vf).to_camel_case()
    }

    fn name(&self) -> String {
        let mut names = self.types.iter()
            .map(|x| RangeUnion::type_to_name(x, false))
            .collect::<Vec<_>>();
        names.sort();
        names.join("") + "Union"
    }

    fn prepare(&mut self) {
        self.types.sort_by_key(|x| RangeUnion::type_to_name(x, false));
    }
}

#[derive(Debug, Clone, Serialize)]
struct EnumImpl {
    name: String,
    ty: String,
    enumval: String,
}

impl EnumImpl {
    fn from_rel_pair(pair: &(String, String), nodemap: &HashMap<String, Node>) -> Self {
        let (id_impl, id_returnval) = pair.clone();
        let node_impl = nodemap.get(&id_impl).expect("missing enum impl node");
        let returnval = nodemap.get(&id_returnval).expect("missing enum impl returnval node");
        if returnval.domain.len() == 0 {
            panic!("cannot determine return type for enum impl: {:?} -- {:?}", pair, returnval);
        }
        let parentnode = nodemap.get(&returnval.domain[0]).expect("missing enum impl parent node");
        Self {
            name: node_impl.typename().to_snake_case(),
            ty: parentnode.typename(),
            enumval: returnval.typename(),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
struct EnumVal {
    id: String,
    name: String,
    label: Option<String>,
    comment: Option<String>,
    impls: Vec<EnumImpl>,
}

impl EnumVal {
    fn from_node(node: &Node, nodemap: &HashMap<String, Node>) -> Self {
        Self {
            id: node.id.as_ref().unwrap().clone(),
            name: node.typename(),
            label: node.label.clone(),
            comment: node.comment.clone(),
            impls: node.rel_pairs.iter().map(|x| EnumImpl::from_rel_pair(x, nodemap)).collect::<Vec<_>>(),
        }
    }

    fn find_impl(&self, implname: &str) -> Option<EnumImpl> {
        self.impls.iter()
            .find(|x| x.name == implname)
            .map(|x| x.clone())
    }
}

#[derive(Debug, Clone, Serialize)]
struct Field {
    id: String,
    name: String,
    ty: DataType,
    comment: Option<String>,
    is_vec: Option<bool>,          // somewhat redundant via Class.array_fields
    is_required: Option<bool>,     // somewhat redundant via Class.required_fields
}

impl Field {
    fn from_node(node: &Node) -> Self {
        let node_id = node.id.as_ref().unwrap().clone();
        let ty = match node.range_enum() {
            Some((_, _, ty)) => DataType::RangeEnum(ty),
            None => {
                if node.range.len() == 0 {
                    to_enum!(DataType, &node_id)
                } else {
                    to_enum!(DataType, &node.range[0])
                }
            }
        };
        let comment = node.comment.as_ref().map(|x| x.as_str());

        // ---- TODO: implement ----
        let is_vec = None;
        let is_required = None;
        // -------------------------

        Self::new(&node_id, &node.fieldname(), &ty, comment, is_vec, is_required)
    }

    fn new(id: &str, name: &str, ty: &DataType, comment: Option<&str>, is_vec: Option<bool>, is_required: Option<bool>) -> Self {
        Self {
            id: id.to_string(),
            name: name.to_string(),
            ty: ty.clone(),
            comment: comment.map(|x| x.to_string()),
            is_vec,
            is_required,
        }
    }

    fn is_vec(&self) -> bool {
        self.is_vec.unwrap_or(false)
    }

    fn is_required(&self) -> bool {
        self.is_required.unwrap_or(false)
    }
}

#[derive(Debug, Default, Clone, Serialize)]
struct Class {
    id: String,
    name: String,
    properties: Vec<Field>,
    enum_vals: Vec<EnumVal>,
    array_fields: Vec<String>,
    required_fields: Vec<String>,
    comment: Option<String>,
}

impl Class {
    fn from_node(node: &Node) -> Self {
        let mut default = Self::default();
        default.id = node.id.as_ref().unwrap().clone();
        default.name = node.typename();
        // TODO: node required fields
        // TODO: node array fields
        default.comment = node.comment.clone();
        default
    }

    fn is_enum(&self) -> bool {
        self.enum_vals.len() > 0
    }

    fn add_enumval(&mut self, enumval: EnumVal) {
        self.enum_vals.push(enumval);
    }

    fn add_field(&mut self, field: Field) {
        // make sure we copy our required/vec bits to the containing class
        if field.is_vec.is_some() && field.is_vec() {
            self.array_fields.push(field.id.clone());
        } else if field.is_required.is_some() && field.is_required() {
            self.required_fields.push(field.id.clone());
        }
        self.properties.push(field);
    }

    fn prepare(&mut self) {
        self.properties.sort_by_key(|x| x.name.clone());
        self.enum_vals.sort_by_key(|x| x.name.clone());

        let required_fields = self.required_fields();
        let array_fields = self.array_fields();
        for prop in &mut self.properties {
            if array_fields.contains(&prop.id) {
                prop.is_vec = Some(true);
            } else if required_fields.contains(&prop.id) {
                prop.is_required = Some(true);
            }
        }
    }

    fn prop_enum_vals(&self, prop: &Field) -> Vec<EnumVal> {
        self.enum_vals.iter()
            .filter(|x| x.impls.iter().find(|y| y.name == prop.name).is_some())
            .map(|x| x.clone())
            .collect::<Vec<_>>()
    }

    fn array_fields(&self) -> Vec<String> {
        let mut fields = self.array_fields.clone();
        // NOTE: overrides. remove these once the rdf spec has array fields
        fields.append(&mut match self.id.as_str() {
            "https://w3id.org/valueflows#AgentRelationship" => vec![
                "https://w3id.org/valueflows#inScopeOf",
            ],
            "https://w3id.org/valueflows#Claim" => vec![
                "https://w3id.org/valueflows#inScopeOf",
                "https://w3id.org/valueflows#resourceClassifiedAs",
            ],
            "https://w3id.org/valueflows#Commitment" => vec![
                "https://w3id.org/valueflows#inScopeOf",
                "https://w3id.org/valueflows#resourceClassifiedAs",
            ],
            "https://w3id.org/valueflows#EconomicEvent" => vec![
                "https://w3id.org/valueflows#inScopeOf",
                "https://w3id.org/valueflows#resourceClassifiedAs",
            ],
            "https://w3id.org/valueflows#EconomicResource" => vec![
                "https://w3id.org/valueflows#classifiedAs",
            ],
            "https://w3id.org/valueflows#Intent" => vec![
                "https://w3id.org/valueflows#inScopeOf",
                "https://w3id.org/valueflows#resourceClassifiedAs",
            ],
            "https://w3id.org/valueflows#Process" => vec![
                "https://w3id.org/valueflows#classifiedAs",
                "https://w3id.org/valueflows#inScopeOf",
            ],
            "https://w3id.org/valueflows#Proposal" => vec![
                "https://w3id.org/valueflows#inScopeOf",
            ],
            "https://w3id.org/valueflows#RecipeProcess" => vec![
                "https://w3id.org/valueflows#processClassifiedAs",
            ],
            "https://w3id.org/valueflows#RecipeResource" => vec![
                "https://w3id.org/valueflows#resourceClassifiedAs",
            ],
            "https://w3id.org/valueflows#Scenario" => vec![
                "https://w3id.org/valueflows#inScopeOf",
            ],
            _ => vec![],
        }.iter().map(|x| x.to_string()).collect::<Vec<_>>());
        fields
    }

    fn required_fields(&self) -> Vec<String> {
        let mut fields = self.required_fields.clone();
        // NOTE: overrides. remove these once the rdf spec has required fields
        fields.append(&mut match self.id.as_str() {
            "http://xmlns.com/foaf/0.1/Agent" => vec![
                "https://w3id.org/valueflows#name",
            ],
            "https://w3id.org/valueflows#AgentRelationship" => vec![
                "https://w3id.org/valueflows#object",
                "https://w3id.org/valueflows#relationship",
                "https://w3id.org/valueflows#subject",
            ],
            "https://w3id.org/valueflows#AgentRelationshipRole" => vec![
                "https://w3id.org/valueflows#roleLabel",
            ],
            "https://w3id.org/valueflows#Appreciation" => vec![
                "https://w3id.org/valueflows#appreciationOf",
                "https://w3id.org/valueflows#appreciationWith",
            ],
            "https://w3id.org/valueflows#Claim" => vec![
                "https://w3id.org/valueflows#action",
                "https://w3id.org/valueflows#provider",
                "https://w3id.org/valueflows#receiver",
                "https://w3id.org/valueflows#triggeredBy",
            ],
            "https://w3id.org/valueflows#Commitment" => vec![
                "https://w3id.org/valueflows#action",
                "https://w3id.org/valueflows#provider",
                "https://w3id.org/valueflows#receiver",
            ],
            "https://w3id.org/valueflows#EconomicEvent" => vec![
                "https://w3id.org/valueflows#action",
                "https://w3id.org/valueflows#provider",
                "https://w3id.org/valueflows#receiver",
            ],
            "https://w3id.org/valueflows#EconomicResource" => vec![
                "https://w3id.org/valueflows#conformsTo",
            ],
            "https://w3id.org/valueflows#Fulfillment" => vec![
                "https://w3id.org/valueflows#fulfilledBy",
                "https://w3id.org/valueflows#fulfills",
            ],
            "https://w3id.org/valueflows#Intent" => vec![
                "https://w3id.org/valueflows#action",
            ],
            "https://w3id.org/valueflows#Process" => vec![
                "https://w3id.org/valueflows#name",
            ],
            "https://w3id.org/valueflows#ProcessSpecification" => vec![
                "https://w3id.org/valueflows#name",
            ],
            "https://w3id.org/valueflows#ProposedIntent" => vec![
                "https://w3id.org/valueflows#publishedIn",
                "https://w3id.org/valueflows#publishes",
            ],
            "https://w3id.org/valueflows#ProposedTo" => vec![
                "https://w3id.org/valueflows#proposed",
                "https://w3id.org/valueflows#proposedTo",
            ],
            "https://w3id.org/valueflows#RecipeFlow" => vec![
                "https://w3id.org/valueflows#action",
            ],
            "https://w3id.org/valueflows#RecipeProcess" => vec![
                "https://w3id.org/valueflows#name",
                "https://w3id.org/valueflows#processConformsTo",
            ],
            "https://w3id.org/valueflows#RecipeResource" => vec![
                "https://w3id.org/valueflows#name",
            ],
            "https://w3id.org/valueflows#ResourceSpecification" => vec![
                "https://w3id.org/valueflows#name",
            ],
            "https://w3id.org/valueflows#Satisfaction" => vec![
                "https://w3id.org/valueflows#satisfiedBy",
                "https://w3id.org/valueflows#satisfies",
            ],
            "https://w3id.org/valueflows#Scenario" => vec![
                "https://w3id.org/valueflows#name",
            ],
            "https://w3id.org/valueflows#ScenarioDefinition" => vec![
                "https://w3id.org/valueflows#name",
            ],
            "https://w3id.org/valueflows#Settlement" => vec![
                "https://w3id.org/valueflows#settledBy",
                "https://w3id.org/valueflows#settles",
            ],
            _ => vec![],
        }.iter().map(|x| x.to_string()).collect::<Vec<_>>());
        fields
    }
}

#[derive(Debug, Default, Serialize)]
struct Namespace {
    unions: Vec<RangeUnion>,
    classes: Vec<Class>,
}

impl Namespace {
    fn add_union<'a>(&'a mut self, union: RangeUnion) -> &'a mut RangeUnion {
        if self.unions.iter().filter(|x| *x == &union).count() == 0 {
            self.unions.push(union.clone());
        }
        self.get_union_mut(&union).unwrap()
    }

    fn get_union_mut<'a>(&'a mut self, union: &RangeUnion) -> Option<&'a mut RangeUnion> {
        self.unions.iter_mut().find(|x| x == &union)
    }

    fn add_class<'a>(&'a mut self, class: Class) -> &'a mut Class {
        let id = class.id.clone();
        if self.classes.iter().filter(|x| x.id == class.id).count() == 0 {
            self.classes.push(class);
        }
        self.get_class_mut(&id).unwrap()
    }

    fn get_class_mut<'a>(&'a mut self, id: &str) -> Option<&'a mut Class> {
        self.classes.iter_mut().find(|x| x.id == id)
    }

    fn prepare(&mut self) {
        self.unions.sort_by_key(|x| x.name());
        self.classes.sort_by_key(|x| x.name.clone());
        for item in &mut self.unions { item.prepare(); }
        for item in &mut self.classes { item.prepare(); }
    }
}

#[derive(Debug, Default, Serialize)]
struct Schema {
    ns: HashMap<String, Namespace>,
}

// -----------------------------------------------------------------------------
// Parsing logic
// -----------------------------------------------------------------------------

/// Parses our heroic .ttl file and turns all the triples into a namespace ->
/// struct/enum -> field hierarchy (sorry, anarchists)
fn gen_schema() -> Schema {
    let file = fs::File::open(SCHEMA_LOCATION).expect("error opening schema file");
    let bufread = BufReader::new(file);

    // our saved nodes from the first round of parsing
    let mut nodemap: HashMap<String, Node> = HashMap::new();

    // first pass! we loop over the parsed turtle file and group all of our
    // triples by their ids effectively. this gives us a more structured set of
    // data we can use to make our graph
    let mut cur_node_id: String = "".to_string();
    let mut cur_list_id: Option<String> = None;
    let mut cur_list: Vec<String> = vec![];
    TurtleParser::new(bufread, "file:vf.ttl").unwrap().parse_all(&mut |t| -> Result<(), TurtleError> {
        // destructure our triple
        let Triple { subject, predicate: predicate_named, object } = t;
        let NamedNode { iri: predicate } = predicate_named;

        // grab our id, but check if the node is named or blank
        let (id, blank): (String, bool) = match subject {
            NamedOrBlankNode::NamedNode(NamedNode { iri }) => (iri.into(), false),
            NamedOrBlankNode::BlankNode(BlankNode { id }) => (id.into(), true),
        };
        // destructure our object a bit
        let blank_id: Option<String> = if id != "" && blank { Some(id.clone()) } else { None };
        let (obj_id, obj_val, _obj_blank): (Option<String>, Option<String>, bool) = match object.clone() {
            Term::Literal(Literal::Simple { value: string }) => (None, Some(string.into()), false),
            Term::NamedNode(NamedNode { iri }) => (Some(iri.into()), None, false),
            Term::BlankNode(BlankNode { id }) => (Some(id.into()), None, true),
            _ => panic!("unknown `Object` combo: {:?}", object),
        };

        // if we have a named node, set the current id as id
        if !blank {
            cur_node_id = id.clone();
        }

        // pull out our current node, or create if needed
        let cur_node = nodemap.entry(cur_node_id.clone()).or_insert(Node::new(&cur_node_id));

        // we can skip parsing the ontology record itself
        if cur_node.id == Some("https://w3id.org/valueflows/".to_string()) {
            return Ok(());
        }

        // process the relationship
        let rel = to_enum!(Relationship, predicate);
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
            Relationship::Range => {
                if obj_id.is_some() && obj_id == cur_list_id {
                    // really ties the list together
                    cur_node.range = cur_list.clone();
                    cur_list = vec![];
                } else if let Some(type_id) = obj_id {
                    cur_node.range = vec![type_id];
                }
            }
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
    }).expect("error parsing rdf/turtle file");

    // helps us make some hardcoded top-level types we want to "import" by
    // "hand" "so to" "speak."
    macro_rules! custom_type {
        ($id:expr, $typename:expr, $namespace:expr, $comment:expr) => {
            let default = Node::new($id);
            let mut cur_node = nodemap.entry($id.into()).or_insert(default);
            if cur_node.ty.is_none() { cur_node.ty = Some(NodeType::StructOrEnum); }
            if cur_node.comment.is_none() { cur_node.comment = Some($comment.into()); }
            if cur_node.custom.is_none() { cur_node.custom = Some(($typename.into(), $namespace.into())) };
        }
    }
    // foaf:Agent
    // NOTE: as an executive decision, going to put this into the vf namespace
    custom_type!("http://xmlns.com/foaf/0.1/Agent", "Agent", "vf", "A person or group or organization with economic agency.");
    // geo:SpatialThing
    custom_type!("http://www.w3.org/2003/01/geo/wgs84_pos#SpatialThing", "SpatialThing", "geo", "A mappable location.");
    // dfc:ProductBatch
    custom_type!("http://www.virtual-assembly.org/DataFoodConsortium/BusinessOntology#ProductBatch", "ProductBatch", "dfc", "A lot or batch, defining a resource produced at the same time in the same way. From DataFoodConsortium vocabulary https://datafoodconsortium.gitbook.io/dfc-standard-documentation/.");
    // om2:Measure
    custom_type!("http://www.ontology-of-units-of-measure.org/resource/om-2/Measure", "Measure", "om2", "A numeric value with its unit of measure.");
    // om2:Unit
    custom_type!("http://www.ontology-of-units-of-measure.org/resource/om-2/Unit", "Unit", "om2", "A unit of measure.");

    // loop over our node map and build our graph
    let mut schema = Schema::default();
    for (_, node) in &nodemap {
        let ns_id = node.namespace();

        match node.ty.clone() {
            Some(NodeType::StructOrEnum) => {
                if ns_id == "" { continue; }
                let ns = schema.ns.entry(ns_id.clone()).or_insert(Namespace::default());
                let class = ns.add_class(Class::from_node(node));

                // check if we have any fields WE want to add (ie, fields from
                // other specs that aren't included here)
                let extra_fields = (to_enum!(DataType, &class.id)).extra_fields();
                for field in extra_fields {
                    class.add_field(field);
                }
            }
            Some(NodeType::Field) | Some(NodeType::EnumVal) | Some(NodeType::DataType) | None => {
                if let Some((_, range, _)) = node.range_enum() {
                    let ns = schema.ns.entry(ns_id.clone()).or_insert(Namespace::default());
                    ns.add_union(RangeUnion::new(range));
                }
                for domain in &node.domain {
                    // only bother saving if our field has a parent node
                    if let Some(parent) = nodemap.get(domain) {
                        let ns = schema.ns.entry(parent.namespace()).or_insert(Namespace::default());
                        let graph_parent = ns.add_class(Class::from_node(&parent));

                        if node.ty == Some(NodeType::EnumVal) {
                            graph_parent.add_enumval(EnumVal::from_node(&node, &nodemap));
                        } else {
                            graph_parent.add_field(Field::from_node(&node));
                        }
                    }
                }
            }
            // "at this point we should have no literal types," he said, with a
            // boyish grin
            Some(NodeType::Literal(_)) => {}
            // none of these either
            Some(NodeType::Ontology) => {}
        }
    }
    schema
}

// -----------------------------------------------------------------------------
// Output
// -----------------------------------------------------------------------------

/// Print an enum that allows selection between two different types (ie, what is
/// a range union in rdf)
fn print_range_union(out: &mut StringWriter, range_union: &RangeUnion) {
    let types_array = range_union.types.iter().map(|x| RangeUnion::type_to_name(x, false)).collect::<Vec<_>>();
    out.line(&format!("/// An enum that allows a type union for ({})", types_array.join(", ")));
    out.line("#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]");
    out.line(&format!("pub enum {} {{", range_union.name()));
    out.inc_indent();
    for ty in &range_union.types {
        let typename = RangeUnion::type_to_name(ty, false);
        out.line(&format!("{}({}),", typename, typename));
    }
    out.dec_indent();
    out.line("}");
}

/// Print an enum defined in the schema
fn print_enum(out: &mut StringWriter, class: &Class) {
    if let Some(comment) = class.comment.as_ref() {
        out.line(&format!("/// {}", comment));
    }
    out.line("#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]");
    out.line(&format!("pub enum {} {{", class.name));
    out.inc_indent();
    for val in &class.enum_vals {
        if let Some(comment) = val.comment.as_ref() {
            out.line(&format!("/// {}", comment));
        }
        let label = if let Some(label) = val.label.as_ref() {
            label
        } else {
            &val.name
        };
        out.line(&format!(r#"#[serde(rename = "{}")]"#, label.to_kebab_case()));
        out.line(&format!("{},", val.name));
    }
    out.dec_indent();
    out.line("}");

    if class.properties.len() == 0 { return; }

    // now print our impls
    out.nl();
    out.line(&format!("impl {} {{", class.name));
    out.inc_indent();
    for prop in &class.properties {
        let prop_enum_vals = class.prop_enum_vals(prop);
        let returnclass = prop.ty.to_string(false).to_camel_case();
        let partial_impl = prop_enum_vals.len() != class.enum_vals.len();
        let returntype = if partial_impl {
            format!("Option<{}>", returnclass)
        } else {
            returnclass.clone()
        };
        out.line("#[allow(dead_code)]");
        out.line(&format!("pub fn {}(&self) -> {} {{", prop.name.to_snake_case(), returntype));
        out.inc_indent();
        out.line("match self {");
        out.inc_indent();
        for enumval in prop_enum_vals {
            let implval = enumval.find_impl(&prop.name).expect("missing enum impl val!");
            let implreturn = format!("{}::{}", returnclass, implval.enumval);
            let implreturn = if partial_impl {
                format!("Some({})", implreturn)
            } else {
                implreturn
            };
            out.line(&format!("Self::{} => {},", enumval.name, implreturn));
        }
        if partial_impl {
            out.line("_ => None,");
        }
        out.dec_indent();
        out.line("}");
        out.dec_indent();
        out.line("}");
        out.nl();
    }
    out.dec_indent();
    out.line("}");
}

/// Print a struct
fn print_struct(out: &mut StringWriter, class: &Class) {
    // start the struct
    if let Some(comment) = class.comment.as_ref() {
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
    out.line(&format!("pub struct {} {{", class.name));
    out.inc_indent();
    for field in &class.properties {
        let fieldname = field.name.to_snake_case();
        let fieldtype = field.ty.to_string(true);
        let mut meta = field.ty.meta(field.is_vec(), field.is_required());
        let fieldtype = if field.is_vec() {
            format!("Vec<{}>", fieldtype)
        } else if !field.is_required() {
            meta.push(r#"#[serde(skip_serializing_if = "Option::is_none")]"#.to_string());
            meta.push("#[builder(setter(into, strip_option), default)]".to_string());
            format!("Option<{}>", fieldtype)
        } else {
            fieldtype
        };
        if let Some(comment) = field.comment.as_ref() {
            out.line(&format!("/// {}", comment));
        }
        for metaline in meta {
            out.line(&metaline);
        }
        out.line(&format!("{}: {},", fieldname, fieldtype));
    }
    out.dec_indent();
    out.line("}");

    if cfg!(feature = "into_builder") {
        // build into_builder()
        out.nl();
        out.line(&format!("impl {} {{", class.name));
        out.inc_indent();
        out.line(&format!("/// Turns {0} into {0}Builder", class.name));
        out.line("#[allow(dead_code)]");
        out.line(&format!("pub fn into_builder(self) -> {}Builder {{", class.name));
        out.inc_indent();
        let fields = class.properties.iter()
            .map(|x| x.name.to_snake_case())
            .collect::<Vec<_>>()
            .join(", ");
        out.line(&format!("let {} {{ {} }} = self;", class.name, fields));
        out.line(&format!("let mut builder = {}Builder::default();", class.name));
        for field in &class.properties {
            if field.is_vec() || field.is_required() {
                out.line(&format!("builder = builder.{0}({0});", field.name.to_snake_case()));
            } else {
                out.line(&format!("builder = match {0} {{ Some(x) => builder.{0}(x), None => builder }};", field.name.to_snake_case()));
            }
        }
        out.line("builder");
        out.dec_indent();
        out.line("}");
        out.dec_indent();
        out.line("}");
    }
}

/// Prints our top-level schema "recursively" (ie, prints child nodes)
fn print_schema(mut schema: Schema) -> String {
    let mut out = StringWriter::new();
    let namespaces: Vec<String> = sorted_keys(&schema.ns);
    for ns in namespaces {
        if ns == "" { continue; }
        let namespace = schema.ns.get_mut(&ns).unwrap();

        // prepare the schema:
        // - sort the siblings at each level of our schema tree, which gives us
        //   deterministic output
        // - propagate optional/vec fields from structs into their respective
        //   fields
        namespace.prepare();

        out.line(&format!("mod {} {{", ns));
        out.inc_indent();
        out.line("use super::*;");
        for range_union in &namespace.unions {
            out.nl();
            print_range_union(&mut out, range_union);
        }
        for class in &namespace.classes {
            out.nl();
            if class.is_enum() {
                print_enum(&mut out, class);
            } else {
                print_struct(&mut out, class);
            }
        }
        out.dec_indent();
        out.line("}");
        out.nl();
    }
    out.to_string()
}

/// Prints the standard header for our generated output
fn print_header() -> String {
    let mut header = String::new();
    header.push_str("use std::time::Duration;\n");
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
    let header = print_header();
    let contents = print_schema(gen_schema());
    save(format!("{}\n{}", header, contents));
}

