//! A set of ValueFlows structs and utils auto-generated from the [RDF schema][1].
//! The structs are all serde-(de)serializable and are generically typed to
//! allow a number of different methods of data modeling.
//!
//! The schema imports a number of structs from different RDF schemas, and each
//! of them is namespaced in this crate, with the main classes generated living
//! under `vf::*`.
//!
//! Given the nature of this library, it's important that various use cases are
//! possible. For instance, you might want to model a VF system with all types
//! tightly linked to each other as structs. You might want to loosely link the
//! objects together with IDs (such as when storing in a normalized database).
//! This means that the VF structs exported have generics for any references to
//! other VF objects. This allows using whatever types are desired when building
//! out your desired system.
//!
//! The structs exported have builder structs defined for them using the
//! wonderful [derive_builder][2] crate. So for example `Agent` also has a corresponding
//! `AgentBuilder` struct. Builder structs use the "owned" pattern, meaning the
//! builder methods consume the builder and return a new instance on each call.
//! The best way to create builders is by using the built-in `builder()`
//! function for each type (ie, `Agent::builder()`). Also,
//! given an existing `Agent` struct instance, you can call
//! `myagent.into_builder()` to convert (consume) it into an `AgentBuilder`,
//! which makes immutable updates fairly easy. The builder methods implement
//! Into so any type that has Into implemented for the field type can be
//! passed. Note that builder methods also strip Option, so if a struct field
//! is an `Option<T>` you can just pass T to the builder method.
//!
//! This library defines getters and setters for the provided structs via the
//! [getset][3] crate. It's important to note that by default, only getters are
//! defined. If you want setters, you can compiled with the feature
//! `getset_setters` and if you want mutable getters, use `getset_getmut`. This
//! allows side-stepping some of the enforced functional nature of the library
//! if you find that sort of thing obnoxious.
//!
//! Features:
//!
//! - `into_builder` - (default) implements `.into_builder()` for provided
//! structs so existing structs can be modified via the builder pattern.
//! - `getset_setters` - implements setters on the generated structs so they can
//! be mutated in-place via setter methods
//! - `getset_getmut` - implements mutable getters on the generated structs so
//! they can be mutated in-place via &mut getters
//!
//! Note that *all* features are enabled when building the docs to give a sense
//! of the library's full abilities.
//!
//! ```rust
//! use vf_rs::vf;
//!
//! // build a new agent with the builder pattern, using String for the id field type
//! let agent: vf::Agent = vf::Agent::builder()
//!     .name("Andrew")
//!     .note(Some("His hands are big".into()))
//!     .build().unwrap();
//! assert_eq!(agent.name(), "Andrew");
//! assert_eq!(agent.note(), &Some("His hands are big".into()));
//! assert_eq!(agent.image(), &None);
//! // create a new agent with a different label
//! let new_agent = agent.into_builder()
//!     .note(Some("DOES NOT HAVE SMALL HANDS".into()))
//!     .build().unwrap();
//! assert_eq!(new_agent.name(), "Andrew");
//! assert_eq!(new_agent.note(), &Some("DOES NOT HAVE SMALL HANDS".into()));
//! ```
//!
//! Note that this library contains absolutely no ValueFlows logic and exists
//! solely as a definition for VF types.
//!
//! [1]: https://github.com/valueflows/valueflows/blob/master/release-doc-in-process/all_vf.TTL
//! [2]: https://colin-kiegel.github.io/rust-derive-builder/
//! [3]: https://docs.rs/getset/

mod gen;

// import everything lol
pub use gen::*;

#[cfg(test)]
mod test {
    use super::*;
    use serde_json;
    use url::Url;
    use chrono::prelude::*;

    #[test]
    fn builder() {
        let agent: vf::Agent = vf::Agent::builder()
            .name("Andrew")
            .note(Some("His hands are big".into()))
            .build().unwrap();
        assert_eq!(agent.name(), "Andrew");
        assert_eq!(agent.note(), &Some("His hands are big".to_string()));
        assert_eq!(agent.image(), &None);
    }

    #[cfg(feature = "into_builder")]
    #[test]
    fn into_builder() {
        let agent: vf::Agent = vf::Agent::builder()
            .name("Andrew")
            .note(Some("His hands are big".into()))
            .build().unwrap();
        let agent_builder = agent.clone().into_builder();
        let agent2 = agent_builder.build().unwrap();
        assert_eq!(agent, agent2);

        let agent3 = agent.clone().into_builder()
            .name("LARRY".to_string())
            .build().unwrap();
        assert!(agent2 != agent3);
    }

    #[test]
    fn builder_throws_on_incomplete_struct() {
        let res: Result<vf::EconomicResource<Url, String, String, String, String>, String> = vf::EconomicResource::builder()
            .name(Some("hi my name is butch".into()))
            .build();
        match res {
            Ok(_) => panic!("Builder did not throw on missing required field"),
            Err(_) => {}
        }
    }

    #[test]
    fn builder_setter_into() {
        let agent: vf::Agent = vf::Agent::builder()
            .name("Andrew".to_string())
            .build().unwrap();
        assert_eq!(agent.name(), "Andrew");
        let agent: vf::Agent = vf::Agent::builder()
            .name("Andrew")
            .build().unwrap();
        assert_eq!(agent.name(), "Andrew");
    }

    #[cfg(feature = "with_serde")]
    #[test]
    fn serializes() {
        let location = geo::SpatialThing::builder()
            .name(Some("https://basisproject.gitlab.io/public/".into()))
            .build().unwrap();
        let agent: vf::Agent = vf::Agent::builder()
            .image("https://basisproject.gitlab.io/public/assets/images/red_star.256.outline.png".parse::<Url>().unwrap())
            .name("Basis")
            .primary_location(location)
            .build().unwrap();
        let json = serde_json::to_string(&agent).unwrap();
        assert_eq!(json, r#"{"image":"https://basisproject.gitlab.io/public/assets/images/red_star.256.outline.png","name":"Basis","primary_location":{"name":"https://basisproject.gitlab.io/public/"}}"#);
    }

    #[cfg(feature = "with_serde")]
    #[test]
    fn deserializes() {
        let json = r#"{"image":"https://basisproject.gitlab.io/public/assets/images/red_star.256.outline.png","name":"Basis","primary_location":{"name":"https://basisproject.gitlab.io/public/"}}"#;
        let agent: vf::Agent = serde_json::from_str(json).unwrap();
        let location = agent.primary_location().as_ref().unwrap();
        assert_eq!(agent.image(), &Some("https://basisproject.gitlab.io/public/assets/images/red_star.256.outline.png".parse::<Url>().unwrap()));
        assert_eq!(agent.name(), "Basis");
        assert_eq!(agent.note(), &None);
        assert_eq!(location.name(), &Some("https://basisproject.gitlab.io/public/".into()));
    }

    #[cfg(feature = "getset_setters")]
    #[test]
    fn getset_setters() {
        let mut plan = vf::Plan::builder()
            .created("2018-04-01T00:01:01Z".parse::<DateTime<Utc>>().unwrap())
            .name("GOSHPLAN".to_string())
            .build().unwrap();
        assert_eq!(plan.name(), &Some("GOSHPLAN".to_string()));
        plan.set_name(Some("Gffft".into()));
        assert_eq!(plan.name(), &Some("Gffft".to_string()));
    }

    #[cfg(feature = "getset_getmut")]
    #[test]
    fn getset_getmut() {
        let mut plan = vf::Plan::builder()
            .created("2018-04-01T00:01:01Z".parse::<DateTime<Utc>>().unwrap())
            .name("GOSHPLAN".to_string())
            .build().unwrap();
        assert_eq!(plan.name(), &Some("GOSHPLAN".to_string()));
        (*plan.name_mut()) = Some("Gffft".into());
        assert_eq!(plan.name(), &Some("Gffft".to_string()));
    }
}

