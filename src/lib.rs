//! A set of ValueFlows structs and utils auto-generated from the [RDF schema][1].
//!
//! The schema imports a number of structs from different RDF schemas, and each
//! of them is namespaced in this crate, with the main classes generated living
//! under `vf::*`.
//!
//! The structs exported have builder structs defined for them using the
//! wonderful [derive_builder][2] crate. So `Agent` also has a corresponding
//! `AgentBuilder` struct. Builder structs use the "owned" pattern, meaning the
//! builder methods consume the builder and return a new instance on each call.
//! Given an existing `Agent` struct instance, you can call
//! `myaction.into_builder()` to convert (consume) it into an `AgentBuilder`,
//! which makes immutable updates fairly easy.
//!
//! This library defines getters and setters for the provided structs via the
//! [getset][3] crate. It's important to note that by default, only getters are
//! defined. If you want setters, you can compiled with the feature
//! `getset_setters` and if you want mutable getters, use `getset_getmut`. This
//! allows side-stepping some of the enforced functional nature of the library
//! if you find that sort of thing obnoxious.
//!
//! ```rust
//! use vf_rs::vf;
//!
//! // build a new action with the builder pattern
//! let agent = vf::AgentBuilder::default()
//!     .name("Andrew".to_string())
//!     .note("His hands are big".to_string())
//!     .build().unwrap();
//! assert_eq!(agent.name(), "Andrew");
//! assert_eq!(agent.note(), &Some("His hands are big".to_string()));
//! assert_eq!(agent.image(), &None);
//! // create a new action with a different label
//! let new_agent = agent.into_builder()
//!     .note("DOES NOT HAVE SMALL HANDS".to_string())
//!     .build().unwrap();
//! assert_eq!(new_agent.name(), "Andrew");
//! assert_eq!(new_agent.note(), &Some("DOES NOT HAVE SMALL HANDS".to_string()));
//! ```
//!
//! [1]: https://github.com/valueflows/valueflows/blob/master/release-doc-in-process/all_vf.TTL
//! [2]: https://colin-kiegel.github.io/rust-derive-builder/
//! [3]: https://docs.rs/getset/

mod ser;
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
        let agent = vf::AgentBuilder::default()
            .name("Andrew".to_string())
            .note("His hands are big".to_string())
            .build().unwrap();
        assert_eq!(agent.name(), "Andrew");
        assert_eq!(agent.note(), &Some("His hands are big".to_string()));
        assert_eq!(agent.image(), &None);
    }

    #[cfg(feature = "into_builder")]
    #[test]
    fn into_builder() {
        let agent = vf::AgentBuilder::default()
            .name("Andrew".to_string())
            .note("His hands are big".to_string())
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
        let res = vf::EconomicResourceBuilder::default()
            .name(String::from("hi my name is butch"))
            .build();
        match res {
            Ok(_) => panic!("Builder did not throw on missing required field"),
            Err(_) => {}
        }
    }

    #[test]
    fn serializes() {
        let location = geo::SpatialThingBuilder::default()
            .name(String::from("https://basisproject.gitlab.io/public/"))
            .build().unwrap();
        let agent = vf::AgentBuilder::default()
            .image("https://basisproject.gitlab.io/public/assets/images/red_star.256.outline.png".parse::<Url>().unwrap())
            .name("Basis".into())
            .primary_location(location)
            .build().unwrap();
        let json = serde_json::to_string(&agent).unwrap();
        assert_eq!(json, r#"{"image":"https://basisproject.gitlab.io/public/assets/images/red_star.256.outline.png","name":"Basis","primary_location":{"name":"https://basisproject.gitlab.io/public/"}}"#);
    }

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
        let mut plan = vf::PlanBuilder::default()
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
        let mut plan = vf::PlanBuilder::default()
            .created("2018-04-01T00:01:01Z".parse::<DateTime<Utc>>().unwrap())
            .name("GOSHPLAN".to_string())
            .build().unwrap();
        assert_eq!(plan.name(), &Some("GOSHPLAN".to_string()));
        (*plan.name_mut()) = Some("Gffft".into());
        assert_eq!(plan.name(), &Some("Gffft".to_string()));
    }
}

