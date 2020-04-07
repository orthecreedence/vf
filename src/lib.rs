//! A set of ValueFlows structs and utils auto-generated from the [JSON schema][1].
//!
//! The structs defined use the same names as the `title` field in the
//! restecpive schema. Enums are also defined for enum types and are namespaced
//! with the corresponding struct. For instance, the `Action` struct lives under
//! `action::Action` and has an enum field `label` that uses an enum type
//! define under `action::Label`. The organization of this project closely ties
//! in the with schema itself.
//!
//! The structs exported have builder structs defined for them using the
//! wonderful [derive_builder][2] crate. So `Action` also has a corresponding
//! `ActionBuilder` struct. Builder structs use the "owned" pattern, meaning the
//! builder methods consume the builder and return a new instance on each call.
//! Given an existing `Action` struct instance, you can call
//! `myaction.into_builder()` to convert (consume) it into an `ActionBuilder`,
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
//! use vf_rs::action;
//!
//! // build a new action with the builder pattern
//! let action = action::ActionBuilder::default()
//!     .label(action::Label::TransferAllRights)
//!     .resource_effect(action::ResourceEffect::Increment)
//!     .build().unwrap();
//! assert_eq!(action.label(), &Some(action::Label::TransferAllRights));
//! assert_eq!(action.resource_effect(), &action::ResourceEffect::Increment);
//! // create a new action with a different label
//! let new_action = action.into_builder()
//!     .label(action::Label::TransferCustody)
//!     .build().unwrap();
//! assert_eq!(new_action.label(), &Some(action::Label::TransferCustody));
//! assert_eq!(new_action.resource_effect(), &action::ResourceEffect::Increment);
//! ```
//!
//! [1]: https://github.com/valueflows/vf-json-schema
//! [2]: https://colin-kiegel.github.io/rust-derive-builder/
//! [3]: https://docs.rs/getset/

mod ser;
mod gen;

pub use gen::*;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn builder() {
        let action = action::ActionBuilder::default()
            .input_output(action::InputOutput::Output)
            .label(action::Label::Consume)
            .resource_effect(action::ResourceEffect::Increment)
            .build()
            .unwrap();
        assert_eq!(action.input_output(), &Some(action::InputOutput::Output));
        assert_eq!(action.label(), &Some(action::Label::Consume));
        assert_eq!(action.pairs_with(), &None);
        assert_eq!(action.resource_effect(), &action::ResourceEffect::Increment);
    }

    #[test]
    fn into_builder() {
        let action = action::ActionBuilder::default()
            .input_output(action::InputOutput::Output)
            .label(action::Label::Consume)
            .resource_effect(action::ResourceEffect::Increment)
            .build()
            .unwrap();
        let action_builder = action.clone().into_builder();
        let action2 = action_builder.build().unwrap();
        assert_eq!(action, action2);

        let action3 = action.clone().into_builder()
            .label(action::Label::Dropoff)
            .build().unwrap();
        assert!(action2 != action3);
    }

    #[test]
    fn builder_throws_on_incomplete_struct() {
        let res = economic_resource::EconomicResourceBuilder::default()
            .name(String::from("hi my name is butch"))
            .build();
        match res {
            Ok(_) => panic!("Builder did not throw on missing required field"),
            Err(_) => {}
        }
    }

    #[cfg(feature = "getset_setters")]
    #[test]
    fn getset_setters() {
        let mut plan = plan::PlanBuilder::default()
            .created("2018-04-01T00:01:01Z".parse().unwrap())
            .name("GOSHPLAN".into())
            .build().unwrap();
        assert_eq!(plan.name(), "GOSHPLAN");
        plan.set_name("Gffft".into());
        assert_eq!(plan.name(), "Gffft");
    }

    #[cfg(feature = "getset_getmut")]
    #[test]
    fn getset_getmut() {
        let mut plan = plan::PlanBuilder::default()
            .created("2018-04-01T00:01:01Z".parse().unwrap())
            .name("GOSHPLAN".into())
            .build().unwrap();
        assert_eq!(plan.name(), "GOSHPLAN");
        (*plan.name_mut()) = "Gffft".into();
        assert_eq!(plan.name(), "Gffft");
    }
}

