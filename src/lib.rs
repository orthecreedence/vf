//! A set of ValueFlows structs and utils auto-generated from the [JSON schema][1].
//!
//! The structs defined use the same names as the `title` field in the
//! restecpive schema. Enums are also defined for enum types, for instance the
//! Action struct's `label` field has an ActionLabel type, which is an enum that
//! has the enum values from the schema defined in it.
//!
//! The structs exported have builder structs defined for them using the
//! wonderful derive_builder crate. So `Action` also has a corresponding
//! `ActionBuilder` struct. Builder structs use the "owned" pattern, meaning the
//! builder methods consume the builder and return a new instance on each call.
//! Given an existing `Action` struct instance, you can call
//! `myaction.into_builder()` to convert (consume) it into an `ActionBuilder`,
//! which makes immutable updates fairly easy.
//!
//! ```rust
//! use vf_rs::action;
//!
//! // build a new action with the builder pattern
//! let action = action::ActionBuilder::default()
//!     .label(action::Label::TransferAllRights)
//!     .resource_effect(action::ResourceEffect::Increment)
//!     .build().unwrap();
//! // create a new action with a different label
//! let new_action = action.into_builder()
//!     .label(action::Label::TransferCustody)
//!     .build().unwrap();
//! ```
//!
//! [1]: https://github.com/valueflows/vf-json-schema

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
        assert_eq!(action.input_output, Some(action::InputOutput::Output));
        assert_eq!(action.label, Some(action::Label::Consume));
        assert_eq!(action.pairs_with, None);
        assert_eq!(action.resource_effect, action::ResourceEffect::Increment);
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
}

