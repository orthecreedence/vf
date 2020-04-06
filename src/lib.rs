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
//! use vf_rs::*;
//!
//! let action = ActionBuilder::default()
//!     // notice the label field has "action" prepended to namespace the enum
//!     // into ActionLabel
//!     .label(ActionLabel::Consume)
//!     .resource_effect(ActionResourceEffect::Increment)
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
        let action = ActionBuilder::default()
            .input_output(ActionInputOutput::Output)
            .label(ActionLabel::Consume)
            .resource_effect(ActionResourceEffect::Increment)
            .build()
            .unwrap();
        assert_eq!(action.input_output, Some(ActionInputOutput::Output));
        assert_eq!(action.label, Some(ActionLabel::Consume));
        assert_eq!(action.pairs_with, None);
        assert_eq!(action.resource_effect, ActionResourceEffect::Increment);
    }

    #[test]
    fn into_builder() {
        let action = ActionBuilder::default()
            .input_output(ActionInputOutput::Output)
            .label(ActionLabel::Consume)
            .resource_effect(ActionResourceEffect::Increment)
            .build()
            .unwrap();
        let action_builder = action.clone().into_builder();
        let action2 = action_builder.build().unwrap();
        assert_eq!(action, action2);

        let action3 = action.clone().into_builder()
            .label(ActionLabel::Dropoff)
            .build().unwrap();
        assert!(action2 != action3);
    }
}

