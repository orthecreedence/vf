//! A set of ValueFlows structs and utils auto-generated from the [JSON schema][1].
//!
//! [1]: https://github.com/valueflows/vf-json-schema

mod ser;
mod gen;

pub use gen::*;


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn builders_work() {
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

