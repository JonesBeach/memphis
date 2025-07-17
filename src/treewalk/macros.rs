macro_rules! impl_typed {
    ($type_name:ident, $type_enum:expr) => {
        impl $crate::treewalk::type_system::Typed for $type_name {
            fn get_type() -> $crate::domain::Type {
                $type_enum
            }
        }
    };
}

macro_rules! impl_method_provider {
    ($type_name:ident, [$($method:expr),* $(,)?]) => {
        impl $crate::treewalk::type_system::MethodProvider for $type_name {
            fn get_methods() -> Vec<Box<dyn $crate::treewalk::type_system::CloneableCallable>> {
                vec![
                    $(Box::new($method)),*
                ]
            }
        }
    };
}

macro_rules! impl_descriptor_provider {
    ($type_name:ident, [$($method:expr),* $(,)?]) => {
        impl $crate::treewalk::type_system::DescriptorProvider for $type_name {
            fn get_descriptors() -> Vec<Box<dyn $crate::treewalk::type_system::CloneableNonDataDescriptor>> {
                vec![
                    $(Box::new($method)),*
                ]
            }
        }
    };
}

macro_rules! impl_data_descriptor_provider {
    ($type_name:ident, [$($method:expr),* $(,)?]) => {
        impl $crate::treewalk::type_system::DataDescriptorProvider for $type_name {
            fn get_data_descriptors() -> Vec<Box<dyn $crate::treewalk::type_system::CloneableDataDescriptor>> {
                vec![
                    $(Box::new($method)),*
                ]
            }
        }
    };
}

macro_rules! impl_iterable {
    ($iter_name:ident) => {
        impl $crate::treewalk::protocols::Iterable for $iter_name {
            fn try_next(
                &mut self,
            ) -> $crate::treewalk::TreewalkResult<Option<$crate::treewalk::TreewalkValue>> {
                Ok(Iterator::next(self))
            }
        }
    };
}

pub(crate) use impl_data_descriptor_provider;
pub(crate) use impl_descriptor_provider;
pub(crate) use impl_iterable;
pub(crate) use impl_method_provider;
pub(crate) use impl_typed;
