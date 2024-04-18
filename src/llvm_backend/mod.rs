use inkwell::{
    context::Context,
    targets::{InitializationConfig, Target},
    IntPredicate,
};

use crate::core::{log, log_impure, LogLevel};

pub fn compile_ast_to_llvm() {
    log(LogLevel::Warn, || {
        "llvm-backend is HIGHLY EXPERIMENTAL. Use with caution.".to_string()
    });

    let context = Context::create();
    Target::initialize_native(&InitializationConfig::default()).unwrap();

    let module = context.create_module("ast_to_llvm");
    let execution_engine = module
        .create_jit_execution_engine(inkwell::OptimizationLevel::None)
        .unwrap();

    let builder = context.create_builder();

    let i32_type = context.i32_type();
    let void_type = context.void_type();
    let fn_type = void_type.fn_type(&[], false);
    let main_function = module.add_function("main", fn_type, None);
    let entry_block = context.append_basic_block(main_function, "entry");
    let loop_cond_block = context.append_basic_block(main_function, "loop_cond");
    let loop_body_block = context.append_basic_block(main_function, "loop_body");
    let after_loop_block = context.append_basic_block(main_function, "after_loop");

    builder.position_at_end(entry_block);
    let i_initial = i32_type.const_int(0, false);
    let _ = builder.build_unconditional_branch(loop_cond_block);

    builder.position_at_end(loop_cond_block);
    let phi_i = builder.build_phi(i32_type, "i").unwrap();
    phi_i.add_incoming(&[(&i_initial, entry_block)]);

    let n_val = i32_type.const_int(10000, false);
    let cond = builder
        .build_int_compare(
            IntPredicate::SLT,
            phi_i.as_basic_value().into_int_value(),
            n_val,
            "loopcond",
        )
        .unwrap();
    let _ = builder.build_conditional_branch(cond, loop_body_block, after_loop_block);

    builder.position_at_end(loop_body_block);
    let one = i32_type.const_int(1, false);
    let new_i_val = builder
        .build_int_add(phi_i.as_basic_value().into_int_value(), one, "newi")
        .unwrap();
    phi_i.add_incoming(&[(&new_i_val, loop_body_block)]);
    let _ = builder.build_unconditional_branch(loop_cond_block);

    builder.position_at_end(after_loop_block);
    let _ = builder.build_return(None);

    module.verify().unwrap();

    log_impure(LogLevel::Trace, || module.print_to_stderr());

    unsafe {
        let main_fn = execution_engine
            .get_function::<unsafe extern "C" fn()>("main")
            .unwrap();
        main_fn.call();
    }

    // We should eventually support print statements such as these in the IR though I don't know
    // yet what that looks like.
    println!("Done");
}
