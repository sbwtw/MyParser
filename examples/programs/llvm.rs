extern crate llvm_sys;

use llvm_sys::core::*;

fn main() {
    unsafe {
        let context = LLVMContextCreate();
        let module = LLVMModuleCreateWithNameInContext(b"sum\0".as_ptr() as *const _, context);
        let builder = LLVMCreateBuilderInContext(context);

        let i64t = LLVMInt64TypeInContext(context);
        let mut argts = [i64t, i64t];
        let function_type = LLVMFunctionType(i64t, argts.as_mut_ptr(), argts.len() as u32, 0);

        let function = LLVMAddFunction(module, b"sum\0".as_ptr() as *const _, function_type);

        let bb = LLVMAppendBasicBlockInContext(context, function, b"entry\0".as_ptr() as *const _);

        LLVMPositionBuilderAtEnd(builder, bb);

        // LLVMBuildRet(builder, sum);
        LLVMDisposeBuilder(builder);
        LLVMDumpModule(module);
    }
}
