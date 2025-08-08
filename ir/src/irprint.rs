use crate::code::Code;
use crate::{BlockRef, Instr, InstrRef, Meta, PhiRef};
use common::TypeId;
use std::fmt::Write;

/// Printer for IR code that produces human-readable assembly-like output
pub struct IrPrinter<'a> {
    code: &'a Code,
    use_colors: bool,
    buffer: String,
}

#[derive(Clone, Copy)]
enum Color {
    Reset,
    Instruction, // Blue
    Reference,   // Green
    Type,        // Yellow
    Constant,    // Cyan
    Comment,     // Gray
}

impl Color {
    fn as_ansi(self) -> &'static str {
        match self {
            Color::Reset => "\x1b[0m",
            Color::Instruction => "\x1b[34m", // Blue
            Color::Reference => "\x1b[32m",   // Green
            Color::Type => "\x1b[33m",        // Yellow
            Color::Constant => "\x1b[36m",    // Cyan
            Color::Comment => "\x1b[90m",     // Gray
        }
    }
}

impl<'a> IrPrinter<'a> {
    /// Create a new IR printer without colors
    pub fn new(code: &'a Code) -> Self {
        Self {
            code,
            use_colors: false,
            buffer: String::with_capacity(1024),
        }
    }

    /// Create a new IR printer with ANSI colors
    pub fn new_colored(code: &'a Code) -> Self {
        Self {
            code,
            use_colors: true,
            buffer: String::with_capacity(1024),
        }
    }

    /// Print the IR code and return the formatted string
    pub fn print(mut self) -> String {
        self.buffer.clear();

        self.print_constants();
        self.print_scheduled_code();

        self.buffer
    }

    fn print_constants(&mut self) {
        let mut has_constants = false;

        // Check if we have any negative indices (constants)
        for (instr_ref, _) in self.code.iter_with_refs() {
            if instr_ref.get() < 0 {
                has_constants = true;
                break;
            }
        }

        if !has_constants {
            return;
        }

        self.write_colored(Color::Comment, "; === CONSTANTS & UNSCHEDULED ===\n");

        for (instr_ref, instr) in self.code.iter_with_refs() {
            if instr_ref.get() < 0 {
                self.print_constant_instruction(instr_ref, instr);
            }
        }

        self.buffer.push('\n');
    }

    fn print_scheduled_code(&mut self) {
        let mut has_scheduled = false;

        // Check if we have any positive indices (scheduled instructions)
        for (instr_ref, _) in self.code.iter_with_refs() {
            if instr_ref.get() > 0 {
                has_scheduled = true;
                break;
            }
        }

        if !has_scheduled {
            return;
        }

        self.write_colored(Color::Comment, "; === FUNCTION CODE ===\n");

        for (instr_ref, instr) in self.code.iter_with_refs() {
            if instr_ref.get() > 0 {
                match instr {
                    Instr::Label(_, block_ref) => {
                        // Print block header comment, don't print the label instruction
                        let block_name = self.format_block_ref(*block_ref);
                        write!(&mut self.buffer, "; Block {}:\n", block_name).unwrap();
                    }
                    _ => {
                        self.print_scheduled_instruction(instr_ref, instr);
                    }
                }
            }
        }
    }

    fn print_constant_instruction(&mut self, instr_ref: InstrRef, instr: &Instr) {
        // Format: "c1:    const_i32  42        ; i32"
        let const_label = self.format_const_ref(instr_ref);
        self.write_colored(Color::Reference, &format!("{}:", const_label));
        self.buffer.push_str("    ");

        self.print_instruction_body(instr);
        self.print_type_comment(instr.get_meta());
        self.buffer.push('\n');
    }

    fn print_scheduled_instruction(&mut self, instr_ref: InstrRef, instr: &Instr) {
        // Only print instruction label for non-void instructions
        if instr.get_type_id() != TypeId::UNIT {
            let instr_label = self.format_instr_ref(instr_ref);
            self.write_colored(Color::Reference, &format!("{}:", instr_label));
            self.buffer.push_str("    ");
        } else {
            // Extra spaces to align with labeled instructions
            self.buffer.push_str("       ");
        }

        self.print_instruction_body(instr);

        // Only print type comment for non-void
        if instr.get_type_id() != TypeId::UNIT {
            self.print_type_comment(instr.get_meta());
        }

        self.buffer.push('\n');
    }

    fn print_instruction_body(&mut self, instr: &Instr) {
        match instr {
            Instr::Nop(_) => {
                let name = self.get_instr_name(instr);
                self.write_colored(Color::Instruction, name);
            }
            Instr::Never(_) => {
                let name = self.get_instr_name(instr);
                self.write_colored(Color::Instruction, name);
            }
            Instr::Identity(_, operand) => {
                let name = self.get_instr_name(instr);
                self.print_unary_instr(name, *operand);
            }
            Instr::Print(_, operand) => {
                let name = self.get_instr_name(instr);
                self.print_unary_instr(name, *operand);
            }
            Instr::Label(_, block_ref) => {
                let name = self.get_instr_name(instr);
                self.write_colored(Color::Instruction, name);
                self.buffer.push_str(self.calculate_spacing(name));
                self.write_colored(Color::Reference, &self.format_block_ref(*block_ref));
            }
            Instr::Jump(_, block_ref) => {
                let name = self.get_instr_name(instr);
                self.write_colored(Color::Instruction, name);
                self.buffer.push_str(self.calculate_spacing(name));
                self.write_colored(Color::Reference, &self.format_block_ref(*block_ref));
            }
            Instr::Branch(_, cond, true_block, false_block) => {
                let name = self.get_instr_name(instr);
                self.write_colored(Color::Instruction, name);
                self.buffer.push_str(self.calculate_spacing(name));
                self.write_colored(Color::Reference, &self.format_instr_ref(*cond));
                self.buffer.push_str(", ");
                self.write_colored(Color::Reference, &self.format_block_ref(*true_block));
                self.buffer.push_str(", ");
                self.write_colored(Color::Reference, &self.format_block_ref(*false_block));
            }
            Instr::Ret(_, value) => {
                let name = self.get_instr_name(instr);
                self.print_unary_instr(name, *value);
            }
            Instr::Upsilon(_, phi_ref, value) => {
                let name = self.get_instr_name(instr);
                self.write_colored(Color::Instruction, name);
                self.buffer.push_str(self.calculate_spacing(name));
                self.write_colored(Color::Reference, &self.format_phi_ref(*phi_ref));
                self.buffer.push_str(", ");
                self.write_colored(Color::Reference, &self.format_instr_ref(*value));
            }
            Instr::Phi(_, phi_ref) => {
                let name = self.get_instr_name(instr);
                self.write_colored(Color::Instruction, name);
                self.buffer.push_str(self.calculate_spacing(name));
                self.write_colored(Color::Reference, &self.format_phi_ref(*phi_ref));
            }
            Instr::ConstUnit(_) => {
                let name = self.get_instr_name(instr);
                self.write_colored(Color::Instruction, name);
            }
            Instr::ConstBool(_, value) => {
                let name = self.get_instr_name(instr);
                self.write_colored(Color::Instruction, name);
                self.buffer.push_str(" ");
                self.write_colored(Color::Constant, &value.to_string());
            }
            Instr::ConstInt(_, value) => {
                let name = self.get_instr_name(instr);
                self.write_colored(Color::Instruction, name);
                self.buffer.push_str("  ");
                self.write_colored(Color::Constant, &value.to_string());
            }
            Instr::ConstUInt(_, value) => {
                let name = self.get_instr_name(instr);
                self.write_colored(Color::Instruction, name);
                self.buffer.push_str("  ");
                self.write_colored(Color::Constant, &value.to_string());
            }
            Instr::ConstFloat(_, value) => {
                let name = self.get_instr_name(instr);
                self.write_colored(Color::Instruction, name);
                self.buffer.push_str("  ");
                self.write_colored(Color::Constant, &value.to_string());
            }
            Instr::ConstTemplate(_, value) => {
                let name = self.get_instr_name(instr);
                self.write_colored(Color::Instruction, name);
                self.buffer.push_str("  ");
                self.write_colored(Color::Constant, &value.to_string());
            }
            Instr::ConstFunction(_, value) => {
                let name = self.get_instr_name(instr);
                self.write_colored(Color::Instruction, name);
                self.buffer.push_str("  ");
                self.write_colored(Color::Constant, &value.to_string());
            }
            Instr::ConstType(_, value) => {
                let name = self.get_instr_name(instr);
                self.write_colored(Color::Instruction, name);
                self.buffer.push_str("  ");
                self.write_colored(Color::Constant, &value.to_string());
            }
            Instr::Arg(_, index) => {
                let name = self.get_instr_name(instr);
                self.write_colored(Color::Instruction, name);
                self.buffer.push_str(self.calculate_spacing(name));
                self.write_colored(Color::Constant, &index.to_string());
            }
            Instr::Call(_, _func, _args) | Instr::PureCall(_, _func, _args) => {
                let name = self.get_instr_name(instr);
                self.write_colored(Color::Instruction, name);
                // TODO: print call instructions
            }
            // Binary operators - all follow same pattern
            Instr::Cons(_, left, right)
            | Instr::Add(_, left, right)
            | Instr::Sub(_, left, right)
            | Instr::Mul(_, left, right)
            | Instr::Div(_, left, right)
            | Instr::Eq(_, left, right)
            | Instr::Neq(_, left, right)
            | Instr::Lt(_, left, right)
            | Instr::Gt(_, left, right)
            | Instr::LtEq(_, left, right)
            | Instr::GtEq(_, left, right)
            | Instr::And(_, left, right)
            | Instr::Or(_, left, right) => {
                let name = self.get_instr_name(instr);
                self.print_binary_instr(name, *left, *right);
            }
            // Unary operators - all follow same pattern
            Instr::Neg(_, operand)
            | Instr::Not(_, operand)
            | Instr::CheckedCast(_, operand)
            | Instr::UncheckedCast(_, operand)
            | Instr::BitCast(_, operand) => {
                // TODO: properly handle the casts (show target type)
                let name = self.get_instr_name(instr);
                self.print_unary_instr(name, *operand);
            }
            // Memory operations
            Instr::StackAlloc(_, size) => {
                let name = self.get_instr_name(instr);
                self.write_colored(Color::Instruction, name);
                self.buffer.push_str("  ");
                self.write_colored(Color::Constant, &size.to_string());
            }
            Instr::Load(_, ptr) => {
                let name = self.get_instr_name(instr);
                self.print_unary_instr(name, *ptr);
            }
            Instr::Store(_, ptr) => {
                let name = self.get_instr_name(instr);
                self.print_unary_instr(name, *ptr);
            }
        }
    }

    fn get_instr_name(&self, instr: &Instr) -> &'static str {
        match instr {
            Instr::Nop(_) => "nop",
            Instr::Never(_) => "never",
            Instr::Identity(..) => "identity",
            Instr::Print(..) => "print",
            Instr::Call(..) => "call",
            Instr::Label(..) => "label",
            Instr::Jump(..) => "jump",
            Instr::Branch(..) => "branch",
            Instr::Ret(..) => "ret",
            Instr::Upsilon(..) => "upsilon",
            Instr::Phi(..) => "phi",
            Instr::ConstUnit(..) => "const_unit",
            Instr::ConstBool(..) => "const_bool",
            Instr::ConstInt(..) => "const_int",
            Instr::ConstUInt(..) => "const_uint",
            Instr::ConstFloat(..) => "const_float",
            Instr::ConstTemplate(..) => "const_generic_fn",
            Instr::ConstFunction(..) => "const_concrete_fn",
            Instr::ConstType(..) => "const_type_id",
            Instr::Arg(..) => "arg",
            Instr::PureCall(..) => "pure_call",
            Instr::Cons(..) => "cons",
            Instr::Add(..) => "add",
            Instr::Sub(..) => "sub",
            Instr::Mul(..) => "mul",
            Instr::Div(..) => "div",
            Instr::Eq(..) => "eq",
            Instr::Neq(..) => "neq",
            Instr::Lt(..) => "lt",
            Instr::Gt(..) => "gt",
            Instr::LtEq(..) => "lt_eq",
            Instr::GtEq(..) => "gt_eq",
            Instr::And(..) => "and",
            Instr::Or(..) => "or",
            Instr::Neg(..) => "neg",
            Instr::Not(..) => "not",
            Instr::CheckedCast(..) => "checked_cast",
            Instr::UncheckedCast(..) => "unchecked_cast",
            Instr::BitCast(..) => "bit_cast",
            Instr::StackAlloc(..) => "stack_alloc",
            Instr::Load(..) => "load",
            Instr::Store(..) => "store",
        }
    }

    fn calculate_spacing(&self, instr_name: &str) -> &'static str {
        // Match the exact spacing from original code for compatibility
        match instr_name {
            // Most operators: 8 spaces (total width ~11-12)
            "add" | "sub" | "mul" | "div" | "neq" | "and" | "neg" | "not" | "ret" | "arg" | "phi" => "        ",
            // Short operators get 9 spaces
            "eq" | "lt" | "gt" | "or" => "         ",
            // 5-char operators get 6 spaces
            "lt_eq" | "gt_eq" => "      ",
            // 4-char operators get 7 spaces
            "jump" => "       ",
            // Special cases that need different spacing
            "label" | "print" => "      ",
            "upsilon" => "    ",
            "branch" => "     ",
            "identity" => "  ",
            // Fallback for any missed cases
            _ => "        ",
        }
    }

    fn print_binary_instr(&mut self, name: &str, left: InstrRef, right: InstrRef) {
        self.write_colored(Color::Instruction, name);
        self.buffer.push_str(self.calculate_spacing(name));
        self.write_colored(Color::Reference, &self.format_instr_ref(left));
        self.buffer.push_str(", ");
        self.write_colored(Color::Reference, &self.format_instr_ref(right));
    }

    fn print_unary_instr(&mut self, name: &str, operand: InstrRef) {
        self.write_colored(Color::Instruction, name);
        self.buffer.push_str(self.calculate_spacing(name));
        self.write_colored(Color::Reference, &self.format_instr_ref(operand));
    }

    fn print_type_comment(&mut self, meta: Meta) {
        let type_str = format!("{}", meta.get_type_id().get_type());

        // Add padding to align comments
        let padding = 20_i32.saturating_sub(self.buffer.len() as i32 % 100); // Rough alignment
        for _ in 0..padding.max(2) {
            self.buffer.push(' ');
        }

        self.write_colored(Color::Comment, "; ");
        self.write_colored(Color::Type, &type_str);
    }

    fn format_const_ref(&self, instr_ref: InstrRef) -> String {
        let index = instr_ref.get().abs() as u32;
        format!("c{}", index)
    }

    fn format_instr_ref(&self, instr_ref: InstrRef) -> String {
        let index = instr_ref.get();
        if index < 0 {
            self.format_const_ref(instr_ref)
        } else {
            format!("i{}", index)
        }
    }

    fn format_block_ref(&self, block_ref: BlockRef) -> String {
        format!("b{}", block_ref.get().abs())
    }

    fn format_phi_ref(&self, phi_ref: PhiRef) -> String {
        format!("p{}", phi_ref.get().abs())
    }

    fn write_colored(&mut self, color: Color, text: &str) {
        if self.use_colors {
            self.buffer.push_str(color.as_ansi());
            self.buffer.push_str(text);
            self.buffer.push_str(Color::Reset.as_ansi());
        } else {
            self.buffer.push_str(text);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::*;
    use common::TypeId;

    #[test]
    fn test_empty_code() {
        let code = Code::new();
        let printer = IrPrinter::new(&code);
        let output = printer.print();

        insta::assert_snapshot!(output, @"");
    }

    #[test]
    fn test_constants_only() {
        let mut code = Code::new();
        code.push_unpinned(Instr::const_int(42));
        code.push_unpinned(Instr::const_bool(true));

        let printer = IrPrinter::new(&code);
        let output = printer.print();

        insta::assert_snapshot!(output, @r###"
        ; === CONSTANTS & UNSCHEDULED ===
        c2:    const_bool true  ; bool
        c1:    const_i32  42  ; i32

        "###);
    }

    #[test]
    fn test_scheduled_instructions() {
        let mut code = Code::new();
        let const_ref = code.push_unpinned(Instr::const_int(42));
        code.push_pinned(Instr::Label(Meta::new(TypeId::UNIT), BlockRef::new(1).unwrap()));
        code.push_pinned(Instr::Print(Meta::new(TypeId::UNIT), const_ref));
        code.push_pinned(Instr::Ret(Meta::new(TypeId::UNIT), const_ref));

        let printer = IrPrinter::new(&code);
        let output = printer.print();

        insta::assert_snapshot!(output, @r###"
        ; === CONSTANTS & UNSCHEDULED ===
        c1:    const_i32  42  ; i32

        ; === FUNCTION CODE ===
        ; Block b1:
               print      c1
               ret        c1
        "###);
    }

    #[test]
    fn test_non_void_instruction_labeling() {
        let mut code = Code::new();
        let const_ref = code.push_unpinned(Instr::const_int(42));
        code.push_pinned(Instr::Label(Meta::new(TypeId::UNIT), BlockRef::new(1).unwrap()));
        let add_ref = code.push_pinned(Instr::Add(Meta::new(TypeId::I32), const_ref, const_ref));
        code.push_pinned(Instr::Ret(Meta::new(TypeId::UNIT), add_ref));

        let printer = IrPrinter::new(&code);
        let output = printer.print();

        insta::assert_snapshot!(output, @r###"
        ; === CONSTANTS & UNSCHEDULED ===
        c1:    const_i32  42  ; i32

        ; === FUNCTION CODE ===
        ; Block b1:
        i2:    add        c1, c1  ; i32
               ret        i2
        "###);
    }

    #[test]
    fn test_comprehensive_example() {
        let mut code = Code::new();

        // Constants
        let const42 = code.push_unpinned(Instr::const_int(42));
        let _const_true = code.push_unpinned(Instr::const_bool(true));
        let const10 = code.push_unpinned(Instr::const_int(10));

        // Function with multiple blocks
        code.push_pinned(Instr::Label(Meta::new(TypeId::UNIT), BlockRef::new(1).unwrap()));
        let add_result = code.push_pinned(Instr::Add(Meta::new(TypeId::I32), const42, const10));
        let eq_result = code.push_pinned(Instr::Eq(Meta::new(TypeId::BOOL), add_result, const42));
        code.push_pinned(Instr::Branch(
            Meta::new(TypeId::UNIT),
            eq_result,
            BlockRef::new(2).unwrap(),
            BlockRef::new(3).unwrap(),
        ));

        code.push_pinned(Instr::Label(Meta::new(TypeId::UNIT), BlockRef::new(2).unwrap()));
        code.push_pinned(Instr::Print(Meta::new(TypeId::UNIT), const42));
        code.push_pinned(Instr::Ret(Meta::new(TypeId::UNIT), const42));

        code.push_pinned(Instr::Label(Meta::new(TypeId::UNIT), BlockRef::new(3).unwrap()));
        code.push_pinned(Instr::Ret(Meta::new(TypeId::UNIT), const10));

        let printer = IrPrinter::new(&code);
        let output = printer.print();

        insta::assert_snapshot!(output, @r###"
        ; === CONSTANTS & UNSCHEDULED ===
        c3:    const_i32  10  ; i32
        c2:    const_bool true  ; bool
        c1:    const_i32  42       ; i32

        ; === FUNCTION CODE ===
        ; Block b1:
        i2:    add        c1, c3  ; i32
        i3:    eq         i2, c1  ; bool
               branch     i3, b2, b3
        ; Block b2:
               print      c1
               ret        c1
        ; Block b3:
               ret        c3
        "###);
    }

    #[test]
    fn test_colored_output() {
        let mut code = Code::new();
        let const_ref = code.push_unpinned(Instr::const_int(42));
        code.push_pinned(Instr::Label(Meta::new(TypeId::UNIT), BlockRef::new(1).unwrap()));
        code.push_pinned(Instr::Ret(Meta::new(TypeId::UNIT), const_ref));

        let printer = IrPrinter::new_colored(&code);
        let output = printer.print();

        // Should contain ANSI color codes
        assert!(output.contains("\x1b["));
        assert!(output.contains("const_i32"));
        assert!(output.contains("ret"));

        // Test structure without colors using snapshot
        let uncolored_printer = IrPrinter::new(&code);
        let uncolored_output = uncolored_printer.print();

        insta::assert_snapshot!(uncolored_output, @r###"
        ; === CONSTANTS & UNSCHEDULED ===
        c1:    const_i32  42  ; i32

        ; === FUNCTION CODE ===
        ; Block b1:
               ret        c1
        "###);
    }
}
