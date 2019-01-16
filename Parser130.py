# py-glsl-parser - Python GLSL Parser
#
# Copyright (C) 2018  Alexander Kraus <nr4@z10.info>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
# Tokens and Grammar from https://www.khronos.org/registry/OpenGL/specs/gl/GLSLangSpec.1.30.pdf

import ply.lex as lex 
import ply.yacc as yacc
import argparse

from ASTNode import *
from VariableIdentifier import *

tokens = ['ATTRIBUTE', 'CONST', 'BOOL', 'FLOAT', 'INT', 'UINT', 'BREAK', 'CONTINUE', 'DO', 'ELSE', 'FOR', 'IF', 'DISCARD', 'RETURN', 'SWITCH', 'CASE', 'DEFAULT', 'BVEC2', 'BVEC3', 'BVEC4', 'IVEC2', 'IVEC3', 'IVEC4', 'UVEC2', 'UVEC3', 'UVEC4', 'VEC2', 'VEC3', 'VEC4', 'MAT2', 'MAT3', 'MAT4', 'CENTROID', 'IN', 'OUT', 'INOUT', 'UNIFORM', 'VARYING', 'NOPERSPECTIVE', 'FLAT', 'SMOOTH', 'MAT2X2', 'MAT2X3', 'MAT2X4', 'MAT3X2', 'MAT3X3', 'MAT3X4', 'MAT4X2', 'MAT4X3', 'MAT4X4', 'SAMPLER1D', 'SAMPLER2D', 'SAMPLER3D', 'SAMPLERCUBE', 'SAMPLER1DSHADOW', 'SAMPLER2DSHADOW', 'SAMPLERCUBESHADOW', 'SAMPLER1DARRAY', 'SAMPLER2DARRAY', 'SAMPLER1DARRAYSHADOW', 'SAMPLER2DARRAYSHADOW', 'ISAMPLER1D', 'ISAMPLER2D', 'ISAMPLER3D', 'ISAMPLERCUBE', 'ISAMPLER1DARRAY', 'ISAMPLER2DARRAY', 'USAMPLER1D', 'USAMPLER2D', 'USAMPLER3D', 'USAMPLERCUBE', 'USAMPLER1DARRAY', 'USAMPLER2DARRAY', 'STRUCT', 'VOID', 'WHILE', 'IDENTIFIER', 'TYPE_NAME', 'FLOATCONSTANT', 'INTCONSTANT', 'UINTCONSTANT', 'BOOLCONSTANT', 'FIELD_SELECTION', 'LEFT_OP', 'RIGHT_OP', 'INC_OP', 'DEC_OP', 'LE_OP', 'GE_OP', 'EQ_OP', 'NE_OP', 'AND_OP', 'OR_OP', 'XOR_OP', 'MUL_ASSIGN', 'DIV_ASSIGN', 'ADD_ASSIGN', 'MOD_ASSIGN', 'LEFT_ASSIGN', 'RIGHT_ASSIGN', 'AND_ASSIGN', 'XOR_ASSIGN', 'OR_ASSIGN', 'SUB_ASSIGN', 'LEFT_PAREN', 'RIGHT_PAREN', 'LEFT_BRACKET', 'RIGHT_BRACKET', 'LEFT_BRACE', 'RIGHT_BRACE', 'DOT', 'COMMA', 'COLON', 'EQUAL', 'SEMICOLON', 'BANG', 'DASH', 'TILDE', 'PLUS', 'STAR', 'SLASH', 'PERCENT', 'LEFT_ANGLE', 'RIGHT_ANGLE', 'VERTICAL_BAR', 'CARET', 'AMPERSAND', 'QUESTION', 'INVARIANT', 'HIGH_PRECISION', 'MEDIUM_PRECISION', 'LOW_PRECISION', 'PRECISION']

# Specification of the regular expressions for each token
ATTRIBUTE = 'attribute'
CONST = 'const'
BOOL = 'bool'
FLOAT = 'float'
INT = 'int'
UINT = 'uint'
BREAK = 'break'
CONTINUE = 'continue'
DO = 'do'
ELSE = 'else'
FOR = 'for'
IF = 'if'
DISCARD = 'discard'
RETURN = 'return'
SWITCH = 'switch'
CASE = 'case'
DEFAULT = 'default'
BVEC2 = 'bvec2'
BVEC3 = 'bvec3'
BVEC4 = 'bvec4'
IVEC2 = 'ivec2'
IVEC3 = 'ivec3'
IVEC4 = 'ivec4'
UVEC2 = 'uvec2'
UVEC3 = 'uvec3'
UVEC4 = 'uvec4'
VEC2 = 'vec2'
VEC3 = 'vec3'
VEC4 = 'vec4'
MAT2 = 'mat2'
MAT3 = 'mat3'
MAT4 = 'mat4'
CENTROID = 'centroid'
IN = 'in'
OUT = 'out'
INOUT = 'inout'
UNIFORM = 'uniform'
VARYING = 'varying'
NOPERSPECTIVE = 'noperspective'
FLAT = 'flat'
SMOOTH = 'smooth'
MAT2X2 = 'mat2x2'
MAT2X3 = 'mat2x3'
MAT2X4 = 'mat2x4'
MAT3X2 = 'mat3x2'
MAT3X3 = 'mat3x3'
MAT3X4 = 'mat3x4'
MAT4X2 = 'mat4x2'
MAT4X3 = 'mat4x3'
MAT4X4 = 'mat4x4'
SAMPLER1D = 'sampler1d'
SAMPLER2D = 'sampler2d'
SAMPLER3D = 'sampler3d'
SAMPLERCUBE = 'samplercube'
SAMPLER1DSHADOW = 'sampler1dshadow'
SAMPLER2DSHADOW = 'sampler2dshadow'
SAMPLERCUBESHADOW = 'samplercubeshadow'
SAMPLER1DARRAY = 'sampler1darray'
SAMPLER2DARRAY = 'sampler2darray'
SAMPLER1DARRAYSHADOW = 'sampler1darrayshadow'
SAMPLER2DARRAYSHADOW = 'sampler2darrayshadow'
ISAMPLER1D = 'isampler1d'
ISAMPLER2D = 'isampler2d'
ISAMPLER3D = 'isampler3d'
ISAMPLERCUBE = 'isamplercube'
ISAMPLER1DARRAY = 'isampler1darray'
ISAMPLER2DARRAY = 'isampler2darray'
USAMPLER1D = 'usampler1d'
USAMPLER2D = 'usampler2d'
USAMPLER3D = 'usampler3d'
USAMPLERCUBE = 'usamplercube'
USAMPLER1DARRAY = 'usampler1darray'
USAMPLER2DARRAY = 'usampler2darray'
STRUCT = 'struct'
VOID = 'void'
WHILE = 'while'
IDENTIFIER = r'[a-zA-Z_][a-zA-Z0-9_]*'
TYPE_NAME = 'type_name'
FLOATCONSTANT = '((\d*\.{1}\d+)|(\d+\.{1}\d*)){1}(e{1}[+-]?\d+)?'
INTCONSTANT = r'-?\d+'
UINTCONSTANT = r'\d+'
BOOLCONSTANT = r'true|false'
FIELD_SELECTION = r'\.{1}([xyzw]+|[rgba]+)'
LEFT_OP = '<<'
RIGHT_OP = '>>'
INC_OP = '\+\+'
DEC_OP = '--'
LE_OP = '<='
GE_OP = '>='
EQ_OP = '=='
NE_OP = '!='
AND_OP = '&&'
OR_OP = '\|\|'
XOR_OP = '\^\^'
MUL_ASSIGN = '*='
DIV_ASSIGN = '/='
ADD_ASSIGN = '+='
MOD_ASSIGN = '%='
LEFT_ASSIGN = '<<='
RIGHT_ASSIGN = '>>='
AND_ASSIGN = '&='
XOR_ASSIGN = '^='
OR_ASSIGN = '|='
SUB_ASSIGN = '-='
LEFT_PAREN = '\('
RIGHT_PAREN = '\)'
LEFT_BRACKET = '\['
RIGHT_BRACKET = '\]'
LEFT_BRACE = '\{'
RIGHT_BRACE = '\}'
DOT = '\.'
COMMA = ','
COLON = ':'
EQUAL = '='
SEMICOLON = ';'
BANG = '!'
DASH = '-'
TILDE = '~'
PLUS = '\+'
STAR = '\*'
SLASH = '/'
PERCENT = '%'
LEFT_ANGLE = '<'
RIGHT_ANGLE = '>'
VERTICAL_BAR = '|'
CARET = '\^'
AMPERSAND = '&'
QUESTION = '\?'
INVARIANT = 'invariant'
HIGH_PRECISION = 'highp'
MEDIUM_PRECISION = 'mediump'
LOW_PRECISION = 'lowp'
PRECISION = 'precision'

# Ignored characters 
t_ignore = " \t" 

# Count line numbers
def t_newline(t): 
    r'\n+' 
    t.lexer.lineno += t.value.count("\n") 

# Print error message on lexing error
def t_error(t): 
    print("Illegal character '%s'" % t.value[0]) 
    t.lexer.skip(1)
    
# Build the lexer
lexer = lex.lex()

# Set the precedence rules
precedence = (
    #('left', 'LEFT_PAREN', 'RIGHT_PAREN'),
    ('left', 'LEFT_BRACKET', 'RIGHT_BRACKET', 'LEFT_PAREN', 'RIGHT_PAREN', 'DOT', 'INC_OP', 'DEC_OP'),
    # 'INC_OP', 'DEC_OP'
    #('right', 'PLUS', 'DASH', 'TILDE', 'BANG'),
    ('left', 'STAR', 'SLASH', 'PERCENT'),
    ('left', 'PLUS', 'DASH'),
    ('left', 'LEFT_OP', 'RIGHT_OP'),
    ('left', 'LEFT_ANGLE', 'RIGHT_ANGLE', 'LE_OP', 'GE_OP'),
    ('left', 'EQ_OP', 'NE_OP'),
    ('left', 'AMPERSAND'),
    ('left', 'CARET'),
    ('left', 'VERTICAL_BAR'),
    ('left', 'AND_OP'),
    ('left', 'XOR_OP'),
    ('left', 'OR_OP'),
    ('right', 'QUESTION', 'COLON'),
    ('right', 'EQUAL', 'ADD_ASSIGN', 'SUB_ASSIGN', 'MUL_ASSIGN', 'DIV_ASSIGN', 'MOD_ASSIGN', 'LEFT_ASSIGN', 'RIGHT_ASSIGN', 'AND_ASSIGN', 'XOR_ASSIGN', 'OR_ASSIGN'),
    ('left', 'COMMA')
    )

node = ASTNode(None)

def p_variable_identifier(t):
    'variable_identifier : IDENTIFIER'
    child = VariableIdentifier(node, t[1])
    node.appendChild(child)
    pass
    
def p_primary_expression(t): 
    '''primary_expression : variable_identifier 
    | INTCONSTANT 
    | UINTCONSTANT 
    | FLOATCONSTANT 
    | BOOLCONSTANT 
    | LEFT_PAREN expression RIGHT_PAREN'''
    pass

def p_postfix_expression(t):
    '''postfix_expression : primary_expression 
    | postfix_expression LEFT_BRACKET integer_expression RIGHT_BRACKET 
    | function_call 
    | postfix_expression DOT FIELD_SELECTION 
    | postfix_expression INC_OP 
    | postfix_expression DEC_OP'''
    pass

def p_integer_expression(t):
    'integer_expression : expression'
    pass

def p_function_call(t):
    'function_call : function_call_or_method'
    pass

def p_function_call_or_method(t):
    '''function_call_or_method : function_call_generic 
    | postfix_expression DOT function_call_generic'''
    pass

def p_function_call_generic(t):
    '''function_call_generic : function_call_header_with_parameters RIGHT_PAREN 
    | function_call_header_no_parameters RIGHT_PAREN'''
    pass

def p_function_call_header_no_parameters(t):
    '''function_call_header_no_parameters : function_call_header VOID 
    | function_call_header'''
    pass

def p_function_call_header_with_parameters(t):
    '''function_call_header_with_parameters : function_call_header assignment_expression 
    | function_call_header_with_parameters COMMA assignment_expression'''
    pass

def p_function_call_header(t):
    'function_call_header : function_identifier LEFT_PAREN'
    pass

# Grammar Note: Constructors look like functions, but lexical analysis recognized most of them as
# keywords.  They are now recognized through “type_specifier”.
def p_function_identifier(t):
    '''function_identifier : type_specifier 
    | IDENTIFIER 
    | FIELD_SELECTION'''
    pass

def p_unary_expression(t):
    '''unary_expression : postfix_expression 
    | INC_OP unary_expression 
    | DEC_OP unary_expression 
    | unary_operator unary_expression'''
    pass

# Grammar Note:  No traditional style type casts.
def p_unary_operator(t):
    '''unary_operator : PLUS 
    | DASH 
    | BANG 
    | TILDE'''
    pass

# Grammar Note:  No '*' or '&' unary ops.  Pointers are not supported.
def p_multiplicative_expression(t):
    '''multiplicative_expression : unary_expression 
    | multiplicative_expression STAR unary_expression 
    | multiplicative_expression SLASH unary_expression 
    |  multiplicative_expression PERCENT unary_expression'''
    pass

def p_additive_expression(t):
    '''additive_expression : multiplicative_expression 
    | additive_expression PLUS multiplicative_expression 
    | additive_expression DASH multiplicative_expression'''
    pass

def p_shift_expression(t):
    '''shift_expression : additive_expression 
    | shift_expression LEFT_OP additive_expression 
    | shift_expression RIGHT_OP additive_expression'''
    pass

def p_relational_expression(t):
    '''relational_expression : shift_expression 
    | relational_expression LEFT_ANGLE shift_expression 
    | relational_expression RIGHT_ANGLE shift_expression 
    |  relational_expression LE_OP shift_expression 
    | relational_expression GE_OP shift_expression'''
    pass

def p_equality_expression(t):
    '''equality_expression : relational_expression 
    | equality_expression EQ_OP relational_expression 
    | equality_expression NE_OP relational_expression'''
    pass

def p_and_expression(t):
    '''and_expression : equality_expression 
    | and_expression AMPERSAND equality_expression'''
    pass

def p_exclusive_or_expression(t):
    '''exclusive_or_expression : and_expression 
    | exclusive_or_expression CARET and_expression'''
    pass

def p_inclusive_or_expression(t):
    '''inclusive_or_expression : exclusive_or_expression 
    | inclusive_or_expression VERTICAL_BAR exclusive_or_expression'''
    pass

def p_logical_and_expression(t):
    '''logical_and_expression : inclusive_or_expression 
    | logical_and_expression AND_OP inclusive_or_expression'''
    pass

def p_logical_xor_expression(t):
    '''logical_xor_expression : logical_and_expression 
    | logical_xor_expression XOR_OP logical_and_expression'''
    pass

def p_logical_or_expression(t):
    '''logical_or_expression : logical_xor_expression 
    | logical_or_expression OR_OP logical_xor_expression'''
    pass

def p_conditional_expression(t):
    '''conditional_expression : logical_or_expression 
    | logical_or_expression QUESTION expression COLON assignment_expression'''
    pass

def p_assignment_expression(t):
    '''assignment_expression : conditional_expression 
    | unary_expression assignment_operator assignment_expression'''
    pass

def p_assignment_operator(t):
    '''assignment_operator : EQUAL 
    | MUL_ASSIGN 
    | DIV_ASSIGN 
    | MOD_ASSIGN 
    | ADD_ASSIGN 
    | SUB_ASSIGN 
    | LEFT_ASSIGN 
    | RIGHT_ASSIGN 
    | AND_ASSIGN 
    | XOR_ASSIGN 
    | OR_ASSIGN'''
    pass

def p_expression(t):
    '''expression : assignment_expression 
    | expression COMMA assignment_expression'''
    pass

def p_constant_expression(t):
    'constant_expression : conditional_expression'
    pass

def p_declaration(t):
    '''declaration : function_prototype SEMICOLON 
    | init_declarator_list SEMICOLON 
    | PRECISION precision_qualifier type_specifier_no_prec SEMICOLON'''
    pass

def p_function_prototype(t):
    'function_prototype : function_declarator RIGHT_PAREN'
    pass

def p_function_declarator(t):
    '''function_declarator : function_header 
    | function_header_with_parameters'''
    pass

def p_function_header_with_parameters(t):
    '''function_header_with_parameters : function_header parameter_declaration 
    | function_header_with_parameters COMMA parameter_declaration'''
    pass

def p_function_header(t):
    'function_header : fully_specified_type IDENTIFIER LEFT_PAREN'
    pass

def p_parameter_declarator(t):
    '''parameter_declarator : type_specifier IDENTIFIER 
    | type_specifier IDENTIFIER LEFT_BRACKET constant_expression RIGHT_BRACKET'''
    pass

def p_parameter_declaration(t):
    '''parameter_declaration : parameter_type_qualifier parameter_qualifier parameter_declarator 
    | parameter_qualifier parameter_declarator 
    | parameter_type_qualifier parameter_qualifier parameter_type_specifier 
    | parameter_qualifier parameter_type_specifier'''
    pass

def p_parameter_qualifier(t):
    '''parameter_qualifier : 
    | IN 
    | OUT 
    | INOUT'''
    pass

def p_parameter_type_specifier(t):
    'parameter_type_specifier : type_specifier'
    pass

def p_init_declarator_list(t):
    '''init_declarator_list : single_declaration 
    | init_declarator_list COMMA IDENTIFIER 
    | init_declarator_list COMMA IDENTIFIER LEFT_BRACKET  RIGHT_BRACKET 
    | init_declarator_list COMMA IDENTIFIER LEFT_BRACKET constant_expression RIGHT_BRACKET 
    | init_declarator_list COMMA IDENTIFIER LEFT_BRACKET RIGHT_BRACKET EQUAL initializer 
    | init_declarator_list COMMA IDENTIFIER LEFT_BRACKET constant_expression RIGHT_BRACKET EQUAL initializer 
    | init_declarator_list COMMA IDENTIFIER EQUAL initializer'''
    pass

def p_single_declaration(t):
    '''single_declaration : fully_specified_type 
    | fully_specified_type IDENTIFIER 
    | fully_specified_type IDENTIFIER LEFT_BRACKET  RIGHT_BRACKET 
    | fully_specified_type IDENTIFIER LEFT_BRACKET constant_expression RIGHT_BRACKET 
    | fully_specified_type IDENTIFIER LEFT_BRACKET RIGHT_BRACKET EQUAL initializer 
    | fully_specified_type IDENTIFIER LEFT_BRACKET constant_expression 
    | fully_specified_type IDENTIFIER EQUAL initializer 
    | INVARIANT IDENTIFIER'''
    pass

# Grammar Note:  No 'enum', or 'typedef'.
def p_fully_specified_type(t):
    '''fully_specified_type : type_specifier 
    | type_qualifier type_specifier'''
    pass

def p_invariant_qualifier(t):
    'invariant_qualifier : INVARIANT'
    pass

def p_interpolation_qualifier(t):
    '''interpolation_qualifier : SMOOTH 
    | FLAT 
    | NOPERSPECTIVE'''
    pass

def p_parameter_type_qualifier(t):
    'parameter_type_qualifier : CONST'
    pass

def p_type_qualifier(t):
    '''type_qualifier : storage_qualifier 
    | interpolation_qualifier type_qualifier 
    | invariant_qualifier type_qualifier 
    | invariant_qualifier interpolation_qualifier type_qualifier'''
    pass

def p_storage_qualifier(t):
    '''storage_qualifier : 
    | CONST 
    | ATTRIBUTE 
    | VARYING 
    | CENTROID VARYING 
    | IN 
    | OUT 
    | CENTROID IN 
    | CENTROID OUT 
    | UNIFORM'''
    pass

def p_type_specifier(t):
    '''type_specifier : type_specifier_no_prec 
    | precision_qualifier type_specifier_no_prec'''
    pass

def p_type_specifier_no_prec(t):
    '''type_specifier_no_prec : type_specifier_nonarray 
    | type_specifier_nonarray LEFT_BRACKET RIGHT_BRACKET 
    | type_specifier_nonarray LEFT_BRACKET constant_expression RIGHT_BRACKET'''
    pass

def p_type_specifier_noarray(t):
    '''type_specifier_nonarray : VOID 
    | FLOAT 
    | INT 
    | UINT 
    | BOOL 
    | VEC2 
    | VEC3 
    | VEC4 
    | BVEC2 
    | BVEC3 
    | BVEC4 
    | IVEC2 
    | IVEC3 
    | IVEC4 
    | UVEC2 
    | UVEC3 
    | UVEC4 
    | MAT2 
    | MAT3 
    | MAT4 
    | MAT2X2 
    | MAT2X3 
    | MAT2X4 
    | MAT3X2 
    | MAT3X3 
    | MAT3X4 
    | MAT4X2 
    | MAT4X3 
    | MAT4X4 
    | SAMPLER1D 
    | SAMPLER2D 
    | SAMPLER3D 
    | SAMPLERCUBE 
    | SAMPLER1DSHADOW 
    | SAMPLER2DSHADOW 
    | SAMPLERCUBESHADOW 
    | SAMPLER1DARRAY 
    | SAMPLER2DARRAY 
    | SAMPLER1DARRAYSHADOW 
    | SAMPLER2DARRAYSHADOW 
    | ISAMPLER1D 
    | ISAMPLER2D 
    | ISAMPLER3D 
    | ISAMPLERCUBE 
    | ISAMPLER1DARRAY 
    | ISAMPLER2DARRAY 
    | USAMPLER1D 
    | USAMPLER2D 
    | USAMPLER3D 
    | USAMPLERCUBE 
    | USAMPLER1DARRAY 
    | USAMPLER2DARRAY 
    | struct_specifier 
    | TYPE_NAME'''
    pass

def p_precision_qualifier(t):
    '''precision_qualifier : HIGH_PRECISION 
    | MEDIUM_PRECISION 
    | LOW_PRECISION'''
    pass

def p_struct_specifier(t):
    '''struct_specifier : STRUCT IDENTIFIER LEFT_BRACE struct_declaration_list RIGHT_BRACE 
    | STRUCT LEFT_BRACE struct_declaration_list RIGHT_BRACE'''
    pass

def p_struct_declaration_list(t):
    '''struct_declaration_list : struct_declaration 
    | struct_declaration_list struct_declaration'''
    pass

def p_struct_declaration(t):
    'struct_declaration : type_specifier struct_declarator_list SEMICOLON'
    pass

def p_struct_declarator_list(t):
    '''struct_declarator_list : struct_declarator 
    | struct_declarator_list COMMA struct_declarator'''
    pass

def p_struct_declarator(t):
    '''struct_declarator : IDENTIFIER 
    | IDENTIFIER LEFT_BRACKET constant_expression RIGHT_BRACKET'''
    pass

def p_initializer(t):
    'initializer : assignment_expression'
    pass

def p_declaration_statement(t):
    '''declaration_statement : 
    | declaration'''
    pass

def p_statement(t):
    '''statement : compound_statement 
    | simple_statement'''
    pass

# Grammar Note:  labeled statements for SWITCH only; 'goto' is not supported.
def p_simple_statement(t):
    '''simple_statement : declaration_statement 
    | expression_statement 
    | selection_statement 
    | switch_statement 
    | case_label 
    | iteration_statement 
    | jump_statement'''
    pass

def p_compound_statement(t):
    '''compound_statement : LEFT_BRACE RIGHT_BRACE 
    | LEFT_BRACE statement_list RIGHT_BRACE'''
    pass

def p_statement_no_new_scope(t):
    '''statement_no_new_scope : compound_statement_no_new_scope 
    | simple_statement'''
    pass

def p_compound_statement_no_new_scope(t):
    '''compound_statement_no_new_scope : LEFT_BRACE RIGHT_BRACE 
    | LEFT_BRACE statement_list RIGHT_BRACE'''
    pass

def p_statement_list(t):
    '''statement_list : statement 
    | statement_list statement'''
    pass

def p_expression_statement(t):
    '''expression_statement : SEMICOLON 
    | expression SEMICOLON'''
    pass

def p_selection_statement(t):
    'selection_statement : IF LEFT_PAREN expression RIGHT_PAREN selection_rest_statement'
    pass

def p_selection_rest_statement(t):
    '''selection_rest_statement : statement ELSE statement 
    | statement'''
    pass

def p_condition(t):
    '''condition : expression 
    | fully_specified_type IDENTIFIER EQUAL initializer'''
    pass

def p_switch_statement(t):
    'switch_statement : SWITCH LEFT_PAREN expression RIGHT_PAREN LEFT_BRACE switch_statement_list RIGHT_BRACE'
    pass

def p_switch_statement_list(t):
    '''switch_statement_list : 
    | statement_list'''
    pass

def p_case_label(t):
    '''case_label : CASE expression COLON 
    | DEFAULT COLON'''
    pass

def p_iteration_statement(t):
    '''iteration_statement : WHILE LEFT_PAREN condition RIGHT_PAREN statement_no_new_scope 
    | DO statement WHILE LEFT_PAREN expression RIGHT_PAREN SEMICOLON 
    | FOR LEFT_PAREN for_init_statement for_rest_statement RIGHT_PAREN 
    | statement_no_new_scope'''
    pass

def p_for_init_statement(t):
    '''for_init_statement : expression_statement 
    | declaration_statement'''
    pass

def p_conditionopt(t):
    '''conditionopt : condition 
    | '''
    pass

def p_for_rest_statement(t):
    '''for_rest_statement : conditionopt SEMICOLON 
    | conditionopt SEMICOLON expression'''
    pass

# Grammar Note:  No 'goto'.  Gotos are not supported.
def p_jump_statement(t):
    '''jump_statement : CONTINUE SEMICOLON 
    | BREAK SEMICOLON 
    | RETURN SEMICOLON 
    | RETURN expression SEMICOLON 
    | DISCARD SEMICOLON '''
    pass

def p_translation_unit(t):
    '''translation_unit : external_declaration 
    | translation_unit external_declaration'''
    pass

def p_external_declaration(t):
    '''external_declaration : function_definition 
    | declaration'''
    pass

def p_function_definition(t):
    'function_definition : function_prototype compound_statement_no_new_scope'
    pass

def p_error(t):
    print("Syntax error at ", t.value)
    
parser = yacc.yacc()

def parse(string):
    parser.parse(string)
    return node.root()
