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

# Those tokens are from https://www.khronos.org/registry/OpenGL/specs/gl/GLSLangSpec.1.30.pdf
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
AND_OP = '&'
OR_OP = '\|'
XOR_OP = '\^'
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
import ply.lex as lex 
lexer = lex.lex()

# Set the precedence rules

print(lexer)
