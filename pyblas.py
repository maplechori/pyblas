#!/usr/bin/env python
# coding: latin-1

__author__ = 'adrianmo'

import ply.lex as lex
import ply.yacc as yacc
import string
import re
import codecs,sys, unicodedata
import pprint
import logging

sys.stdout = codecs.getwriter('utf-8')(sys.stdout)

data = None

with codecs.open("hrs_c.txt", encoding='iso8859') as fl:
        data=fl.read()


push_str = []

states = (
  ('ccode','exclusive'),
  ('fillcode', 'exclusive'),
  ('layoutcode', 'exclusive'),)
#  ('tagcode', 'inclusive')
#)

tokens = [
    "RULES",
    "ENDBLOCK",
    "ENDPROCEDURE",
    "TAG",

    "DOTDOT",
    "LESSEQUALTHAN",
    "EQUALMORETHAN",
    "LESSTHAN",
    "MORETHAN",
    "LITERAL",
    "MULT",
    "MULTMULT",
    "DIVIDE",
    "COMMA",
    "SEMI",
    "FLOAT",
    "COUNT",
    "COMMENT",
    "IDENTIFIER",
    "COMPARE",
    "ASSIGN",
    "COLON",
    "UMINUS",
    "DECIMAL",
    "MINUS",
    "FILL",
    "DOUBLESINGLES",
      "LPAREN",
    "RPAREN",
    "LBRACKET",
    "RBRACKET",
    "PIPE",
    "PLUS",
    "DIFF",





]

reserved = {
 'CHECK' : 'CHECK',
'IF' : 'IF' ,
'THEN' : 'THEN',
'TYPE' : 'TYPE',
'ELSE' : 'ELSE',
'NOT' : 'NOT',
'OR' : 'OR',
'AND' : 'AND',
'ELSEIF' : 'ELSEIF',
'ENDIF' : "ENDIF",
'BLOCK' : "BLOCK",
'PROCEDURE' : 'PROCEDURE',
'AUXFIELDS':'AUXFIELDS',
'IMPORT' : 'IMPORT',
'EXPORT' : 'EXPORT',
'PARAMETERS':'PARAMETERS',
'SHOW' : 'SHOW',
'KEEP' : 'KEEP',
'ORD' : 'ORD',
'CARDINAL' : 'CARDINAL',
'ASK' : 'ASK',
'EMPTY' : 'EMPTY',
'NOEMPTY' : 'NOEMPTY',
'STRING' : 'STRING',
'INTEGER' : 'INTEGER',
'NORF' : 'NORF',
'NODK' : 'NODK',
'DK' : 'DK',
'SET' : 'SET',
'ARRAY' : 'ARRAY',
'OF' : 'OF',
'RF' :'RF',
'IN' : 'IN',
'LOCALS' : 'LOCALS',
'FIELDS' : 'FIELDS',
'AUXFIELDS' : 'AUXFIELDS',
'REPEAT' : 'REPEAT',
'TO' : 'TO',
'DOWNTO' : 'DOWNTO',
'WHILE' : 'WHILE',
'UNTIL' : 'UNTIL',
'FOR' : 'FOR',
'DO' : 'DO',
'TRANSIT' : 'TRANSIT',
'NOT' : 'NOT',
'ENDDO' : 'ENDDO',
'ENDWHILE' : 'ENDWHILE',
'INVOLVING' : 'INVOLVING',
'SIGNAL' : 'SIGNAL',
'MOD' : 'MOD',
'LEN' : 'LEN',
'STR' : 'STR',
'SUBSTRING'  : 'SUBSTRING',
'RANDOM' : 'RANDOM',


}


tokens += list(reserved.values())


t_ignore = " \t\r"


def t_ccode(t):
    r'\{'
    t.lexer.code_start = t.lexer.lexpos        # Record the starting position
    t.lexer.level = 1                          # Initial brace level
    t.lexer.begin('ccode')                     # Enter 'ccode' state

def t_ccode_lbrace(t):
    r'\{'
    t.lexer.level +=1

def t_ccode_rbrace(t):
    r'\}'
    t.lexer.level -=1
    # If closing brace, return the code fragment
    if t.lexer.level == 0:
         t.value = t.lexer.lexdata[t.lexer.code_start:t.lexer.lexpos-1]
         t.type = "COMMENT"
         t.lexer.lineno += t.value.count('\n')
         t.lexer.begin('INITIAL')
         pass


t_ccode_ignore = ' \t'


def t_ccode_error(t):
    t.lexer.skip(1)


def t_layoutcode(t):
    r'[Ll][Aa][Yy][Oo][Uu][Tt]'
    t.lexer.code_start = t.lexer.lexpos        # Record the starting position
    t.lexer.level = 1                          # Initial brace level
    t.lexer.begin('layoutcode')
    print "LAYOUT START"

def t_layoutcode_TO(t):
    r'[Tt][Oo]'
    pass

def t_layoutcode_ENDBLOCK(t):
    r'[Ee][Nn[Dd][Bb][Ll][Oo][Cc][Kk]'
    t.lexer.level -=1
    # If closing brace, return the code fragment
    if t.lexer.level == 0:
        print "LAYOUT END"
        t.value = t.lexer.lexdata[t.lexer.code_start:t.lexer.lexpos-1]
        t.type = "COMMENT"
        t.lexer.lineno += t.value.count('\n')
        t.lexer.begin('INITIAL')
        pass

t_layoutcode_ignore = ' \t'

def t_layoutcode_error(t):
    t.lexer.skip(1)



def t_layoutcode_RULES(t):
    r'RULES'
    t.lexer.level -=1
    # If closing brace, return the code fragment
    if t.lexer.level == 0:
         t.value = t.lexer.lexdata[t.lexer.code_start:t.lexer.lexpos-1]
         t.type = "COMMENT"
         t.lexer.lineno += t.value.count('\n')
         t.lexer.begin('INITIAL')
         print "LAYOUT END"
         pass

lex_fill_positions = []

def t_fillcode(t):
    r'\"|\''

    if t.value == '\'':
        setattr(t.lexer, "singles", 1)
        setattr(t.lexer, "doubles", 0)

    if t.value == '\"':
        setattr(t.lexer, "doubles", 1)
        setattr(t.lexer, "singles", 0)


    t.lexer.code_start = t.lexer.lexpos        # Record the starting position
    t.lexer.level = 1                          # Initial brace level
    t.lexer.begin('fillcode')                     # Enter 'ccode' state

def t_fillcode_FILL(t):
    r'\^[A-Za-z0-9\[\]]*'
    lex_fill_positions.append(t.lexer.lexpos)
    #print t.lexpos
    #print lex_fill_positions
    pass

def t_fillcode_DOUBLESINGLES(t):
    r'\'\''
    pass


def t_fillcode_LITERAL(t):
    r'\"|\''

    if t.value == '\'' and getattr(t.lexer,"singles") > 0:
        setattr(t.lexer, "singles", 0)
        t.value = t.lexer.lexdata[t.lexer.code_start:t.lexer.lexpos-1]
        t.type = "LITERAL"
        t.lexer.lineno += t.value.count('\n')
        t.lexer.begin('INITIAL')
        return t


    if t.value == '\"' and getattr(t.lexer,"doubles") > 0:
        setattr(t.lexer, "doubles", 1)
        t.value = t.lexer.lexdata[t.lexer.code_start:t.lexer.lexpos-1]
        t.type = "LITERAL"
        t.lexer.lineno += t.value.count('\n')
        t.lexer.begin('INITIAL')
        return t

    pass




t_fillcode_ignore = ' \t'


def t_TAG(t):
    u"\([A-Z][0-9]+[\.\d_A-Z]+\)"
    print t.value
    return t

def t_RULES(t):
    r'[Rr][Uu][Ll][Ee][Ss]'
    return t

def t_fillcode_error(t):
    t.lexer.skip(1)

def t_ENDPROCEDURE(t):
    r'[Ee][Nn][Dd][Pp][Rr][Oo][Cc][Ee][Dd][Uu][Rr][Ee]'
    return t


def t_ENDBLOCK(t):
    r'[Ee][Nn][Dd][Bb][Ll][Oo][Cc][Kk]'
    return t


def t_DIFF(t):
    r'<>'
    return t


def t_DOTDOT(t):
    r'\.\.'
    return t

def t_MULTMULT(t):
    r'\*\*'
    return t

def t_FLOAT(t):
    r'[0-9]+\.[0-9]+'
    t.value = float(t.value)
    return t

def t_COUNT(t):
    r"[0-9]+"
    t.value = int(t.value)
    return t


def t_PIPE(t):
    r'\|'
    return t


def t_FILL(t):
    r'\^'
    return t

def t_SEMI(t):
    r';'
    pass



def t_COMMA(t):
    r','
    return t

def t_MINUS(t):
    r'-'
    return t

def t_PLUS(t):
    r'\+'
    return t

def t_MULT(t):
    r'\*'
    return t

def t_DIVIDE(t):
    r'/'
    return t


def t_ASSIGN(t):
    r'\:\='
    return t


def t_COLON(t):
    r'\:'
    return t


def t_cr(t):
    r'\r+'


def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")



def t_LBRACKET(t):
    r"\["
    return t

def t_RBRACKET(t):
    r"\]"
    return t

def t_DECIMAL(t):
    r"\."
    return t

def t_LESSEQUALTHAN(t):
    r'<='
    return t

def t_EQUALMORETHAN(t):
    r'>='
    return t


def t_COMPARE(t):
    r'='
    return t


def t_MORETHAN(t):
    r'>'
    return t


def t_LESSTHAN(t):
    r'<'
    return t



def t_IDENTIFIER(t):
    ur"[\^a-zA-Z_\xF3][a-zA-Zñáéíóúü0-9_\xF3]*"
    t.type = reserved.get(string.upper(t.value), 'IDENTIFIER')
    #print t
    return t

def t_LPAREN(t):
    u"\("
    return t

def t_RPAREN(t):
    u"\)"
    return t

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


lexer = lex.lex(reflags=(re.UNICODE))


lexer.input(data)




#for tok in iter(lexer.token, None):
    #print tok.type,

    #if type(tok.value) == type(u's'):
       #pass
    #    print tok.value
    #else:
        #pass
    #    print repr(tok.value)
 #   print tok


precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULT', 'DIVIDE', 'MOD'),
    ('right', 'UMINUS'),


)


start = 'codeblock'

def p_codeblock(p):
    r'''
        codeblock : procedure_and_block_declaration_part codeblock
                    |  tempty
    '''


def p_procedure_and_block_declaration_part(p):
    r'''
        procedure_and_block_declaration_part : procedure_or_block_declaration_list

    '''


def p_procedure_or_block_declaration_list(p):
    r'''
    procedure_or_block_declaration_list :  proc_or_block_declaration

    '''

# this is where we need to add blocks
def p_proc_or_block_declaration(p):
    r'''
            proc_or_block_declaration : procedure_declaration
                                       | block_declaration

                                            '''

def p_procedure_declaration(p):
    r'''
        procedure_declaration : procedure_identification procedure_block ENDPROCEDURE


    '''


def p_block_declaration(p):
    r'''
        block_declaration : block_identification procedure_block ENDBLOCK


    '''



def p_procedure_identification(p):
    r'''
            procedure_identification : PROCEDURE IDENTIFIER
    '''

    print "PROCEDURE " + p[2]

def p_block_identification(p):
    r'''
            block_identification : BLOCK IDENTIFIER
    '''
    print "BLOCK " + p[2]

def p_type_array(p):
    r'''
        type_array : LBRACKET COUNT RBRACKET
                    | LBRACKET RBRACKET

    '''

def p_type_denoter(p):
    '''
            type_denoter : IDENTIFIER
                        | STRING  type_array
                        | INTEGER type_array
                        | STRING
                        | INTEGER
                        | new_type
    '''


def p_new_type(p):
    '''
        new_type : new_ordinal_type
                | new_structured_type

    '''


def p_new_ordinal_type(p):
    '''
        new_ordinal_type : enumerated_type
                        | subrange_type
    '''

def p_enumerated_type(p):
    '''
        enumerated_type : LPAREN enumerated_list RPAREN COMMA tmodifiers_list
                    | LPAREN enumerated_list RPAREN

    '''


def p_enum_languages_list(p):
    r'''
        enum_languages_list : LITERAL enum_languages_list
                        | LITERAL
    '''



def p_enum_num_arg(p):
    '''
            enum_num_arg : LPAREN COUNT RPAREN
              |
    '''

def p_enumerated_list(p):
    r'''
            enumerated_list : IDENTIFIER enum_num_arg enum_languages_list COMMA enumerated_list
                        | IDENTIFIER enum_num_arg enum_languages_list

    '''

def p_numeric_type(p):
    r'''
    numeric_type : FLOAT
                | COUNT
    '''

def p_subrange_type(p):
    '''
        subrange_type : numeric_type DOTDOT numeric_type

    '''



def p_new_structured_type(p):
    '''
    new_structured_type : structured_type
    '''



def p_structured_type(p):
    '''
        structured_type : array_type
                        | set_type

    '''

def p_array_type(p):
    '''
    array_type : ARRAY LBRACKET index_list RBRACKET OF component_type
    '''

def p_index_list(p):
    '''
    index_list : index_list COMMA index_type
                | index_type
    '''

def p_index_type(p):
    '''
    index_type :  ordinal_type
    '''

def p_ordinal_type(p):
    '''
        ordinal_type : new_ordinal_type
                    | IDENTIFIER
    '''

def p_component_type(p):
    '''
    component_type : type_denoter

    '''

def p_set_type(p):
    '''
    set_type : SET OF base_type
            | SET LBRACKET COUNT RBRACKET OF base_type

    '''

def p_base_type(p):
    '''
    base_type : ordinal_type

    '''

def p_identifier_list(p):
    r'''
        identifier_list : identifier_list COMMA IDENTIFIER

                        | IDENTIFIER
    '''
    p[0] = p[1]
   # print p[0]



def p_type_definition_list(p):
    r'''
            type_definition_list : type_definition_list type_definition
                                 | type_definition
    '''

def p_type_definition(p):
    r'''
        type_definition : IDENTIFIER COMPARE type_denoter tmodifiers_list
                | IDENTIFIER COMPARE type_denoter
    '''


def p_type_definition_part(p):
    r'''
                type_definition_part : TYPE type_definition_list


    '''


def p_tmodifiers(p):
    '''
    tmodifiers :  EMPTY
                | NODK
                | NORF
                | DK
                | RF
                | NOEMPTY


'''




def p_tmodifiers_list(p):
    ''' tmodifiers_list : tmodifiers COMMA tmodifiers_list
        | tmodifiers


    '''


def p_rules_part(p):
    r'''
        rules_part : RULES statement_part
                  | locals_part
    '''

    print "\tRULES"

def p_locals_part(p):
    r'''
        locals_part : LOCALS locals_declaration_list
                    | fields_part

    '''
    print "\tLOCALS"

def p_fields_part(p):
    r'''
        fields_part : FIELDS fields_declaration_list
                    | auxfields_part

    '''
    print "\tFIELDS"




def p_auxfields_part(p):
    r'''
        auxfields_part : AUXFIELDS fields_declaration_list
                       | type_definition_part


    '''

    print "\tAUXFIELDS"

def p_optional_sections_list(p):
    '''
        optional_sections_list : optional_sections_list optional_sections
                                | optional_sections


    '''

def p_optional_sections(p):
    '''
            optional_sections : parameters_declaration_part


    '''


def p_procedure_block(p):
    r'''
        procedure_block :   optional_sections_list

    '''

def p_parameter_modifiers(p):
    r''' parameter_modifiers : IMPORT
                              | EXPORT
                              | TRANSIT
                              |

'''

def p_locals_declaration_list(p):
    r'''
    locals_declaration_list : locals_declaration_list locals_declaration
                         | locals_declaration
    '''

def p_locals_declaration(p):
    '''
    locals_declaration : identifier_list COLON type_denoter
    '''



def p_fields_declaration_list(p):
    r'''
    fields_declaration_list : fields_declaration_list fields_declaration
                         | fields_declaration
    '''



def p_field_description(p):
    '''
        field_description : DIVIDE LITERAL

    '''


def p_tag_rule(p):
    '''
    tag_rule  : LPAREN LITERAL RPAREN
        |
    '''


def p_fields_declaration(p):
    '''
    fields_declaration : identifier_list tag_rule enum_languages_list field_description COLON type_denoter
                       | identifier_list field_description COLON type_denoter
                       | identifier_list COLON type_denoter COMMA tmodifiers_list
                       | identifier_list  COLON type_denoter
                       | IDENTIFIER TAG enum_languages_list field_description COLON type_denoter COMMA tmodifiers_list
                       | IDENTIFIER TAG enum_languages_list field_description COLON type_denoter
    '''

    print "FIELDS DECLARATION: " + p[1]


def p_parameters_declaration_part(p):
    r'''
        parameters_declaration_part : PARAMETERS parameters_declaration_list
                                    | rules_part
    '''
    print "\t\tPARAMETERS"

def p_statement_part(p):
    r'''
        statement_part : statement_list
    '''

def p_parameters_declaration_list(p):
    r'''
            parameters_declaration_list : parameters_declaration_list parameter_declaration
                                    | parameter_declaration

    '''


def p_parameter_declaration(p):
    r'''
            parameter_declaration : parameter_modifiers identifier_list COLON type_denoter
                                    | procedure_declaration
                                    | block_declaration

    '''





def p_sign(p):
    r''' sign : PLUS
                | MINUS
    '''

def p_non_string(p):
    r'''
            non_string : COUNT
                        | IDENTIFIER
                        | FLOAT
    '''
    p[0] = p[1]



def p_control_variable(p):
    r'''
        control_variable : IDENTIFIER
    '''
    p[0] = p[1]

def p_initial_value(p):
    r'''
        initial_value : expression
    '''

def p_direction(p):
    r'''
        direction : TO
                | DOWNTO
    '''
    p[0] = p[1]

def p_final_value(p):
    r'''
        final_value : expression
    '''
    p[0] = p[1]

def p_boolean_expression(p):
    r'''
            boolean_expression : expression
    '''



def p_expression(p):
    r'''
            expression : or_expression

    '''


def p_or_expression(p):
    r'''
        or_expression : or_expression OR and_expression
                    | and_expression

    '''

def p_and_expression(p):
    r'''
        and_expression : and_expression AND not_expression
                        | not_expression
    '''

def p_not_expression(p):
    r'''
        not_expression : NOT not_expression
                        | compare_expression
    '''



def p_compare_expression(p):
    r'''
        compare_expression : compare_expression relop add_expression
                           | add_expression
    '''

def p_relop(p):
    '''
            relop : COMPARE
                    | LESSTHAN
                    | DIFF
                    | EQUALMORETHAN
                    | MORETHAN
                    | LESSEQUALTHAN

    '''



def p_add_expression(p):

    r'''
            add_expression : add_expression PLUS mult_expression
                           | add_expression MINUS mult_expression
                           | add_expression MOD mult_expression
                           | mult_expression

    '''

    try:
        if len(p) > 2:
            if p[2] == "+":
                p[0] = p[1] + p[3]
            elif p[2] == "-":
                p[0] = p[1] - p[3]
            elif p[2] == "MOD":
                p[0] = p[1] % p[3]
        else:
            p[0] = p[1]

    except:
        print "Add Expression Error"


def p_mult_expression(p):
    r'''
            mult_expression : mult_expression MULT unary_expression
                            | mult_expression DIVIDE unary_expression
                            | mult_expression IN unary_expression
                            | unary_expression
    '''

    try:
        if len(p) > 2:
            if p[2] == "*":
                p[0] = p[1] + p[3]
            elif p[2] == "/":
                p[0] = p[1] - p[3]
            elif p[2] == "IN":
                print "IN"
                print p[3]
        else:
            p[0] = p[1]

    except:
        print "Mult Expression Error"

def p_unary_expression(p):
    r'''
                    unary_expression : sign unary_expression
                                    | exp_expression
    '''



def p_exp_expression(p):
    r'''
                    exp_expression : primary MULTMULT exp_expression
                                    | primary
    '''





def p_built_in_functions(p):
    r'''
            built_in_functions : STR
                            | LEN
                            | SUBSTRING
                            | RANDOM

    '''
    p[0] = p[1]


def p_primary(p):
    r'''
        primary : variable_access
                | unsigned_constant
                | set_constructor
                | COUNT
                | FLOAT
                | LPAREN expression RPAREN
                | built_in_functions LPAREN index_expression_list RPAREN
                | IDENTIFIER LPAREN index_expression_list RPAREN

    '''


def p_unsigned_constant(p):
    r'''
        unsigned_constant : LITERAL
                        | RF
                        | DK
                        | EMPTY
    '''
    p[0] = p[1]


def p_set_constructor(p):
    r'''
        set_constructor : LBRACKET member_designator_list RBRACKET
                        | LBRACKET RBRACKET

    '''



def p_member_designator_list(p):
    r'''
        member_designator_list : member_designator_list COMMA member_designator
                                | member_designator

    '''

def p_member_designator(p):
    r'''
            member_designator : member_designator DOTDOT expression tmodifiers_list
                            | member_designator DOTDOT expression
                            | expression
    '''

def p_variable_access(p):
    r'''
            variable_access : IDENTIFIER
                            | indexed_variable
                            | field_designator

    '''
    p[0] = p[1]

    if p[1] != None:
        print "IDENTIFIER: "  + str(p[1])



def p_indexed_variable(p):
    r'''
        indexed_variable :  variable_access LBRACKET index_expression_list RBRACKET

    '''
    p[0] = p[3]

def p_index_expression_list(p):
    r'''
        index_expression_list : index_expression_list COMMA index_expression
                        | index_expression
    '''
    p[0] = p[1]

def p_index_expression(p):
    r'''        index_expression : expression
        '''

def p_field_designator(p):
    r'''
        field_designator : variable_access DECIMAL IDENTIFIER
                        | variable_access DECIMAL ORD
                          | variable_access DECIMAL CARDINAL

    '''
    #print "."
    #print p[3]
    try:
        p[0] = p[1] + "" + p[2] + "" + p[3]
        print p[0]
    except:
        pass


def p_method_statement(p):
    r'''
         method_statement : variable_access DECIMAL KEEP

    '''

def p_procedure_statement(p):
    r'''
        procedure_statement : IDENTIFIER params
        | IDENTIFIER
    '''

    print "CALL: " + p[1]
    p[0] = p[1]

def p_params(p):
    r'''
        params : LPAREN actual_parameter_list RPAREN

    '''


def p_actual_parameter_list(p):
    r'''
        actual_parameter_list : actual_parameter_list COMMA actual_parameter
                            | actual_parameter
    '''

def p_actual_parameter(p):
    r'''
            actual_parameter :  expression
                            |  expression COLON expression
                            |  expression COLON expression COLON expression

    '''



def p_statement_list(p):
    r'''statement_list :  statement_list statement
                        | statement
      '''



def p_module_statement(p):
    r'''
        module_statement : variable_access LBRACKET index_expression_list RBRACKET LPAREN index_expression_list RPAREN
    '''


def p_statement(p):
    ''' statement :  assignment_statement
                    | method_statement
                    | procedure_statement
                    | repeat_statement
                    | if_statement
                    | while_statement
                    | for_statement
                    | check_statement
                    | signal_statement
                    | not_statement
                    | module_statement


    '''
    p[0] = p[1]


def p_repeat_statement(p):
    '''
        repeat_statement : REPEAT statement_list UNTIL boolean_expression
    '''


def p_while_statement(p):
    '''
        while_statement : WHILE boolean_expression DO statement_list ENDWHILE
    '''



def p_for_statement(p):
    '''
        for_statement : FOR control_variable ASSIGN initial_value direction final_value DO statement_list ENDDO
    '''

def p_involving_vars(p):
    '''
        involving_vars : INVOLVING LPAREN statement_list RPAREN
                    | INVOLVING params
                    | INVOLVING TAG
                    |

    '''

def p_check_statement(p):
    '''
        check_statement : CHECK boolean_expression involving_vars LITERAL
    '''

def p_signal_statement(p):
    '''
        signal_statement : SIGNAL boolean_expression involving_vars LITERAL
    '''


def p_not_statement(p):
    r'''
            not_statement : NOT boolean_expression statement
                            | NOT boolean_expression involving_vars LITERAL

    '''

def p_if_statement(p):
    r'''
        if_statement :  IF boolean_expression THEN statement else_statement_list ENDIF
                        | IF boolean_expression THEN statement_list else_statement_list ENDIF
                        | IF boolean_expression THEN statement_list ENDIF
 '''

    if p[2] and p[4]:
        print "IF " + p[2] + " THEN " + p[4]


def p_else_statement_list(p):
    r'''
        else_statement_list : ELSEIF boolean_expression THEN statement_list else_statement_list
                            | ELSEIF boolean_expression THEN statement_list
                            | ELSE statement_list

    '''

    p[0] = p[2]



def p_assignment_statement(p):
    r'''
        assignment_statement : variable_access ASSIGN expression
    '''
    p[0] = p[1] = p[3]


def p_tempty(p):
    r'tempty :'
    pass


def p_error(p):
    print "Syntax error at '%s %s %d %d'" % (p.value, p.type, p.lineno, p.lexpos)

import logging
logging.basicConfig(
    level = logging.DEBUG,
    filename = "parselog.txt",
    filemode = "w",
    format = "%(filename)10s:%(lineno)4d:%(message)s"
)
log = logging.getLogger()




parser = yacc.yacc()


s = data
result = parser.parse(s, debug=log)
pprint.pprint(result,width=20)
#print result