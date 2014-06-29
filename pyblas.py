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

def stdout_encode(u, default='iso8859'):
    if sys.stdout.encoding:
        return u.encode(sys.stdout.encoding)
    return u.encode(default)

data = None

with codecs.open("hrs_c.txt", encoding='iso8859') as fl:
        data=fl.read()


class Symbol: pass




class Expr: pass



class UnsignedConstant(Expr):
    def __init__(self, type, value):
        self.type = "Unsigned Constant"
        self.value = value
        self.type = type

    def __repr__(self):
        return stdout_encode('<Unsigned Constant> {0}').format(self.type)




class TypeC(Expr):
    def __init__(self, name, value, modifiers = None):
        self.type = "Type"
        self.name =  name
        self.value = value
        self.modifiers = modifiers

    def __repr__(self):
        return u"%s: %s Value: %s  - Modifiers [%s]" % (self.type, self.name, self.value, self.modifiers)

class TypeRange(Expr):

  def __init__(self, value, min, max):
        self.type = "TypeRange"
        self.value = value
        self.upperlimit = min
        self.lowerlimit = max

  def __repr__(self):

        return u"TypeRange %s[%s..%s]" % (str(self.value) , str(self.lowerlimit), str(self.upperlimit))


class TypeDenoter(Expr):

    def __init__(self, value, array=False, size=0):
        self.type = "TypeDenoter"
        self.value = value
        self.array = array
        self.size = size


    def __repr__(self):

        if self.array and self.size > 0:
            return u"TypeDenoter %s[%d]" % (str(self.value) , self.size)
        elif self.array:
            return u"TypeDenoter %s[]" % (str(self.value))
        else:
            return u"TypeDenoter %s" % str(self.value)




class Parameter(Expr):
    def __init__(self, name, typepar, modifiers):
        self.type = typepar
        self.name =  name
        self.modifiers = modifiers

    def __repr__(self):
        return u"Parameter: %s Modifiers: %s  - Type [%s]" % (self.name, self.modifiers, self.type)




vars = {}


states = (
  ('ccode','exclusive'),
  ('fillcode', 'exclusive'),
  ('layoutcode', 'exclusive'),)

tokens = [
    "RULES",
    "ENDBLOCK",
    "TRANSIENT",
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
    "TYPE"
]

reserved = {
 'CHECK' : 'CHECK',
'IF' : 'IF' ,
'THEN' : 'THEN',
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
'REPEAT' : 'REPEAT',
'TO' : 'TO',
'DOWNTO' : 'DOWNTO',
'WHILE' : 'WHILE',
'UNTIL' : 'UNTIL',
'FOR' : 'FOR',
'DO' : 'DO',
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
'YEAR' : 'YEAR'

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
        t.value = unicode(t.lexer.lexdata[t.lexer.code_start:t.lexer.lexpos-1])
        t.type = "LITERAL"
        t.lexer.lineno += t.value.count('\n')
        t.lexer.begin('INITIAL')
        return t


    if t.value == '\"' and getattr(t.lexer,"doubles") > 0:
        setattr(t.lexer, "doubles", 1)
        t.value = unicode( t.lexer.lexdata[t.lexer.code_start:t.lexer.lexpos-1] )
        t.type = "LITERAL"
        t.lexer.lineno += t.value.count('\n')
        t.lexer.begin('INITIAL')
        return t

    pass




t_fillcode_ignore = ' \t'


def t_TAG(t):
    u"\([A-Z][0-9]+[\.\d_A-Z]+\)"
    #print t.value
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

def t_TYPE(t):
    r'[Tt][Yy][Pp][Ee]'
    print "TOKEN TYPE"
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
    if len(p) > 2:
        p[0] = [p[1], p[2]]




def p_procedure_and_block_declaration_part(p):
    r'''
        procedure_and_block_declaration_part : procedure_or_block_declaration_list

    '''


    p[0] = p[1]


def p_procedure_or_block_declaration_list(p):
    r'''
    procedure_or_block_declaration_list :  proc_or_block_declaration

    '''

    p[0] = p[1]

# this is where we need to add blocks
def p_proc_or_block_declaration(p):
    r'''
            proc_or_block_declaration : auxfields_declaration
                                       | procedure_declaration
                                       | block_declaration


                                            '''
    p[0] = p[1]

class Field(Expr):

    def __init__(self, name, tag, languages, description, typeOf, modifiers = None):
        self.type = "Field"
        self.name  = name
        self.tag = tag
        self.languages = languages
        self.description = description
        self.typeOf = typeOf
        self.modifiers = modifiers

    def __repr__(self):
        return u"Field: %s Type: %s  Tag: %s " % (self.name, self.typeOf, self.tag)


class BinOp(Expr):
    def __init__(self,left,op,right):
        self.type = "binop"
        self.left = left
        self.right = right
        self.op = op

    def __repr__(self):

        if self.left != None and self.right != None:
            return u"L:%s %s R:%s" % (self.left, self.op,  self.right)
        else:
            return "MISSING BINOP"


class IfStatement(Expr):
    def __init__(self,left, right):
        self.type = "IF"
        self.left = left
        self.right = right


    def __repr__(self):

        if self.left != None and self.right != None:
            return u"IF %s THEN %s" % (self.left, self.right)
        else:
            return "Missing IF"


class IfElseStatement(Expr):
    def __init__(self,first, second, third):
        self.type = "IF ELSE"
        self.first = first
        self.second = second
        self.third = third


    def __repr__(self):

        if self.left != None and self.right != None:
            return u"IF 1:%s THEN 2:%s ELSE 3:%s" % (self.first, self.second, self.third)
        else:
            return "MISSING IF ELSE"


class ElseStatement(Expr):
    def __init__(self,first):
        self.type = "ELSE"
        self.first = first


    def __repr__(self):

        if self.first != None:
            return u"ELSE 1:%s" % (self.first)
        else:
            return "MISSING ELSE"



class Unary(Expr):
    def __init__(self,op,value):
        self.type = "Unary"
        self.value = value
        self.op = op

    def __repr__(self):

        if self.left and self.right:
            return u"%s %s" % (self.op, self.value)
        else:
            return "None"

class Not(Expr):
    def __init__(self,value):
        self.type = "NOT"
        self.value = value

    def __repr__(self):

        if self.value:
            return u"NOT %s" % (self.value)
        else:
            return "None"


class Procedure(Expr):
    def __init__(self,name, value):
        self.type = "Procedure"
        self.name = name
        self.value = value

    def __repr__(self):
        return u"Procedure %s" % self.name



def p_procedure_declaration(p):
    r'''
        procedure_declaration : procedure_identification procedure_block ENDPROCEDURE
    '''
    p[0] = Procedure(p[1], p[2])

class Block(Expr):
    def __init__(self,name, value):
        self.type = "Block"
        self.name = name
        self.value = value

    def __repr__(self):
        return u"Block %s %s" % (self.name, self.value)



class SetIn(Expr):
    def __init__(self,value):
        self.type = "SetIn"
        self.value = value

    def __repr__(self):
        return u"Set: %s" % (self.value)

def p_block_declaration(p):
    r'''
        block_declaration : block_identification procedure_block ENDBLOCK
    '''
    print "BLOCK: " , p[1]
    p[0] = Block(p[1], p[2])


class Auxfields(Expr):
    def __init__(self, value):
        self.type = "Auxfields"
        self.value = value

    def __repr__(self):
        return u"Auxfields Block %s" % str(self.value)


def p_auxfields_declaration(p):
    r'''
        auxfields_declaration : AUXFIELDS fields_declaration_list

    '''

    p[0] = Auxfields(p[2])

def p_procedure_identification(p):
    r'''
            procedure_identification : PROCEDURE IDENTIFIER
    '''

    p[0] = p[2]

def p_block_identification(p):
    r'''
            block_identification : BLOCK IDENTIFIER
    '''
    p[0] = p[2]


def p_type_array(p):
    r'''
        type_array : LBRACKET COUNT RBRACKET
                   | LBRACKET RBRACKET

    '''
    if len(p) > 2:
        p[0] = int(p[2])
    else:
        p[0] = int(0)




def p_type_denoter(p):
    '''
            type_denoter : IDENTIFIER
                         | STRING  type_array
                         | INTEGER type_array
                         | STRING
                         | INTEGER
                         | new_type
    '''


    if len(p) > 2:
        p[0] = TypeDenoter(p[1],True, p[2])
    else:
        p[0] = TypeDenoter(p[1])

        if p.slice[1].type == "IDENTIFIER":
            #print "This needs to look at the symbol table"
            pass




def p_new_type(p):
    '''
        new_type : new_ordinal_type
                 | new_structured_type

    '''
    p[0] = p[1]


def p_new_ordinal_type(p):
    '''
        new_ordinal_type : enumerated_type
                         | subrange_type
    '''

    p[0] = p[1]

def p_enum(p):
    '''
            p_enum : LPAREN enumerated_list RPAREN
        '''

    p[0] = p[2]

def p_enumerated_type(p):
    '''
        enumerated_type : p_enum xprepeat

    '''
    p[0] = ('enumerated', p[1])


def p_xprepeat(p):
                '''
                xprepeat :
               | COMMA tmodifiers_list
'''

def p_enum_languages_list(p):
    r'''
        enum_languages_list :  enum_languages_list LITERAL
                        | LITERAL

    '''


    items = []
    if len(p) > 2:
        if p[1] != None and isinstance(p[1], list):
            for i in p[1]:
                items.append(i)

            items.append(p[2])
            p[0] = items
        elif p[1] != None:
             p[0] = [p[1],p[2]]
        else:
            p[0] = p[2]


    else:
        p[0] = p[1]


def p_enum_num_arg(p):
    '''
            enum_num_arg : LPAREN COUNT RPAREN
                         |
    '''

    if len(p) > 2:
        p[0] = p[2]


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


    if p[1] == "FLOAT":
        p[0] = float(p[1])
    else:
        p[0] = int(p[1])

def p_subrange_type(p):
    '''
        subrange_type : numeric_type DOTDOT numeric_type

    '''

    p[0] = TypeRange("Range", p[1], p[3])


def p_new_structured_type(p):
    '''
        new_structured_type : structured_type
    '''

    p[0] = p[1]

def p_structured_type(p):
    '''
        structured_type : array_type
                        | set_type
    '''
    p[0] = p[1]


def p_array_type(p):
    '''
        array_type : ARRAY LBRACKET index_list RBRACKET OF component_type
    '''
    p[0] = ('array', p[3])

def p_index_list(p):
    '''
        index_list : index_list COMMA index_type
                   | index_type
    '''

    if len(p) > 2:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = p[1]

def p_index_type(p):
    '''
        index_type :  ordinal_type
    '''
    p[0] = p[1]

def p_ordinal_type(p):
    '''
        ordinal_type : new_ordinal_type
                     | IDENTIFIER
    '''

    if p.slice[1].type == "IDENTIFIER":
        p[0] = Identifier(p[1])
    else:
        p[0] = p[1]

def p_component_type(p):
    '''
        component_type : type_denoter
    '''

    p[0] = p[1]

def p_set_type(p):
    '''
        set_type : SET OF base_type
                 | SET LBRACKET COUNT RBRACKET OF base_type

    '''
    p[0] = ('set', p[3])

def p_base_type(p):
    '''
        base_type : ordinal_type
    '''
    p[0] = p[1]

def p_identifier_list(p):
    r'''
        identifier_list : identifier_list COMMA IDENTIFIER
                        | IDENTIFIER
    '''

    items = []
    if len(p) > 2:
       if p[1] != None and isinstance(p[1], list):
            for i in p[1]:
                items.append(i)

            items.append(Identifier(p[3]))
            p[0] = items
       elif p[1] != None:
            p[0] = [p[1],Identifier(p[3])]
       else:
            p[0] = Identifier(p[3])

    else:
        p[0] = Identifier(p[1])




class TypeLocal(TypeC):

    def __init__(self, name, value, modifiers = None):
            self.type = "TypeLocal"
            self.name =  name
            self.value = value
            self.modifiers = modifiers





def p_locals_declaration_list(p):
    r'''
        locals_declaration_list : locals_declaration_list locals_declaration
                                | locals_declaration
    '''

    if len(p) > 2:
        if isinstance(p[1], list) and isinstance(p[2],list):
            p[0] = p[1] + p[2]
        elif isinstance(p[1], list) and p[2] != None:
            p[0] = p[1] + [p[2]]
        elif p[1] != None and isinstance(p[2], list):
            p[0] = [p[1]] + p[2]
        elif isinstance(p[1], list):
            p[0] = p[1]
        else:
            if p[1] != None and p[2] != None:
                p[0] = [p[1], p[2]]

    else:
        p[0] = p[1]


def p_locals_declaration(p):
    '''
        locals_declaration : identifier_list COLON type_denoter
    '''

    items = []

    if p[1] != None and isinstance(p[1],list):
        for i in p[1]:
            items.append(i)

        p[0] =  TypeLocal( items, TypeDenoter(p[3]) )

    else:
        p[0] =  TypeLocal( p[1], TypeDenoter(p[3]))






def p_type_definition_list(p):
    r'''
            type_definition_list : type_definition_list type_definition
                                 | type_definition
    '''

    items = []

    if len(p) > 2:

        if p[1] != None and p[2] != None and isinstance(p[1],list) and isinstance(p[2],list):
            p[0] = p[1] + p[2]
        elif p[1] != None and isinstance(p[1], list):
           for i in p[1]:
              items.append(i)
           items.append(p[2])
           p[0] = items
        elif p[2] != None and isinstance(p[2], list) and p[1] != None:
            for i in p[1]:
                items.append(i)
            items.insert(0, p[1])
            p[0] = items
        elif p[1] != None:
            if isinstance(p[2],list):
                p[0] =  [p[1]] + p[2]
            else:
                p[0] = [p[1],p[2]]


        else:
            p[0] = p[2]

    else:

        p[0] = p[1]


def p_type_definition(p):
    r'''
        type_definition : IDENTIFIER COMPARE type_denoter tmodifiers_list
                        | IDENTIFIER COMPARE type_denoter
    '''

    if len(p) > 4:
        p[0] = TypeC(p[1], p[3], p[4])
    else:
        p[0] = TypeC(p[1], p[3])


def p_type_definition_part(p):
    r'''
                type_definition_part : TYPE type_definition_list

    '''

    p[0] = p[2]


class TModifier(Expr):
    def __init__(self, value):
        self.type = "TModifier"
        self.value = value

    def __repr__(self):
        return u"TModifier: " + self.value

def p_tmodifiers(p):
    '''
    tmodifiers :  EMPTY
                | NODK
                | NORF
                | DK
                | RF
                | NOEMPTY
'''
    p[0] = TModifier(p[1])


def p_tmodifiers_list(p):
    ''' tmodifiers_list : tmodifiers_list COMMA tmodifiers
                        | tmodifiers
    '''

    if len(p) > 2:
        if isinstance(p[3], list):
            p[0] =  [p[1]] + p[3]
        else:
            p[0] =  [p[1]] + [p[3]]

    else:
        p[0] =  p[1]


def p_rules_part(p):
    r'''
        rules_part : RULES statement_part
                   | locals_part
    '''

    print "\tRULES"
    if len(p) > 2:
        p[0] = p[2]
    else:
        p[0] = p[1]



def p_locals_part(p):
    r'''
        locals_part : LOCALS locals_declaration_list
                    | type_definition_part
                    | fields_part

    '''
    print "\tLOCALS"


    if len(p) > 2:
        p[0] = p[2]
    else:
        p[0] = p[1]

    pprint.pprint(p[0])

def p_fields_part(p):
    r'''
        fields_part : FIELDS fields_declaration_list
                    | auxfields_part
    '''
    #print "\tFIELDS"

    if len(p) > 2:
        p[0] = p[2]
    else:
        p[0] = p[1]

    #print p[0]


def p_auxfields_part(p):
    r'''
        auxfields_part : AUXFIELDS fields_declaration_list
                       | type_definition_part
    '''

    if len(p) > 2:
        p[0] = p[2]
    else:
        p[0] = p[1]

    print "\tAUXFIELDS"

    #print p[0]

def p_optional_sections_list(p):
    '''
        optional_sections_list : optional_sections_list optional_sections
                               | optional_sections
    '''
    items = []

    if len(p) > 2:

        if p[1] != None and p[2] != None and isinstance(p[1],list) and isinstance(p[2],list):
            p[0] = p[1] + p[2]
        elif p[1] != None and isinstance(p[1], list):
           for i in p[1]:
              items.append(i)
           items.append(p[2])
           p[0] = items
        elif p[2] != None and isinstance(p[2], list) and p[1] != None:
            for i in p[2]:
                items.append(i)
            items.insert(0, p[1])
            p[0] = items
        elif p[1] != None:
            if isinstance(p[2],list):
                p[0] =  [p[1]] + p[2]
            else:
                p[0] = [p[1],p[2]]


        else:
            p[0] = p[2]

    else:

        p[0] = p[1]


def p_optional_sections(p):
    '''
            optional_sections : parameters_declaration_part

    '''
    p[0] = p[1]

def p_procedure_block(p):
    r'''
            procedure_block : optional_sections_list
    '''

    p[0] = p[1]


class ParamModifier(Expr):
    def __init__(self, value):
        self.type = "ParamModifier"
        self.value = value

        if value == None:
            self.value = "IMPORT"



    def __repr__(self):
        if self.value == None:
            return u"ParamModifier: None"
        return u"ParamModifier: " + self.value

def p_parameter_modifiers(p):
    r'''    parameter_modifiers :
                                | IMPORT
'''

    if len(p) > 1:
        p[0] = p[1]



def p_fields_declaration_list(p):
    r'''
        fields_declaration_list : fields_declaration_list fields_declaration
                                | fields_declaration
    '''

    items = []

    if len(p) > 2:

        if p[1] != None and p[2] != None and isinstance(p[1],list) and isinstance(p[2],list):
            p[0] = p[1] + p[2]
        elif p[1] != None and isinstance(p[1], list):
           for i in p[1]:
              items.append(i)
           items.append(p[2])
           p[0] = items

        elif p[1] != None:
            if isinstance(p[2],list):
                p[0] =  [p[1]] + p[2]
            else:
                p[0] = [p[1],p[2]]
        else:
            p[0] = p[2]

    else:

        p[0] = p[1]




def p_field_description(p):
    '''
        field_description : DIVIDE LITERAL
    '''

    p[0] = p[2]


def p_tag_rule(p):
    '''
    tag_rule  : LPAREN LITERAL RPAREN
              | tempty
    '''


    if len(p) > 2:
        p[0] = p[2]





def p_fields_declaration(p):
    '''
    fields_declaration : identifier_list tag_rule enum_languages_list field_description COLON type_denoter
                       | identifier_list field_description COLON type_denoter
                       | identifier_list COLON type_denoter COMMA tmodifiers_list
                       | identifier_list  COLON type_denoter
                       | IDENTIFIER TAG enum_languages_list field_description COLON type_denoter COMMA tmodifiers_list
                       | IDENTIFIER TAG enum_languages_list field_description COLON type_denoter

    '''


    items = []

    if p.slice[1].type == "identifier_list":
        #def __init__(self, name, tag, languages, description, typeOf, modifiers = None):
        if p[1] != None:
            if isinstance(p[1],list):
                for i in p[1]:
                    items.append(i)

        if p.slice[2].type == 'COLON' and len(p.slice) > 4:
            items.append(Field(p[1],None,None,None, p[3],p[4]))
        elif p.slice[2].type == 'COLON':
            items.append(Field(p[1],None,None,None,p[3]))
        elif p.slice[2].type == 'field_description':
            items.append(Field(p[1],None,None, p[2],p[4]))
        else:
            items.append(Field(p[1],p[2],p[3],p[4],p[6]))

        p[0] = items

        for i in items:
            vars[str(i.name)] = i

    else:

        if p.slice[1].type == "IDENTIFIER" and len(p) > 6:
            p[0] = Field(Identifier(p[1]), p[2], p[3], p[4], p[6])
        elif p.slice[1].type == "IDENTIFIER":
            p[0] = Field(Identifier(p[1]), p[2], p[3], p[4], p[6])

        vars[str(p[0].name)] = p[0]


def p_parameters_declaration_part(p):
    r'''
        parameters_declaration_part : PARAMETERS parameters_declaration_list
                                    | rules_part
    '''
    print "\t\tPARAMETERS"


    if len(p) > 2:
        p[0] = p[2]
    else:
        p[0] = p[1]

def p_statement_part(p):
    r'''
        statement_part : statement_list
    '''

    p[0] = p[1]

def p_parameters_declaration_list(p):
    r'''
            parameters_declaration_list : parameter_declaration parameters_declaration_list
                                        | parameter_declaration

    '''

    items = []
    if len(p) > 2:

        if p[1] != None and p[2] != None and isinstance(p[1],list) and isinstance(p[2],list):
            p[0] = p[1] + p[2]
        elif p[1] != None and isinstance(p[1], list):
           for i in p[1]:
              items.append(i)
           items.append(p[2])
           p[0] = items

        elif p[1] != None:
            if isinstance(p[2],list):
                p[0] =  [p[1]] + p[2]
            else:
                p[0] = [p[1],p[2]]
        else:
            p[0] = p[2]

    else:
        p[0] = p[1]


def p_parameter_declaration(p):
    r'''
              parameter_declaration : parameter_modifiers identifier_list COLON type_denoter
                                    | EXPORT identifier_list COLON type_denoter
                                    | TRANSIENT identifier_list COLON type_denoter
                                    | block_declaration
                                    | procedure_declaration

    '''

    items = []

  # if p[1] != None and isinstance(p[1],list):
  #      for i in p[1]:
  #          items.append(i)
  #
  #      p[0] =  TypeLocal( items, TypeDenoter(p[3]) )

  #  else:
#     p[0] =  TypeLocal( p[1], TypeDenoter(p[3]))


    if len(p) > 2 and p.slice[2].type == 'identifier_list':
        print "INSIDE PARAMS: ", p[1], p[2], p[4]
        if p[2] != None and isinstance(p[2], list):
            for i in p[2]:
                items.append(i)
                 #def __init__(self, name, typepar, modifiers)
                 # :
            print "ooooooooooooooooooooooooooooooo"
            print p[1]
            if p[1] == None or p[1] == "":
                p[1] = "IMPORT"
            p[0] = Parameter(items,p[4], p[1])
        else:
            p[0] = Parameter(p[2],p[4],p[1])
    else:
        p[0] = p[1]



def p_sign(p):
    r''' sign : PLUS
                | MINUS
    '''
    p[0] = p[1]

#def p_non_string(p):
#    r'''
#            non_string : COUNT
#                        | IDENTIFIER
#                        | FLOAT
#    '''
#    if p.slice[1] == 'IDENTIFIER':
#        p[0] = Identifier(p[1])
#    elif p.slice[1] == 'FLOAT':
#        p[0] = float(p[1])
#    else:
#        p[0] = int(p[1])


def p_control_variable(p):
    r'''
        control_variable : IDENTIFIER
    '''
    p[0] = Identifier(p[1])

def p_initial_value(p):
    r'''
        initial_value : expression
    '''

    p[0] = p[1]

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

    p[0] = p[1]


def p_expression(p):
    r'''
            expression : or_expression
    '''
    p[0] = p[1]


def p_or_expression(p):
    r'''
        or_expression : or_expression OR and_expression
                      | and_expression
    '''

    if len(p) > 2:

        p[0] = BinOp(p[1],'OR', p[3])
    else:
        p[0] = p[1]

def p_and_expression(p):
    r'''
        and_expression : and_expression AND not_expression
                        | not_expression
    '''

    if len(p) > 2:
        p[0] = BinOp(p[1], 'AND', p[3])
    else:
        p[0] = p[1]


def p_not_expression(p):
    r'''
        not_expression : NOT not_expression
                        | compare_expression
    '''


    if len(p) > 2:
        p[0] = Not(p[2])
    else:
        p[0] = p[1]


def p_compare_expression(p):
    r'''
        compare_expression : compare_expression relop add_expression
                           | add_expression
    '''

    if len(p) > 2:
        p[0] = BinOp(p[1], p[2], p[3])
    else:
        p[0] = p[1]



def p_relop(p):
    '''
            relop : COMPARE
                    | LESSTHAN
                    | DIFF
                    | EQUALMORETHAN
                    | MORETHAN
                    | LESSEQUALTHAN

    '''

    p[0] = p[1]

def p_add_expression(p):

    r'''
            add_expression : add_expression PLUS mult_expression
                           | add_expression MINUS mult_expression
                           | mult_expression

    '''

    try:
        if len(p) > 2:
            p[0] = BinOp(p[1],p[2],p[3])
        else:
            p[0] = p[1]

    except:
        #print "Add Expression Error"
        pass


def p_mult_expression(p):
    r'''
            mult_expression : mult_expression MULT unary_expression
                            | mult_expression DIVIDE unary_expression
                            | mult_expression MOD unary_expression
                            | mult_expression IN unary_expression
                            | unary_expression
    '''



    try:
        if len(p) > 2:
            p[0] = BinOp(p[1],p[2],p[3])
        else:
            p[0] = p[1]

    except:
        pass
        #print "Mult Expression Error"

def p_unary_expression(p):
    r'''
                    unary_expression : sign unary_expression
                                     | exp_expression
    '''




    if len(p) > 2:
        p[0] = Unary(p[1], p[2])
    else:
        p[0] = p[1]

def p_exp_expression(p):
    r'''
                    exp_expression : primary MULTMULT exp_expression
                                    | primary
    '''

    try:
        if len(p) > 2:
            p[0] = BinOp(p[3],p[2],p[1])
        else:
            p[0] = p[1]

    except:
        pass




def p_built_in_functions(p):
    r'''
            built_in_functions : STR
                               | LEN
                               | SUBSTRING
                               | RANDOM
                               | YEAR

    '''
    p[0] =  p[1]




def p_primary(p):
    r'''
        primary : variable_access
                | unsigned_constant
                | set_constructor
                | COUNT
                | FLOAT
                | LPAREN expression RPAREN
                | built_in_functions LPAREN index_expression_list RPAREN

    '''

    #                | IDENTIFIER LPAREN index_expression_list RPAREN


    if p.slice[1].type == 'built_in_functions':
        p[0] = CallBuildIn(p[1],p[3])
    elif p.slice[1].type == 'IDENTIFIER':
        print "CallProc"
        p[0] = CallProc(p[1], p[3])
    elif p.slice[1].type ==  'LPAREN':
        p[0] = p[2]
    elif p.slice[1].type == 'set_constructor':
        print "SET: ", p[1]
        p[0] = p[1]
    else:
        p[0] = p[1]



def p_unsigned_constant(p):
    r'''
        unsigned_constant : LITERAL
                           | RF
                           | DK
                           | EMPTY
    '''

    if p.slice[1].type == "RF" or p.slice[1].type == "EMPTY" or p.slice[1].type == "DK":
        p[0] = UnsignedConstant(p[1], p[1])
    else:
        p[0] = UnsignedConstant("LITERAL", p[1])


def p_set_constructor(p):
    r'''
        set_constructor : LBRACKET member_designator_list RBRACKET


    '''
#                        | LBRACKET RBRACKET

    p[0] = p[2]

def p_member_designator_list(p):
    r'''
        member_designator_list : member_designator_list COMMA member_designator
                                | member_designator

    '''

    if len(p) > 2:
        if isinstance(p[1],list):
            p[0] = [p[1]] + [p[3]]
        else:
            if p[1]:
                p[0] = [p[1]] + [p[3]]
            else:
                p[0] = p[3]
    else:
        p[0] = p[1]

def p_member_designator(p):
    r'''
            member_designator : member_designator DOTDOT expression
                              | expression
    '''

# member_designator DOTDOT expression tmodifiers_list

    if p.slice[1].type == 'member_designator':
        p[0] = TypeRange(None, p[1],p[2])
    else:
        p[0] = p[1]



class Identifier(Expr):
    def __init__(self, name):
        self.type = "Identifier"
        self.name = name


    def __repr__(self):
        return stdout_encode('{0}').format(self.name)


def p_variable_access(p):
    r'''
            variable_access : IDENTIFIER
                            | indexed_variable
                            | field_designator

    '''
    if p.slice[1].type == "IDENTIFIER":
        p[0] = Identifier(p[1])
    else:
        p[0] = p[1]



class IndexedVariable(Expr):
    def __init__(self, name, index):
        self.type = "Indexed Variable"
        self.name = name
        self.index = index


    def __repr__(self):
        return stdout_encode('<Indexed Variable> {0}[{1}]').format(self.name, self.index)


def p_indexed_variable(p):
    r'''
        indexed_variable :  variable_access LBRACKET index_expression_list RBRACKET

    '''


    p[0] = IndexedVariable(p[1], p[3])

def p_index_expression_list(p):
    r'''
        index_expression_list : index_expression_list COMMA index_expression
                        | index_expression
    '''
    if len(p) > 2:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = p[1]

def p_index_expression(p):
    r'''        index_expression : expression
        '''
    p[0] = p[1]

def p_field_designator(p):
    r'''
        field_designator : variable_access DECIMAL IDENTIFIER
                        | variable_access DECIMAL ORD
                          | variable_access DECIMAL CARDINAL

    '''

    try:
        p[0] = p[1] + "" + p[2] + "" + p[3]
        print p[0]
    except:
        pass


def p_method_statement(p):
    r'''
            method_statement : variable_access DECIMAL KEEP
    '''

    print "KEEP method"
    p[0] = "KEEP"


class CallProc(Expr):
    def __init__(self, name, params = None):
        self.type = "Call Procedure"
        self.name = name
        self.params = params

    def __repr__(self):
        return stdout_encode('<(Call Proc: {0} Params: {1})>').format(self.name, self.params)

class CallBuildIn(Expr):
    def __init__(self, name, params = None):
        self.type = "Call BuildIn"
        self.name = name
        self.params = params

    def __repr__(self):
        return stdout_encode('<(Call BuildIn: {0} Params: {1})>').format(self.name, self.params)



def p_procedure_statement(p):
    r'''
            procedure_statement : IDENTIFIER params
                               | IDENTIFIER
    '''

    if len(p) > 2:
        p[0] = CallProc(p[1],p[2])
    else:
        p[0] = CallProc(p[1])

def p_params(p):
    r'''
            params : LPAREN actual_parameter_list RPAREN

    '''

    p[0] = p[2]


def p_actual_parameter_list(p):
    r'''
        actual_parameter_list : actual_parameter_list COMMA actual_parameter
                            | actual_parameter
    '''

    if len(p) > 2:
        if isinstance(p[1],list):
            p[0] = p[1] + [p[3]]
        else:
            p[0] = [p[1]] + [p[3]]
    else:
        p[0] = p[1]


def p_actual_parameter(p):
    r'''
            actual_parameter :  expression '''
    p[0] = p[1]


"""
                            |  expression COLON expression
                            |  expression COLON expression COLON expression


"""


def p_statement_list(p):
    r'''statement_list :  statement_list statement
                        | statement
      '''

    items = []

    if len(p) > 2:
        if p[1] != None and isinstance(p[1], list):

            for i in p[1]:
                items.append(i)

            items.append(p[2])
            p[0] = items

        elif p[1] != None:
            p[0] = [p[1],p[2]]
        else:
            p[0] = p[2]


    else:
        p[0] = p[1]


#def p_module_statement(p):
#    r'''
#        module_statement : variable_access LBRACKET index_expression_list RBRACKET LPAREN index_expression_list RPAREN
#    '''
#
#    print "MODULE CALL"


def p_statement(p):
    ''' statement :   assignment_statement
                    | method_statement
                    | procedure_statement
                    | repeat_statement
                    | if_statement
                    | while_statement
                    | for_statement
                    | check_statement
                    | signal_statement
                    | not_statement


    '''
    p[0] =  p[1]

   #        | module_statement


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
    print "FOR "

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
        if_statement :    IF boolean_expression THEN statement_list else_statement_list ENDIF

                        | IF boolean_expression THEN statement_list ENDIF
 '''


    if p.slice[5] == 'else_statement_list':
        p[0] = IfElseStatement(p[2], p[4], p[5])
    else:
        p[0] = IfStatement(p[2], p[4])

    #| IF boolean_expression THEN statement_list else_statement_list ENDIF

    #print p[0]

def p_else_statement_list(p):
    r'''
        else_statement_list : ELSEIF boolean_expression THEN statement_list else_statement_list
                            | ELSEIF boolean_expression THEN statement_list
                            | ELSE statement_list

    '''

    if p.slice[2] == 'boolean_expression' and len(p.slice) > 5:
        p[0] = IfElseStatement(p[2], p[4], p[5])
    elif p.slice[2] == 'boolean_expression':
        p[0] = IfStatement(p[2], p[4])
    else:
        p[0] = ElseStatement(p[2])

    #print p[0]



def p_assignment_statement(p):
    r'''
        assignment_statement : variable_access ASSIGN expression
    '''

    p[0] = BinOp(p[1], ':=', p[3])
    vars[str(p[1])] = p[3]



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



pprint.pprint(result,width=120)
#print result


#pprint.pprint( vars )

#
#while 1:
#    try:
#        choice=input("-> ")
#        print choice
#        if choice in ["Q","q"]: break
#
#    except Exception as e:
#        print e


