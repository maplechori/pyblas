#!/usr/bin/env python
# -*- coding: UTF-8 -*-
__author__ = 'adrianmo'
from datetime import date
import ply.lex as lex
import ply.yacc as yacc
import string
import re
import codecs,sys, unicodedata
import pprint
import logging
import MySQLdb
import pandas as pd1
import pandas.io.sql as psql
import os, types
import g2import


vars = {
        "ACTIVELANGUAGE" : { 'name': "ACTIVELANGUAGE" , 'type' : "INTEGER", 'value' : 3} ,
                "CORENG" : { 'name' :"CORENG", 'type' : "INTEGER", 'value' : 1},
                "CORSPN" : { 'name' :"CORSPN", 'type' : "INTEGER", 'value' : 2},
                "PRXENG" : { 'name' :"PRXENG", 'type' : "INTEGER", 'value' : 3},
                "PRXSPN" : { 'name' :"PRXSPN", 'type' : "INTEGER", 'value' : 4},
                "SPPENG" : { 'name' :"SPPENG", 'type' : "INTEGER", 'value' : 5},
                "SPPSPN" : { 'name' :"SPPSPN",'type' : "INTEGER", 'value' : 6},
                "EXTENG" : { 'name' :"EXTENG", 'type' : "INTEGER", 'value' : 7},
                "EXTSPN" : { 'name' :"EXTSPN",'type' : "INTEGER", 'value' : 8},
                "MEDIA" : { 'name' :"MEDIA", 'type' : "INTEGER", 'value' : 9},
                "TYPEACONSISTENT" : { 'name' :"TYPEACONSISTENT", 'type' : "INTEGER", 'value' : 1},
                "TYPEBCONSISTENT" : { 'name' :"TYPEBCONSISTENT", 'type' : "INTEGER", 'value' : 2},
                "CASHBALANCE" : { 'name' :"CASHBALANCE", 'type' : "INTEGER", 'value' : 3},
                "INCONSISTENT" : { 'name' : "INCONSISTENT", 'type' : "INTEGER", 'value' : 4},
                "NO": { 'name' :"NO", 'type' : 'TYESNO' , 'value' : 5 },
                "YES" : { 'name' :"YES", 'type' : 'TYESNO' , 'value' : 1},
                "PIUNFINDEX" : { 'name' :"PIUNFINDEX", 'type' : 'STRING' , 'value' : ''},
                "PIEMPLOYERNAME" :  {'name' :"PIEMPLOYERNAME", 'type' : 'STRING' , 'value' : '[EMPLOYER NAME]'},
                "X038MO" : { "type" : "STRING", "value"  : "[PREV WAVE IW MONTH OF FINANCIAL R]"}

        }


sys.stdout = codecs.getwriter('utf8')(sys.stdout)

def stdout_encode(u, default='utf8'):
        return u

data = None

def include_file(fname):
    with codecs.open(fname, encoding='utf8') as fl:
        return fl.read()


data6 = include_file("hrs_basis.inc")
data4 = include_file("hrs_fills_init.inc")
data = include_file("hrs_typ.inc")
data2 = include_file("hrs_proc.inc")


# data3 = include_file("hrs_c4.txt")
# data3 = include_file("hrs10_m1.inc")
# data3 = include_file("hrs10_m2.inc")
# data3 = include_file("hrs10_s.inc")
# data3 = include_file("hrs10_t.inc")
#data3 = include_file("hrs10_tn.inc")
#data3 = include_file("hrs_a.txt")
#data3 = include_file("hrs10_b.inc")
#data3 = include_file("hrs10_d.inc")
#data3 = include_file("hrs10_e.inc")
#data3 = include_file("hrs10_f.inc")
#data3  = include_file("hrs10_g.inc")
#data3  = include_file("hrs10_h.inc")
#data3  = include_file("hrs10_j.inc")
#data3  = include_file("hrs10_k.inc")
#data3  = include_file("hrs10_l.inc")
#data3  = include_file("hrs10_n.inc")
#data3  = include_file("hrs10_p.inc")
#data3  = include_file("hrs10_q.inc")
#data3  = include_file("hrs10_r.inc")
#data3  = include_file("hrs10_u.inc")


#data3  = include_file("hrs10_v1.inc")
#data3  += include_file("hrs10_v2.inc")
#data3  += include_file("hrs10_v3.inc")
#data3  += include_file("hrs10_v4.inc")
#data3  += include_file("hrs10_v5.inc")
#data3  += include_file("hrs10_v6.inc")
#data3  += include_file("hrs10_v7.inc")
#data3  += include_file("hrs10_v8.inc")
#data3  += include_file("hrs10_v9.inc")
#data3  += include_file("hrs10_v10.inc")
#data3  += include_file("hrs10_v.inc")
#data3  = include_file("hrs10_y1.inc")

#data3  = include_file("hrs10_v_physmeasintro.inc")
#data3  += include_file("hrs10_v_physicalmeasures.inc")

#data3 = include_file("hrs10_w.inc")
#data3 += include_file("hrs10_w1.inc")
#data3 += include_file("hrs10_w2.inc")
#data3 += include_file("hrs10_w_events.inc")


#data3 += include_file("hrs10_v_pensiondocs.inc")
#data3 += include_file("hrs10_pension.inc")


#data3 = include_file("hrs10_a2_init.inc")
#data3 += include_file("hrs10_a2.inc")



data = data6 + data4 + data + data2



#data =  data3

def new_scope():
    return {}

levelScope = 0
symbolTable = {0 : new_scope()}
procedureTable = {}
blockTable = {}
Questions = {}
Fills = {}
Auxs = {}
Fills["UNFTEXT"] = []

def push_scope(scope):
    global levelScope, symbolTable

    levelScope += 1


    if not symbolTable.has_key(levelScope):
        symbolTable[levelScope] = scope

def pop_scope():

    global levelScope, symbolTable
    levelScope -= 1

class Expr:
    pass

def p_new_scope(p):
    "new_scope :"
    # Create a new scope for local variables
    scope = new_scope()
    push_scope(scope)


class String(unicode):
    def __init__(self, value):
        self.type = "STRING"
        self.value = value

    def __repr__(self):
        return ("%s %s" % (self.type, self.value)).encode('iso8859-1', 'ignore')

class Boolean(Expr):
    def __init__(self, value):
        self.type = "BOOLEAN"
        self.value = value


    def __repr__(self):
        return stdout_encode('{0} [{1}]').format(self.type, self.value)

class TypeC(Expr):
    def __init__(self, name, value, modifiers = None):
        self.type = "TYPE"
        self.name =  name.upper()
        self.value = value
        self.modifiers = modifiers

    def __repr__(self):
        return "%s: %s Value: %s  - Modifiers [%s]" % (self.type, self.name, self.value, self.modifiers)

class TypeRange(Expr):

  def __init__(self, value, max, min):
        self.type = "RANGE"
        self.value = value
        self.upperlimit = max
        self.lowerlimit = min

  def __repr__(self):

        return "TYPERANGE %s[%s..%s]" % (str(self.value) , str(self.lowerlimit), str(self.upperlimit))


class TypeDenoter(Expr):

    def __init__(self, value, array=False, size=0):
        self.type = "TYPEDENOTER"

        if isinstance(value, unicode):
            value = value.upper()
            #print "UPPERCASE:" , value

        self.value = value
        self.array = array
        self.size = size


    def __repr__(self):

        if self.array and self.size > 0:
            return "TYPEDENOTER %s[%d]" % (str(self.value) , self.size)
        elif self.array:
            return "TYPEDENOTER %s[]" % (str(self.value))
        else:
            return "TYPEDENOTER %s" % str(self.value)

class Parameter(Expr):
    def __init__(self, name, typepar, modifiers):
        self.type = "PARAMETER"
        self.name =  name
        self.modifiers = modifiers
        self.value = typepar



    def __repr__(self):
        return "Parameter: %s Modifiers: %s  - Type [%s]" % (self.name, self.modifiers, self.value)



state_in_types = 0

states = (
  ('ccode','exclusive'),
  ('fillcode', 'exclusive'))

tokens = [
    "IN",
    "RULES",
    "ENDTABLE",
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
    "TYPE",
    "YEAR",
    "MONTH",
    "INTEGER",
    "STRING",
    "SHOW",
    "RESERVECHECK",
    "INSERT",
    "NONRESPONSE",

    ]

reserved = {
'PROCEDURE' : 'PROCEDURE',
'CHECK' : 'CHECK',
'IF' : 'IF' ,
'THEN' : 'THEN',
'ELSE' : 'ELSE',
'NOT' : 'NOT',
'OR' : 'OR',
'AND' : 'AND',
"CAP" : "CAP",
"ROUND" : "ROUND",
"ABS"  : "ABS",
"LOWERCASE" : "LOWERCASE",
"UPPERCASE" : "UPPERCASE",
'ELSEIF' : 'ELSEIF',
'ENDIF' : "ENDIF",
'BLOCK' : "BLOCK",
'TABLE' : 'TABLE',
'AUXFIELDS':'AUXFIELDS',
'TRANSIT':'TRANSIT',
'IMPORT' : 'IMPORT',
'TODATE' : 'TODATE',
'EXPORT' : 'EXPORT',
'PARAMETERS':'PARAMETERS',
'KEEP' : 'KEEP',
'TRUNC' : 'TRUNC',
'ORD' : 'ORD',
'CARDINAL' : 'CARDINAL',
'ASK' : 'ASK',
'EMPTY' : 'EMPTY',
'NOEMPTY' : 'NOEMPTY',
'NORF' : 'NORF',
'NODK' : 'NODK',
'DK' : 'DK',
'SET' : 'SET',
'ARRAY' : 'ARRAY',
'OF' : 'OF',
'RF' :'RF',
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
'VAL' : 'VAL',
'STR' : 'STR',
'SUBSTRING'  : 'SUBSTRING',
'RANDOM' : 'RANDOM',
'SYSDATE' : 'SYSDATE',
'FRAC' :  'FRAC',
'POSITION' : 'POSITION',
"LAYOUT" : "LAYOUT",
"FROM" : "FROM",
"TO" : "TO",
"FIELDPANE" :  "FIELDPANE"
""
}


tokens += list(reserved.values())

t_ignore = " \t\r"

def t_newpage(t):
    '[Nn][Ee][Ww][Pp][Aa][Gg][Ee]'
    pass



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

def t_IN(t):
    r'[Ii][Nn]\s'
    return t

def t_TYPE(t):
    r'[Tt][Yy][Pp][Ee]\s'
    global state_in_types
    state_in_types  = 1
    t.type = "TYPE"
    return t

def t_MONTH(t):
    r'[Mm][Oo][Nn][Tt][Hh](?=\(|\s)'
    t.type = "MONTH"
    t.value = "MONTH"
    print "MONTH: " + str(state_in_types) + "****************"
    global state_in_types
    if state_in_types == 1:
        t.value = "IDENTIFIER"
        t.type = "IDENTIFIER"
    return t

def t_YEAR(t):
    r'[Yy][Ee][Aa][Rr](?=\(|\s)'
    t.type = "YEAR"
    t.value = "YEAR"
    global state_in_types
    print "YEAR: " + str(state_in_types) + "****************"
    if state_in_types == 1:
        t.value = "IDENTIFIER"
        t.type = "IDENTIFIER"


    return t

def t_LOCALS(t):
    r'[Ll][Oo][Cc][Aa][Ll][Ss]\s'
    t.type = "LOCALS"
    t.value = "LOCALS"
    global state_in_types
    state_in_types = 0
    return t


def t_RULES(t):
    r'[Rr][Uu][Ll][Ee][Ss]\s'
    t.type = "RULES"
    t.value="RULES"
    global state_in_types
    state_in_types = 0
    return t

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

def t_fillcode_error(t):
    t.lexer.skip(1)


def t_TAG(t):
    r"\([A-Z][0-9]+[\.A-Z0-9]*[_]?\)|\([a-zA-Z]+[_][A-Z]+[_]?[A-Z]*\)|\(GENDER\)|(\(Seq[0-9]P+[\.A-Z0-9]*[_]?[n]?[0-9]?[0-9]?\))"
    #print t.value
    return t

def t_ENDPROCEDURE(t):
    r'[Ee][Nn][Dd][Pp][Rr][Oo][Cc][Ee][Dd][Uu][Rr][Ee]'
    return t

def t_ENDTABLE(t):
    r'[Ee][Nn][Dd][Tt][Aa][Bb][Ll][Ee]'
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

def t_STRING(t):
    r'[Ss][Tt][Rr][Ii][Nn][Gg]'
    return t

def t_INTEGER(t):
    r'[Ii][Nn][Tt][Ee][Gg][Ee][Rr]'
    return t

def t_PIPE(t):
    r'\|'
    return t

def t_INSERT(t):
    r'[Ii][Nn][Ss][Ee][Rr][Tt]'
    return t

def t_RESERVECHECK(t):
    r'RESERVECHECK'
    pass

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

#def t_INITIAL_TYPE(t):
#    r'[Tt][Yy][Pp][Ee]\s'
#    print "TOKEN TYPE"
#    return t

def t_IDENTIFIER(t):
    r"[\^a-zA-Z_\xF3][a-zA-Zñáéíóúü0-9_\xF3]*"
    t.type = reserved.get(string.upper(t.value), 'IDENTIFIER')
    #print t
    return t

def t_LPAREN(t):
    "\("
    return t

def t_RPAREN(t):
    "\)"
    return t

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex(reflags=(re.UNICODE))
lexer.input(data)

#for tok in iter(lexer.token, None):
#   print tok.type

#if type(tok.value) == type(u's'):
#pass
  #print tok
#else:
#pass
# print repr(tok.value)
  #print tok


precedence = (
    ('left', 'ASSIGN'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULT', 'DIVIDE', 'MOD'),
    ('right', 'sign')
)

start = 'codeblock'

def p_codeblock(p):
    r'''
        codeblock : procedure_and_block_declaration_part codeblock
                  |  tempty
    '''

    items = []

    if len(p) > 2:

        if p[2] != None:
            if isinstance(p[1],list):
                for x in p[1]:
                    items.append(x)
            else:
                items.append(p[1])

            if isinstance(p[2],list):
                for y in p[2]:
                    items.append(y)
            else:
                items.append(p[2])

            p[0] = items
        else:
            p[0] = p[1]

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
            proc_or_block_declaration :  type_declaration
                                       | auxfields_declaration
                                       | procedure_declaration
                                       | block_declaration

'''

    p[0] = p[1]

class Field(Expr):

    global Questions

    def __init__(self, name, tag, languages, description, typeOf, modifiers = None):
        self.type = "FIELD"
        self.name  = name
        self.tag = tag
        self.languages = languages
        self.description = description
        self.typeOf = typeOf
        self.modifiers = modifiers


        tagQ = str(tag).strip("()")

        if Fills.has_key(tagQ):
            Fills[tagQ].append(description)
        else:
            Fills[tagQ] = [ description ]


        tag = str(tag).strip("()_")

        QName = ""

        if not isinstance(name, list):


            if re.findall("_", str(name.name)):
                try:
                    if str(name.name).index("_") == len(name.name) - 1:
                        QName = str(name.name[:str(name.name).index("_")])
                    else:
                        QName = str(name.name)

                except Exception, e:
                    print e
            else:
                QName = name.name

                if QName != "" and QName[len(QName)-1] == "S":
                   QName = QName[:len(QName)-1] + QName[len(QName)-1:].replace("S", "")
                if QName != "" and QName[len(QName)-1] == "A":
                   QName = QName[:len(QName)-1] + QName[len(QName)-1:].replace("A", "")
                if QName != "" and QName[len(QName)-1] == "T":
                   QName = QName[:len(QName)-1] + QName[len(QName)-1:].replace("T", "")




            if (tag and tag != "None"):


                if Fills.has_key(tag):
                    Fills[tag].append(description)
                else:
                    Fills[tag] = [ description ]


                if tag[len(tag)-1] == "S":
                    tag = tag[:len(tag)-1] + tag[len(tag)-1:].replace("S", "")
                if tag[len(tag)-1] == "A":
                    tag = tag[:len(tag)-1] + tag[len(tag)-1:].replace("A", "")
                if tag[len(tag)-1] == "T":
                    tag = tag[:len(tag)-1] + tag[len(tag)-1:].replace("T", "")

                if Questions.has_key(tag):
                    Questions[QName] = self
                else:
                    Questions[tag] = self

            elif QName != "":

                Questions[QName] = self


                if Fills.has_key(QName):
                    Fills[QName].append(description)
                else:
                    Fills[QName] = [ description ]

            else:
                print self.name
                print "*** We shouldnt be here ***"


    def __repr__(self):
        return ("Field: %s Type: %s  Tag: %s " % (self.name, self.typeOf, self.tag)).encode('ascii','ignore')

    def save(obj):
        return (obj.__class__, obj.__dict__)

    def load(cls, attributes):
        obj = cls.__new__(cls)
        obj.__dict__.update(attributes)
        return obj



class BinOp(Expr):
    def __init__(self,left,op,right):
        self.type = "BINOP"
        self.left = left
        self.right = right
        self.op = op

    def __repr__(self):

        if self.left != None and self.right != None:
            return ("L:%s OP[%s] R:%s" % (self.left, self.op,  self.right)).encode('ascii','ignore')
        else:
            return "MISSING BINOP %s" % ( self.op )


class IfStatement(Expr):
    def __init__(self,left, right):
        self.type = "IF"
        self.left = left
        self.right = right


    def __repr__(self):

        if self.left != None and self.right != None:
            return ("IF %s THEN %s" % (self.left, self.right)).encode('ascii','ignore')
        else:
            return "Missing IF"


class IfElseStatement(Expr):
    def __init__(self,first, second, third = None, fourth = None):
        self.type = "IFELSE"
        self.first = first
        self.second = second
        self.third = third
        self.fourth = fourth

    def __repr__(self):
            return ("IF 1:[%s] THEN 2:[%s] ELSEIF 3:[%s] ELSE 4:[%s]" % (self.first, self.second, self.third, self.fourth)).encode('ascii','ignore')

class ElseIfStatement(Expr):
    def __init__(self,first, second, third = None):
        self.type = "ELSEIF"
        self.first = first
        self.second = second
        self.third = third

    def __repr__(self):
            return ("ELSEIF 1:[%s] THEN 2:[%s] ELSEIF 3:[%s]" % (self.first, self.second, self.third)).encode('ascii','ignore')


class ElseStatement(Expr):
    def __init__(self,first):
        self.type = "ELSE"
        self.first = first


    def __repr__(self):

        if self.first != None:
           return ("ELSE 1: %s" % (self.first)).encode('ascii','ignore')
        else:
            return "MISSING ELSE"



class Unary(Expr):
    def __init__(self,op,value):
        self.type = "UNARY"
        self.value = value
        self.op = op

    def __repr__(self):

        if self.op and self.value:
            return ("%s %s" % (self.op, self.value)).encode('ascii', 'ignore')
        else:
            return "None"

class Not(Expr):
    def __init__(self,value):
        self.type = "NOT"
        self.value = value

    def __repr__(self):

        if self.value:
            return "NOT %s" % (self.value)
        else:
            return "None"


class Procedure(Expr):
    def __init__(self,name, value):
        self.type = "PROCEDURE"
        self.name = name
        self.value = value

    def __repr__(self):
        return "Procedure %s" % self.name



def p_procedure_declaration(p):
    r'''
        procedure_declaration : procedure_identification new_scope procedure_block ENDPROCEDURE
    '''
    global symbolTable, procedureTable
    global levelScope

    items = []
    symbolTable[levelScope][p[1].upper()] = {}
    for i in p[3]:
        if hasattr(i, 'type') and (i.type == "TYPEDENOTER" or i.type == "TYPE" or i.type == "TYPELOCAL" or i.type == "FIELD" or i.type == "PARAMETER"):
                items.append(i)

                if isinstance(i.name,list):
                    for j in i.name:
                            x = i
                            x.name = j.name
                            symbolTable[levelScope][p[1].upper()][j.name] = x
                else:
                    if i.type == "FIELD" or i.type == "PARAMETER":
                        symbolTable[levelScope][p[1].upper()][i.name.name] = i
                    else:
                        symbolTable[levelScope][p[1].upper()][i.name] = i

    procedureTable[p[1].upper()] = p[3]

    p[0] = Procedure(p[1].upper(), p[3])

    pop_scope()

class Block(Expr):
    def __init__(self,name, value):
        self.type = "BLOCK"
        self.name = name
        self.value = value

    def __repr__(self):
        return "Block %s %s" % (self.name, self.value)


class SetIn(Expr):
    def __init__(self,value):
        self.type = "SETIN"
        self.value = value

    def __repr__(self):
        return "Set: %s" % (self.value)

def p_block_declaration(p):
    r'''
        block_declaration : block_identification new_scope procedure_block ENDBLOCK
                          | block_identification new_scope procedure_block ENDTABLE
    '''
    global symbolTable, levelScope

    items = []
    symbolTable[levelScope][p[1].upper()] = {}

    for i in p[3]:
        if hasattr(i, 'type') and (i.type == "TYPEDENOTER" or i.type == "TYPE" or i.type == "TYPELOCAL" or i.type == "FIELD" or i.type == "PARAMETER" or i.type == "TYPEDEF" or i.type == "AUXFIELDS"):

                if hasattr(i,"name") and isinstance(i.name,list):
                    for j in i.name:
                            x = i
                            x.name = j.name
                            symbolTable[levelScope][p[1].upper()][j.name] = x
                else:
                    if i.type == "FIELD" or i.type == "PARAMETER":
                        symbolTable[levelScope][p[1].upper()][i.name.name] = i
                    elif i.type != "TYPEDEF":
                        symbolTable[levelScope][p[1].upper()][i.name] = i

    blockTable[p[1].upper()] = p[3]
    p[0] = Block(p[1].upper(), p[3])
    pop_scope()


class Auxfields(Expr):
    def __init__(self, value):
        self.type = "AUXFIELDS"
        self.value = value

    def __repr__(self):
        return "Auxfields Block %s" % unicode(self.value)


class TypeDefinition(Expr):
    def __init__(self, value):
        self.type = "TYPEDEF"
        self.value = value

    def __repr__(self):
        return "TypeDefinition Block %s" % unicode(self.value)

def p_auxfields_declaration(p):
    r'''
        auxfields_declaration : AUXFIELDS new_scope fields_declaration_list


    '''

    global symbolTable
    global levelScope
    #print symbolTable, " ", levelScope


    items = []

    symbolTable[levelScope][p[1].upper()] = {}

    for i in p[3]:
        if hasattr(i, 'type') and (i.type == "TYPEDENOTER" or i.type == "TYPE" or i.type == "TYPELOCAL" or i.type == "FIELD" or i.type == "PARAMETER" or i.type == "TYPEDEF"):
                items.append(i)

                if isinstance(i.name,list):
                    for j in i.name:
                            x = i
                            x.name = j.name
                            symbolTable[levelScope][p[1].upper()][j.name] = x
                else:
                    print i.type
                    if i.type == "FIELD" or i.type == "PARAMETER":
                        symbolTable[levelScope][p[1].upper()][i.name.name] = i
                    elif i.type != "TYPEDEF":
                        print i.type
                        symbolTable[levelScope][p[1].upper()][i.name] = i


       # vars[i.name] = { 'type' : i.typeOf , 'value' : ''}



    p[0] = Auxfields(p[3])
    pop_scope()



def p_type_declaration(p):
    r'''
                type_declaration : TYPE type_definition_list


    '''

    p[0] = TypeDefinition(p[2])
    print p[2]


def p_procedure_identification(p):
    r'''
            procedure_identification : PROCEDURE IDENTIFIER
    '''

    p[0] = p[2]

def p_block_identification(p):
    r'''
            block_identification : BLOCK IDENTIFIER
                                | TABLE  IDENTIFIER
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

class Enumerated(Expr):

    def __init__(self, value):
        self.type = "ENUMERATED"
        self.value = value

    def __repr__(self):
        return "Enumerated %s" % self.value

def p_enumerated_type(p):
    '''
        enumerated_type : LPAREN enumerated_list RPAREN
    '''

    items = []

    if p[2] != None and isinstance(p[2], list):
        for i in p[2]:
            items.append(i)
        p[0] = Enumerated(items)
    else:
        p[0] = Enumerated([2])


def p_enum_languages_list(p):
    r'''
        enum_languages_list : enum_languages_list IDENTIFIER LITERAL
                            | enum_languages_list LITERAL
                            | IDENTIFIER LITERAL
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
                         | tempty
    '''

    if len(p) > 2:
        p[0] = p[2]


class TypeEnumeratedItem(Expr):

    def __init__(self, name, value, languages):
        self.type = "ENUMERATEDITEM"
        self.name = name.upper()
        self.languages = languages
        self.value = value

    def __repr__(self):
        return "EnumeratedItem %s %s"  % (self.name, self.value)


def p_enumerated_list(p):
    r'''
        enumerated_list : IDENTIFIER enum_num_arg enum_languages_list COMMA enumerated_list
                        | IDENTIFIER enum_num_arg enum_languages_list
                        | IDENTIFIER enum_num_arg COMMA enumerated_list
                        | IDENTIFIER enum_num_arg

    '''

    items = []



    if len(p) > 5:
        if p[1] != None and isinstance(p[5], list) and p[5] != None:
            for i in p[5]:
                items.append(i)

        items.insert(0, TypeEnumeratedItem(p[1], p[2], p[3]))
        p[0] = items
    elif len(p) > 3:
        p[0] = [TypeEnumeratedItem(p[1], p[2], p[3])]
    else:
        p[0] = [TypeEnumeratedItem(p[1], p[2], "")]




class Integer(int):

    def __init__(self, value):
        self.type = "INTEGER"
        self.value = value

    def __repr__(self):
        return "%d"  % (self.value)

    def save(obj):
        return (obj.__class__, obj.__dict__)

    def load(cls, attributes):
        obj = cls.__new__(cls)
        obj.__dict__.update(attributes)
        return obj

class Real(float):

    def __init__(self, value):
        self.type = "REAL"
        self.value = value

    def __repr__(self):
        return "REAL %d"  % (self.value)

def p_numeric_type(p):
    r'''
        numeric_type : FLOAT
                     | COUNT
    '''


    if p[1] == "FLOAT":
        p[0] = Real(float(p[1]))
    else:
        p[0] = Integer(int(p[1]))

def p_subrange_type(p):
    '''
        subrange_type : sign numeric_type DOTDOT sign numeric_type
                        | sign numeric_type DOTDOT  numeric_type
                        | numeric_type DOTDOT sign numeric_type
                        | numeric_type DOTDOT  numeric_type

    '''

    if len(p) > 4:
        if p.slice[1].type == 'sign' and p.slice[4] == 'sign':
            p[0] = TypeRange("RANGE", str(p[4])+str(p[5]), str(p[1])+str(p[2]))
        else:
            p[0] = TypeRange("RANGE", str(p[4]), str(p[1])+str(p[2]))
    else:
        if p.slice[3].type == 'sign':
            p[0] = TypeRange("RANGE",  str(p[2])+str(p[3]), p[1])
        else:
            p[0] = TypeRange("RANGE",  p[3], p[1])


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




class Array(Expr):

    def __init__(self, size, array_t = None):
            self.type = "ARRAY"
            self.array_type = array_t
            self.size = size

    def __repr__(self):
        return "Array [%s] of %s"  % (self.array_type, self.size)




def p_array_type(p):
    '''
        array_type : ARRAY LBRACKET index_list RBRACKET OF component_type
    '''

    p[0] = Array(p[3], TypeDenoter(p[6]))

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
        p[0] = p[1]
    else:
        p[0] = p[1]

def p_component_type(p):
    '''
        component_type : type_denoter
    '''

    p[0] = p[1]


class Set(Expr):

    def __init__(self, set_t = None,  size = 0):
            self.type = "SET"
            self.set_t = set_t
            self.size = size

    def __repr__(self):
        return "Set [%s] of %s"  % (self.set_t, self.size)

def p_set_type(p):
    '''
        set_type : SET OF base_type
                 | SET LBRACKET COUNT RBRACKET OF base_type

    '''

    if len(p) > 4:
        p[0] = Set(p[6],p[3])
    else:
        p[0] = Set(p[3])

def p_base_type(p):
    '''
        base_type : ordinal_type
    '''
    p[0] = p[1]

def p_identifier_list(p):
    r'''
        identifier_list : identifier_list COMMA variable_access
                        | variable_access
    '''

    items = []

    if len(p) > 2:

       if p[1] != None and isinstance(p[1], list):
            for i in p[1]:
                items.append(i)

            items.append(p[3])
            p[0] = items

       elif p[1] != None:

            if isinstance(p[3], list):
                for x in p[3]:
                    items.append(x)

                p[0] = items.insert(0,p[1])

            elif p[3] != None:
                p[0] = [p[1],p[3]]
            else:
                p[0] = p[1]


       else:

            p[0] = p[3]

    else:
        p[0] = p[1]

class TypeLocal(TypeC):

    def __init__(self, name, value, modifiers = None):
            self.type = "TYPELOCAL"
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
    global symbolTable, levelScope
    items = []

    if p[1] != None and isinstance(p[1],list):
        for i in p[1]:
            items.append(i)
            if hasattr(i.name, "name"):
                symbolTable[levelScope][str(i.name.name).upper()] = TypeLocal(i.name.name,p[3] )
            else:
                symbolTable[levelScope][str(i.name).upper()] = TypeLocal(i.name,p[3] )


        p[0] =  TypeLocal( items, p[3] )

    else:

        p[0] =  TypeLocal( str(p[1].name).upper(),p[3] )
        if hasattr(p[1].name, "name"):
            symbolTable[levelScope][str(p[1].name.name).upper()] = p[3]
        else:
            symbolTable[levelScope][str(p[1].name).upper()] = p[3]

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
        type_definition : IDENTIFIER COMPARE LPAREN identifier_list RPAREN COMMA tmodifiers_list
                        | IDENTIFIER COMPARE LPAREN identifier_list RPAREN
                        | IDENTIFIER COMPARE type_denoter COMMA tmodifiers_list
                        | IDENTIFIER COMPARE type_denoter

    '''

    global symbolTable, levelScope

    if len(p) > 6 :
        t =  TypeC(p[1], p[4], p[7])
        p[0] = t
    elif len(p) == 6 and p.slice[5].type == 'RPAREN':
        t = TypeC(p[1], p[4], None)
        p[0] = t
    elif len(p) == 6 and p.slice[5].type == 'tmodifiers_list':
        t = TypeC(p[1], p[3], p[5])
        p[0] = t
    else:
        t = TypeC(p[1], p[3])
        p[0] = t

    symbolTable[levelScope][p[0].name.upper()] = t




def p_type_definition_part(p):
    r'''
                type_definition_part : TYPE type_definition_list



    '''

    if len(p) > 1:
        p[0] = p[2]
    else:
        p[0] = p[1]


class TModifier(Expr):
    def __init__(self, value):
        self.type = "TModifier"
        self.value = value

    def __repr__(self):
        return "TModifier: " +  self.value

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

    if len(p) > 2:
        p[0] = p[2]
    else:
        p[0] = p[1]



def p_locals_part(p):
    r'''
        locals_part : LOCALS  new_scope locals_declaration_list
                    | fields_part

    '''
   # print "\tLOCALS"



    if len(p) > 2:
        p[0] = p[3]
        pop_scope()

    else:
        p[0] = p[1]




def p_fields_part(p):
    r'''
        fields_part : FIELDS fields_declaration_list
                    | auxfields_part
    '''


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

#    print "\tAUXFIELDS"

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
        self.type = "PARAMMODIFIER"
        self.value = value

        if value == None:
            self.value = "IMPORT"

    def __repr__(self):
        if self.value == None:
            return "ParamModifier: None"
        return "ParamModifier: " + self.value

def p_parameter_modifiers(p):
    r'''    parameter_modifiers :
                                | IMPORT
                                | EXPORT
                                | TRANSIT
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
              |
    '''
    if len(p) > 2:
        p[0] = p[2]

def p_fields_declaration(p):
    '''
    fields_declaration : identifier_list tag_rule enum_languages_list field_description COLON type_denoter COMMA tmodifiers_list
                       | identifier_list tag_rule enum_languages_list field_description COLON type_denoter
                       | identifier_list field_description COLON type_denoter COMMA tmodifiers_list
                       | identifier_list field_description COLON type_denoter
                       | identifier_list tag_rule enum_languages_list COLON type_denoter COMMA tmodifiers_list
                       | identifier_list tag_rule enum_languages_list COLON type_denoter
                       | identifier_list COLON type_denoter COMMA tmodifiers_list
                       | identifier_list COLON type_denoter
                       | identifier_list TAG enum_languages_list field_description COLON type_denoter COMMA tmodifiers_list
                       | identifier_list TAG enum_languages_list field_description COLON type_denoter

    '''


    items = []
    #print p.slice, p[1]

    if p.slice[1].type == "identifier_list":
        #def __init__(self, name, tag, languages, description, typeOf, modifiers = None):

        if p.slice[2].type == 'COLON' and len(p.slice) > 4:
            f = Field(p[1],None,None,None, p[3],p[4])
            items.append(f)
        elif p.slice[2].type == 'COLON':
            f = Field(p[1],None,None,None,p[3])
            items.append(f)
        elif p.slice[2].type == 'field_description':
            f = Field(p[1],None,None, p[2],p[4])
            items.append(f)
        elif p.slice[5].type == 'type_denoter':
            f = Field(p[1],p[2],p[3],None,p[5])
            items.append(f)
        else:
            f = Field(p[1],p[2],p[3],p[4],p[6])
            items.append(f)

        p[0] = items

        if p[0] != None:

            if isinstance(p[1],list):
                for i in p[1]:
                    #print i.name
                    pass
                    #symbolTable[levelScope][str(i.name).upper()] = f

    else:


        if p.slice[1].type == "IDENTIFIER" and len(p) > 6:
            f =   Field(Identifier(p[1]), p[2], p[3], p[4], p[6])
            p[0] = f
        elif p.slice[1].type == "IDENTIFIER":
            f = Field(Identifier(p[1]), p[2], p[3], p[4], p[6])
            p[0] = f



def p_parameters_declaration_part(p):
    r'''
        parameters_declaration_part : PARAMETERS parameters_declaration_list
                                    | rules_part
                                    | block_declaration
                                    | procedure_declaration
    '''
    #print "\t\tPARAMETERS"


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
    '''
    global symbolTable
    global levelScope

    items = []

    if len(p) > 2 and p.slice[2].type == 'identifier_list':
        if p[2] != None and isinstance(p[2], list):
            for i in p[2]:
                items.append(i)

            if isinstance(p[1], types.NoneType):
                paramType = "IMPORT"
            else:
                paramType = p[1]

            Parameter(items,p[4], paramType)
            p[0] = Parameter(items,p[4], paramType)
        else:
            paramType = p[1]


            if isinstance(p[1], types.NoneType):
                paramType = "IMPORT"

            p[0] = Parameter(p[2],p[4],paramType)
            symbolTable[levelScope][p[2].name] = p[0]
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
        not_expression : NOT compare_expression
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
            if p.slice[2].type == "IN":
                p[0] = BinOp(p[1],"IN",p[3])
            else:
                p[0] = BinOp(p[1],p[2],p[3])
        else:
            p[0] = p[1]

    except:
        pass
        #print "Mult Expression Error"

def p_unary_expression(p):
    r'''
                    unary_expression : sign unary_expression %prec sign
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
                               | INSERT
                               | SUBSTRING
                               | CAP
                               | UPPERCASE
                               | LOWERCASE
                               | ROUND
                               | ABS
                               | TRUNC
                               | RANDOM
                               | POSITION
                               | YEAR
                               | MONTH
                               | VAL
                               | TODATE
                               | FRAC


    '''
    p[0] =  p[1]

def p_primary(p):
    r'''
        primary : variable_access
                | unsigned_constant
                | set_constructor
                | COUNT
                | FLOAT
                | LPAREN index_expression_list RPAREN
                | built_in_functions LPAREN index_expression_list RPAREN
    '''



    if p.slice[1].type == 'built_in_functions':
        p[0] = CallBuildIn(p[1],p[3])
#    elif p.slice[1].type == 'IDENTIFIER':
#        print "CallProc"
#        p[0] = CallProc(p[1], p[3])
    elif p.slice[1].type ==  'LPAREN':
        p[0] = p[2]
    elif p.slice[1].type == 'set_constructor':
       # print "SET: ", p[1]
        p[0] = p[1]
    elif p.slice[1].type == "FLOAT":
        p[0] = Real(p[1])
    elif p.slice[1].type == "COUNT":
        p[0] = Integer(p[1])
    else:
        p[0] = p[1]

def p_unsigned_constant(p):
    r'''
        unsigned_constant : LITERAL
                           | RF
                           | DK
                           | EMPTY
                           | NONRESPONSE
    '''

    if p.slice[1].type == "RF" or p.slice[1].type == "EMPTY" or p.slice[1].type == "DK":
        p[0] = String(p[1])
    else:
        p[0] = String(p[1])


def p_set_constructor(p):
    r'''
        set_constructor : LBRACKET member_designator_list RBRACKET
    '''
    # | LBRACKET RBRACKET

    p[0] = p[2]

def p_member_designator_list(p):
    r'''
        member_designator_list : member_designator_list COMMA member_designator
                                | member_designator

    '''

    if len(p) > 2:
        if isinstance(p[1],list):
            p[0] = p[1] + [p[3]]
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
        p[0] = TypeRange(None, p[1],p[3])
    else:
        p[0] = p[1]

class Identifier(Expr):
    def __init__(self, name, value = None):
        self.type = "IDENTIFIER"
        self.name = name.upper()
        self.value = value


    def __repr__(self):
        return stdout_encode('ID:{0}').format(self.name)


def p_variable_access(p):
    r'''
            variable_access : IDENTIFIER
                            | indexed_variable
                            | field_designator
                            | SYSDATE


    '''
    if p.slice[1].type == "IDENTIFIER":
        p[0] = Identifier(p[1])
    else:
        p[0] = p[1]


class IndexedVariable(Expr):
    def __init__(self, name, index):
        self.type = "INDEXEDVARIABLE"
        self.name = name
        self.index = index


    def __repr__(self):
        return stdout_encode('{0}[{1}]').format(self.name, self.index)


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
        if isinstance(p[1],list):
            p[0] = p[1] + [p[3]]
        else:
            p[0] = [p[1]] + [p[3]]
    else:
        p[0] = p[1]

def p_index_expression(p):
    r'''        index_expression : expression
        '''
    p[0] = p[1]

class FieldDesignator(Expr):

    def __init__(self, value, method):
        self.type = "FIELDDESIGNATOR"
        self.value = value
        self.method = method

    def __repr__(self):
        return "%s.%s " % (self.value, self.method)


def p_field_designator(p):
    r'''
        field_designator : variable_access DECIMAL IDENTIFIER
                         | variable_access DECIMAL ORD
                         | variable_access DECIMAL CARDINAL
                         | variable_access DECIMAL SHOW
                         | variable_access DECIMAL KEEP
                         | variable_access DECIMAL INSERT LPAREN expression RPAREN


    '''

    p[0] = FieldDesignator(p[1], p[3])



class CallProc(Expr):
    def __init__(self, name, params = None):
        self.type = "CALLPROC"
        self.name = name
        self.params = params

    def __repr__(self):

        if self.params == None:
            return stdout_encode('|QUESTION [{0}]|').format(self.name)
        else:
            return stdout_encode('|[Procedure or Block]: {0} Params:{1} |').format(self.name, self.params)

class CallBuildIn(Expr):
    def __init__(self, name, params = None):
        self.type = "CALLBUILD"
        self.name = name
        self.params = params

    def __repr__(self):
        return stdout_encode('| [BUILTIN]{0} Params: {1} |').format(self.name, self.params)


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
        actual_parameter :  expression
    '''

    p[0] = p[1]

def p_statement_list(p):
    r'''statement_list :  statement_list statement
                        | statement
      '''
    global levelScope

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


class ModuleCall(Expr):

    def __init__(self, name, index, params = None):
        self.type = "CALLMODULE"
        self.name = name
        self.index = index
        self.params = params

    def __repr__(self):
        return stdout_encode('{0}[{1}]()').format(self.name, self.index, self.params)


def p_procedure_statement(p):
    r'''
            procedure_statement : variable_access LPAREN index_expression_list RPAREN
                                | variable_access


    '''

    if len(p) > 2:
        p[0] = CallProc(p[1],p[2])
    else:
        p[0] = CallProc(p[1])




def p_statement(p):
    ''' statement :   assignment_statement
                    | procedure_statement
                    | repeat_statement
                    | if_statement
                    | while_statement
                    | for_statement
                    | check_statement
                    | signal_statement
                    | not_statement
                    | layout_statement

    '''
    p[0] =  p[1]

                   #| module_statement

def p_repeat_statement(p):
    '''
        repeat_statement : REPEAT statement_list UNTIL boolean_expression
    '''


def p_while_statement(p):
    '''
        while_statement : WHILE boolean_expression DO statement_list ENDWHILE
    '''


class ForStatement(Expr):

    def __init__(self, control, initial, direction, final, statements):
        global levelScope
        self.type = "FOR"
        self.control = control
        self.initial = initial
        self.direction = direction
        self.final = final
        self.value = statements


    def __repr__(self):
        return stdout_encode('FOR: {0} {1} {2} {3} DO {4}').format(self.control, self.initial, self.direction, self.final, self.value)


def p_for_statement(p):
    '''
        for_statement : FOR control_variable ASSIGN initial_value direction final_value DO statement_list ENDDO
    '''

    p[0] = ForStatement(p[2],p[4],p[5],p[6],p[8])


class InvolvingStatement(Expr):
    def __init__(self,name, value):
        self.type = "INVOLVING"
        self.name = name
        self.value = value

    def __repr__(self):

        if self.name != None and self.value != None:
            return "INVOLVING STATEMENT %s %s" % (self.name, self.value)
        else:
            return "Missing INVOLVING Statement"

def p_involving_vars(p):
    '''
        involving_vars : INVOLVING LPAREN statement_list RPAREN
                       | INVOLVING params
                       | INVOLVING TAG
                       |
    '''

    if len(p) > 2:
        if p.slice[2].type == 'LPAREN':
            p[0] = InvolvingStatement("statement", p[3])
        elif p.slice[2].type == "params":
            p[0] = InvolvingStatement("params", p[2])
        else:
            p[0] = InvolvingStatement("tag", p[2])



class CheckStatement(Expr):
    def __init__(self,left, right = None, literal = None):
        self.type = "CHECK"
        self.left = left
        self.right = right
        self.literal = literal


    def __repr__(self):
        return "CHECK STATEMENT %s %s %s" % (self.left, self.right, self.literal)



def p_check_statement(p):
    '''
        check_statement : CHECK boolean_expression involving_vars LITERAL
    '''

    p[0] = CheckStatement(p[2],p[3],p[4])




class LayoutStatement(Expr):
    def __init__(self,left, right = None, literal = None):
        self.type = "LAYOUT"
        self.left = left
        self.right = right
        self.literal = literal


    def __repr__(self):
            return "LAYOUT STATEMENT %s %s %s" % (self.left, self.right, self.literal)


def p_layout_statement(p):
    r'''
        layout_statement : LAYOUT FROM IDENTIFIER TO IDENTIFIER FIELDPANE IDENTIFIER
    '''

    p[0] = LayoutStatement(p[3],p[5], p[7])

class SignalStatement(Expr):
    def __init__(self,left, right = None, literal = None):
        self.type = "SIGNAL"
        self.left = left
        self.right = right
        self.literal = literal


    def __repr__(self):
        return "SIGNAL STATEMENT %s %s %s" % (self.left, self.right, self.literal)


def p_signal_statement(p):
    '''
        signal_statement : SIGNAL boolean_expression involving_vars LITERAL
    '''

    p[0] = SignalStatement(p[2], p[3],p[4])



class NotBinStatement(Expr):
    def __init__(self,left, right = None, literal = None):
        self.type = "NOTBIN"
        self.left = left
        self.right = right
        self.literal = literal


    def __repr__(self):
        return "NOTBINSTATEMENT %s %s %s" % (self.left, self.right, self.literal)



def p_not_statement(p):
    r'''
            not_statement : NOT boolean_expression statement
                          | NOT boolean_expression involving_vars LITERAL

    '''


    if len(p) > 4:
        p[0] = NotBinStatement(p[2], p[3],p[4])
    else:
        p[0] = NotBinStatement(p[2], p[3])



def p_if_statement(p):
    r'''
        if_statement :    IF boolean_expression THEN statement_list elif_statements else_statement_list ENDIF
                    |     IF boolean_expression THEN  elif_statements else_statement_list ENDIF

'''

    if len(p.slice) > 6:
        p[0] = IfElseStatement(p[2],p[4], p[5],p[6])
    else:
        p[0] = IfElseStatement(p[2],None, p[4],p[5])




def p_elif_statements(p):
        r''' elif_statements :
                                |  ELSEIF boolean_expression THEN statement_list elif_statements '''


        if len(p.slice) > 1:
            p[0] = ElseIfStatement(p[2],p[4],p[5])


def p_else_statement_list(p):
    r'''
        else_statement_list :
                            | ELSE statement_list
                            | ELSE
            '''

    if len(p.slice) > 2 and p[2] != None:
        p[0] = ElseStatement(p[2])


def p_assignment_statement(p):
    r'''
        assignment_statement : variable_access ASSIGN expression
    '''
    global Fills
    p[0] = BinOp(p[1], ':=', p[3])

    if p[1].type == "IDENTIFIER" :
        if not Fills.has_key(p[1].name):
            Fills[p[1].name] = list()

        if hasattr(p[3], "type") and p[3].type == "STRING" and p[3].value != '':
            Fills[p[1].name] = list( Fills[p[1].name])
            Fills[p[1].name].append(p[3].value)
        elif hasattr(p[3], "type") and p[3].type != "STRING" and p[3]:
            if p[3].type == "IDENTIFIER" and re.match("PIBASICFILL",p[3].name):
                pass

            else:
                Fills[p[1].name] = list(Fills[p[1].name])
                Fills[p[1].name].append(p[3])


def p_tempty(p):
    r'tempty :'
    pass


def p_error(p):
    if hasattr(p, "value"):
        print "Syntax error at '%s %s %s %d %d'" % (p, p.value, p.type, p.lineno, p.lexpos)
    else:
        print p

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



#pprint.pprint(result,width=120)
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



def getSymbol(symb):
    global symbolTable
    #pprint.pprint(vars)
    print "Looking for symbol", symb

    if isinstance(symb, list):
        return None

    for i in range(len(symbolTable)-1,-1,-1):
        if symbolTable[i].has_key(symb) :
                return symbolTable[i][symb]
        else:
            for k in symbolTable[i].iterkeys():

                if isinstance(symbolTable[i][k], dict) and symbolTable[i][k].has_key(symb):
                    return symbolTable[i][k][symb]

    return None


for i in range(1,84):
    arg = "C%02d" % i
    vars[arg] = { 'type' : "INTEGER", 'value' : i}

def evaluateSubtree(node, debug = True):
    global symbolTable, levelScope, vars

    retVal = None

    if isinstance(node, str):
        print node.encode('ascii', 'ignore')

    if isinstance(node,list):
        for i in node:

            if hasattr(i, "value") and i.type != "FOR":
                if isinstance(i.value,list):
                    #print "list[", i, "]->", i.value, i.type
                    evaluateSubtree(i.value)
                else:
                     #print "HAS[",i,"]->", i, i.type
                     evaluateSubtree(i)
            else:
                #print "NO[",i,"]-->", i,  i.type
                retVal = evaluateSubtree(i)

    elif not isinstance(node, list) and node.type == "STRING":
        return { 'type' : node.type, 'value' : node.value }

    elif not isinstance(node, list) and node.type == "INTEGER":

        return { 'type' : node.type, 'value' : node.value }

    elif not isinstance(node, list) and node.type == "REAL":

        return { 'type' : node.type, 'value' : node.value }

    elif not isinstance(node, list) and node.type == "IDENTIFIER":

        return node

    elif not isinstance(node, list) and node.type == "CALLBUILD":

        return { 'name' :   node.name , 'value' : '' , 'type' : 'STRING' }

    elif not isinstance(node, list) and node.type == "INDEXEDVARIABLE":

        return {  'name' :   node.name, 'value' : '' , 'type' : 'STRING' }

    #if not isinstance(node, list):
    #    print "PASSED: " , node, " TYPE: ", node.type

    if not isinstance(node, list) and node.type == "FOR":
           print "FOR LOOP"
           control = evaluateSubtree(node.control)

           direction = 1

           if node.direction == "DOWNTO":
               direction = -1

           initial = evaluateSubtree(node.initial)
           final = evaluateSubtree(node.final)

           for i in range(initial['value'],final['value'], direction):
               vars[control['name']] = { 'type' : 'INTEGER', 'name' : control['name'], 'value' : i }
               print "LOOP, ", i, node.value, node.control
               pprint.pprint(node.value)
               evaluateSubtree(node.value)



    if not isinstance(node, list) and node.type == "IF":
        print "IF", node.left, node.right
        nleft = evaluateSubtree(node.left)

        print nleft['value']

        if nleft['value'] == 1:
            nright = evaluateSubtree(node.right)

    if not isinstance(node, list) and node.type == "IFELSE":
        print "IFELSE"
        nleft = evaluateSubtree(node.first)

        if nleft['value']:
            nright = evaluateSubtree(node.second)
        elif node.third != None and evaluateSubtree(node.third):
            nright = evaluateSubtree(node.third)
        else:
            if node.fourth:
                nright = evaluateSubtree(node.fourth)

    if not isinstance(node, list) and node.type == "ELSEIF":
        print "ELSEIF"

        nleft = evaluateSubtree(node.first)

        if nleft['value']:
            nright = evaluateSubtree(node.second)
        else:
            nright = evaluateSubtree(node.third)


    if not isinstance(node, list) and node.type == "ELSE":
         evaluateSubtree(node.first)

    if not isinstance(node, list) and node.type == "BINOP":

        nleft = evaluateSubtree(node.left)
        nright = evaluateSubtree(node.right)


        left = nleft
        right = nright

        if node.op == ":=":

            print "TRYING ASSIGN ON ", left.name, right

            if (getSymbol(left.name)):
                 print "Symbol: ", getSymbol(left.name)

                 if getSymbol(left.name) == "PIUNFTEXT":

                     if not Fills.has_key('UNFTEXT'):
                        Fills["UNFTEXT"] = []

                     Fills['UNFTEXT'].append(right.value)



                 s = getSymbol(left.name)

                 if hasattr(right, "type"):
                     if right.type == "STRING": # we are assigning a literal
                         vars[left.name] = { 'type' : "STRING", 'value' : right.value }

                     if right.type == "INTEGER": # we are assigning an integer
                         vars[left.name] = { 'type' : "INTEGER", 'value' : right.value }

                 else:
                     # this is a variable value, assign
                    if getSymbol(left.name) == "PIUNFTEXT":

                        if not Fills.has_key('UNFTEXT'):
                            Fills["UNFTEXT"] = []

                        Fills['UNFTEXT'].append(right['value'])


                    vars[left.name] = right


        elif node.op == "=":

            print left, "---COMPARE---", right


            typeOfSymbol = None


            if left.type == "IDENTIFIER" and not vars.has_key(left.name):


                    typeOfSymbol = getSymbol(left.name).value.value
                    typeDefinition = getSymbol(typeOfSymbol)

                    if not typeOfSymbol == "STRING" and not typeOfSymbol == "INTEGER":

                        if typeDefinition.value.value.type == "ENUMERATED":
                            for tz in typeDefinition.value.value.value:
                                vars[tz.name] = { 'name' : tz.name , 'value' : tz.value , 'type' : typeOfSymbol}


            if left.type == "FIELDDESIGNATOR":
                variableName = left.value.name

                functionName = left.method

                if functionName == "ord":
                    setattr(left,"name", variableName)



            if not hasattr(right,'type'):

                if right['type'] == "INTEGER":

                    if vars[left.name]['value'] == right['value']:
                        return { "type" : 'INTEGER' , 'value' : 1 }
                    else:
                        return { "type" : 'INTEGER' , 'value' : 0 }




            if right.type == "IDENTIFIER" and not vars.has_key(right.name):

                # we need to figure out this comparison then
                # get the type of the left symbol
                typeDefinition = getSymbol(vars[left.name]['type'])

                if typeDefinition.value.value.type == "ENUMERATED":
                    for tz in typeDefinition.value.value.value:
                        vars[tz.name] = { 'name' : tz.name , 'value' : tz.value , 'type' : vars[left.name]['type']}



            if vars[left.name]['value'] == vars[right.name]['value']:
                return { "type" : 'INTEGER' , 'value' : 1 }
            else:
                return { "type" : 'INTEGER' , 'value' : 0 }


        elif node.op == "*":
            print left
            print right
            if left['type'] == "INTEGER" and right['type'] == "INTEGER":
                return  { "type" : 'INTEGER' , 'value' : int(left['value']) * int(right['value']) }

            if left['type'] == "REAL" and right['type'] == "REAL":
                return { 'type' : 'REAL', 'value' : float(left['value']) * float(right['value']) }


        elif node.op == "+":
            print left, " add ", right

            if isinstance(left, dict) and isinstance(right, dict):


                if left['type'] == "INTEGER" and right['type'] == "INTEGER":
                    return { "type" : 'INTEGER' , 'value' : int(left['value']) + int(right['value']) }
                else:
                    return { "type" : "STRING" , 'value' : unicode(left['value']) + " " + unicode(right['value']) }

            elif isinstance(left, dict) and not isinstance(right,dict):

                if left['type'] == "STRING":

                    if (right.type == "IDENTIFIER" and re.match("[A-Za-z_0-9]+\[?[A-Z0-9_]*\]?(?:\.[A-Za-z_0-9]+\[?[A-Za-z0-9_]*\]+)?(?:\.?[A-Za-z0-9_]+)?",right.name) and Fills.has_key(right.name)):
                        return { "type" : "STRING", 'value' : unicode(left['value'] + " " + "^" + right.name ) }
                    elif (right.type == "FIELDDESIGNATOR" and Fills.has_key(str(right.method).upper())):
                        return { "type" : "STRING", 'value' : unicode(left['value'] + " " + "^" + str(right.method).upper() ) }
                    elif re.search("[X|Z][0-9]+_?[0-9A-Z_]*", right.name):
                         res = re.findall("[X|Z][0-9]+_?[0-9A-Z_]*", right.name)
                         print res
                         return { "type" : "STRING", 'value' : unicode(left['value']) + " " + Fills[res[0]][0] }
                    else:

                        for qk, qq in Questions.iteritems():
                            if right.name == qq.name.name:
                                return { "type" : "STRING", 'value' : unicode(left['value'] + " " + qq.description) }

                        return { "type" : "STRING", 'value' : unicode(left['value'] + " " + vars[right.name]['value']) }
            elif isinstance(right, dict) and not isinstance(left,dict):

                if right['type'] == "STRING":

                    if left.type == "IDENTIFIER" and re.match("[A-Za-z_0-9]+\[?[A-Z0-9_]*\]?(?:\.[A-Za-z_0-9]+\[?[A-Za-z0-9_]*\]+)?(?:\.?[A-Za-z0-9_]+)?",left.name) and Fills.has_key(left.name):

                        return { "type" : "STRING", 'value' : unicode("^" + left.name + " " + right['value']) }

                    elif (left.type == "FIELDDESIGNATOR" and Fills.has_key(str(left.method).upper())):
                        return { "type" : "STRING", 'value' : unicode("^" + str(left.method) + " " + right['value']) }

                    else:
                        return { "type" : "STRING", 'value' : unicode(vars[left.name]['value'] + " " + right['value'] ) }
            else:
                if left.type == "IDENTIFIER" and right.type == "IDENTIFIER":
                    if re.match("[A-Za-z_0-9]+\[?[A-Z0-9_]*\]?(?:\.[A-Za-z_0-9]+\[?[A-Za-z0-9_]*\]+)?(?:\.?[A-Za-z0-9_]+)?",left.name) and Fills.has_key(left.name) and re.match("[A-Za-z_0-9]+\[?[A-Z0-9_]*\]?(?:\.[A-Za-z_0-9]+\[?[A-Za-z0-9_]*\]+)?(?:\.?[A-Za-z0-9_]+)?",right.name) and Fills.has_key(right.name):
                          return { "type" : "STRING", 'value' : unicode("^" + left.name + " " + "^" + right.name) }


        elif node.op == "-":
            if left['type'] == "INTEGER" and right['type'] == "INTEGER":
               return  { "type" : 'INTEGER' , 'value' : int(left['value']) - int(right['value']) }
        elif node.op == "/":
             if left['type'] == "INTEGER" and right['type'] == "INTEGER":
               return  { "type" : 'INTEGER' , 'value' : int(left['value']) / int(right['value']) }
        elif node.op == "MOD":
            return { "type" : 'INTEGER' , 'value' : int(left['value']) % int(right['value']) }
        elif node.op == ">":
            if left['value'] > right['value']:
                return { "type" : 'INTEGER' , 'value' : 1 }
            else:
                return { "type" : 'INTEGER' , 'value' : 0 }
        elif node.op == ">=":
            if left['value'] >= right['value']:
                return { "type" : 'INTEGER' , 'value' : 1 }
            else:
                return { "type" : 'INTEGER' , 'value' : 0 }
        elif node.op == "<":
            if left['value'] < right['value']:
                return { "type" : 'INTEGER' , 'value' : 1 }
            else:
                return { "type" : 'INTEGER' , 'value' : 0 }
        elif node.op == "<=":
            if left['value'] <= right['value']:
                return { "type" : 'INTEGER' , 'value' : 1 }
            else:
                return { "type" : 'INTEGER' , 'value' : 0 }

        elif node.op == "<>":

            print left, "---DIFF---", right


            typeOfSymbol = None

            if left.type == "IDENTIFIER" and not vars.has_key(left.name):


                    typeOfSymbol = getSymbol(left.name).value.value
                    typeDefinition = getSymbol(typeOfSymbol)

                    if not typeOfSymbol == "STRING" and not typeOfSymbol == "INTEGER":

                        if typeDefinition.value.value.type == "ENUMERATED":
                            for tz in typeDefinition.value.value.value:
                                vars[tz.name] = { 'name' : tz.name , 'value' : tz.value , 'type' : typeOfSymbol}


            if right.type == "IDENTIFIER" and not vars.has_key(right.name):

                # we need to figure out this comparison then
                # get the type of the left symbol
                typeDefinition = getSymbol(vars[left.name]['type'])

                if typeDefinition.value.value.type == "ENUMERATED":
                    for tz in typeDefinition.value.value.value:
                        vars[tz.name] = { 'name' : tz.name , 'value' : tz.value , 'type' : vars[left.name]['type']}



            if vars[left.name]['value'] == vars[right.name]['value']:
                return { "type" : 'INTEGER' , 'value' : 1 }
            else:
                return { "type" : 'INTEGER' , 'value' : 0 }


        elif node.op == "AND":
            if left['value'] and right['value']:
                return { "type" : 'INTEGER' , 'value' : 1 }
            else:
                return { "type" : 'INTEGER' , 'value' : 0 }

        elif node.op == "OR":
            if left['value'] or right['value']:
                return { "type" : 'INTEGER' , 'value' : 1 }
            else:
                return { "type" : 'INTEGER' , 'value' : 0 }

        elif node.op == "IN":
            #if right['value'].has_key(left['value']):
                return { "type" : 'INTEGER' , 'value' : 1 }
            #else:
            #    return { "type" : 'INTEGER' , 'value' : 0 }

    return node



# push the parameters to the vars
#
#procedure BC_Txt_Bronchitis
#  parameters
#  import
#    piC185_DifferentReporter :TYESNO
#    piRVarsZ104_Lung_V: TYesNo
#    piRVarsZ076_ReIwR_V: TEverInterviewed
#    piRespondents1X058AFName: String

#for kk, z in Fills.iteritems():
#    if ()
#    vars[kk] = { 'type' : "STRING" , "value" : z[0] }

if Fills.has_key("Seq8P"):
    Fills["P173_"] = Fills["Seq8P"][1]
    Fills["P028_"] =  Fills["Seq8P_22"][1]

if Fills.has_key("FLW043_1"):
    Fills["PIW049"]  = Fills["FLW043_1"]
    Fills["FLTHISTHESE"] = Fills["PETHISTHESE"]
    Fills["FLACCOUNTS"]  = Fills["PEACCOUNTS"]
    Fills["FLW002"] = Fills["FLW002A"]

Fills["MOSTRECENTJOB.L008_"] = ["[COMPANY NAME]"]
Fills["MOSTRECENTJOB.L027_"] = ["[INCLUDED IN PENSION PLAN"]
Fills["MOSTRECENTJOB.L009_"] = ["[START WRK-YR]"]
Fills["MOSTRECENTJOB.L019_LftRecJobCalcYr".upper()] = ["[YR R LAST WORKED AT PREV JOB- CALC]"]
Fills["MOSTRECENTJOB.L012_YrMostRecJob".upper()] = ["[YR STARTED WORKING AT PREV JOB- CALC]"]
Fills["MOSTRECENTJOB.L016_YrLeftRecJob".upper()] = ["[LEFT EMPLOYER-YR]"]
Fills["PTMAINPLAN"] = [u"MAIN PRIVATE PLAN"]
if Fills.has_key("PEFLL013"):
    Fills["FLL013"] = Fills["PEFLL013"]
    Fills["FLLTHATEMP"] = Fills["peFLthatEmp".upper()]
    Fills["FLLLEAVETHATEMP"] = Fills["peFLLeavethatEmp".upper()]
    Fills["FLL027"] = Fills["peFLL027".upper()]
    Fills["FLL027ext".upper()] = Fills["peFLL027ext".upper()]
    Fills["FLL005"] = Fills["peFLL005".upper()]
    Fills["FLL007"] = Fills["peFLL007".upper()]
    Fills["FLL025"] = Fills["peFLL025".upper()]
    Fills["FLLINDEX"] = Fills["peFLLIndex".upper()]
    Fills["FLL087"] = Fills["peFLL087".upper()]

    Fills["FLLTHEFIRSTSUCH".upper()] = Fills["peFLLtheFirstSuch".upper()]
    Fills["peL094_EARLIESTJOB".upper()] = [u"EARLIEST JOB YR-REPORTED IN SECS J-K-L"]
    Fills["PIL094_EARLIESTJOB"] = Fills["peL094_EARLIESTJOB".upper()]

if Fills.has_key("FLN013"):
    Fills["FLN056"] = Fills["FLN013"]
    Fills["FLN056_2"] = Fills["FLN013_2"]
    Fills["PISECACONTINUINTERVIEWA019_RAGE"] = [u"[R's AGE]"]

Fills["PIPERSONNAME"] = [u"HOUSEHOLD MEMBER NAME"]

Fills["PIN070_DENCOVNAME"] = ["[NAME DENTAL COVERAGE PLAN]"]
Fills["PIINITA061TLCY_A"] = ["[LAST CALENDAR YEAR]"]
Fills["PISTRINITA061TLCY_A"] = ["[LAST CALENDAR YEAR]"]
Fills["X038MO"] =  ["[PREV WAVE IW MONTH OF FINANCIAL R]"]
Fills["Z092"] = [ "PREV WAVE IW MONTH" ]
Fills["PIINITA062T2YRSAGO_A"] = [ "2 YEARS AGO"]
vars["FLHHX038_FINRIWMO_V"] =   { 'type' : "STRING" , "value" :    "[PREV WAVE IW MONTH OF FINANCIAL R]" }
vars["FLPWIWDATE"] = { 'type' : "STRING" , "value" : "[PREV WAVE FIRST R IW MONTH],[PREV WAVE FIRST R IW YEAR]" }
vars["FLPWIWMO"] = {'type' : "STRING" , "value" : "[PREV WAVE FIRST R IW MONTH" }
vars["PWFAMIWMO"] = vars["FLPWIWMO"]
vars["PIPRELOAD_R1X058AFNAME".upper()] = { 'type' : "STRING", 'value' : "R's FIRST NAME" }
vars["piPreloadR1X058AFName".upper()] =  { 'type' : "STRING", 'value' : "R's FIRST NAME" }
#vars["piRespondents1X058AFName".upper()] = { 'type' : "STRING", 'value' : "[Respondent]", "name"  : "piRespondents1X058AFName".upper() }
vars["PIR2X058AFNAME"] =  { 'type' : "STRING", 'value' : "FIRST NAME OF PERSON 2" }
vars["PIR1X058AFNAME"] =  { 'type' : "STRING", 'value' : "FIRST NAME OF PERSON 1" }
vars["piRVarsZ076_ReIwR_V".upper()] = { 'type' : "TEVERINTERVIEWED", 'value' : 1, "name"  : "piRVarsZ076_ReIwR_V".upper() }
vars["piRVarsZ104_Lung_V".upper()] = { 'type' : "TYESNO", 'value' : 1, "name"  : "piRVarsZ104_Lung_V".upper() }
vars["piC185_DifferentReporter".upper()] = { 'type' : "TYESNO", 'value' : 1, "name"  : "piC185_DifferentReporter".upper() }
vars["piRespondents1X060ASex".upper()] = { 'type' : 'TSEX', 'value' : 1 , 'name' : "piRespondents1X060ASex".upper() }
vars['AIMPORT'] = { 'name' : 'AIMPORT', 'type' : 'TMONTH', 'value' : 1}
vars["sZ092_IwMo_V".upper()] = { 'type' : "STRING", 'value' : "[" + getSymbol('Z092_IwMo_V'.upper()).description + "]" }
vars["sZ093_Iwyr_V".upper()] = { 'type' : "STRING", 'value' : "[" + getSymbol('Z093_IwYR_V'.upper()).description + "]" }
vars["Z092IwMoV".upper()] = { 'type' : "STRING", 'value' : "[" + "prev interview month" + "]" }
vars["Z093IwYrV".upper()] = { 'type' : "STRING", 'value' : "[" + "prev interview year" + "]" }
vars["STRZ092".upper()] = { 'type' : "STRING", 'value' : "[" + "PREV WAVE IW [MONTH, ]YEAR" + "]" }
vars["FILLHAS".upper()] = { 'type' : "STRING", 'value' : "DISPUTES PREVIOUS WAVE RECORD, BUT NOW HAS CONDITION"}
vars["FILLNOTHAVE".upper()] = { 'type' : "STRING", 'value' : "DISPUTES PREVIOUS WAVE RECORD, DOES NOT HAVE CONDITION"}
vars["FILLNO".upper()] = { 'type' : "STRING", 'value' : "NO"}
vars["PIBA_RESPONDENTS1X058AFNAME"] = { 'type' : "STRING" , 'value' : "[R's FIRST NAME]"}
vars["PIRESPONDENTS1X058AFNAME"] = { 'type' : "STRING" , 'value' : "[R's FIRST NAME]"}
vars['Z091'] = { 'type' : "STRING", 'value' : "PREV WAVE EMPLOYER NAME"}
vars['Z092'] = { 'type' : "STRING", 'value' : "PREV WAVE IW MONTH" }
vars['PIRVARSZ092_IWMO_V'.upper()] = { 'type' : "STRING", 'value' : "[" + "prev interview month" + "]" }
vars['RVARS.Z092_IWMO_V'.upper()] = { 'type' : "STRING", 'value' : "[" + "Number of times injured].[" +   getSymbol('Z092_IwMo_V'.upper()).description +  "]"}
vars['RVARS.Z093_IWYR_V'.upper()] = { 'type' : "STRING", 'value' : "[" + "Number of times injured].[" +   getSymbol('Z093_Iwyr_V'.upper()).description +  "]"}
vars['M009_'] = { 'type' : "STRING", 'value' : '[WHEN IMPAIRMENT 1ST BOTHER - YR]'}
vars['M014_'] = { 'type' : "STRING", 'value' : '[IMPAIRMNT BEGIN  INTERFER WORK-YR]'}
vars['M016_'] = { 'type' : "STRING", 'value' : '[HEALTH PROB PREVENT WRK-YR]'}
vars['AARRAYSTRING'] = { 'type' : "STRING", 'value' : '[PERSON LIST]'}
vars['PIPRELOAD_HHX025_MAINCTY'] = { 'type' : "STRING", 'value' : '[MAIN RES ADDRESS CITY]'}

Fills["PLANIDENTIFIERS[1]"] = [u"[First Pension]"  ]
Fills["PLANIDENTIFIERS[2]"] = [u"[Second Pension]" ]
Fills["PLANIDENTIFIERS[3]"] = [u"[Third Pension]" ]
Fills["PLANIDENTIFIERS[4]"] = [u"[Fourth Pension]"]
Fills["PLANIDENTIFIERSMERGED[1]"] = [u"[pension plans or pension accounts at a job where you worked]"]
Fills["PLANIDENTIFIERSMERGED[2]"] = [u"[pension plans or pension accounts at a job where you worked]"]
Fills["PLANIDENTIFIERSMERGED[3]"] =[u"[pension plans or pension accounts at a job where you worked]"]
Fills["PLANIDENTIFIERSMERGED[4]"] = [u"[pension plans or pension accounts at a job where you worked]"]

Fills["X025_MainCty"] = ['[MAIN RES ADDRESS CITY]']

for i in range(1,30,1):
    vars['FL_MONTH[' + str(i) + "]"] = { 'type' : "STRING", 'value' : '[MONTH][' + str(i) + "]"}

vars['VAL6ST'] = {  'name' :"VAL6ST", 'type' : "STRING", 'value' : u"VAL6ST" }
vars['VAL5ST'] = {  'name' :"VAL5ST", 'type' : "STRING", 'value' : u"VAL5ST" }
vars['VAL4ST'] = {  'name' :"VAL4ST", 'type' : "STRING", 'value' : u"VAL4ST" }
vars['VAL3ST'] = {  'name' :"VAL3ST", 'type' : "STRING", 'value' : u"VAL3ST" }
vars['VAL2ST'] = {  'name' :"VAL2ST", 'type' : "STRING", 'value' : u"VAL2ST" }
vars['VAL1ST'] = {  'name' :"VAL1ST", 'type' : "STRING", 'value' : u"VAL1ST" }
vars['STRMONTH'] = { 'name' : 'STRMONTH' , 'type' : "STRING", 'value' : "MONTH" }
vars['MONTH'] = { 'name' : 'MONTH' , 'type' : "STRING", 'value' : "MONTH" }
vars['FLMONTH23'] = { 'name' : 'MONTH' , 'type' : "STRING", 'value' : "MONTH" }
vars['FLMONTH63'] = { 'name' : 'MONTH' , 'type' : "STRING", 'value' : "MONTH" }
vars['FLMONTH248'] = { 'name' : 'MONTH' , 'type' : "STRING", 'value' : "MONTH" }
vars['FLMONTH17'] = { 'name' : 'MONTH' , 'type' : "STRING", 'value' : "MONTH" }
vars['FLMONTH92'] = { 'name' : 'MONTH' , 'type' : "STRING", 'value' : "MONTH" }
vars['FLMONTH2'] = { 'name' : 'MONTH' , 'type' : "STRING", 'value' : "MONTH" }

if Fills.has_key("FLJ268"):
    Fills["FLLAST2YRS"] = Fills["FLJ268"]

Fills['VAL1ST'] = [u"VAL1ST"]
Fills['VAL2ST'] = [u"VAL2ST"]
Fills['VAL3ST'] = [u"VAL3ST"]
Fills['VAL4ST'] = [u"VAL4ST"]
Fills['VAL5ST'] = [u"VAL5ST"]
Fills['VAL6ST'] = [u"VAL6ST"]
Fills["FLD167"] = "[VOCABULARY WORD]"
Fills["FLD163"] = "[VOCABULARY WORD]"
Fills["FLD164"] = "[VOCABULARY WORD]"
Fills["FLD165"] = "[VOCABULARY WORD]"
Fills["FLD166"] = "[VOCABULARY WORD]"
Fills["FLD161"] = "[VOCABULARY WORD]"
Fills["FLD162"] = "[VOCABULARY WORD]"
Fills["FLD169"] = "[VOCABULARY WORD]"
Fills["FLE075_"] = ["(or any deed to a house)"]


if Fills.has_key("FLF107"):
    Fills["FLF104"] = ["(deceased) parents", "parents", "(deceased) mother", "(deceased) father", "father", "mother", "(deceased) mother (and/or her husband)", "(deceased) father (and/or his wife)"]
    Fills["FLF114"] = Fills["FLF107"]
    Fills["FLF154"] = Fills["FLF107"]
    Fills["FLF142"] = Fills["FLF107"]
    Fills["PIFLSINCEPWMY"] = Fills["FLF107"]
    Fills["PIRELATER"] = ["him/her/(he/she)"]
    Fills["FLF135"] = Fills["FLF122"]
    Fills["FLF137"] = Fills["FLF117"]
    Fills["PIFLSINCEPWMY_2"] = Fills["FLSincePWMY_2".upper()]
    Fills["FLX061"] = ["SIBLING RELATIONSHIP TO R"]
    Fills["PINAME"] = ["FIRST NAME OF INDIVIDUAL"]
    Fills["FLF154"] = Fills["FLF122"]
    Fills["FLF012"] = Fills["FLF002"]
    Fills["FLF013"] = Fills["FLF003"]


Fills['VAL1'] = ["VAL1"]
Fills['VAL2'] = ["VAL2"]
Fills['VAL3'] = ["VAL3"]
Fills['VAL4'] = ["VAL4"]
Fills['VAL5'] = ["VAL5"]
Fills["PIRTAB1X081_RLOGID"] = "[2004 MAILED DOCUMENTS RESP LINK ID]"
Fills['PIPRELOAD_HHX029_2NDRESCTY'] = "[2ND ADDRESS CITY]"
Fills["UNFTEXTMONTH"] = ['per month', 'per year', 'per hour']
Fills["MONTH_STR"] = ['[START MONTH]']
Fills["FLB040MO"] = ["[MONTH]"]
Fills["X004TMOBORN"] = ['[R MONTH BORN]']
Fills["AFILLMONTH"] = ['[CURRENT MONTH']
if Fills.has_key('A067'):
    Fills["A067_CtyNHome".upper()] = list(Fills["A067"])
    Fills["A031_MOSTOPLIVTOGTHR"] = list(Fills["A031"])
    Fills["A032_YrStopLivTogthr".upper()] = list(Fills["A032"])
    Fills["A033_SpInNHome".upper()] = list(Fills["A033"])
    Fills["A034_RMarSep".upper()] = list(Fills["A034"])
    Fills["A035_SepWPartnr".upper()] = list(Fills["A035"])
    Fills["A037_AYrStartCouple".upper()] = list(Fills["A037"])
    Fills["A024_MOSTOPCOHABIT".upper()] = list(Fills["A024"])
    Fills["A075TCURRESCTY_A"]  = list(Fills["A075T"])
    Fills["A080TOTHRESCTY_A"] = list(Fills["A080T"])
    Fills["A170_A081TOthResST_A".upper()] = list(Fills["A081T"])
    Fills["A081TOTHRESST_A".upper()]= list(Fills["A081T"])
    Fills["R_ROSTER.RGRID"] = "[ROSTER NAME]"
    Fills["A082SOTHRESST_S"] = list(Fills["A082S"])
    Fills["X005TDABORN"] = list(Fills["X005ADABORN"])
    Fills["STARTINTERVIEW.A007TRALIVE_A"] = "[Is R living?]"
    Fills["StartInterview.A009_SelfPrxy".upper()] = "[Type of IW]"
    Fills["Relations.A038TCouplenss_A".upper()] = "[R's coupleness status]"
    Fills["Relations.A028_RInNHome".upper()] = "[ R lives in a nursing home]"
    vars['A080TOTHRESCTY_A'.upper()] = { "type" : "STRING", "value" : list(Fills["A080T"])[0]}
    vars['A081TOthResST_A'.upper()] = { "type" : "STRING", "value" : list(Fills["A081T"])[0]}
    vars['A082SOthResST_S'.upper()]  =  { "type" : "STRING", "value" : list(Fills["A082S"])[0]}
    vars['A084_NumRes'.upper()] =  { "type" : "STRING", "value" : list(Fills["A084"])[0]}


vars["PIPRELOAD_HHX029_2NDRESCTY"] = { "type" : "STRING", "value" : "[2ND ADDRESS CITY]" }
vars["piPreload_HHX025_MainCty".upper()] = { "type" : "STRING", "value" : "[MAIN CITY]" }
vars['A075TCURRESCTY_A'.upper()] = { "type" : "STRING", "value" : "[CURRENT CITY ]" }

vars["STRSTATE"] = { 'name' : "STRSTATE", 'type' : "STRING", 'value' : "[STATE]" }
vars['VAL1'] = {  'name' :"VAL1", 'type' : "STRING", 'value' : "VAL5" }
vars['VAL2'] = {  'name' :"VAL2", 'type' : "STRING", 'value' : "VAL4" }
vars['VAL3'] = {  'name' :"VAL3", 'type' : "STRING", 'value' : "VAL3" }
vars['VAL4'] = {  'name' :"VAL4", 'type' : "STRING", 'value' : "VAL2" }
vars['VAL5'] = {  'name' :"VAL5", 'type' : "STRING", 'value' : "VAL1" }

Fills["SecI.V932_BookletID".upper()] = [u"BOOKLET ID"]
Fills["V939_BookletBarCodeID".upper()] = [u"BARCODE ID"]

symbolTable[0]['TIMETYPE'] = TypeC("TIMETYPE", TypeDenoter(String("TIME")))
symbolTable[0]['DATETYPE'] = TypeC("DATETYPE", TypeDenoter(String("DATE")))
symbolTable[0]['INTEGER'] = getSymbol('TINTEGER')
symbolTable[0]['REAL'] = getSymbol('TINTEGER')

symbolTable[0]['TYESNO'] = TypeC("TYESNO", TypeDenoter(Enumerated([TypeEnumeratedItem("YES", 1, "Yes"), TypeEnumeratedItem("NO", 5, "No")])))
evaluateSubtree(procedureTable['BASIS_FILLS'])
for i in range(1,19):
    #push param
    vars["PIUNFINDEX"] = { 'type' : "INTEGER", 'value' : i}
    #call fills
    evaluateSubtree(procedureTable['TXTUNFTEXT'])



pprint.pprint(evaluateSubtree(procedureTable['TXTTMONTH']))
#evaluateSubtree(procedureTable['TXT_C226'])
#evaluateSubtree(procedureTable['BC_TXT_BRONCHITIS'])
#print pprint.pprint(symbolTable)
#pprint.pprint(result[1].value)
#pprint.pprint(evaluateSubtree(result))
#    Z092IwMoV /"prev interview month": STRING Z092IwMoV
#    Z093IwYrV /"prev interview year": STRING piRvarsZ093_IwYr_V
#    Z093IwYrV
#pprint.pprint(vars)

StandardFills = {}


for k,v in Questions.iteritems():

    if not hasattr(Questions[k], 'languages') or v.languages == None:
        continue

    print k, v.languages


    print k,v
    o = re.findall("\^[A-Za-z_0-9]+\[?[A-Z0-9_]*\]?(?:\.[A-Za-z_0-9]+\[?[A-Za-z0-9_]*\]+)?(?:\.?[A-Za-z0-9_]+)?",Questions[k].languages[0])

    #AlternativeFills[k] = getFillList(Questions[k].languages[0])

    if o:
        if not StandardFills.has_key(k):
                StandardFills[k] = []

        if isinstance(o, list):
                for x in o:
                    Questions[k].languages[0] = Questions[k].languages[0].replace(x, x.upper())
                    StandardFills[k].append(x.upper())

        else:
            Questions[k].languages[0] = Questions[k].languages[0].replace(o, o.upper())
            StandardFills[k].append(o.upper())

    print k, " ", pprint.pprint(Questions[k].languages[0])

Fills['FLHWP'] = ["husband/wife/partner"]
vars["FLHWP"] = { "type" : "STRING", "value" : "husband/wife/partner"}
Fills['FLSHWP'] = ["husband/wife/partner"]
Fills['PIFLSHWP'] = ["husband/wife/partner"]
Fills['FLUandYOUR'.upper()] = ["you and your"]
Fills['FLandyourHWPs'.upper()] = ["and your husband/wife/partner"]
Fills['FLYOUR'] = [""]
Fills['FLandyourHWP_s'.upper()] =["and your husband/wife/partner's"]
Fills['FLoryourHWP_s'.upper()] =  ["or your husband/wife/partner's"]
Fills['FLUorYOUR'.upper()] = ["you or your"]
Fills['FLDoYOU'.upper()] =  ["do you"]
Fills['FLSinceInThe'.upper()]  = ["Since PREV INTERVIEW MONTH, YEAR"]
Fills['FLsinceinthe_1'.upper()] = ["Since PREV INTERVIEW MONTH, YEAR"]
Fills['FLHimHer'.upper()] =  ["him/her"]
Fills['FLHeShe'.upper()] = ["he/she"]
Fills['FLHisHer'.upper()] = ["his/her"]
Fills['FLHisHers'.upper()] = ["his/hers"]
Fills['FLHimHerSelf'.upper()] = ["him/her/self"]
Fills['FLSYSYR'] = [ str(date.today().year) ]
Fills['FLSYSMO'] = [ str(date.today().month ) ]
Fills['FLinlastIWMoYr'.upper()] = ["In the last INTERVIEW MONTH, YEAR"]
Fills['FLinlastIWMoYrParen'.upper()] = ["(In the last INTERVIEW MONTH, YEAR)"]
Fills['FLDOESSPP'] = ["Do you/Does he/she"]
Fills['FLDoesRandSPP'.upper()] = ["Do(es) R and your husband/wife/partner"]
Fills['FLDoesRorSPP'.upper()] =  ["Do(es) R or your husband/wife/partner"]
Fills['FLdoRandSPP'.upper()] = ["do(es) R and your husband/wife/partner"]
Fills['FLdoRorSPP'.upper()] =   ["do(es) R or your husband/wife/partner" ]
Fills['FLdidRandSPP'.upper()] = ["did  R  and your husband/wife/partner"]
Fills['FLPWMonthYearFAM'.upper()] =  [ "Since PREV INTERVIEW MONTH, YEAR OF FAM IW" ]
Fills['FLsinceintheFAM'.upper()] = ["Since PREV INTERVIEW MONTH, YEAR OF FAM IW"]
Fills['FLsinceinthe_1FAM'.upper()] = ["Since PREV INTERVIEW MONTH, YEAR OF FAM IW"]
Fills['FLinlastIWMoYr'.upper()] =["In the last INTERVIEW MONTH, YEAR"]
Fills['FLinlastIWMoYrParen'.upper()] = [ "(In the last INTERVIEW MONTH, YEAR)" ]
Fills['FLAreSPP'.upper()] = ["Are you/Is he/she"]
Fills['FLSelf'.upper()] = ["Self"]
Fills['FLSPProxy'.upper()] = ["PROXY, SPOUSE IS REPORTER"]
Fills['FLNONSPProxy'.upper()] = [ "PROXY, NON-SPOUSE IS REPORTER" ]
Fills['flempname'.upper()] = ["CURRENT EMPLOYER"]
Fills['FLMONTH'] = [""]
Fills['FL_MONTH'] = [""]
Fills["STRSTATE"]  = ["[STATE]"]

if Fills.has_key("FLG097"):
    Fills["PIFLG097"] = Fills["FLG097"]

if Fills.has_key("FLG084"):
    Fills["PIFLG084"] = Fills["FLG084"]


if Fills.has_key("B090S"):
    vars["PIB090S"] = { "type" : "STRING" , "value" : Fills["B090S"][0]}

if Fills.has_key('S010'):
    Fills['S010A'] = Fills['S010']

if Fills.has_key('S004'):
    Fills['S004A'] = Fills['S004']


if Fills.has_key('FLC211'):
    Fills['FLC215'] = Fills['FLC211']

if Fills.has_key('PEFLS004A'):
    Fills['FLS004A'] = Fills['PEFLS004A']

if Fills.has_key('PEFLS003'):
    Fills['FLS003'] = Fills['PEFLS003']

if Fills.has_key('PEFLS003B'):
    Fills['FLS003B'] = Fills['PEFLS003B']


if Fills.has_key('PEFLS010A'):
    Fills['FLS010A'] = Fills['PEFLS010A']

if Fills.has_key('PEFLS037'):
    Fills['FLS058'] = Fills['PEFLS037']

if Fills.has_key('PEFLS037'):
    Fills['FLS037'] = Fills['PEFLS037']


if Fills.has_key('PEFLS037'):
    Fills['FLS055'] = Fills['PEFLS037']


if Fills.has_key('PEFLS016'):
    Fills['FLS016'] = Fills['PEFLS016']

if Fills.has_key('PEFLFORMERLATE'):
    Fills['FLFORMERLATE'] = Fills['PEFLFORMERLATE']


if Fills.has_key('PEFLSUBJECTHISHER'):
    Fills['FLSUBJECTHISHER'] = Fills['PEFLSUBJECTHISHER']

if Fills.has_key('PEFLINCOMETYPE'):
    Fills['FLINCOMETYPE'] = Fills['PEFLINCOMETYPE']


if Fills.has_key('PEFLDID'):
    Fills['FLDID'] = Fills['PEFLDID']


Fills["RVARS.Z092_IWMO_V"] = ["[" + "Number of times injured].[" +   getSymbol('Z092_IwMo_V'.upper()).description +  "]"]
Fills["RVARS.Z093_IWYR_V"] = ["[" + "Number of times injured].[" +   getSymbol('Z093_Iwyr_V'.upper()).description +  "]"]
Fills["PIRVARSZ092_IWMO_V"] = ["[" + "PREV WAVE IW MONTH" + "]" ]
Fills["PIRVARSZ093_IWYR_V"] = ["["  "PREV WAVE IW YEAR"  + "]" ]
Fills["PIRVARSZ091_EMPLNAME_V"] =  ["[" +   "PREV WAVE EMPLOYER NAME" + "]"]
Fills['Z091'] = ["PREV WAVE EMPLOYER NAME"]
Fills['FLIWER'] = [ 'If R disputes report from previous wave, probe as necessary to determine whether R has since been told by a doctor that he/she has the condition. If you wish, you may describe the situation in an F2 comment']
Fills['FLC070'] = ['[PREVIOUS WAVE: Yes/No/Unknown]']
Fills['PIA061TLCY_A'] = ["LAST CALENDAR YR CALCULATED"]
Fills['RTAB'] = ["MODE CHANGE"]
Fills['YEARLOOP'] = [ "WHICH MONTHS" ]
Fills['STEMQUESTIONFILL'] = ['Enter all months that apply']
vars['PIFLSHWP'] = { 'type' : "STTRING", 'value' : "husband/wife/partner" }
vars["PIZ094_PRXYNAME_V"] = { 'type' : "STTRING", 'value' : "prev wave proxy name" }
vars['RVARS.Z092_IWMO_V'.upper()] = { 'type' : "STRING", 'value' : "[" + "Number of times injured].[" +   getSymbol('Z092_IwMo_V'.upper()).description +  "]"}
vars['RVARS.Z093_IWYR_V'.upper()] = { 'type' : "STRING", 'value' : "[" + "Number of times injured].[" +   getSymbol('Z093_Iwyr_V'.upper()).description +  "]"}
vars["Pisecjcurrentjobbj_j158employerinfow158_currempname".upper()] = { 'type' : "STRING", 'value' : "[" + "CURRENT EMPLOYER" +  "]"}
vars["RELATSTR"] = {  'type' : "STRING", 'value' :  "R RELATIONSHIP" }
vars["PTHHX025_MAINCTY"] = { "type" : "STRING" , "value" : "MAIN RES ADDRESS CITY" }
vars["PIHHX025_MAINCTY"] = { "type" : "STRING" , "value" : "MAIN RES ADDRESS CITY" }
vars['PISECLJOBHISTORYMOSTRECENTJOBL008_'] =  { 'type' : "STRING", 'value' : "[" + "MOST RECENT EMPLOYER" +  "]"}
vars['PISECKPENSIONLOOPBK_EMPLOYERINFOW158_CURREMPNAME'] = { 'type' : "STRING", 'value' : "[" + "LAST EMPLOYER" +  "]"}
Fills["STARTYEAR"] = ["START YEAR"]
Fills["FIRSTYEAR"] = ["FIRST YEAR"]
Fills["PIFILL1"] = ["were you born'"]
Fills["PIFILL4"] = ["SPOUSE/PARTNER'S"]
Fills["TXTPROXY"] = ['or doing a proxy for (him/her).']
vars["B089TEMP1"] =  { 'type' : "STRING", 'value' : 'White Caucasian'}
vars["B089TEMP2"] =  { 'type' : "STRING", 'value' : 'African American'}
vars["B089TEMP3"] =  { 'type' : "STRING", 'value' : 'American Indian'}
vars["B089TEMP4"] =  { 'type' : "STRING", 'value' : 'Alaska Native'}
vars["B089TEMP5"] =  { 'type' : "STRING", 'value' : 'Asian'}
vars["B089TEMP6"] =  { 'type' : "STRING", 'value' : 'Native Hawaiian'}
vars["B089TEMP7"] =  { 'type' : "STRING", 'value' : 'Pacific Islander'}
Fills["PIFLWHICHMAINRES"] = ["[MAIN RES ADDRESS CITY, STATE (STATE ABBREVIATION)]"]
Fills["PISECAHOUSINGLOCATIONA086TMAINCTYST_A"] = ["[MAIN RES ADDRESS CITY, STATE (STATE ABBREVIATION)]"]
vars["PISPOUSENAME"] = { "type" : "STRING", "value" : "SPOUSENAME"}
vars["X036"] = { "type" : "STRING" , "value" : "[PREV WAVE IW MONTH OF FAMILY R]"}
Fills["X036"] = ["[PREV WAVE IW MONTH OF FAMILY R]" ]

vars["FLE042TLCY_A"] = { "type" : "STRING" , "value" : "[CURRENT YEAR MINUS 1]"}
vars["E042TLCY_A"] = { "type" : "STRING" , "value" : "[CURRENT YEAR MINUS 1]"}
vars["FLNRKID"] = { "type" : "STRING" , "value" : "[NON RES KID]"}

if Fills.has_key("FLE012_2"):

    Fills["FLE012_2"][0] = [" (in CITY NURSINGHOME)"]
    Fills["FLE012_2"][1] = [" (in CITY NURSINGHOME)"]


for i in range(0, 100):
    Fills["F223_F135_aArrayString".upper() + "[" + str(i) + "]"] = [u"[SIBLING]"]
    Fills["F225_F137_aArrayString".upper() + "[" + str(i) + "]"] = [u"[SIBLING]"]



def replaceFills(strn):
        print (strn).encode('ascii', 'ignore')
        t = re.findall("\^[A-Za-z_0-9]+\[?[A-Z0-9_]*\]?(?:\.[A-Za-z_0-9]+\[?[A-Za-z0-9_]*\]+)?(?:\.?[A-Za-z0-9_]+)?", strn)

        for z in t:

            rpl = z[1:].upper()
            if vars.has_key(rpl):
                strn = string.replace(strn, z, vars[rpl]['value'])
            elif Fills.has_key(rpl):
                Fills[rpl] = [x for x in Fills[rpl] if x is not None]

                fixFills = []

                for x in Fills[rpl]:
                    if not isinstance(x, unicode):
                        n = evaluateSubtree(x)

                        fixFills.append(n['value'])
                    else:
                        fixFills.append(x)

                pprint.pprint(fixFills)
                strn = string.replace(strn, z, "/".join(fixFills))



        return strn

def resolveType(kv, tdf, types = None):

    if types != None:
        print kv

        if isinstance(types, dict):
            return { 'type' : 1, 'value' : ""}

        print types
        print types.value
        setattr(tdf, "typeOf", types.value)

    if tdf.typeOf.value == "OPEN": # type 1
        print kv, " ", pprint.pprint(tdf.typeOf.value)
        return { 'type' : 1,  'value' : "" }

    elif hasattr(tdf.typeOf.value, "set_t") and  tdf.typeOf.value.type == "SET":     # type 4
        print kv
        items = []

        if isinstance(tdf.typeOf.value.set_t, unicode):
            sz = getSymbol(tdf.typeOf.value.set_t.upper())
            return resolveType(kv, tdf, sz)
        else:
            for i in tdf.typeOf.value.set_t.value:
                if isinstance(i.languages,list):
                    items.append([i.value, replaceFills(i.languages[0])])
                    print i.value, i.languages[0]
                else:
                    items.append([i.value, replaceFills(i.languages)])
                    print i.value, i.languages

            return { 'type' : 4,  'value' : items }

    elif hasattr(tdf.typeOf.value, "type") and  tdf.typeOf.value.type == "ENUMERATED": # type  4
        print kv, "enumerated"


        items = []

        for i in tdf.typeOf.value.value:
            if isinstance(i.languages,list):
                print i.value, (i.languages[0]).encode('ascii', 'ignore')
                items.append([i.value, replaceFills(i.languages[0])])
                print i.value, (i.languages[0]).encode('ascii', 'ignore')

            else:
                print i.value, i.languages
                items.append([i.value, replaceFills(i.languages)])
                print i.value, i.languages

        return { 'type' : 4,  'value' : items }

    elif hasattr(tdf.typeOf.value, "type") and tdf.typeOf.value.type == "RANGE":    # type 12
        print "[", kv, "] RANGE", tdf.typeOf.value.lowerlimit, "..", tdf.typeOf.value.upperlimit,
        return { 'type' : 12,  'value' : '' + str(tdf.typeOf.value.lowerlimit) + ".." + str(tdf.typeOf.value.upperlimit) }
    elif tdf.typeOf.value == "STRING": # type 1
        print kv, "OPEN"
        return { 'type' : 1,  'value' : ''}
    elif hasattr(tdf.typeOf.value, "type") and tdf.typeOf.value.type == "ARRAY":
        return { 'type' : 1 , 'value' : ''}
    else:
        if types == None:
            sz = getSymbol(tdf.typeOf.value)
            return resolveType(kv, tdf, sz)

    return { 'type' : 0, 'value' : ''}


listofQuestions = []



for k,v in Questions.iteritems():

    desc = False
    if isinstance(Questions[k].languages, types.NoneType) and isinstance(Questions[k].description, types.NoneType):
        print "Deleting: ",Questions[k], Questions[k].description, Questions[k].tag
        continue

    if isinstance(Questions[k].languages, types.NoneType) and not isinstance(Questions[k].description, types.NoneType):
        desc = True

    typeAns = resolveType(k, Questions[k], None)

    if not desc:
        t = re.findall("\^[A-Za-z_0-9]+\[?[A-Z0-9_]*\]?(?:\.[A-Za-z_0-9]+\[?[A-Za-z0-9_]*\]+)?(?:\.?[A-Za-z0-9_]+)?", Questions[k].languages[0])
    else:
        t = re.findall("\^[A-Za-z_0-9]+\[?[A-Z0-9_]*\]?(?:\.[A-Za-z_0-9]+\[?[A-Za-z0-9_]*\]+)?(?:\.?[A-Za-z0-9_]+)?", Questions[k].description)

    for z in t:

        #print unicode.replace(Questions[k].languages[0], z, z.upper())
        if not desc:
            Questions[k].languages[0] = string.replace(Questions[k].languages[0], z, z.upper())
        else:
            Questions[k].description = string.replace(Questions[k].description, z, z.upper())

    #listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'C. Physical Health'  ] )
    #listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'M1. Disability For Reinterviews'  ] )
    #listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'M2. Disability For Non-reinterviews'  ] )
    #listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'S. Widowhood And Divorce'  ] )
    #listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'T. Wills And Life Insurance'  ] )

    #if k[0] == "T" and k[1] == "N":
    #    listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'TN. Thumbnails'  ] )

    #if k[0] == "W" and k[1] == "3":
    #    listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'W. Events, Internet Use & Social Security Permission'  ] )

 #   if k[0] == "A":
  #      if not desc:
   #         listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'],  'A. Coverscreen' ] )
    #    else:
     #       listofQuestions.append([k, Questions[k].description, Questions[k].description , typeAns['type'], typeAns['value'],  'A. Coverscreen' ] )

    #if k[0] == "B" or k[0] == "H":
    #
    #    if not desc:
    #        listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'B. Demographics'  ] )
    #    else:
    #        listofQuestions.append([k, Questions[k].description, Questions[k].description, typeAns['type'], typeAns['value'], 'B. Demographics'  ] )

    #if k[0] == "D":
    #
    #    if not desc:
    #        listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'D. Cognition'  ] )
    #    else:
    #        listofQuestions.append([k, Questions[k].description, Questions[k].description, typeAns['type'], typeAns['value'], 'D. Cognition'  ] )

   # if k[0] == "E":
#
 #       if not desc:
  #          listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'E. Family Structure'  ] )
   #     else:
    #        listofQuestions.append([k, Questions[k].description, Questions[k].description, typeAns['type'], typeAns['value'], 'E. Family Structure'  ] )

    #if k[0] == "F":

#        if not desc:
#            listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'F. Parents & Sibling Couple Decisions'  ] )
     #   else:
     #       listofQuestions.append([k, Questions[k].description, Questions[k].description, typeAns['type'], typeAns['value'], 'F. Parents & Sibling Couple Decisions'  ] )

    #if k[0] == "G":

 #       if not desc:
 #           listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'G. Functional Limitations, ADL IADL, Helpers'  ] )
 #       else:
 #           listofQuestions.append([k, Questions[k].description, Questions[k].description, typeAns['type'], typeAns['value'], 'G. Functional Limitations, ADL IADL, Helpers'  ] )
    #if k[0] == "H":

        #if not desc:
        #    listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'H. Housing'  ] )
        #else:
        #    listofQuestions.append([k, Questions[k].description, Questions[k].description, typeAns['type'], typeAns['value'], 'H. Housing'  ] )


#    if k[0] == "J":

#        if not desc:
#            listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'J. Employment'  ] )
#        else:
#            listofQuestions.append([k, Questions[k].description, Questions[k].description, typeAns['type'], typeAns['value'], 'J. Employment'  ] )

#    if k[0] == "K":
#
#        if not desc:
#            listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'K. Last Job'  ] )
#        else:
#            listofQuestions.append([k, Questions[k].description, Questions[k].description, typeAns['type'], typeAns['value'], 'K. Last Job'  ] )

  #  if k[0] == "L":
#
 #       if not desc:
  #          listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'L. Job History'  ] )
   #     else:
    #        listofQuestions.append([k, Questions[k].description, Questions[k].description, typeAns['type'], typeAns['value'], 'L. Job History'  ] )


#    if k[0] == "N":
#
 #       if not desc:
  #          listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'N. Health Services And Insurance'  ] )
   #     else:
   #         listofQuestions.append([k, Questions[k].description, Questions[k].description, typeAns['type'], typeAns['value'], 'N. Health Services And Insurance'  ] )

  #  if k[0] == "P" or (k[0] == "S" and k[1] == "e"):
#
 #       if not desc:
  #          listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'P. Expectations'  ] )
   #     else:
    #        listofQuestions.append([k, Questions[k].description, Questions[k].description, typeAns['type'], typeAns['value'], 'P. Expectations'  ] )


    #if k[0] == "Q":
#
 #       if not desc:
  #          listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'Q. Assets and Income'  ] )
   #     else:
    #        listofQuestions.append([k, Questions[k].description, Questions[k].description, typeAns['type'], typeAns['value'], 'Q. Assets and Income'  ] )

    #if k[0] == "R":
#
 #       if not desc:
  #          listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'R. Asset change'  ] )
   #     else:
    #        listofQuestions.append([k, Questions[k].description, Questions[k].description, typeAns['type'], typeAns['value'], 'R. Asset change'  ] )


    #if k[0] == "U":
#
 #       if not desc:
  #          listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'R. Asset Reconciliation'  ] )
   #     else:
    #        listofQuestions.append([k, Questions[k].description, Questions[k].description, typeAns['type'], typeAns['value'], 'R. Asset Reconciliation'  ] )



    #if k[0] == "V":
#
 #       if not desc:
  #          listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'V. Modules'  ] )
   #     else:
    #        listofQuestions.append([k, Questions[k].description, Questions[k].description, typeAns['type'], typeAns['value'], 'V. Modules'  ] )



    #if k[0] == "Y":
#
 #       if not desc:
  #          listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'Y. Time Calculations'  ] )
   #     else:
    #        listofQuestions.append([k, Questions[k].description, Questions[k].description, typeAns['type'], typeAns['value'], 'Y. Time Calculations'  ] )

    #if k[0] == "V" or k[0] == "I":

     #   if not desc:
      #      listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'I. Physical Measures'  ] )
      #  else:
      #      listofQuestions.append([k, Questions[k].description, Questions[k].description, typeAns['type'], typeAns['value'], 'I. Physical Measures'  ] )


  #  if k[0] == "V" or k[0] == "W":
#
 #       if not desc:
  #          listofQuestions.append([k, Questions[k].description, Questions[k].languages[0], typeAns['type'], typeAns['value'], 'PE. PENSION'  ] )
   #     else:
    #        listofQuestions.append([k, Questions[k].description, Questions[k].description, typeAns['type'], typeAns['value'], 'PE. PENSION'  ] )


# HRS
# study 1
# survey 2010 72
# module section c 1369

if Fills.has_key('W205'):
    Fills['W205A'] = Fills['W205']

if Fills.has_key('FLE105'):
    Fills["FLE111"] = Fills["FLE105"]
    Fills["FLLATE"] = ["late husband/late wife/late partner"]

if Fills.has_key("peFLthatEmp".upper()):
    Fills["FLTHATEMP"] = Fills["peFLthatEmp".upper()]

vars["STRRELATION"] = { 'type' : "STRING", 'value' : "[RELATIONSHIP TO R]" }

from g2import import insertBlaise
#frame, validated = insertBlaise.LoadItems(listofQuestions,{ 'C. Physical Health' : 1369 })
#frame, validated = insertBlaise.LoadItems(listofQuestions,{ 'M1. Disability For Reinterviews' : 1371 })
#frame, validated = insertBlaise.LoadItems(listofQuestions,{ 'M2. Disability For Non-reinterviews' : 1372 })
#frame, validated = insertBlaise.LoadItems(listofQuestions,{ 'S. Widowhood And Divorce' : 1373 })
#frame, validated = insertBlaise.LoadItems(listofQuestions,{ 'T. Wills And Life Insurance' : 1374 })
#frame, validated = insertBlaise.LoadItems(listofQuestions,{  'TN. Thumbnails' : 1375 })
#frame, validated = insertBlaise.LoadItems(listofQuestions,{  'W. Events, Internet Use & Social Security Permission'  : 1376 })
#frame, validated = insertBlaise.LoadItems(listofQuestions,{  'A. Coverscreen'  : 1370 })
## MODULE C ###
#frame, validated = insertBlaise.LoadItems(listofQuestions,{  'B. Demographics' : 1377 })
#frame, validated = insertBlaise.LoadItems(listofQuestions,{  'D. Cognition' : 1378 })
#frame, validated = insertBlaise.LoadItems(listofQuestions,{  'E. Family Structure' : 1379 })
#frame, validated = insertBlaise.LoadItems(listofQuestions,{  'F. Parents & Sibling Couple Decisions' : 1380 })
#frame, validated = insertBlaise.LoadItems(listofQuestions,{  'G. Functional Limitations, ADL IADL, Helpers' : 1401 }) # really 1401 was 1381
#frame, validated = insertBlaise.LoadItems(listofQuestions,{  'H. Housing' : 1402 }) # really was 1382 / 1402
#frame, validated = insertBlaise.LoadItems(listofQuestions,{  'J. Employment' : 1403 }) # really was 1382 / 1402
#frame, validated = insertBlaise.LoadItems(listofQuestions,{  'K. Last Job' : 1431 }) # really was 1382 / 1402
#frame, validated = insertBlaise.LoadItems(listofQuestions,{  'L. Job History' : 1433 }) # really was 1382 / 1402
#frame, validated = insertBlaise.LoadItems(listofQuestions,{  'N. Health Services And Insurance' : 1435 }) # really was 1382 / 1402
#frame, validated = insertBlaise.LoadItems(listofQuestions,{  'P. Expectations' : 1436 }) # really was 1382 / 1402
#frame, validated = insertBlaise.LoadItems(listofQuestions,{  'Q. Assets and Income' : 1437 }) # really was 1382 / 1402
#frame, validated = insertBlaise.LoadItems(listofQuestions,{  'R. Asset change' : 1438 }) # really was 1382 / 1402
#frame, validated = insertBlaise.LoadItems(listofQuestions,{  'R. Asset Reconciliation' : 1439 }) # really was 1382 / 1402
#frame, validated = insertBlaise.LoadItems(listofQuestions,{  'V. Modules' : 1440 }) # really was 1382 / 1402
#frame, validated = insertBlaise.LoadItems(listofQuestions,{  'Y. Time Calculations' : 1441 }) # really was 1382 / 1402
#frame, validated = insertBlaise.LoadItems(listofQuestions,{  'I. Physical Measures' : 1442 }) # really was 1382 / 1402
#frame, validated = insertBlaise.LoadItems(listofQuestions,{  'PE. PENSION' : 1443 }) # really was 1382 / 1402
FirstPassFills = {}


spanishWords = u"¿|número|trabajo|durante|pago|rehúso|rehúsa|mandaré|más|dígame|extras|por|Cuánto|cuenta|pago|serían|área|valor|aproximadamente|administrar|vestirse|Ahora|tengo|algunas|preguntas|sobre|sus|hermanos|hermanas|padre|Padre|Madre|él|frecuencia|hecha|veces|respuestas|hijos|padre|madre|nietos|familia|compańero(a)|matrimonio|último|enviudó|divorció|empty|tenemos|domicilio|diferencia|nacimiento|Oprima|siguiente|o su|esposo|confirmar|acuerdo|cambiar|compańeros|ańos|ańo|compańero|compañeros|años|año|compañero|PASADO|pasado|Desde|desde|fumado|concentrarse|demencia|memoria|ella|ellos|embarazo|primera|enfermedad|nuevo|reposo|FLHESHE|FLHIMHERSELF|juntos|FLHISHER|hogar|falleciera|fallecimiento|difunto|difunta|divorcio|beneficios|Seguro\sSocial|Ingresos|testamento|fideicomiso|NUEVO|employer|empleo|cambiado|EMPLEO|SOLICITUD|RECIBIR|documento|participante|entrevistador|Participante|transifirió|estampilla|elegibles|hecha|veces|Nunca|nunca|formulario|tiempo|firme|dejaron|Viven|datos|casa|apartamento|ellos|estaba|tiene|usted"

for k, v in StandardFills.iteritems():

    for ff in v:
        xtr = None
        if not FirstPassFills.has_key(ff[1:]):
            FirstPassFills[ff[1:]] = []

        if not Fills.has_key(ff[1:]):

            res = re.findall("[X|Z][0-9]+_?[0-9A-Z_]*", ff[1:])

            if res and (Fills.has_key(res[0])):

                if isinstance(Fills[res[0]],list):
                    Fills[ff[1:]] = Fills[res[0]][0]
                else:
                    Fills[ff[1:]] = Fills[res[0]]
            else:

                for qk, qv in Questions.iteritems():

                    if qv.name.name == ff[1:]:
                        xtr = qv.description

        if not xtr:
            xtr = Fills[ff[1:]]


        print ff
        if not isinstance(xtr, list):
                try:
                    print type(xtr), xtr

                    if re.search(spanishWords, xtr):
                        continue
                    print "APPENDING", xtr , "oooooooo"
                    FirstPassFills[ff[1:]].append(xtr)
                except Exception, e:
                    print e
        else:
            for cc in xtr:

                if isinstance(cc, unicode) or isinstance(cc, str):

                   if re.search(spanishWords, cc):
                        continue
                   else:
                        FirstPassFills[ff[1:]].append(cc)

                elif hasattr(cc, "type") and cc.type == 'IDENTIFIER':


                    try:
                        tz = Fills[cc.name][0]
                        tz['value'].decode('ascii')

                        if re.search(spanishWords, tz['value']):
                            continue
                        FirstPassFills[ff[1:]].append(tz['value'])
                    except Exception, e:
                        pass
                        #print e

                elif not isinstance(cc, types.NoneType):
                    tz = evaluateSubtree(cc)


                    try:
                        print type(tz['value'])


                        tz['value'].decode('ascii')
                        if re.search(spanishWords, tz['value']):
                            continue

                        FirstPassFills[ff[1:]].append(tz['value'])
                    except Exception, e:
                        print e


finalFills = []

for k, v in FirstPassFills.iteritems():
    FirstPassFills[k] = list(set(FirstPassFills[k]))
    for z in FirstPassFills[k]:
        t = re.findall("\^[A-Za-z_0-9]+\[?[A-Z0-9_]*\]?(?:\.[A-Za-z_0-9]+\[?[A-Za-z0-9_]*\]+)?(?:\.?[A-Za-z0-9_]+)?", z, re.UNICODE)

        if len(t) > 0:
            for c in t:
                if not FirstPassFills.has_key(c[1:]):
                    finalFills.append(c)

finalFills = list(set(finalFills))





for ff in finalFills:
    xtr = Fills[ff[1:]]

    if not FirstPassFills.has_key(ff[1:]):
        FirstPassFills[ff[1:]] = []


    print xtr
    for cc in xtr:

        if not hasattr(cc,"type"):
            try:
                cc.decode('ascii')

                if re.search(spanishWords, cc):
                    continue

                FirstPassFills[ff[1:]].append(cc)
            except Exception, e:
                print e
        else:
            if cc.type == 'IDENTIFIER':


                try:
                    tz = Fills[cc.name][0]
                    tz['value'].decode('ascii')

                    if re.search(spanishWords, tz['value']):
                        continue
                    FirstPassFills[ff[1:]].append(tz['value'])
                except Exception, e:
                    print e

            else:
                tz = evaluateSubtree(cc)
                print tz


                try:
                    print type(tz['value'])

                    tz['value'].decode('ascii')
                    if re.search(spanishWords, tz['value']):
                        continue

                    FirstPassFills[ff[1:]].append(tz['value'])
                except Exception, e:
                    print e



def replaceFinalFills(k, strn):
        import copy
        finald = copy.copy(strn)

        counter = 0

        if isinstance(finald,list):


            for d in finald:
                t = re.findall("\^[A-Za-z_0-9]+\[?[A-Z0-9_]*\]?(?:\.[A-Za-z_0-9]+\[?[A-Za-z0-9_]*\]+)?(?:\.?[A-Za-z0-9_]+)?", d)

                for z in t:
                    print z
                    if isinstance(FirstPassFills[z[1:]],list):
                        print finald[counter]


                        if isinstance(FirstPassFills[z[1:]],list):

                            strn[counter] = strn[counter].replace(z, "/".join(FirstPassFills[z[1:]]))
                            #print strn[counter]
                        else:
                            strn[counter] = strn[counter].replace(z, FirstPassFills[z[1:]])
                            #print strn[counter]



                        #finald = d.replace(z,"/".join(FirstPassFills[trim(z[1:])]))
                    else:
                        print "ee ", counter, z, d, " ee"
                        print "1xx ", string.replace(d, z, FirstPassFills[z[1:]]), " xx1"
                        print "1aa ", counter, z, d, FirstPassFills[z[1:]], " aa1"
                        print counter, z, d
                        #finald = d.replace(z, FirstPassFills[trim(z)[1:]])


                counter = counter + 1

        #else:
        #    t = re.findall("\^[A-Za-z_0-9]+\[?[A-Z0-9_]*\]?(?:\.[A-Za-z_0-9]+\[?[A-Za-z0-9_]*\]+)?(?:\.?[A-Za-z0-9_]+)", finald)
        #
        #    for z in t:
        #
        #        if isinstance(FirstPassFills[z[1:]],list):
        #            finald = strn.replace(z,"/".join(FirstPassFills[z[1:]]))
        #        else:
        #            finald = strn.replace(z, FirstPassFills[z[1:]])


        return strn
        #FirstPassFills[k] = finald
        return FirstPassFills[k]

for k, v in FirstPassFills.iteritems():
    FirstPassFills[k] = list(set(FirstPassFills[k]))
    FirstPassFills[k] = replaceFinalFills(k, FirstPassFills[k])


#insertBlaise.LoadFills(FirstPassFills, 72)