'''
Created on Oct 8, 2012

@author: Deepak Patil
'''
#import operator
#import datetime
import sys
import os
from AST import ExLiteral, Property, ObjectLiteral, ArrayLiteral, \
    FunctionDeclaration, VariableDeclaration, StmtVariableDeclaration, StmtBlock,\
    StmtEmpty, StmtExpression, StmtIf, StmtDoWhile, StmtWhile, StmtFor,\
    StmtForIn, StmtContinue, StmtBreak, StmtReturn, StmtWith, StmtLabelled,\
    StmtSwitch, CaseClause, StmtThrow, StmtTry, CatchClause, FinallyClause,\
    Expression, ExOperator, ExThis, ExParExpression, ExIdentifier, ExArguments,\
    ExNew, ExIndexSuffix, ExPropertyReferenceSuffix, Program, ExRegex    ,\
    Parameter, Closure

log = False
whitespace  = [chr(0x0A), chr(0x20), chr(0x09), chr(0x0C), chr(0x0D)]
keywords    = ["break", "case", "catch", "continue", "debugger", "default", "delete", "do", "else", "finally", "for", "function", "if", "in", "instanceof", "new", "return", "switch", "this", "throw", "try", "typeof", "var", "void", "while", "with"]
hex_alpha   = ['a','b','c','d','e','f','A','B','C','D','E','F']
special_chars = ['(',')']
idf_sym     = ['$','_']
assignment_start_char   = ['=' , '*', '/', '%','+','-','<','>','&','^','|']
assignment_start_char_2 = ['=','<','>']
iterative_keywords      = ["do","for","while"]
unary_operator          = [ '+' , '-', '~', '!']
CHR_NL      = 0x0A
CHR_CR      = 0x0D
cached = {}

# def getTime( ):
#     t = datetime.datetime.now()
    
#     return ( ( t.minute*60 ) + t.second ) * 1000000 + t.microsecond
class Token:
    def __init__(self,value,line,pos):
        self.value = value
        self.line  = line
        self.pos   = pos
    def __str__(self):
        return "Value : " + self.value + " Line No :" + str(self.line) + " Pos :" + str(self.pos)

def tokenize(data):

    size = len(data)
    cur = 0
    toks =[]
    pos = []
    line_no =  0
    char_pos =  0
    char = None
    token = ""
    
    reading_identifier = False
    reading_number  = False    
    
    while cur < size:
        char =  data[cur]        
        if reading_identifier and ( char.isalpha() or char.isdigit() or char in idf_sym):
            token += char
            cur += 1
            char_pos += 1
        elif reading_number and char.isdigit():
            token += char
            cur += 1
            char_pos += 1                   
        elif char.isalpha() or char in idf_sym:
            if token!="":
                toks.append(Token(token,line_no,char_pos))
            token = char
            cur += 1
            char_pos += 1
            reading_identifier = True
            reading_number = False
        elif char.isdigit():
            if token!="":
                toks.append(Token(token,line_no,char_pos))
            token = char
            cur += 1
            char_pos += 1
            reading_number = True
            reading_identifier = False       
        else:
            if token!="":
                toks.append(Token(token,line_no,char_pos))
            token = char
            if char == '\\':
                cur += 1
                char_pos += 1
                token += data[cur]                    
            cur += 1
            if token=='\r' or token =='\n':
                line_no += 1
                char_pos = 0
                if token == '\r' and data[cur] == '\n':
                    cur += 1                     
            else:
                char_pos += 1                
            reading_number = reading_identifier = False
    if token!="":
        toks.append(Token(token,line_no,char_pos))            
    return toks                     
        
    
def isJSKeyword(idf):
    return idf in keywords

class Stream:
    def __init__(self, data):
        self.data = data
        self.max_size = len(data)
        self.cur = 0
        self.last_ws_skipped = -1
    
    def isEnd(self):
        return self.cur >= self.max_size
    
    def get(self):        
        if self.cur >= self.max_size:
            return None
        else:
            return self.data[self.cur].value        
    def move(self,by=1):
        self.cur += by
    def getPos(self):
        return self.cur
    def setPos(self, pos):
        self.cur = pos
        
        
    
class ExpressionReader:
    
    @staticmethod
    def readExpression(stream, noIn = False):
        """
        expression
            : assignmentExpression (LT!* ',' LT!* assignmentExpression)*
        ;
        """
        CodeReader.skipWhiteSpace(stream)                
        tok = stream.get()
        rvalue = None
        """        
        if tok == "function":
            rvalue = CodeReader.readFunctionDeclaration(stream)
        
        if rvalue!=None:                        
            exp = Expression()
            exp.parts = [rvalue]
            rvalue = exp
        """
        if tok not in ['}',']',';',',',')']:                        
            parts = ExpressionReader.readAssignmentExpression(stream, noIn)
            if parts != None:
                while True:
                    CodeReader.skipWhiteSpace(stream)
                    last_pos = stream.getPos()                
                    s = stream.get()
                    if s == ',':
                        stream.move()
                        CodeReader.skipWhiteSpace(stream)
                        parts2 = ExpressionReader.readAssignmentExpression(stream, noIn)
                        if parts2 != None:
                            o = ExOperator()
                            o.value = ExOperator.COMMA
                            parts.append(o)
                            parts.append( parts2 )
                        else:
                            stream.setPos( last_pos )
                            break                    
                                        
                    else:
                        break
                exp = Expression()
                exp.parts = parts
                rvalue = exp         
        return rvalue
                    
    @staticmethod
    def readAssignmentExpressionAsExpression(stream , noIn = False):
        rvalue = None
        exp = ExpressionReader.readAssignmentExpression(stream, noIn)
        if exp!=None:
            rvalue = Expression()
            rvalue.parts = exp
        return rvalue
                     
    @staticmethod
    def readAssignmentExpression(stream , noIn = False):
        """
        assignmentExpression
            : conditionalExpression
            | leftHandSideExpression LT!* assignmentOperator LT!* assignmentExpression
        ;
        """
        rvalue = None        
   
        last_pos = stream.cur        
        parts = ExpressionReader.readLeftHandSideExpression(stream)
        
        if parts != None:
            CodeReader.skipWhiteSpace(stream)
            operator = ExpressionReader.readAssignmentOperator(stream)                
            
            if operator!=None:
                parts.append( operator )
                CodeReader.skipWhiteSpace(stream)
                rparts = ExpressionReader.readAssignmentExpression(stream, noIn)    
                if rparts != None:                        
                    parts.extend( rparts )
                    rvalue = parts                        
                else:
                    stream.cur = last_pos
            else:
                stream.cur = last_pos
        
        if rvalue == None:
            rvalue = ExpressionReader.readConditionalExpression(stream, noIn)
   
        return rvalue
    
    @staticmethod
    def readConditionalExpression(stream , noIn ):
        """
        conditionalExpression
            : logicalORExpression (LT!* '?' LT!* assignmentExpression LT!* ':' LT!* assignmentExpression)?
        ;
        """
        rvalue = None
        parts = ExpressionReader.readLogicalORExpression(stream, noIn)
        if parts!=None:
            CodeReader.skipWhiteSpace(stream)
            if stream.get() == '?':
                stream.move()
                op_if = ExOperator()
                op_if.value = ExOperator.TERNARY_IF
                
                CodeReader.skipWhiteSpace(stream)
                exp_if = ExpressionReader.readAssignmentExpressionAsExpression(stream, noIn)
                if exp_if == None:
                    raise Exception("Expecting expression after ternary if" , str(stream.data[stream.cur]))
                
                CodeReader.skipWhiteSpace(stream)
                if stream.get() != ':':
                    raise Exception("Expecting : after ? expression " , str(stream.data[stream.cur]))
                stream.move()
                
                op_else = ExOperator()
                op_else.value = ExOperator.TERNARY_ELSE
                CodeReader.skipWhiteSpace(stream)
                
                exp_else = ExpressionReader.readAssignmentExpressionAsExpression(stream)
                if exp_else == None:
                    raise Exception("Expecting expression after ternary else" , str(stream.data[stream.cur]))
                rvalue = [ parts , op_if , exp_if , op_else , exp_else ]
            else:
                rvalue = parts                                
                
        return rvalue
        

    @staticmethod
    def readLogicalORExpression(stream , noIn ):
        """
        logicalORExpression
            : logicalANDExpression (LT!* '||' LT!* logicalANDExpression)*
        ;
        """
        rvalue = None
        parts = ExpressionReader.readLogicalANDExpression(stream, noIn)
        
        if parts != None:
            rvalue = parts
            
            while True:
                CodeReader.skipWhiteSpace(stream)
                if stream.get() == '|' and stream.data[ stream.cur + 1].value == '|':                    
                    stream.move(2)                    
                    op = ExOperator()
                    op.value = op.LOGICAL_OR 
                    rvalue.append( op )
                    CodeReader.skipWhiteSpace(stream)
                    parts2 = ExpressionReader.readLogicalANDExpression(stream, noIn)
                    if parts2 == None:                        
                        raise Exception("Expecting expression after || ( logical or )", str(stream.data[stream.cur]))
                    rvalue.append( parts2 )
                else:
                    break
        
        return rvalue
            
    @staticmethod
    def readLogicalANDExpression(stream , noIn ):
        """
        logicalANDExpression
        : bitwiseORExpression (LT!* '&&' LT!* bitwiseORExpression)*
        ;
        """
        rvalue = None
        parts = ExpressionReader.readBitwiseORExpression(stream, noIn)
        if parts != None:
            rvalue = parts
            while True:
                CodeReader.skipWhiteSpace(stream)
                if stream.get() == '&' and stream.data[ stream.cur + 1 ].value == '&':                    
                    stream.move(2)
                    op = ExOperator()
                    op.value = op.LOGICAL_AND
                    rvalue.append( op )
                    CodeReader.skipWhiteSpace(stream)
                    parts2 = ExpressionReader.readBitwiseORExpression(stream, noIn)
                    if parts2 == None:
                        raise Exception("Expecting expression after && ( logical and )", str(stream.data[stream.cur]))
                    rvalue.append(parts2)
                else:
                    break
        
        return rvalue
    @staticmethod
    def readBitwiseORExpression(stream , noIn ):
        """
        bitwiseORExpression
            : bitwiseXORExpression (LT!* '|' LT!* bitwiseXORExpression)*
            ;
        """
        rvalue = None
        parts = ExpressionReader.readBitwiseXORExpression(stream, noIn)
        if parts != None:
            rvalue = parts
            while True:
                last_pos = stream.cur
                CodeReader.skipWhiteSpace( stream )
                if stream.get() == '|':                     
                    stream.move()                    
                    CodeReader.skipWhiteSpace(stream)
                    parts2 = ExpressionReader.readBitwiseXORExpression(stream, noIn)
                    if parts2 != None:
                        op = ExOperator()
                        op.value = op.BITWISE_OR 
                        rvalue.append( op )
                        rvalue.append(parts2)
                    else:
                        stream.cur = last_pos
                        break                                                                    
                else:
                    break
        
        return rvalue
    @staticmethod
    def readBitwiseXORExpression(stream , noIn ):
        """
        bitwiseXORExpression
            : bitwiseANDExpression (LT!* '^' LT!* bitwiseANDExpression)*
            ;
        """
        rvalue = None
        parts = ExpressionReader.readBitwiseANDExpression(stream, noIn)
        if parts != None:
            rvalue = parts
            while True:
                CodeReader.skipWhiteSpace(stream)
                if stream.get() == '^':                     
                    stream.move()
                    op = ExOperator()
                    op.value = op.BITWISE_XOR 
                    rvalue.append( op )
                    CodeReader.skipWhiteSpace(stream)
                    parts2 = ExpressionReader.readBitwiseANDExpression(stream, noIn)
                    if parts2 == None:
                        raise Exception("Expecting expression after ^ (bitwise xor )", str(stream.data[stream.cur]))
                    rvalue.append(parts2)
                else:
                    break
        
        return rvalue
    @staticmethod
    def readBitwiseANDExpression(stream , noIn ):
        """
        bitwiseANDExpression
            : equalityExpression (LT!* '&' LT!* equalityExpression)*
            ;
        """
        rvalue = None
        parts = ExpressionReader.readEqualityExpression(stream, noIn)
        if parts != None:
            rvalue = parts
            while True:
                last_pos = stream.cur                
                CodeReader.skipWhiteSpace(stream)                
                
                if stream.get() == '&':                     
                    stream.move()
                    CodeReader.skipWhiteSpace(stream)
                    parts2 = ExpressionReader.readEqualityExpression(stream, noIn)
                    
                    if parts2 != None:
                        op = ExOperator()
                        op.value = op.BITWISE_AND
                        rvalue.append( op )
                        rvalue.append(parts2)
                    else:
                        stream.cur = last_pos
                        break                                            
                else:                    
                    break
        
        return rvalue
    @staticmethod
    def readEqualityExpression(stream , noIn ):
        """
        equalityExpression
            : relationalExpression (LT!* ('==' | '!=' | '===' | '!==') LT!* relationalExpression)*
            ;
        """
        rvalue = None
        parts = ExpressionReader.readRelationalExpression(stream, noIn)
        if parts != None:
            rvalue = parts
            while True:
                last_pos = stream.cur
                CodeReader.skipWhiteSpace(stream)            
                if stream.get() == '=' or stream.get() == '!' :
                    value = None
                    if stream.get() == '=':
                        stream.move()
                        if stream.get() == '=':
                            stream.move()
                            if stream.get() == '=':
                                stream.move()
                                value = ExOperator.RELATIONAL_T_EQUAL
                            else:
                                value = ExOperator.RELATIONAL_EQUAL                                                
                    else:
                        stream.move()
                        if stream.get() == '=':
                            stream.move()
                            if stream.get() == '=':
                                stream.move()
                                value = ExOperator.RELATIONAL_NOT_T_EQUAL
                            else:
                                value = ExOperator.RELATIONAL_NOT_EQUAL
    
                    if value != None:       
                        op = ExOperator()
                        op.value = value              
                        rvalue.append( op )
                        CodeReader.skipWhiteSpace(stream)
                        parts2 = ExpressionReader.readRelationalExpression(stream, noIn)
                        if parts2 == None:
                            raise Exception("Expecting expression equality operator", str(stream.data[stream.cur]))
                        rvalue.append( parts2 )
                    else:
                        stream.cur = last_pos
                        break                        
                else:
                    stream.cur = last_pos
                    break
        
        return rvalue

    @staticmethod
    def readRelationalExpression(stream, noIn ):
        """
        relationalExpression
            : shiftExpression (LT!* ('<' | '>' | '<=' | '>=' | 'instanceof' | 'in') LT!* shiftExpression)*
        ;
        """
        rvalue = None
        parts = ExpressionReader.readShiftExpression(stream)
        
        if parts != None:
            rvalue = parts
            
            while True:
                CodeReader.skipWhiteSpace(stream)
                last_pos = stream.cur
                value = None
                
                if stream.get() == '<' or stream.get() == '>' :
                    if stream.get() == '<':
                        stream.move()
                        if stream.get() == '=':
                            stream.move()
                            value = ExOperator.RELATIONAL_LESS_THAN_OR_EQUAL
                        else:
                            value = ExOperator.RELATIONAL_LESS_THAN                                                
                    else:
                        stream.move()
                        if stream.get() == '=':
                            stream.move()
                            value = ExOperator.RELATIONAL_GREATER_THAN_EQUAL
                        else:
                            value = ExOperator.RELATIONAL_GREATER_THAN
                else:
                    kw = CodeReader.readIdentifier(stream)
                
                    if kw == "instanceof":
                        value = ExOperator.RELATIONAL_INSTANCEOF
                    
                    elif noIn == False and kw == "in":
                        value = ExOperator.RELATIONAL_IN
                
                if value!=None:
                    op = ExOperator() 
                    op.value = value
                    rvalue.append( op )
                    CodeReader.skipWhiteSpace( stream )
                    parts2 = ExpressionReader.readShiftExpression( stream )
                    if parts2 == None:
                        raise Exception("Expecting expression equality operator", str(stream.data[stream.cur]))
                    rvalue.append( parts2 )
                else:
                    stream.cur = last_pos
                    break
                    
           
        return rvalue
    @staticmethod
    def readShiftExpression(stream):
        """
        shiftExpression
            : additiveExpression (LT!* ('<<' | '>>' | '>>>') LT!* additiveExpression)*
            ;
        """
        rvalue = None
        parts = ExpressionReader.readAdditiveExpression(stream)
        if parts != None:
            rvalue = parts
            while True:
                last_pos = stream.cur
                CodeReader.skipWhiteSpace(stream)
                if stream.get() == '<' or stream.get() == '>' :
                    value = None
                    if stream.get() == '>':
                        stream.move()
                        if stream.get() == '>':
                            stream.move()
                            if stream.get() == '>':
                                stream.move()
                                value = ExOperator.SHIFT_RIGHT_RIGHT
                            else:
                                value = ExOperator.SHIFT_RIGHT                                               
                    else:
                        stream.move()
                        if stream.get() == '<':
                            stream.move()                        
                            value = ExOperator.SHIFT_LEFT
    
                    if value!=None:
                        op = ExOperator()
                        op.value = value                                                                                             
                        rvalue.append( op )
                        CodeReader.skipWhiteSpace(stream)
                        parts2 = ExpressionReader.readAdditiveExpression(stream)
                        if parts2 == None:
                            raise Exception("Expecting expression after shift operator", str(stream.data[stream.cur]))
                        rvalue.append(parts2)
                    else:
                        stream.cur = last_pos 
                        break                       
                else:
                    stream.cur = last_pos
                    break
        
        return rvalue
    
    @staticmethod
    def readAdditiveExpression(stream):
        """
        additiveExpression
            : multiplicativeExpression (LT!* ('+' | '-') LT!* multiplicativeExpression)*
            ;
        """
        rvalue = None
        parts = ExpressionReader.readMultiplicativeExpression(stream)
        
        if parts != None:
            rvalue = parts
            while True:
                last_pos = stream.cur
                CodeReader.skipWhiteSpace(stream)
                ch = stream.get()
                if ch == '+' or ch == '-' :
                    
                    op = ExOperator()
                
                    if ch == '+':
                        op.value = ExOperator.ADDITIVE_PLUS
                    else:
                        op.value = ExOperator.ADDITIVE_MINUS
                    stream.move()                                    
                                                                    
                    parts.append( op )
                    CodeReader.skipWhiteSpace(stream)
                    parts2 = ExpressionReader.readMultiplicativeExpression(stream)
                    
                    if parts2 == None:                        
                        raise Exception("Expecting expression after additive operator", str(stream.data[stream.cur]))                    
                    parts.append(parts2)
                   
                else:   
                    stream.cur = last_pos                 
                    break
        
        return rvalue
    
    @staticmethod
    def readMultiplicativeExpression(stream):
        """
        multiplicativeExpression
            : unaryExpression (LT!* ('*' | '/' | '%') LT!* unaryExpression)*
            ;
        """
        rvalue = None
        parts = ExpressionReader.readUnaryExpression(stream)
        if parts != None:
            rvalue = parts 
            while True:
                last_pos = stream.cur
                CodeReader.skipWhiteSpace(stream)
                if not stream.isEnd() and  ( stream.get() == '*' or stream.get() == '/' or stream.get() == '%' ):
                    value = None
                    if stream.get() == '*':
                        value = ExOperator.MULITPLICATIVE_MULTIPLY
                    elif stream.get() == '/':
                        value = ExOperator.MULTIPLICATIVE_DIVIDE      
                    else:
                        value = ExOperator.MULTIPLICATIVE_MODULUS
    
                    stream.move()
                    if value!=None:
                        op = ExOperator()                                                                                             
                        op.value = value
                        rvalue.append( op )
                        CodeReader.skipWhiteSpace(stream)
                        parts2 = ExpressionReader.readUnaryExpression(stream)
                        if parts2 == None:
                            raise Exception("Expecting expression after multiplicative operator", str(stream.data[stream.cur]))
                        rvalue.append(parts2)
                    else:
                        stream.cur = last_pos                        
                        break
                else:
                    stream.cur = last_pos
                    break
                       
        
        return rvalue
    @staticmethod
    def readUnaryExpression(stream):
        """
        unaryExpression
            : postfixExpression
            | ('delete' | 'void' | 'typeof' | '++' | '--' | '+' | '-' | '~' | '!') unaryExpression
        ;
        """ 
        rvalue = ExpressionReader.readPostfixExpression(stream)
        if rvalue == None:
            last_pos = stream.cur
            s = stream.get()
            
            value = None
            if s in unary_operator:
                stream.move()
                
                if s == '+':
                    if stream.get() == '+':
                        stream.move()
                        value = ExOperator.UNARY_INCREMENT
                    else:
                        value = ExOperator.UNARY_POSITIVE
                
                elif  s == '-':
                    if stream.get() == '-':
                        stream.move()
                        value = ExOperator.UNARY_DECREMENT
                    else:
                        value = ExOperator.UNARY_NEGATIVE
                
                elif s == '~':
                    value = ExOperator.UNARY_BITWISE_NOT
                
                else:#'!'
                    value = ExOperator.UNARY_NOT                     
            else:
            
                kw = CodeReader.readIdentifier(stream)
                
                if kw == "void":
                    value = ExOperator.UNARY_VOID
                
                elif kw == "delete":
                    value = ExOperator.UNARY_DELETE
                
                elif kw == "typeof":
                    value = ExOperator.UNARY_TYPEOF
            
            if value != None:
                op = ExOperator()
                op.value = value
                CodeReader.skipWhiteSpace(stream)
                parts = ExpressionReader.readUnaryExpression(stream)
                
                if parts != None:
                    rvalue = [op, parts]
                else:
                    stream.cur = last_pos
            else:
                stream.cur = last_pos                                    
                
        return rvalue

    @staticmethod
    def readPostfixExpression(stream):
        """
        postfixExpression
            : leftHandSideExpression ('++' | '--')?
            ;
        """
        parts = ExpressionReader.readLeftHandSideExpression(stream)
        rvalue = parts
        
        if parts!=None:
            if not stream.isEnd():
                s = stream.get()                
                if s == '+' or s == '-':
                    value = None
                    ns = stream.data[ stream.cur + 1].value
                    
                    if s == '+' and ns == '+':
                        value = ExOperator.POSTFIX_INCREMENT
                    elif s == '-' and ns == '-':
                        value = ExOperator.POSTFIX_DECREMENT                
                
                    if value != None:
                        op = ExOperator()
                        op.value = value
                        stream.move()
                        stream.move()
                        rvalue = [parts , op]
                            
        return rvalue
        
        

        
    @staticmethod
    def readLeftHandSideExpression(stream):        
        """
        leftHandSideExpression
            : callExpression
            | newExpression
        ;
        """        
        tok = stream.get()
        rvalue = None
        if tok == "true" or tok == "false" or tok == "null":            
            rvalue = ExLiteral()
            rvalue.type = ExLiteral.KEYWORD
            
            if tok == "true":
                rvalue.value = ExLiteral.TRUE
            elif tok == "false":
                rvalue.value = ExLiteral.FALSE
            else:
                rvalue.value = ExLiteral.NULL
            
            stream.move()        
        
        if rvalue != None:
            rvalue = [rvalue]
        
        if rvalue == None and tok.isdigit():
            data = CodeReader.readNumericLiteral(stream)
            if data != None:
                rvalue = ExLiteral()
                rvalue.type = ExLiteral.NUMERIC
                rvalue.value = data
                rvalue = [rvalue] 
                                
        if rvalue == None:        
            rvalue =  ExpressionReader.readCallExpression(stream) or ExpressionReader.readNewExpression(stream)
        return rvalue

         
        
    @staticmethod
    def readCallExpression(stream):
        """
        callExpression
            : memberExpression LT!* arguments (LT!* callExpressionSuffix)*
        ;
        """
        # changed grammar for performance
        """
        callExpression
            : memberExpression LT!* arguments (LT!* callExpressionSuffix)*
            | memberExpression 
        ;
        """        
        rvalue = None
        me = ExpressionReader.readMemberExpression(stream)                
        if me !=  None:
            
            CodeReader.skipWhiteSpace(stream)
            arg = ExpressionReader.readArguments(stream)
            
            if arg!=None:
                rvalue = [ me , arg]
                while True:
                    CodeReader.skipWhiteSpace(stream)
                    ces = ExpressionReader.readCallExpressionSuffix(stream)                                                            
                    if ces != None:
                        rvalue.append(ces)
                    else:
                        break
                    
            else:                
                rvalue = me
                                                
        return rvalue
    @staticmethod
    def readCallExpressionSuffix(stream):
        """
        callExpressionSuffix
            : arguments
            | indexSuffix
            | propertyReferenceSuffix
            ;
        """
        return ExpressionReader.readArguments(stream) or ExpressionReader.readIndexSuffix(stream) or ExpressionReader.readPropertyReferenceSuffix(stream)
    
    @staticmethod
    def readMemberExpression(stream):
        """
        memberExpression
            : (primaryExpression | functionExpression | 'new' LT!* memberExpression LT!* arguments) (LT!* memberExpressionSuffix)*
            ;
        """
        #Changed reading priority
        left = None
        if not stream.isEnd():

            last_pos = stream.cur
            kwNew = CodeReader.readIdentifier(stream)
            if kwNew == "new":                    
                CodeReader.skipWhiteSpace(stream)
                mexp = ExpressionReader.readMemberExpression(stream)
                if mexp != None:
                    CodeReader.skipWhiteSpace(stream)
                    argument = ExpressionReader.readArguments(stream)
                    if argument!=None:
                        left = [ExNew(),mexp,argument]
                    else:
                        stream.cur = last_pos
                else:
                    stream.cur = last_pos
            else:
                stream.cur = last_pos
            
            if left == None:
                left = CodeReader.readFunctionDeclaration(stream)    
            if left == None:
                left = ExpressionReader.readPrimaryExpression(stream)
    
            if left != None:
                if not isinstance(left , list):
                    left = [ left ]
            
            if left != None:
                while True:
                    CodeReader.skipWhiteSpace(stream)
                    if stream.get() not in ['}',']',';',',',')']:
                        mes = ExpressionReader.readMemberExpressionSuffix(stream)
                        if mes!=None:
                            left.append(mes)
                        else:
                            break
                    else:
                        break
       
        return left    
    
    @staticmethod
    def readMemberExpressionSuffix(stream):
        """
        memberExpressionSuffix
            : indexSuffix
            | propertyReferenceSuffix
        ;
        """
        return ExpressionReader.readIndexSuffix(stream) or ExpressionReader.readPropertyReferenceSuffix(stream)
    
    @staticmethod
    def readPropertyReferenceSuffix(stream):
        """
        propertyReferenceSuffix
            : '.' LT!* Identifier
            ;
        """
        rvalue = None
        if not stream.isEnd() and stream.get() == '.':
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            idf = CodeReader.readIdentifier(stream)
            if idf == None:
                raise Exception("Expecting identifier after ." , str(stream.data[stream.cur]))
            rvalue = ExPropertyReferenceSuffix()            
            rvalue.identifier = idf            
        return rvalue
            

    @staticmethod
    def readIndexSuffix(stream):
        """
        indexSuffix
            : '[' LT!* expression LT!* ']'
        ;    
        """
        rvalue = None
        if not stream.isEnd() and stream.get() == '[':
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            exp = ExpressionReader.readExpression(stream)
            if exp == None:
                raise Exception("Expecting expression in index suffix" , str(stream.data[stream.cur]))
            CodeReader.skipWhiteSpace(stream)
            if stream.get() != ']':
                raise Exception("Expecting ]" , str(stream.data[stream.cur]))
            stream.move()
            rvalue = ExIndexSuffix()
            rvalue.exp = exp                                    
            
        return rvalue
        
    @staticmethod
    def readArguments(stream):
        """
        arguments
            : '(' (LT!* assignmentExpression (LT!* ',' LT!* assignmentExpression)*)? LT!* ')'
            ;
        """
        rvalue = None
        if not stream.isEnd() and stream.get() == '(':            
            stream.move()
            args = []
            CodeReader.skipWhiteSpace(stream)
            arg = ExpressionReader.readAssignmentExpressionAsExpression(stream)
            if arg != None:
                args.append(arg)
                
                while True:
                    CodeReader.skipWhiteSpace(stream)
                    if stream.get() == ',':
                        stream.move()
                        
                        CodeReader.skipWhiteSpace(stream)
                        arg = ExpressionReader.readAssignmentExpressionAsExpression(stream)
                        
                        if arg == None:
                            raise Exception("Expecting argument after ," , str(stream.data[stream.cur]))
                        
                        args.append(arg)
                    else:
                        break
                        
            CodeReader.skipWhiteSpace(stream)
            if stream.get() != ')':
                raise Exception("Expecting )" , str(stream.data[stream.cur]))
            stream.move()
            rvalue = ExArguments()
            rvalue.args = args
                      
                
        return rvalue
            
            
    
    @staticmethod
    def readPrimaryExpression( stream ):        
        """
        primaryExpression
        : 'this'
            | Identifier
            | literal
            | arrayLiteral
            | objectLiteral
            | '(' LT!* expression LT!* ')'
        ;
        """        
        last_pos = stream.cur
        rvalue = None
        if not stream.isEnd():
            CodeReader.skipWhiteSpace(stream)
            if stream.get() == '/':         
                #TODO:http://www-archive.mozilla.org/js/language/js20-2000-07/formal/regexp-grammar.html
                #not a standard way to read the grammer read above link to accept grammer
                #print "========="   
                value = '/'
                data     = stream.data
                cur      = stream.cur + 1
                max_size = stream.max_size
                sqb_count = 0 # []
                rb_count = 0  # (
                cb_count = 0 # {
                while ( not (sqb_count == 0 and cb_count ==0 and rb_count ==0) ) or cur < max_size and data[cur].value!='/':
                    v =data[cur].value
                    #print "RE:",sqb_count,rb_count,cb_count,v
                    if v == '\\' or ( v == '^' and data[cur+1].value not in [ '/','[','(','{',']',')','}' ]):
                        value += v + data[cur+1].value
                        cur += 2                    
                    else:
                        value += v
                        cur += 1
                        if v=="[": sqb_count+=1
                        elif v=="]":sqb_count-=1
                        elif v=="(": rb_count+=1
                        elif v==")":rb_count-=1
                        elif v=="{": cb_count+=1
                        elif v=="}":cb_count-=1 
                
                if cur >= max_size:
                    raise Exception("Unexpected end of file", str(stream.data[stream.cur]))
                value += data[cur].value
                cur += 1
                
                while cur < max_size and data[cur].value.isalpha():
                    value += data[cur].value
                    cur += 1
                
                
                stream.cur = cur
                ex= ExRegex()
                ex.value = value
                rvalue   = ex
                
            
            if rvalue == None:
                if stream.get() == "(":                                
                    stream.move() 
                    CodeReader.skipWhiteSpace(stream)
                    exp =  ExpressionReader.readExpression(stream )            
                    if exp != None:
                        CodeReader.skipWhiteSpace(stream)                
                        if stream.get() == ")":                                        
                            stream.move()
                            rvalue = ExParExpression()
                            rvalue.exp = exp
                        else:
                            stream.cur = last_pos                   
                    else:
                        stream.cur = last_pos
                else: 
                     
                    rvalue = CodeReader.readArrayLiteral(stream)
                    
                    if rvalue == None:
                        rvalue = CodeReader.readObjectLiteral(stream)
                    
                        if rvalue == None:
                            rvalue = CodeReader.readLiteral(stream)
                            if rvalue == None:
                                idf = CodeReader.readIdentifier(stream)
                                rvalue = None
                                if idf == "this":
                                    rvalue=  ExThis()
                                elif idf!=None and ( not isJSKeyword(idf) ):
                                    rvalue = ExIdentifier()
                                    rvalue.value = idf                                    
                                else:
                                    stream.cur = last_pos
        return rvalue
                  
    
    @staticmethod
    def readNewExpression(stream):
        """
        newExpression
            : memberExpression
            | 'new' LT!* newExpression
        ;
        """                
        last_pos = stream.cur
        rvalue = ExpressionReader.readMemberExpression(stream)
        if rvalue == None:
            kwNew = CodeReader.readIdentifier(stream)
            if kwNew == "new":
                CodeReader.skipWhiteSpace(stream)
                exp = ExpressionReader.readNewExpression(stream)
                if exp==None:
                    raise Exception("Expecting expression after new", str(stream.data[stream.cur]))
                rvalue = [ExNew(),exp]
            else:                
                stream.cur = last_pos
            
        return rvalue
    
    @staticmethod
    def readAssignmentOperator(stream):
        """
        assignmentOperator
            : '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '>>>=' | '&=' | '^=' | '|='
        ;
        """
        rvalue = None
        last_pos = stream.cur
        s1 = stream.get()
        if s1 in assignment_start_char:
            op = ExOperator()            
            stream.move()
            s2 = stream.get()            
            if s2 in assignment_start_char_2:
                stream.move()
                if s2 == '=':                    
                    if s1 == '*' :
                        op.value = ExOperator.ASSIGN_MULTIPLY
                    elif s1 == '/' :
                        op.value = ExOperator.ASSIGN_DIVIDE
                    elif s1 == '%' :
                        op.value = ExOperator.ASSIGN_MOD
                    elif s1 == '+' :
                        op.value = ExOperator.ASSIGN_PLUS
                    elif s1 == '-' :
                        op.value = ExOperator.ASSIGN_MINUS
                    elif s1 == '&' :
                        op.value = ExOperator.ASSIGN_BIN_AND
                    elif s1 == '^' :
                        op.value = ExOperator.ASSIGN_BIN_XOR
                    elif s1 == '|' :
                        op.value = ExOperator.ASSIGN_BIN_OR                    
                else:                    
                    s3 = stream.get()
                    if s2 == '<' and s3 == '=':
                        stream.move()
                        op.value = ExOperator.ASSIGN_LEFT_SHIFT
                    elif s2 == '>':                        
                        if s3 == '=':
                            stream.move()
                            op.value = ExOperator.ASSIGN_RIGHT_SHIFT
                        elif s3 == '>' and stream.data[ stream.cur+1 ].value == '=':
                            stream.move(2)
                            op.value = ExOperator.ASSIGN_RIGHT_RIGHT_SHIFT
            elif s1 == '=':
                op.value = ExOperator.ASSIGN_NORMAL
            
            if op.value != None:
                rvalue = op
            else:
                stream.cur = last_pos
        return rvalue
            

class CodeReader:
    
    @staticmethod
    def readProgram(stream):
        p = Program()
        elems = []
        while True:
            CodeReader.skipWhiteSpace(stream)
            elem = CodeReader.readSourceElement(stream)
            if elem != None:
                elems.append(elem)
            else:
                break
        p.sourceElements =  elems
        return p    
    
    
    @staticmethod
    def readFormalParameterList(stream):
        """
        formalParameterList
            : '(' (LT!* Identifier (LT!* ',' LT!* Identifier)*)? LT!* ')'
            ;

        """
        last_pos = stream.cur
        rvalue = None
        s = stream.get()
        
        if s == '(':
            params = []
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            idf = CodeReader.readIdentifier(stream)
            
            if idf != None:                
                params.append( Parameter( idf ) )
                
                while True:
                    CodeReader.skipWhiteSpace(stream)
                    s = stream.get()
                    if s == ',':
                        stream.move()
                        CodeReader.skipWhiteSpace(stream)
                        idf = CodeReader.readIdentifier(stream)
                        if idf == None:
                            raise Exception("Exception Identifier after ,", str(stream.data[stream.cur]))
                        params.append( Parameter( idf ) )
                    else:
                        break
                                                    
            CodeReader.skipWhiteSpace(stream)
            s = stream.get()
            
            if s == ')':
                rvalue = params
                stream.move()
            else:
                stream.cur = last_pos
        
        return rvalue
    
    @staticmethod
    def readFunctionBody(stream):
        """
        functionBody
            : '{' LT!* sourceElements LT!* '}'
            ;

        """
        
        
        rvalue = None
        s = stream.get()
        
        if s == '{':
            stream.move()            
            body = []
            while True:
                CodeReader.skipWhiteSpace(stream)
                stmt = CodeReader.readSourceElement(stream)
                
                if stmt != None:
                    body.append(stmt)
                    CodeReader.skipWhiteSpace(stream)                
                else:
                    s = stream.get()
                    if s == '}':                        
                        rvalue = StmtBlock()
                        rvalue.stmts = body
                        stream.move()
                        break
                    else:                                        
                        raise Exception("Missing } ( end of function definition )", str(stream.data[stream.cur]))
        
        return rvalue            
                    
            
    
    @staticmethod
    def readSourceElement(stream):
        """
        sourceElement
            : functionDeclaration
            | statement
            ;
        """
        CodeReader.skipWhiteSpace(stream)
        rvalue = None
        if not stream.isEnd(): 
            rvalue = CodeReader.readFunctionDeclaration(stream)
            if rvalue == None:
                rvalue = CodeReader.readStatement(stream)
        return rvalue
            
    
    @staticmethod
    def readStatement(stream):
        global cached
        CodeReader.skipWhiteSpace(stream)
        last_pos = stream.cur
        if cached.has_key(last_pos):
            tup = cached[last_pos]
            rvalue = tup[0]
            stream.cur = tup[1]
            return rvalue
            
        token = stream.get()
        rvalue = None
        if not stream.isEnd():             
            rvalue = CodeReader.readStmtBlock(stream)
            if rvalue == None and token == "if":
                rvalue = CodeReader.readStmtIf(stream)
            if rvalue == None and token == "var":
                rvalue = CodeReader.readStmtVariableDeclaration(stream)
            if rvalue == None and token == ";":
                rvalue = CodeReader.readStmtEmpty(stream)                        
            if rvalue == None and token in iterative_keywords:
                rvalue = CodeReader.readIterationStatement(stream)
            if rvalue == None and token == "return":
                rvalue = CodeReader.readStmtReturn(stream)
            if rvalue == None and token == "try":
                rvalue = CodeReader.readStmtTry(stream)
            if rvalue == None and token == "break":
                rvalue = CodeReader.readStmtBreak(stream)
            if rvalue == None and token == "throw": 
                rvalue = CodeReader.readStmtThrow(stream)            
            if rvalue == None and token == "continue":
                rvalue = CodeReader.readStmtContinue(stream)
            if rvalue == None and token == "switch":
                rvalue = CodeReader.readStmtSwitch(stream)        
            if rvalue == None and token == "with": 
                rvalue = CodeReader.readStmtWith(stream)
            if rvalue == None:
                rvalue = CodeReader.readStmtLabelled(stream)
            if rvalue == None:
                if not ( token in [')','}'] ): 
                    rvalue = CodeReader.readStmtExpression(stream)
        if rvalue != None:
            cached[last_pos] = ( rvalue , stream.cur )
        return rvalue
                                
                            
                    
                
    @staticmethod
    def readIterationStatement(stream):
        return CodeReader.readStmtForIn(stream) or CodeReader.readStmtFor(stream) or CodeReader.readStmtWhile(stream) or CodeReader.readStmtDoWhile(stream) or None
    
    @staticmethod
    def readStmtTry(stream):
        """
        tryStatement
            : 'try' LT!* statementBlock LT!* (finallyClause | catchClause (LT!* finallyClause)?)
        ;
        """
        last_pos = stream.cur
        rvalue   = None
        kwTry    = CodeReader.readIdentifier(stream)
        if kwTry == "try":
            CodeReader.skipWhiteSpace(stream)
            stmt = StmtTry()
            stmt.stmt = CodeReader.readStmtBlock(stream)
            if stmt.stmt == None:
                raise Exception("Expecting block statement after try ", str(stream.data[stream.cur]))
            CodeReader.skipWhiteSpace(stream)
            
            stmt.finallyClause = CodeReader.readFinallyClause(stream)
            if stmt.finallyClause == None:
                stmt.catchClause = CodeReader.readCatchClause(stream)
                if stmt.catchClause != None:
                    CodeReader.skipWhiteSpace(stream)
                    stmt.finallyClause =  CodeReader.readFinallyClause(stream)
                else:
                    raise Exception(" catch or finally expected after try", str(stream.data[stream.cur]))
            
            rvalue = stmt
                            
        else:
            stream.cur = last_pos
        return rvalue
    @staticmethod
    def readFinallyClause(stream):
        """
        finallyClause
            : 'finally' LT!* statementBlock
        ;
        """
        last_pos = stream.cur
        rvalue   = None
        kwFinally= CodeReader.readIdentifier(stream)
        if kwFinally == "finally":
            CodeReader.skipWhiteSpace(stream)
            stmt = FinallyClause()
            stmt.stmt = CodeReader.readStmtBlock(stream)
            if stmt.stmt == None:
                raise Exception("Expecting Block statement after finally", str(stream.data[stream.cur]))
            rvalue = stmt
        else:
            stream.cur = last_pos
        return rvalue 
    @staticmethod
    def readCatchClause(stream):
        """
        catchClause
            : 'catch' LT!* '(' LT!* Identifier LT!* ')' LT!* statementBlock
        ;
        """
        last_pos = stream.cur
        rvalue   = None
        kwCatch = CodeReader.readIdentifier(stream)
        if kwCatch == "catch":
            stmt = CatchClause()
            CodeReader.skipWhiteSpace(stream)
            s = stream.get()
            if s != '(':
                raise Exception("Expecting ( after catch", str(stream.data[stream.cur]))
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            stmt.name = CodeReader.readIdentifier(stream)
            if stmt.name == None:
                raise Exception("Identifier expected in catch clause", str(stream.data[stream.cur]))
            stmt.name = Parameter(stmt.name)
            CodeReader.skipWhiteSpace(stream)
            s = stream.get()
            if s != ')':
                raise Exception("Expecting ) after catch ( identifier ", str(stream.data[stream.cur]))
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            stmt.stmt = CodeReader.readStmtBlock(stream)
            if stmt.stmt == None:
                raise Exception("Expecting Block Statement after catch", str(stream.data[stream.cur]))
            rvalue = stmt
        else:
            stream.cur = last_pos
        return rvalue
        
    @staticmethod
    def readStmtThrow(stream):
        """
        throwStatement
            : 'throw' expression (LT | ';')!
        ;
        """
        last_pos = stream.cur
        rvalue   = None
        kwThrow  = CodeReader.readIdentifier(stream)        
        if kwThrow == "throw":            
            stmt = StmtThrow()
            CodeReader.skipWhiteSpace(stream)
            stmt.exp = ExpressionReader.readExpression(stream)
            if stmt.exp == None:
                raise Exception("Expression expected after throw", str(stream.data[stream.cur]))
            CodeReader.skipWhiteSpace(stream)
            s = stream.get()
            if s == ';':
                stream.move()
            rvalue = stmt
        else:
            stream.cur = last_pos
        return rvalue
    
    @staticmethod
    def readStmtSwitch(stream):
        """
        switchStatement
            : 'switch' LT!* '(' LT!* expression LT!* ')' LT!* caseBlock
        ;
        """
        last_pos = stream.cur
        rvalue   = None
        kwSwitch = CodeReader.readIdentifier(stream)
        if kwSwitch == "switch":
            stmt = StmtSwitch()
            CodeReader.skipWhiteSpace(stream)
            s = stream.get()
            if s != '(':
                raise Exception("Expecting ( after switch ", str(stream.data[stream.cur]))
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            
            stmt.exp = ExpressionReader.readExpression(stream )
            if stmt.exp == None:
                raise Exception("Expecting expression in switch( ", str(stream.data[stream.cur]))
            
            CodeReader.skipWhiteSpace(stream)
            s = stream.get()
            if s != ')':
                raise Exception("Expecting ) after switch( exp ", str(stream.data[stream.cur]))
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            
            stmt.cases = CodeReader.readCaseBlock(stream)
            if stmt.cases == None:
                raise Exception ("Switch case clause not found ", str(stream.data[stream.cur]))                
            
            rvalue = stmt
        else:
            stream.cur = last_pos
        return rvalue
    
    @staticmethod
    def readCaseBlock(stream):
        """
        caseBlock
            : '{' (LT!* caseClause)* (LT!* defaultClause (LT!* caseClause)*)? LT!* '}'
        ;
        """
        
        rvalue   = None
        s = stream.get()
        if s == '{':        
            stream.move()            
            cases = []
            while True:
                CodeReader.skipWhiteSpace(stream)
                clause = CodeReader.readCaseClause(stream)
                if clause != None: 
                    cases.append(clause)
                else:
                    break
            CodeReader.skipWhiteSpace(stream)
            clause = CodeReader.readDefaultClause(stream)
            if clause!= None:
                cases.append(clause)
                while True:
                    CodeReader.skipWhiteSpace(stream)
                    clause = CodeReader.readCaseClause(stream)
                    if clause != None: 
                        cases.append(clause)
                    else:
                        break
            
            CodeReader.skipWhiteSpace(stream)
            s = stream.get()
            if s != '}':
                raise Exception("Expected } after switch cases", str(stream.data[stream.cur]))        
            stream.move()
            
            rvalue = cases
            
                
        return rvalue

    @staticmethod
    def readCaseClause(stream):
        """
        caseClause
            : 'case' LT!* expression LT!* ':' LT!* statementList?
        ;
        """
        last_pos = stream.cur
        rvalue   = None
        kwCase = CodeReader.readIdentifier(stream)
        if kwCase == "case":
            clause = CaseClause()
            CodeReader.skipWhiteSpace(stream)
            clause.exp = ExpressionReader.readExpression(stream)
            CodeReader.skipWhiteSpace(stream)
            s = stream.get()
            if s != ':':
                raise Exception("Expecting : after case exp", str(stream.data[stream.cur]))        
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            clause.stmtList = CodeReader.readStatementList(stream)
            rvalue = clause            
        else:
            stream.cur = last_pos
        return rvalue
    
    @staticmethod
    def readDefaultClause(stream):
        """
        defaultClause
            : 'default' LT!* ':' LT!* statementList?
        ;
        """
        last_pos = stream.cur
        rvalue   = None
        kwDefault = CodeReader.readIdentifier(stream)
        if kwDefault == "default":
            clause = CaseClause()            
            clause.default = True
            CodeReader.skipWhiteSpace(stream)
            s = stream.get()
            if s != ':':
                raise Exception("Expecting : after default", str(stream.data[stream.cur]))        
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            clause.stmtList = CodeReader.readStatementList(stream)            
            rvalue = clause            
        else:
            stream.cur = last_pos
        return rvalue
    @staticmethod
    def readStmtLabelled(stream):
        """
        labelledStatement
            : Identifier LT!* ':' LT!* statement
        ;
        """
        last_pos = stream.cur
        rvalue = None
        label = CodeReader.readIdentifier(stream)
        CodeReader.skipWhiteSpace(stream)
        s = stream.get()
        if s == ':':        
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            stmt = StmtLabelled()
            stmt.label = label
            stmt.stmt = CodeReader.readStatement(stream)
            
            if stmt.stmt != None:
                #raise Exception("Expecting statement after label")
                rvalue = stmt
            else:
                stream.cur = last_pos            
        else:
            stream.cur = last_pos
        return rvalue
         
    
    
    @staticmethod
    def readStmtWith(stream):
        """
        withStatement
            : 'with' LT!* '(' LT!* expression LT!* ')' LT!* statement
            ;
        """
        last_pos = stream.cur
        rvalue   = None
        kwWith   = CodeReader.readIdentifier(stream)
        if kwWith == "with":
            stmt = StmtWith()
            CodeReader.skipWhiteSpace(stream)
            s = stream.get()
            if s != '(':
                raise Exception("Expecting ( after with ", str(stream.data[stream.cur]))
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            
            stmt.exp = ExpressionReader.readExpression(stream )
            
            CodeReader.skipWhiteSpace(stream)
            s = stream.get()
            if s != ')':
                raise Exception("Expecting ) after with( exp ", str(stream.data[stream.cur]))
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            
            stmt.stmt = CodeReader.readStatement(stream)
            if stmt.stmt == None:
                raise Exception("Expecting statement after ", str(stream.data[stream.cur]))
            rvalue = stmt            
            
        else:
            stream.cur = last_pos
        return rvalue
             

    
    
    @staticmethod
    def readStmtReturn(stream):
        """
        returnStatement
            : 'return' expression? (LT | ';')!
        ;
        """
        last_pos = stream.cur
        rvalue   = None
        kwReturn = CodeReader.readIdentifier(stream)
        if kwReturn == "return":            
            stmt = StmtReturn()
            CodeReader.skipWhiteSpace(stream)
            stmt.exp = ExpressionReader.readExpression( stream )
            
            CodeReader.skipWhiteSpace(stream)            
            s = stream.get()
            if s == ";":
                stream.move()
                
            rvalue = stmt            
        else:
            stream.cur = last_pos
        return rvalue

    @staticmethod
    def readStmtContinue(stream):
        """
        continueStatement
            : 'continue' Identifier? (LT | ';')!
            ;
        """
        last_pos = stream.cur
        kwContinue = CodeReader.readIdentifier(stream)
        rvalue = None
        if kwContinue == "continue":
            stmt = StmtContinue()
            CodeReader.skipWhiteSpace(stream)
            stmt.label = CodeReader.readIdentifier(stream)
            CodeReader.skipWhiteSpace(stream)
            
            s = stream.get()
            if s == ";":
                stream.move()
                
            rvalue = stmt
        else:
            stream.cur = last_pos
        return rvalue
    
    
    @staticmethod
    def readStmtBreak(stream):
        """
        breakStatement
            : 'break' Identifier? (LT | ';')!
            ;
        """
        last_pos = stream.cur
        kwBreak = CodeReader.readIdentifier(stream)
        rvalue = None
        if kwBreak == "break":
            stmt = StmtBreak()
            CodeReader.skipWhiteSpace(stream)
            stmt.label = CodeReader.readIdentifier(stream)
            CodeReader.skipWhiteSpace(stream)
            
            s = stream.get()
            if s == ";":
                stream.move()                
            rvalue = stmt
        else:
            stream.cur = last_pos
        return rvalue
    
    @staticmethod
    def readStmtExpression(stream):
        """
        expressionStatement
            : expression (LT | ';')!
            ;
        """
        rvalue = None
        exp = ExpressionReader.readExpression(stream)
        if exp != None:
            rvalue = StmtExpression()
            rvalue.exp = exp
            CodeReader.skipWhiteSpace(stream)
            s = stream.get()
            if s == ";":
                stream.move()
        return rvalue

    @staticmethod
    def readStmtForIn(stream):
        """
        forInStatement
            : 'for' LT!* '(' LT!* forInStatementInitialiserPart LT!* 'in' LT!* expression LT!* ')' LT!* statement
        ;
        """
        last_pos = stream.cur
        rvalue = None
        kwFor = CodeReader.readIdentifier(stream)
        if kwFor == "for":
            
            CodeReader.skipWhiteSpace(stream)
            s = stream.get()
            if s != '(':
                raise Exception("Expecting ( after for ", str(stream.data[stream.cur]))
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            
            init = CodeReader.readForInStatementInitialiserPart(stream)
            if init!=None:
                CodeReader.skipWhiteSpace(stream)
                
                kwIn = CodeReader.readIdentifier(stream)
                if kwIn == "in":
                    stmt = StmtForIn()
                    stmt.init = init
                    CodeReader.skipWhiteSpace(stream)
                    stmt.exp = ExpressionReader.readExpression(stream)
                    if stmt.exp == None:
                        raise Exception("Expecting expression after for(init in " , str(stream.data[stream.cur]))
                    
                    CodeReader.skipWhiteSpace(stream)
                    s = stream.get()
                    if s != ')':
                        raise Exception("Expecting ) after for for(init in exp", str(stream.data[stream.cur]))
                    stream.move()
                    CodeReader.skipWhiteSpace(stream)
                    
                    stmt.stmt = CodeReader.readStatement(stream)
                    if stmt.stmt == None:
                        raise Exception("Expecting statement after for(init in exp)", str(stream.data[stream.cur]))
                    
                    rvalue = stmt                                        
                    
                else:
                    stream.cur = last_pos
            else:
                stream.cur = last_pos
        else:
            stream.cur = last_pos
        return rvalue
        
    
    @staticmethod
    def readStmtFor(stream):
        """
        forStatement
            : 'for' LT!* '(' (LT!* forStatementInitialiserPart)? LT!* ';' (LT!* expression)? LT!* ';' (LT!* expression)? LT!* ')' LT!* statement
        ;
        """
        last_pos = stream.cur
        rvalue = None
        kwFor = CodeReader.readIdentifier(stream)
        if kwFor == "for":
            stmt = StmtFor()
            CodeReader.skipWhiteSpace(stream)
            s = stream.get()
            if s != '(':
                raise Exception("Expecting ( after for ", str(stream.data[stream.cur]))
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            
            stmt.init = CodeReader.readForStatementInitialiserPart(stream)
            CodeReader.skipWhiteSpace(stream)
            
            s = stream.get()
            if s != ';':
                raise Exception("Expecting ; after for ( init ", str(stream.data[stream.cur]))
            stream.move()
            
            CodeReader.skipWhiteSpace(stream)
            stmt.condition = ExpressionReader.readExpression(stream)
            CodeReader.skipWhiteSpace(stream)
            
            s = stream.get()
            if s != ';':
                raise Exception("Expecting ; after for ( init ; condition ", str(stream.data[stream.cur]))
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            stmt.exp = ExpressionReader.readExpression(stream)

            CodeReader.skipWhiteSpace(stream)
            
            s = stream.get()
            if s != ')':
                raise Exception("Expecting ; after for ( init ; condition ; exp ", str(stream.data[stream.cur]))
            stream.move()
            
            CodeReader.skipWhiteSpace(stream)
            
            stmt.stmt = CodeReader.readStatement(stream)
            
            if stmt.stmt == None:
                raise Exception("Expecting statement after for(;;)", str(stream.data[stream.cur]))
            
            rvalue = stmt            
            
        else:
            stream.cur = last_pos
        return rvalue
            
    @staticmethod
    def readForStatementInitialiserPart(stream):
        """
        forStatementInitialiserPart
            : expressionNoIn
            | 'var' LT!* variableDeclarationListNoIn
            ;

        """
        rvalue = ExpressionReader.readExpression(stream, True)
        if rvalue == None:
            last_pos =  stream.cur            
            kwVar = CodeReader.readIdentifier(stream)
            if kwVar == "var":
                CodeReader.skipWhiteSpace(stream)
                rvalue = CodeReader.readVariableDeclarationList(stream, True)
                if rvalue == None:
                    raise Exception("Expecting variable initializer", str(stream.data[stream.cur]))
            else:
                stream.cur = last_pos
        return rvalue
            
    
    @staticmethod
    def readForInStatementInitialiserPart(stream):
        """
        forInStatementInitialiserPart
            : leftHandSideExpression
            | 'var' LT!* variableDeclarationNoIn
            ;
        """
        rvalue = None
        parts = ExpressionReader.readLeftHandSideExpression(stream)        
        if parts != None:
            ex = Expression()
            ex.parts = parts
            rvalue = ex
        else:
            last_pos =  stream.cur            
            kwVar = CodeReader.readIdentifier(stream)
            if kwVar == "var":
                CodeReader.skipWhiteSpace(stream)
                rvalue = CodeReader.readVariableDeclaration(stream, True)
                if rvalue == None:
                    raise Exception("Expecting variable initializer", str(stream.data[stream.cur]))
            else:
                stream.cur = last_pos
        return rvalue
        
    
        
    @staticmethod
    def readStmtIf(stream):
        """
        ifStatement
            : 'if' LT!* '(' LT!* expression LT!* ')' LT!* statement (LT!* 'else' LT!* statement)?
        ;
        """
        last_pos = stream.cur
        rvalue = None
        kwIf = CodeReader.readIdentifier(stream)
        if kwIf == 'if':
            stmt = StmtIf()
            CodeReader.skipWhiteSpace(stream)
            s = stream.get()
            if s != '(':
                raise Exception("Expecting ( after if ", str(stream.data[stream.cur]))
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            
            stmt.condition = ExpressionReader.readExpression(stream)
            if stmt.condition == None:
                raise Exception("Expecting condition for if statement ", str(stream.data[stream.cur]))
            
            CodeReader.skipWhiteSpace(stream)
            s = stream.get()
            if s != ')':
                raise Exception("Expecting ) after if condition", str(stream.data[stream.cur]))
            
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            stmt.if_stmt = CodeReader.readStatement(stream)
            
            if stmt.if_stmt == None:
                raise Exception("Expecting statement after if", str(stream.data[stream.cur]))
            
            
            last_pos = stream.cur
            CodeReader.skipWhiteSpace(stream)
            kwElse = CodeReader.readIdentifier(stream)
            
            if kwElse == 'else':
                CodeReader.skipWhiteSpace(stream)
                stmt.else_stmt = CodeReader.readStatement(stream)
                
                if stmt.else_stmt == None:
                    raise Exception("Expecting statement after else", str(stream.data[stream.cur]))
            else:
                stream.cur = last_pos
            rvalue = stmt
        else:
            stream.cur = last_pos
        return rvalue

    @staticmethod
    def readStmtWhile(stream):
        """
        whileStatement
            : 'while' LT!* '(' LT!* expression LT!* ')' LT!* statement
            ;
        """
        last_pos = stream.cur
        rvalue = None
        kwWhile = CodeReader.readIdentifier(stream)
        if kwWhile == "while":
            stmt = StmtWhile()
            CodeReader.skipWhiteSpace(stream)
            s = stream.get()
            if s != "(":
                raise Exception("Expecting ( after while", str(stream.data[stream.cur]))            
            
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            stmt.condition = ExpressionReader.readExpression(stream)
            
            CodeReader.skipWhiteSpace(stream)
            
            s = stream.get()
            if s != ")":
                raise Exception("Expecting ) after while( condition ", str(stream.data[stream.cur]))            
            
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            
            stmt.stmt = CodeReader.readStatement(stream)
            
            if stmt.stmt== None:
                raise Exception("Expecting statement after while()", str(stream.data[stream.cur]))
            rvalue = stmt                        
        else:
            stream.cur = last_pos
        return rvalue
    @staticmethod
    def readStmtDoWhile(stream):
        """
        doWhileStatement
            : 'do' LT!* statement LT!* 'while' LT!* '(' expression ')' (LT | ';')!
        ;
        """
        last_pos = stream.cur
        rvalue = None
        kwDo = CodeReader.readIdentifier(stream)
        if kwDo == "do":
            stmt = StmtDoWhile()
            CodeReader.skipWhiteSpace(stream)
            stmt.stmt = CodeReader.readStatement(stream)
            CodeReader.skipWhiteSpace(stream)
            
            kwWhile = CodeReader.readIdentifier(stream)
            if kwWhile!= 'while':
                raise Exception("Expecting while after do statement", str(stream.data[stream.cur]))
            
            CodeReader.skipWhiteSpace(stream)
            s = stream.get()
            if s != "(":
                raise Exception("Expecting ( after do.. while", str(stream.data[stream.cur]))
            
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            stmt.condition = ExpressionReader.readExpression(stream)
            
            if stmt.condition == None:
                raise Exception("Expecting Condition in do .. while( )", str(stream.data[stream.cur]))
            
            CodeReader.skipWhiteSpace(stream)
            s = stream.get()
            if s != ")":
                raise Exception("Expecting ) after do.. while", str(stream.data[stream.cur]))
            
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            
            s = stream.get()
            if s == ";":
                stream.move()
            rvalue = stmt
                        
        else:
            stream.cur = last_pos
        return rvalue
        
            
    @staticmethod
    def readStmtEmpty(stream):
        """
        emptyStatement
            : ';'
            ;
        """
        rvalue = None
        s = stream.get()
        if s == ";":
            stream.move()
            rvalue = StmtEmpty()
        return rvalue
            
    @staticmethod
    def readStmtBlock(stream):
        """
        statementBlock
            : '{' LT!* statementList? LT!* '}'
            ;

        """
        rvalue = None        
        s = stream.get()
        if s == '{':            
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            stmt_list = CodeReader.readStatementList(stream)
            CodeReader.skipWhiteSpace(stream)
            
            s = stream.get()
            if s != '}':
                raise Exception("Invalid end of statement block , expecting }", str(stream.data[stream.cur]))
            
            stream.move()
            rvalue = StmtBlock()
            rvalue.stmts = stmt_list
        
        return rvalue
    
    @staticmethod
    def readStatementList(stream):
        """
        statementList
            : statement (LT!* statement)*
            ;

        """
        rvalue = None
        stmt = CodeReader.readStatement(stream)

        if stmt != None:
            rvalue = []
        
            while stmt != None:
                rvalue.append(stmt)
                CodeReader.skipWhiteSpace(stream)
                stmt = CodeReader.readStatement(stream)
                
        return rvalue
    
    @staticmethod
    def readStmtVariableDeclaration(stream , noIn = False):
        """
        variableStatement
            : 'var' LT!* variableDeclarationList (LT | ';')!
            ;
        """
        last_pos = stream.cur
        rvalue   = None
        kwVar    = CodeReader.readIdentifier(stream)
        if kwVar == 'var':
            CodeReader.skipWhiteSpace(stream)
            vdl = CodeReader.readVariableDeclarationList(stream , noIn)
            CodeReader.skipWhiteSpace(stream)
            s = stream.get()
            
            if s == ";":
                stream.move()
            
            rvalue = StmtVariableDeclaration()
            rvalue.vdl = vdl
        else:
            stream.cur = last_pos
        return rvalue
    
    @staticmethod
    def readVariableDeclarationList(stream , noIn = False):
        """
        variableDeclarationList
            : variableDeclaration (LT!* ',' LT!* variableDeclaration)*
            ;
        """
        rvalue = None
        vd = CodeReader.readVariableDeclaration(stream , noIn )
        if vd != None:
            vd_list = [vd]
            while True:
                CodeReader.skipWhiteSpace(stream)
                s = stream.get()
                if s == ",":
                    stream.move()                   
                    CodeReader.skipWhiteSpace(stream)
                    vd = CodeReader.readVariableDeclaration(stream , noIn )                    
                    if vd == None:
                        raise Exception("Expecting variable declaration after , ", str(stream.data[stream.cur]))
                    vd_list.append(vd)                                            
                else:
                    break
            rvalue = vd_list
        return rvalue
            
    
    @staticmethod
    def readVariableDeclaration(stream , noIn = False):
        """
        variableDeclaration
            : Identifier LT!* initialiser?
            ;
        """
        rvalue = None
        name = CodeReader.readIdentifier(stream)
        if name != None:
            CodeReader.skipWhiteSpace(stream)
            vd = VariableDeclaration()
            vd.name = name
            vd.init = CodeReader.readInitialiser(stream , noIn)
            rvalue = vd
            
        return rvalue
            
        
    @staticmethod
    def readInitialiser(stream , noIn = False):
        """
        initialiser
            : '=' LT!* assignmentExpression
            ;
        """
        s = stream.get()
        rvalue = None
        if s == "=":
            stream.move()
            CodeReader.skipWhiteSpace(stream)
            rvalue = ExpressionReader.readAssignmentExpressionAsExpression(stream, noIn)
            if rvalue == None:
                raise Exception("Expecting Assignment Expression", str(stream.data[stream.cur]))
        return rvalue
            
        

    
    @staticmethod
    def readFunctionDeclaration(stream):
        """
        functionDeclaration
            : 'function' LT!* Identifier LT!* formalParameterList LT!* functionBody
            ;
    
        functionExpression
            : 'function' LT!* Identifier? LT!* formalParameterList LT!* functionBody
            ;
        """
        rvalue = None
        last_pos = stream.cur
        line = stream.data[stream.cur].line
        pos  = stream.data[stream.cur].pos
        kwFunction = CodeReader.readIdentifier(stream)
                        
        if kwFunction == "function":                                    
            fd = FunctionDeclaration()            
            CodeReader.skipWhiteSpace(stream)
            fd.name = CodeReader.readIdentifier(stream)
            CodeReader.skipWhiteSpace(stream)
            fd.params = CodeReader.readFormalParameterList(stream)
            
            if fd.params == None:
                raise Exception("Missing formal parameters", str(stream.data[stream.cur]))
            CodeReader.skipWhiteSpace(stream)
            fd.body = CodeReader.readFunctionBody(stream)
            
            if fd.body == None:
                raise Exception("Missing body for function", str(stream.data[stream.cur]))
            fd.line = line
            fd.pos  = pos
            rvalue = fd
            
        else:
            stream.cur = last_pos
        return rvalue
                
    @staticmethod
    def readArrayLiteral(stream):
        """
        // arrayLiteral definition.
        arrayLiteral
            : '[' LT!* assignmentExpression? (LT!* ',' (LT!* assignmentExpression)?)* LT!* ']'
        ;
        """
        last_pos = stream.cur
        rvalue = None
        s = stream.get()
        if s == '[':
            al = ArrayLiteral() 
            stream.move()
            
            while True:
                
                CodeReader.skipWhiteSpace(stream)
                """
                ar = [54,,67]//need to accept no value in array
                """            
                exp = ExpressionReader.readAssignmentExpressionAsExpression(stream)
                
                if exp != None:
                    al.values.append(exp)

                else:
                    if stream.get() == ']':
                        #raise Exception("Parse Error expecting ]", str(stream.data[stream.cur]))
                        #else:
                        stream.move()
                        rvalue = al
                        break
                    elif stream.get() == ',':
                        idf = ExIdentifier()
                        idf.value = ""
                        al.values.append(idf)

                CodeReader.skipWhiteSpace(stream)
                
                if stream.get() == ',':
                    stream.move()
                    CodeReader.skipWhiteSpace(stream)
            if rvalue == None:
                stream.cur = last_pos
        
        return rvalue
        
    
    @staticmethod
    def readObjectLiteral(stream):
        """
        // objectLiteral definition.
        objectLiteral
            : '{' LT!* propertyNameAndValue (LT!* ',' LT!* propertyNameAndValue)* LT!* '}'
        ;
        """
        last_pos = stream.cur
        
        if cached.has_key(last_pos):
            tup = cached[last_pos]
            rvalue = tup[0]
            stream.cur = tup[1]
            return rvalue
        
        rvalue = None
        s = stream.get()
        if s == '{':            
            ol = ObjectLiteral()
            stream.move()
            
            while True:
                
                CodeReader.skipWhiteSpace(stream)            
                prop = CodeReader.readPropertyNameAndValue(stream)
                
                if prop != None:                    
                    ol.props.append(prop)                            
                else:
                    if stream.get() != '}':                        
                        raise Exception("Parse Error ,expecting }", str(stream.data[stream.cur]))
                    else:
                        stream.move()
                        rvalue = ol
                        break

                CodeReader.skipWhiteSpace(stream)
                
                if stream.get() == ',':
                    stream.move()
                    CodeReader.skipWhiteSpace(stream)
            if rvalue == None:
                stream.cur = last_pos 
        if rvalue != None:
            cached[last_pos] = ( rvalue , stream.cur )          
        
        return rvalue
                
                     
                        
                                            
                
                
                
                
            
    
    
    @staticmethod
    def readPropertyNameAndValue(stream):
        """
        propertyNameAndValue
            : propertyName LT!* ':' LT!* assignmentExpression
            ;
        """
        last_pos = stream.cur
        rvalue = None
        if not stream.isEnd():
            name = CodeReader.readPropertyName(stream)
            if name != None:
                CodeReader.skipWhiteSpace(stream)
                s = stream.get()
                if s == ':':
                    stream.move()
                    CodeReader.skipWhiteSpace(stream)
                    value = ExpressionReader.readAssignmentExpressionAsExpression(stream)
                    if value != None:
                        p = Property()
                        p.name = name
                        p.value = value
                        rvalue = p
    
            if rvalue == None:
                stream.cur = last_pos
        return rvalue


    @staticmethod
    def skipWhiteSpace(stream):
        if  stream.last_ws_skipped != stream.cur and not stream.isEnd():
            data = stream.data
            cur  = stream.cur
            max_size = stream.max_size
            while cur < max_size:                
                tok = data[cur].value                
                ntok = None
                if cur + 1 < max_size:
                    ntok = data[cur + 1].value                
                if tok == " " :
                    cur +=1 
                elif tok == "/" and ntok == "/":
                    cur += 2                    
                    while cur < max_size:
                        tok = data[cur].value                        
                        if len(tok)==1 :
                            v = ord(tok)
                            if v == CHR_NL or v == CHR_CR:
                                break
                        cur += 1                    
                    cur += 1
                elif tok == "/" and ntok == "*":
                    cur += 2
                    while cur + 1  < max_size and not ( data[cur].value == '*' and data[cur + 1].value == '/' ):
                        cur += 1                                            
                    cur += 2
                elif tok in whitespace:
                    cur += 1
                else:
                    break
            stream.cur = cur
            stream.last_ws_skipped = cur
           
    
    @staticmethod
    def readPropertyName(stream):   
        """
        propertyName
        : Identifier
        | StringLiteral
        | NumericLiteral
    ;

        """     
        return CodeReader.readStringLiteral(stream) or CodeReader.readNumericLiteral(stream) or CodeReader.readIdentifier(stream)   
            
    @staticmethod
    def readLiteral(stream):        
        """
        literal
            : 'null'
            | 'true'
            | 'false'
            | StringLiteral
            | NumericLiteral
        ;
        """
        
        rvalue = None        
        tok = stream.get()
        
        if tok == "true" or tok == "false" or tok == "null":
            rvalue = ExLiteral()
            rvalue.type = ExLiteral.KEYWORD
            
            if tok == "true":
                rvalue.value = ExLiteral.TRUE
            elif tok == "false":
                rvalue.value = ExLiteral.FALSE
            else:
                rvalue.value = ExLiteral.NULL
            
            stream.move()
                            
        if rvalue == None:
            data = CodeReader.readStringLiteral(stream)
            
            if data != None:
                rvalue = ExLiteral()
                rvalue.type = ExLiteral.STRING
                rvalue.value = data
            
            if rvalue == None:
                data = CodeReader.readNumericLiteral(stream)
                if data != None:
                    rvalue = ExLiteral()
                    rvalue.type = ExLiteral.NUMERIC
                    rvalue.value = data
        return rvalue
    
    @staticmethod
    def readNumericLiteral(stream):
        """
        NumericLiteral
            : DecimalLiteral
            | HexIntegerLiteral
        ;
        """
        return FragmentReader.readHexIntegerLiteral(stream) or FragmentReader.readDecimalLiteral(stream)        
    
    @staticmethod
    def readStringLiteral(stream):        
        rvalue = None
        last_pos = stream.cur
        tok = stream.get()
        if tok == '"' or tok == "'":
            endChar  = tok
            value    = endChar
            data     = stream.data
            max_size = stream.max_size
            cur      = stream.cur
            cur += 1
            tok = data[cur].value
            while cur < max_size and tok != endChar:
                if tok == '\\':
                    value += tok
                    cur += 1
                    tok    = data[cur].value
                value += tok
                cur   += 1
                tok    = data[cur].value
            if tok == endChar:
                value += tok
                rvalue = value
                stream.cur = cur + 1 
            else:
                stream.cur = last_pos
        return rvalue
        
    
    @staticmethod
    def readIdentifier(stream): 
               
        rvalue = None
        if not stream.isEnd():
            tok = stream.get()
            char = tok[0]
            if char.isalpha() or char == "$" or char == "_":
                rvalue = tok                
                stream.move()               
                        
        return rvalue    
                
class FragmentReader:
    
    @staticmethod
    def readUnicodeLetter(stream):
        rvalue = None                
        s = stream.get()
        if s.isalpha():
            stream.move()
            rvalue = s
        return rvalue
    
    @staticmethod
    def readUnicodeConnectorPunctuation(stream):
        rvalue = None                
        s = stream.get()
        if s in idf_sym:
            stream.move() 
            rvalue = s
        return rvalue
    
    @staticmethod
    def readUnicodeDigit(stream):
        rvalue = None                
        s = stream.get()
        if s.isdigit():
            stream.move() 
            rvalue = s
        return rvalue    
    
    
    @staticmethod
    def readIdentifierStart(stream):
        """
        fragment IdentifierStart
            : UnicodeLetter
            | '$'
            | '_'
            | '\\' UnicodeEscapeSequence
        ;
        """
        #TODO:UnicodeEscapeSequence is not handled
        rvalue = None
        if not stream.isEnd():            
            s = FragmentReader.readUnicodeLetter(stream);
            if s != None:
                rvalue = s
            else:
                s = stream.get()
                if s in idf_sym:
                    rvalue = s
                    stream.move()
        return rvalue

    @staticmethod
    def readIdentifierPart(stream):
        """
        fragment IdentifierPart
            : (IdentifierStart) => IdentifierStart // Avoids ambiguity, as some IdentifierStart chars also match following alternatives.
            | UnicodeDigit
            | UnicodeConnectorPunctuation
        ;
        """
        rvalue = None
        if not stream.isEnd():
            rvalue = FragmentReader.readIdentifierStart(stream)
            
            if rvalue == None:
                rvalue = FragmentReader.readUnicodeDigit(stream)
                if rvalue == None:
                    rvalue = FragmentReader.readUnicodeConnectorPunctuation(stream)
            
        return rvalue
    
    @staticmethod
    def readExponentPart(stream):
        """
        fragment ExponentPart
            : ('e' | 'E') ('+' | '-') ? DecimalDigit+
        ;
        """
        last_pos = stream.cur
        rvalue = None         
        if not stream.isEnd():            
            s = stream.get()            
            l = len(s)
            if s[0] == 'e' or s[0] == 'E':                                                                            
                if l > 1:                                                 
                    if s[1:].isdigit():
                        rvalue = s
                        stream.move()
                elif l == 1:                    
                    stream.move()
                    ns = stream.get()
                    if ( ns[0] == '+' or ns[0] == '-' ):
                        data = s + ns 
                        stream.move()
                        s = stream.get()
                        if s.isdigit():
                            data += s
                            rvalue = data
                            stream.move()
                        
        if rvalue == None:
            stream.cur = last_pos
        return rvalue
                        
    @staticmethod
    def readDecimalLiteral(stream):
        """
        fragment DecimalLiteral
            : DecimalDigit+ '.' DecimalDigit* ExponentPart?
            | '.'? DecimalDigit+ ExponentPart?
            ;
        """
        last_pos = stream.cur
        dec_digit = FragmentReader.readDecimalDigit(stream)
        rvalue = None
        if dec_digit != None:
            data = ""
            while dec_digit != None:
                data += dec_digit
                dec_digit = FragmentReader.readDecimalDigit(stream)
            
            s = stream.get()
            if s == '.':
                stream.move()
                data += s                
                
                dec_digit = FragmentReader.readDecimalDigit(stream)
                
                while dec_digit != None:
                    data += dec_digit
                    dec_digit = FragmentReader.readDecimalDigit(stream)
                
                exp = FragmentReader.readExponentPart(stream)
                
                if exp != None:
                    data += exp
                
                rvalue = data

        if rvalue == None:
            stream.cur = last_pos
            data = ""
            s = stream.get()
            if s == '.':
                stream.move()
                data += s
            
            dec_digit = FragmentReader.readDecimalDigit(stream)

            if dec_digit != None:                
                
                while dec_digit != None:
                    data += dec_digit
                    dec_digit = FragmentReader.readDecimalDigit(stream)
                
                exp = FragmentReader.readExponentPart(stream)
                
                if exp != None:
                    data += exp
                
                rvalue = data
            else:
                stream.cur = last_pos
                     
        return rvalue
                
    @staticmethod
    def readHexIntegerLiteral(stream):
        """
        fragment HexIntegerLiteral
            : '0' ('x' | 'X') HexDigit+
        ;
        """
        rvalue = None
        last_pos = stream.cur
        s = stream.get()
        
        if s == '0':
            stream.move()            
            s = stream.get()            
            
            if (s[0] == 'x' or s[0] == 'X') and FragmentReader.isHex(s[1:]):
                rvalue = '0'+s
                stream.move()                
        
        if rvalue == None:
            stream.cur = last_pos
            
        return rvalue
    
    @staticmethod
    def readDecimalDigit(stream):
        """
        fragment DecimalDigit
            : ('0'..'9')
        ;
        """
        rvalue = None
        s = stream.get()
        if s.isdigit():
            rvalue = s
            stream.move() 
        return rvalue 
    
    @staticmethod
    def isHex(value):        
        rvalue = True
        index = 0
        l = len(value)        
        while index>l:
            ch = value[index]
            if not ( ch.isdigit() or ( ch.isalpha() and ch in hex_alpha) ):
                rvalue = False
                break    
            index+=1
        return rvalue
    
    @staticmethod
    def readHexDigit(stream):
        """
        fragment HexDigit
            : DecimalDigit | ('a'..'f') | ('A'..'F')
        ;
        """
        s = stream.get()
        if s!=None and len(s)>0 and FragmentReader.isHex(s):
            rvalue = s                                
            stream.move()
            
        return rvalue
            
def jscompress(data,reducePropertyNames = False):
    sys.setrecursionlimit(90000)
    toks = tokenize(data)
    s = Stream(toks)    
    p = None
    #st = getTime()
    global cached
    cached = {}
    Closure.propNameOpt = reducePropertyNames
    Closure.globals = {}
    Closure.closures = []
    Closure.idfrefs = []
    Closure.undeclared = []
    Closure.usedvars = []
    Closure.unusedvars = []        
    Closure.duplicatevars =[]     
    Closure.global_var_count = 0    
    Closure.currentClosure = None
    Closure.topmostClosure = None
    #try:
    p = CodeReader.readProgram(s)
    p.assignIDs()
    Closure.processVars()
    #except Exception,e:
    #    print e , s.data[s.cur]                    
    #return p.toMC()# [ Closure.undeclared  ]
    cc = p.toMC()
    
    return [cc, Closure.globals.keys(), Closure.undeclared, Closure.unusedvars ]    
    
        
        
if __name__ == "__main__":
    infile = None
    outfile = None
    for arg in sys.argv[1:]:
        arg = arg.strip()
        if infile == None:
            infile = arg
        else:
            outfile = arg
    if infile == None:
        print """
            Usage:
                python Minify.py <infile> [<outfile>]
        """
    
    elif os.path.exists(infile):
        data = open(infile,"r").read()
        (compressed_code, globals , undeclared , unusedvars ) = jscompress(data, reduce )
        if outfile==None:
            print compressed_code
        else:
            open(outfile,"w").write(compressed_code)
            print "Undeclared:",undeclared
            print "Unusedvars:",unusedvars
            print "globals:",globals
    else:
        print "File not found :" +infile
    
    #sorted_h = sorted(ExLiteral.h.iteritems(), key=operator.itemgetter(1), reverse=True)
    #for k in sorted_h:
    #    print k 
    #print Closure.undeclared
    #print Closure.globals
    #print p.toMC(),
    