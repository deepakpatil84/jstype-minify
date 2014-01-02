'''
Created on Oct 8, 2012

@author: Deepak Patil
'''

import operator
TAB_SIZE = 4

keywords = ["break", "case", "catch", "continue", "debugger", "default", "delete", "do", "else", "finally", "for", "function", "if", "in", "instanceof", "new", "return", "switch", "this", "throw", "try", "typeof", "var", "void", "while", "with"]
id_chars = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']

def isJSKeyword(idf):
    return idf in keywords
   
    
    

class Closure:
    propNameOpt = False
    globals = {}
    closures = []
    idfrefs = []
    undeclared = []
    usedvars = []
    unusedvars = []
    duplicatevars = []            
    global_var_count = 0    
    currentClosure = None
    topmostClosure = None        
    
    @staticmethod
    def addRef(name):
        if name == None:
            raise Exception("Null " +name)
        
        if Closure.currentClosure != None :
            Closure.currentClosure.idfrefs.append(name)
        else:
            Closure.idfrefs.append(name)
        
    @staticmethod
    def addVar(name, obj):
        
        if name == None:
            raise Exception("Null " +name)
        
        if obj == None:
            raise Exception("Null " + str(obj))

        if Closure.currentClosure == None:
            if not Closure.globals.has_key(name):
                Closure.globals[name] = obj
                Closure.global_var_count += 1                            
        else:
            if not Closure.currentClosure.vars.has_key(name):
                Closure.currentClosure.vars[name] = obj
                Closure.currentClosure.var_count += 1
            else:
                Closure.duplicatevars.append(name)
            
    
    @staticmethod
    def getVar(name):
        if name == None:
            raise Exception("Null " +name)       
        return Closure.getVarImpl(Closure.currentClosure , name)
    
    @staticmethod
    def getVarImpl(closure, name):
         
        rvalue = None
        if closure == None:
            if Closure.globals.has_key(name):
                rvalue = Closure.globals[name]
        else:            
            if closure.vars.has_key(name):
                rvalue = closure.vars[name]
            else:
                rvalue = Closure.getVarImpl(closure.parent, name)
        return rvalue
        
    @staticmethod
    def processVars():
        
        for v in Closure.globals:
            var = Closure.globals[v]
            Closure.usedvars.append(var.name)
        
        for idf in Closure.idfrefs:
            value = idf
            if not isinstance(idf, str):
                value = idf.value                
            #else:
            #    print "XX ",value
            ref = Closure.getVarImpl(None, value)
            
            if ref == None:
                if value not in Closure.undeclared:
                    Closure.undeclared.append(value)                    
            else:
                if not isinstance(idf, str):
                    idf.decl = ref
            Closure.usedvars.append(value)
            
        for closure in Closure.closures:
            if Closure.propNameOpt and closure.propnames != None:
                #sorted_h = sorted(closure.propnames.iteritems(), key=operator.itemgetter(1), reverse=True)                
                vds = StmtVariableDeclaration()
                vds.vdl = []
                for name in closure.propnames:  
                    if closure.propnames[name] > 2 and len(name)>3:    
                        vd = VariableDeclaration()
                        vd.name = "__s_"+name+"_"
                        vd.usecount = closure.propnames[name]
                        vds.vdl.append( vd )
                        i = ExIdentifier()                                                                
                        closure.vars[ vd.name ] = vd
                        exp = Expression()
                        slit = ExLiteral()
                        slit.type = ExLiteral.STRING 
                        slit.value = "\""+name+"\""
                        exp.parts = [slit]
                        vd.init = exp
                #closure.body.insert(0,vds)
                closure.body.stmts.insert(0,vds)
            Closure.assignIDsToClosureVar(closure, 0)
        
            
        for closure in Closure.closures:
            Closure.assignNamesToClosureVar(closure)

    @staticmethod
    def assignNamesToClosureVar(closure):
        usedvars = []
        for name in closure.toprefs:
            ref = Closure.getVarImpl(closure, name)
            if ref == None:
                usedvars.append(name)
            else:
                if ref.shortname != None:
                    usedvars.append(ref.shortname)
                else:
                    usedvars.append(name)
                
        
        h = {}
        
        for v in closure.vars:            
            var = closure.vars[v]            
            h[var] = var.usecount
        sorted_h = sorted(h.iteritems(), key=operator.itemgetter(1), reverse=True)
        
        count = 0
        for pair in sorted_h:            
            var = pair[0]
            var.shortname = Closure.getVarNameForVIDEx(count, usedvars)
            usedvars.append(var.shortname)
            count += 1
                               
        for cc in closure.childrens:
            Closure.assignNamesToClosureVar(cc)
            
    @staticmethod
    def assignIDsToClosureVar(closure,start):
        
        #for v in closure.vars:
        #    obj = closure.vars[v]
        #    obj.usecount = 1                
        
        for idf in closure.idfrefs:
            value = None
            if isinstance(idf,str):
                #print "_YY_",idf
                value = idf
                ref = Closure.getVarImpl(closure, value)
                if ref != None:
                    if not closure.vars.has_key(value):
                        if value not in closure.toprefs:
                            closure.toprefs.append(value)
            else:
                value = idf.value
                ref = Closure.getVarImpl(closure, value)
                if ref == None:
                    if value not in Closure.undeclared:
                        Closure.undeclared.append(value)
                        Closure.usedvars.append(value)
                    if value not in closure.toprefs:
                        closure.toprefs.append(value)   
                else:
                    idf.decl = ref
                    ref.usecount += 1
                    if not closure.vars.has_key(value):
                        if value not in closure.toprefs:
                            closure.toprefs.append(value)

        for cc in closure.childrens:
            Closure.assignIDsToClosureVar(cc, start)
            for idf in cc.toprefs:
                if not closure.vars.has_key(idf):
                    if idf not in closure.toprefs:
                        closure.toprefs.append(idf)


    @staticmethod
    def getVarNameForVIDEx(vid,usedvars):        
        rvalue = Closure.getVarNameForVIDImplEx(vid,usedvars)        
        return rvalue
    
    
    @staticmethod
    def __unused__getVarNameForVIDImplEx1(vid,usedvars):                                            
        if vid >= 52:
            r = vid % 52
            d = (vid - r) / 52                    
            rvalue = id_chars[d] + id_chars[r] #TODO:revisit for more var greater than 2 char
            while ( rvalue  in usedvars ) or isJSKeyword(rvalue):
                vid += 1
                tvid = vid
                name = ""
                while tvid > 52:                    
                    r = tvid % 52
                    name = id_chars[r] + name                    
                    tvid = (tvid - r) / 52                
                rvalue = id_chars[tvid] + name[::-1] #TODO:revisit for more var greater than 2 char                                                                                         
        else:
            rvalue = id_chars[vid]
            if rvalue in usedvars:
                rvalue = Closure.__unused__getVarNameForVIDImplEx1(vid + 1,usedvars)            
        return rvalue
    
    @staticmethod
    def getVarNameForVIDImplEx(vid,usedvars):
        rvalue = None                                            
        while rvalue == None or ( rvalue  in usedvars ) or isJSKeyword(rvalue):
            vid += 1
            tvid = vid
            name = ""
            while tvid > 51:                    
                r = tvid % 51
                name = id_chars[r] + name                    
                tvid = (tvid - r) / 51                            
            rvalue = id_chars[tvid] + name[::-1] #TODO:revisit for more var greater than 2 char                                                                                         
                    
        return rvalue                        
                        

    @staticmethod
    def __unused__getVarNameForVID(vid):
        rvalue = None
        if Closure.varhash.has_key(vid):
            rvalue = Closure.varhash[vid]
        else:
            rvalue = Closure.__unused__getVarNameForVIDImpl(vid)
            Closure.varhash[vid] = rvalue
            Closure.usedvars.append(rvalue)
        return rvalue
    
    
        
    @staticmethod
    def __unused__getVarNameForVIDImpl(vid):                                            
        if vid >= 52:
            r = vid % 52
            d = (vid - r) / 52                    
            rvalue = id_chars[d] + id_chars[r] #TODO:revisit for more var greater than 2 char
            while ( rvalue  in Closure.usedvars ) or isJSKeyword(rvalue):
                vid += 1                    
                r = vid % 52
                d = (vid - r) / 52
                rvalue = id_chars[d] + id_chars[r] #TODO:revisit for more var greater than 2 char                                                                                         
        else:
            rvalue = id_chars[vid]
            if rvalue in Closure.usedvars:
                rvalue = Closure.__unused__getVarNameForVIDImpl(vid + 1)            
        return rvalue
        
            
        
            
def getFormattedCode(values , space=TAB_SIZE):
    if values == None:
        return ""
    code = []
    if isinstance(values , list):
        for v in values:
            code.append(getFormattedCode(v, space + TAB_SIZE))
    elif isinstance(values , str):        
        code.append(values)
    else:
        raise Exception("Unexpected value " + str(values))
    return "\n".join(code)

class JSQuery:
    def get(self,types,options = None):
        raise Exception("Not Implemented roFC " + str(self))
                    
class CodeGenerator(JSQuery):
    def toFC(self):
        raise Exception("Not Implemented toFC " + str(self))
    def toMC(self):
        raise Exception("Not Implemented toFC " + str(self))
    def assignIDs(self):
        raise Exception("Not Implemented toFC " + str(self))
    
class Program(CodeGenerator):
    def __init__(self):
        self.sourceElements = []
        
    
    def toFC(self):
        values = []
        for elm in self.sourceElements:
            values.append(elm.toFC())
        return getFormattedCode(values, 0)
    
    def toMC(self):
        values = []
        for elm in self.sourceElements:
            values.append(elm.toMC())
        return "".join(values)
    
    def assignIDs(self):
        for elm in self.sourceElements:
            elm.assignIDs()

"""     
class SourceElement(CodeGenerator):
    
    def __init__(self):
        self.functionDecl = None
        self.statement = None

    def toFC(self):
        rvalue = None
        if self.functionDecl != None:
            rvalue = self.functionDecl.toFC()
        elif self.statement != None:
            rvalue = self.statement.toFC()
        return rvalue
    
    def toMC(self):
        rvalue = None
        if self.functionDecl != None:
            rvalue = self.functionDecl.toMC()
        elif self.statement != None:
            rvalue = self.statement.toMC()
        return rvalue
    
    def assignIDs(self):    
        if self.functionDecl != None:            
            self.functionDecl.assignIDs()            
        elif self.statement != None:
            self.statement.assignIDs()
"""    
class FunctionDeclaration(CodeGenerator):  # also for FunctionExpression
    
    def __init__(self):
        self.name = None
        self.params = None
        self.body = None
        self.parent = None
        self.vars = {}
        self.propnames = None
        self.idfrefs = []
        self.toprefs = []
        self.childrens = []
        self.var_count = 0        
        self.shortname = None
        self.usecount = 0
        self.line = -1
        self.pos  = -1
        self.returns = []
        
            
            
    def toFC(self):
        s = "function";
        if self.name != None:
            s += " " + self.name
        
        args = []
        for p in self.params:
            args.append(p.toFC())
        s += "(" + ", ".join(args) + " ){"
        
        rvalue = []
        rvalue.append(s)        
        rvalue.append(self.body.toMC())        
        return rvalue
    
    def toMC(self):
        clear_topmost_closure = False
        if Closure.topmostClosure == None:
            Closure.topmostClosure = self
            clear_topmost_closure = True
        s = "function";
        if self.name != None:
            if self.shortname != None:
                s += " " + self.shortname
            else:
                # s +=" FFF_Unused"
                s += " " + self.name      
        args = []
        for p in self.params:
            args.append(p.toMC())
        """
        s += "(" + ",".join(args) + "){"
        total = len(self.body)
        count = 0                        
        for se in self.body:
            count += 1
            if count == total:
                t = se.toMC()
                if t.endswith(";"):
                    t = t[:-1]
                s += t
            else:
                s += se.toMC()
        """
        s += "(" + ",".join(args) + ")"
        s += self.body.toMC()
        if clear_topmost_closure:
            Closure.topmostClosure = None                
        return s 
    
    def assignIDs(self):                 
        
        if self.name != None:            
            Closure.addVar(self.name, self)
                            
        self.parent = Closure.currentClosure
        if self.parent == None:
            Closure.topmostClosure = self 
                    
        Closure.currentClosure = self
        if self.parent!=None:
            self.parent.childrens.append(self)
        else:
            Closure.closures.append(self)
        
        for p in self.params:
            p.assignIDs()                    
        
        #for se in self.body:
        #    se.assignIDs()
        self.body.assignIDs()               
        Closure.currentClosure = self.parent
        if self.parent == None:
            Closure.topmostClosure =  None

        
class Parameter(CodeGenerator):
    def __init__(self, name):
        self.name = name        
        self.shortname = None
        self.usecount = 0        
    
    def toFC(self):
        return self.name
    
    def toMC(self):
        rvalue = None
        if self.shortname == None:
            rvalue = self.name
        else:
            rvalue =  self.shortname
        if self.usecount == 0:
            Closure.unusedvars.append(self.name)
        return rvalue

    def assignIDs(self):                                
        Closure.addVar(self.name, self)
               

class VariableDeclaration(CodeGenerator):
    def __init__(self):
        self.name = None            
        self.init = None        
        self.shortname = None
        self.usecount = 0        
    
    def toFC(self):
        s = self.name
        if self.init:
            s += " = " + self.init.toFC()
        return s
    
    def toMC(self):
        s = self.shortname
        if s == None:
            s = self.name
        if self.init:
            s += "=" + self.init.toMC()
        return s
    
    def assignIDs(self):                
        
        Closure.addVar(self.name, self)            
        if self.init:
            self.init.assignIDs()                
        
            

class Stmt(CodeGenerator):
    def __init__(self):
        pass

class StmtBlock(Stmt):
    
    def __init__(self):
        Stmt.__init__(self)
        self.stmts = None
    
    def toFC(self):
        rvalue = []
        rvalue.append("{")
        if self.stmts != None:
            inner = []
            for st in self.stmts:
                inner.append(st.toFC())
            rvalue.append(inner)
        rvalue.append("}")
        return rvalue
    
    def toMC(self):
        s = "{"
        if self.stmts != None:                                        
            lastVarStr=None                            
            for st in self.stmts:                
                if isinstance(st,StmtVariableDeclaration):
                    if lastVarStr!=None:
                        lastVarStr+=","+st.toMC(True)
                    else:
                        lastVarStr="var "+st.toMC(True)
                else:
                    if lastVarStr!=None:
                        s+=lastVarStr+";"
                        lastVarStr=None
                    s += st.toMC()                                
            
            if s.endswith(";"):
                s = s[:-1]
                                    
        return s + "}"
    
    def assignIDs(self):        
        if self.stmts != None:            
            for st in self.stmts:
                st.assignIDs()            
        
        
class StmtEmpty(Stmt):
    
    def __init__(self):
        Stmt.__init__(self)
    
    def toFC(self):
        return ";"
    
    def toMC(self):
        return ";"
    
    def assignIDs(self):
        pass
        
class StmtVariableDeclaration(Stmt):
    def __init__(self):
        Stmt.__init__(self)
        self.vdl = None
    def toFC(self):
        values = []
        values.append("var ")
        decls = []
        for vd in self.vdl:
            decls.append(getFormattedCode(vd.toFC()))
            if vd.usecount == 0:
                Closure.unusedvars.append(vd.name)   
        values.append("\n,".join(decls) + ";")        
        return values;
    
    def toMC(self,novar=False):                
        decls = []
        for vd in self.vdl:
            decls.append(vd.toMC())            
            if vd.usecount == 0:
               Closure.unusedvars.append(vd.name)
        if novar:
            return ",".join(decls) 
        else:
            return "var " + ",".join(decls) + ";"
        
            
    
    def assignIDs(self):         
        for vd in self.vdl:
            vd.assignIDs()        
             
        
class StmtExpression(Stmt):
    
    def __init__(self):
        Stmt.__init__(self)
        self.exp = None

    def toFC(self):
        return getFormattedCode(self.exp.toFC()) + ";"
    
    def toMC(self):
        return self.exp.toMC() + ";"
    
    def assignIDs(self):
        self.exp.assignIDs()

class StmtIf(Stmt):
    def __init__(self):
        Stmt.__init__(self)
        self.condition = None
        self.if_stmt = None
        self.else_stmt = None
    
    def toFC(self):        
        values = []
        line = "if( " + getFormattedCode(self.condition.toFC()) + " )"
        values.append(line)
        values.append(self.if_stmt.toFC())
        if self.else_stmt != None:
            values.append("else")
            values.append(self.else_stmt.toFC())
        return values
    
    def toMC(self):                
        
        s = "if(" + self.condition.toMC() + ")" + self.if_stmt.toMC()        
        
        if self.else_stmt != None:
            s += "else"
            code = self.else_stmt.toMC()
            
            if code.startswith("{"):
                s += code
            else:
                s += " " + code
        return s
    
    def assignIDs(self):                
        self.condition.assignIDs() 
        self.if_stmt.assignIDs()        
        if self.else_stmt != None:            
            self.else_stmt.assignIDs()            


class StmtDoWhile(Stmt):
    def __init__(self):
        Stmt.__init__(self)
        self.stmt = None
        self.condition = None
    
    def toFC(self):
        values = []
        values.append("do")
        values.append(self.stmt.toFC())
        values.append("while( " + getFormattedCode(self.condition.toFC()) + ")")
        return values
    
    def toMC(self):        
        s = "do" 
        code = self.stmt.toMC()          
        if code.startswith("{"):
            s += code
        else:
            s += " " + code
        s += "while(" + self.condition.toMC() + ")"
        return s        
    
    def assignIDs(self):                
        self.stmt.assignIDs()          
        self.condition.assignIDs()        

class StmtWhile(Stmt):
    
    def __init__(self):
        Stmt.__init__(self)
        self.stmt = None
        self.condition = None
    
    def toFC(self):
        values = []
        values.append("while( " + getFormattedCode(self.condition.toFC()) + ")")        
        values.append(self.stmt.toFC())        
    
        return values
    
    def toMC(self):        
        return  "while(" + self.condition.toMC() + ")" + self.stmt.toMC()        
    
    def assignIDs(self):        
        self.condition.assignIDs()
        self.stmt.assignIDs()
        
class StmtFor(Stmt):        
    def __init__(self):
        Stmt.__init__(self)
        # if ( init , condition, exp ) 
        #  stmt
        self.stmt = None
        self.condition = None
        self.init = None
        self.exp = None
    
    def toFC(self):
        values = []
        line = "for( "
        
        if self.init != None: 
            if isinstance(self.init , CodeGenerator):
                line += getFormattedCode(self.init.toFC())
            else:                
                decls = []
                for vd in self.init:
                    decls.append(getFormattedCode(vd.toFC()))
                line += "var " + ",".join(decls)      
        line += ";"
        
        if self.condition != None: 
            line += getFormattedCode(self.condition.toFC()) 
        line += ";"
        
        if self.exp != None: 
            line += getFormattedCode(self.exp.toFC())
        line += ")"
        values.append(line)
        values.append(self.stmt.toFC())
        
        return values
    
    def toMC(self):
        
        s = "for("
        
        if self.init != None: 
            if isinstance(self.init , CodeGenerator):
                s += self.init.toMC()
            else:                
                decls = [] 
                for vd in self.init:
                    decls.append(vd.toMC())
                s += "var " + ",".join(decls)      
        s += ";"
        
        if self.condition != None: 
            s += self.condition.toMC() 
        s += ";"
        
        if self.exp != None: 
            s += self.exp.toMC()        
        return s + ")" + self.stmt.toMC()        
    
    def assignIDs(self):       
        if self.init != None: 
            if isinstance(self.init , CodeGenerator):
                self.init.assignIDs()
            else:                                 
                for vd in self.init:
                    vd.assignIDs()                
        if self.condition != None: 
            self.condition.assignIDs()                 
        if self.exp != None: 
            self.exp.assignIDs()        
        self.stmt.assignIDs()        


class StmtForIn(Stmt):
    
    def __init__(self):
        Stmt.__init__(self)
        # if ( var init in exp ) 
        #  stmt
        self.stmt = None        
        self.init = None
        self.exp = None
    
    def toFC(self):
        values = []
        line = None
        if isinstance( self.init ,VariableDeclaration ):            
            line = "for(var " + getFormattedCode(self.init.toFC()) + " in " + getFormattedCode(self.exp.toFC()) + " )"
        else:
            line = "for( " + getFormattedCode(self.init.toFC()) + " in " + getFormattedCode(self.exp.toFC()) + " )"
        values.append(line)
        values.append(self.stmt.toFC())
        return values
    
    def toMC(self):        
        rvalue = None
        if isinstance( self.init ,VariableDeclaration ):
            rvalue = "for(var " + self.init.toMC() + " in " + self.exp.toMC() + ")" + self.stmt.toMC()
        else:
            rvalue = "for(" + self.init.toMC() + " in " + self.exp.toMC() + ")" + self.stmt.toMC()
        return rvalue
    
    def assignIDs(self):        
        self.init.assignIDs()
        self.exp.assignIDs()
        self.stmt.assignIDs()


class StmtContinue(Stmt):  
    
    def __init__(self):
        Stmt.__init__(self)
        self.label = None
    
    def toFC(self):
        line = None
        if self.label != None:
            line = "continue " + self.label 
        else:
            line = "continue;"
        
        return line
    
    def toMC(self):
        line = None
        if self.label != None:
            line = "continue " + self.label 
        else:
            line = "continue;"
        
        return line
    
    def assignIDs(self):
        pass
        #print "Continue used"
        #if Closure.currentClosure!=None:
        #    print Closure.currentClosure.line,Closure.currentClosure.pos
        
              
        
class StmtBreak(Stmt):  
    
    def __init__(self):
        Stmt.__init__(self)
        self.label = None
    
    
    def toFC(self):
        line = None
        if self.label != None:
            line = "break " + self.label + ";" 
        else:
            line = "break;"
    
        return line
    
    def toMC(self):
        line = None
        if self.label != None:
            line = "break " + self.label + ";" 
        else:
            line = "break;"
        return line
    
    def assignIDs(self):
        pass
    

class StmtReturn(Stmt):
    
    def __init__(self):
        Stmt.__init__(self)
        self.exp = None
    
    def toFC(self):
    
        rvalue = None
        if self.exp != None:
            rvalue = "return " + getFormattedCode(self.exp.toFC()) + ";"
        else:
            rvalue = "return;"
        
        return rvalue
    
    def toMC(self):
    
        rvalue = None
        if self.exp != None:
            t = self.exp.toMC()
            if t[0].isalnum() or t[0]=='$' or t[0]=='_':
                rvalue = "return " + t + ";"
            else:
                rvalue = "return" + t + ";"
        else:
            rvalue = "return;"
        
        return rvalue
    
    def assignIDs(self):
        cl = Closure.currentClosure
        while cl!=None and (  not isinstance(cl,FunctionDeclaration) ):
            cl = cl.parent
        if cl!=None:
            cl.returns.append(self)
            #if len(cl.returns)>1:
            #    print "Multiple returns"
            #    print cl.line,cl.pos
        else:
            raise Exception("Return statement outside function")    
        if self.exp != None:
            self.exp.assignIDs()
    


class StmtWith(Stmt):
    def __init__(self):
        Stmt.__init__(self)
        self.exp = None
        self.stmt = None
    
    def toFC(self):
        values = []
        line = "with( " + getFormattedCode(self.exp.toFC()) + " )"
        values.append(line)
        values.append(self.stmt.toFC())
    
        return values
    
    def toMC(self):        
        return "with(" + self.exp.toMC() + ")" + self.stmt.toMC() 
    
    def assignIDs(self):        
        self.exp.assignIDs()
        self.stmt.assignIDs()
        
class StmtLabelled(Stmt):
    
    def __init__(self):
        Stmt.__init__(self)
        self.label = None
        self.stmt = None
    
    def toFC(self):
        values = []
        values.append(self.label + " :")
        values.append(self.stmt.toFC())
        return values
    
    def toMC(self):
        return self.label + ":" + self.stmt.toMC()
    
    def assignIDs(self):
        self.stmt.assignIDs()
    
class StmtSwitch(Stmt):    
    
    def __init__(self):
        Stmt.__init__(self)
        self.exp = None
        self.cases = None
    
    def toFC(self):
        values = []
        line = "switch( " + getFormattedCode(self.exp.toFC()) + " )"
        values.append(line)
        values.append("{")
        for case in self.cases:
            values.append(case.toFC())
        values.append("}")
        return values 
    
    def toMC(self):
        
        s = "switch(" + self.exp.toMC() + "){"        
        for case in self.cases:
            s += case.toMC() 
        return s + "}"
    
    def assignIDs(self):
        
        self.exp.assignIDs()        
        for case in self.cases:
            case.assignIDs()         
        
        
class CaseClause(CodeGenerator):
    def __init__(self):
        self.default = False
        self.exp = None
        self.stmtList = None 
    
    def toFC(self):
        values = []        
        if self.default:
            values.append("default :")
        else:
            values.append("case " + getFormattedCode(self.exp.toFC()) + " :")
        if self.stmtList != None:
            for stmt in self.stmtList:
                values.append(stmt.toFC())
        return values
    
    def toMC(self):
        s = None        
        if self.default:
            s = "default:"
        else:
            t = self.exp.toMC()
            if t[0].isalnum() or t[0]=='$' or t[0]=='_':
                s = "case " + t + ":"
            else:
                s = "case" + t + ":"            
        
        if self.stmtList != None:
            for stmt in self.stmtList:
                s += stmt.toMC() 
        return s    
    
    def assignIDs(self):
        if not self.default:
            self.exp.assignIDs()
        if self.stmtList != None:
            for stmt in self.stmtList:
                stmt.assignIDs()        
        
class StmtThrow(Stmt):
    def __init__(self):
        Stmt.__init__(self)
        self.exp = None
    
    def toFC(self):
        return "throw " + getFormattedCode(self.exp.toFC()) 
    
    def toMC(self):
        return "throw " + self.exp.toMC()
    
    def assignIDs(self):
        self.exp.assignIDs()

class StmtTry(Stmt):
    def __init__(self):
        Stmt.__init__(self)
        self.stmt = None
        self.catchClause = None
        self.finallyClause = None
    
    def toFC(self):
        values = []
        values.append("try")
        values.append(self.stmt.toFC())
        
        if self.catchClause != None:
            values.append(self.catchClause.toFC())
        
        if self.finallyClause != None:
            values.append(self.finallyClause.toFC())
        
        return values
    
    def toMC(self):
        s = "try" + self.stmt.toMC() 
        
        if self.catchClause != None:
            s += self.catchClause.toMC() 
        
        if self.finallyClause != None:
            s += self.finallyClause.toMC() 
        
        return s
    
    def assignIDs(self):
        self.stmt.assignIDs() 
        
        if self.catchClause != None:
            self.catchClause.assignIDs() 
        
        if self.finallyClause != None:
            self.finallyClause.assignIDs()         

class CatchClause(CodeGenerator):
    
    def __init__(self):
        self.name = None
        self.stmt = None        
        self.vars = {}
        self.idfrefs = []
        self.toprefs = []
        self.childrens = []
        self.var_count = 0
        self.parent = None
        self.propnames = None
    
    def toFC(self):
        values = []
        values.append("catch( " + self.name.toFC() + " )")
        values.append(self.stmt.toFC())
        return values
    
    def toMC(self):
        return "catch(" + self.name.toMC() + ")" + self.stmt.toMC()
    
    def assignIDs(self):        
        self.parent = Closure.currentClosure         
        Closure.currentClosure = self
        if self.parent!=None:
            self.parent.childrens.append(self)
        else:
            Closure.closures.append(self)        
        
        self.name.assignIDs()        
        self.stmt.assignIDs()
        
        Closure.currentClosure = self.parent        

class FinallyClause(CodeGenerator):
    
    def __init__(self):
        self.stmt = None
    
    def toFC(self):
        values = []
        values.append("finally")
        values.append(self.stmt.toFC())
        return values
    
    def toMC(self):
        return "finally" + self.stmt.toMC()         
    
    def assignIDs(self):
        self.stmt.assignIDs()

# ##
# Expression
# ##        
class ExPart(CodeGenerator):
    def __init__(self):
        pass
class ExThis(ExPart):
    def toFC(self):
        return "this"
    def toMC(self):
        return "this"
    def assignIDs(self):
        pass

class ExNew(ExPart):
    
    def toFC(self):
        return "new "
    
    def toMC(self):
        return "new "
    
    def assignIDs(self):
        pass
    
class ExIdentifier(ExPart):            
    def __init__(self):
        self.value = None
        self.decl = None                
    
    def toFC(self):                    
        return self.value
    
    def toMC(self):
        rvalue = None
        if self.decl != None:
            if self.decl.shortname == None :
                rvalue = self.decl.name
            else:            
                rvalue = self.decl.shortname
        else:
            #print " Global: "+self.value            
            rvalue = self.value                
        return rvalue
    
    def assignIDs(self):
        Closure.addRef(self)

   
class ExParExpression(ExPart):
    
    def __init__(self):
        self.exp = None
    
    def toFC(self):
        return "( " + getFormattedCode(self.exp.toFC()) + " )"
    
    def toMC(self):
        return "(" + self.exp.toMC() + ")"
    
    def assignIDs(self):
        self.exp.assignIDs()

class ExArguments(ExPart):
    
    def __init__(self):
        self.args = None
    
    def toFC(self):        
        values = []
        for arg in self.args:
            values.append(getFormattedCode(arg.toFC()))
        return "(" + ", ".join(values) + ")" 
    
    def toMC(self):        
        values = []
        for arg in self.args:
            values.append(arg.toMC())
        return "(" + ",".join(values) + ")"
    
    def assignIDs(self):                
        for arg in self.args:
            arg.assignIDs()        
        
class ExIndexSuffix(ExPart):
    
    def __init__(self):
        self.exp = None
    
    def toFC(self):
        return "[ " + getFormattedCode(self.exp.toFC()) + " ]"
    
    def toMC(self):
        return "[" + self.exp.toMC() + "]"
    
    def assignIDs(self):
        self.exp.assignIDs()
        
class ExPropertyReferenceSuffix(ExPart):
    
    def __init__(self):
        self.identifier = None        
    
    def toFC(self):            
        return "." + self.identifier

    def toMC(self):
        rvalue = "." + self.identifier
        
        if Closure.topmostClosure!= None:
            closure = Closure.topmostClosure
            name = "__s_"+self.identifier+"_"
            if closure.vars.has_key(name):            
                ref = closure.vars[name]
                rvalue = '[' + ref.shortname +']'            
        return rvalue
    
    def assignIDs(self):
        if Closure.propNameOpt:
            if Closure.topmostClosure != None:
                if Closure.topmostClosure.propnames == None:
                    Closure.topmostClosure.propnames = {}
                propnames = Closure.topmostClosure.propnames
                if propnames.has_key(self.identifier):
                    propnames[self.identifier] += 1
                else:
                    propnames[self.identifier] = 1
                    Closure.addRef("__s_"+self.identifier+"_")
                        
class ExOperator(ExPart):
    LOGICAL_OR = 1
    LOGICAL_AND = 2
    # : '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '>>>=' | '&=' | '^=' | '|='
    
    ASSIGN_NORMAL = 3
    ASSIGN_MULTIPLY = 4
    ASSIGN_DIVIDE = 5
    ASSIGN_MOD = 6
    ASSIGN_PLUS = 7
    ASSIGN_MINUS = 8
    ASSIGN_LEFT_SHIFT = 9
    ASSIGN_RIGHT_SHIFT = 10
    ASSIGN_RIGHT_RIGHT_SHIFT = 11
    ASSIGN_BIN_AND = 12
    ASSIGN_BIN_XOR = 13
    ASSIGN_BIN_OR = 14
    
    BITWISE_OR = 15
    BITWISE_AND = 16
    BITWISE_XOR = 17
      
    # '==' | '!=' | '===' | '!=='
    RELATIONAL_EQUAL = 18
    RELATIONAL_NOT_EQUAL = 19
    RELATIONAL_T_EQUAL = 20
    RELATIONAL_NOT_T_EQUAL = 21
    # '<' | '>' | '<=' | '>=' | 'instanceof' | 'in'
    RELATIONAL_LESS_THAN = 22
    RELATIONAL_GREATER_THAN = 23
    RELATIONAL_LESS_THAN_OR_EQUAL = 24
    RELATIONAL_GREATER_THAN_EQUAL = 25
    RELATIONAL_INSTANCEOF = 26
    RELATIONAL_IN = 27
    
    SHIFT_LEFT = 28
    SHIFT_RIGHT = 29
    SHIFT_RIGHT_RIGHT = 30
    
    ADDITIVE_PLUS = 31
    ADDITIVE_MINUS = 32
    # '*' | '/' | '%'
    MULITPLICATIVE_MULTIPLY = 33
    MULTIPLICATIVE_DIVIDE = 34
    MULTIPLICATIVE_MODULUS = 35
    # 'delete' | 'void' | 'typeof' | '++' | '--' | '+' | '-' | '~' | '!'
    UNARY_DELETE = 36
    UNARY_VOID = 37
    UNARY_TYPEOF = 38
    UNARY_INCREMENT = 39
    UNARY_DECREMENT = 40
    UNARY_POSITIVE = 41
    UNARY_NEGATIVE = 42
    UNARY_NOT = 43
    UNARY_BITWISE_NOT = 44
    
    POSTFIX_INCREMENT = 45
    POSTFIX_DECREMENT = 46
    
    TERNARY_IF = 47
    TERNARY_ELSE = 48
    
    COMMA = 49
    s = {}
    # self.s = s
    ####
    s[LOGICAL_OR] = "||"
    s[LOGICAL_AND] = "&&"
    # : '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '>>>=' | '&=' | '^=' | '|='

    s[ASSIGN_NORMAL] = "="
    s[ASSIGN_MULTIPLY] = "*="
    s[ASSIGN_DIVIDE] = "/="
    s[ASSIGN_MOD] = "%"
    s[ASSIGN_PLUS] = "+="
    s[ASSIGN_MINUS] = "-="
    s[ASSIGN_LEFT_SHIFT] = "<<="
    s[ASSIGN_RIGHT_SHIFT] = ">>="
    s[ASSIGN_RIGHT_RIGHT_SHIFT] = ">>>="
    s[ASSIGN_BIN_AND] = "&="
    s[ASSIGN_BIN_XOR] = "^="
    s[ASSIGN_BIN_OR] = "|="

    s[BITWISE_OR] = "|"
    s[BITWISE_AND] = "&"
    s[BITWISE_XOR] = "^"
  
    # '==' | '!=' | '===' | '!=='
    s[RELATIONAL_EQUAL] = "=="
    s[RELATIONAL_NOT_EQUAL] = "!="
    s[RELATIONAL_T_EQUAL] = "==="
    s[RELATIONAL_NOT_T_EQUAL] = "!=="
    # '<' | '>' | '<=' | '>=' | 'instanceof' | 'in'
    s[RELATIONAL_LESS_THAN] = "<"
    s[RELATIONAL_GREATER_THAN] = ">"
    s[RELATIONAL_LESS_THAN_OR_EQUAL] = "<="
    s[RELATIONAL_GREATER_THAN_EQUAL] = ">="
    s[RELATIONAL_INSTANCEOF] = " instanceof "
    s[RELATIONAL_IN] = " in "

    s[SHIFT_LEFT] = "<<"
    s[SHIFT_RIGHT] = ">>"
    s[SHIFT_RIGHT_RIGHT] = ">>>"

    s[ADDITIVE_PLUS] = "+"
    s[ADDITIVE_MINUS] = "-"
    # '*' | '/' | '%'
    s[MULITPLICATIVE_MULTIPLY] = "*"
    s[MULTIPLICATIVE_DIVIDE] = "/"
    s[MULTIPLICATIVE_MODULUS] = "%"
    
    # 'delete' | 'void' | 'typeof' | '++' | '--' | '+' | '-' | '~' | '!'
    s[UNARY_DELETE] = "delete "
    s[UNARY_VOID] = "void "
    s[UNARY_TYPEOF] = "typeof "
    s[UNARY_INCREMENT] = "++"
    s[UNARY_DECREMENT] = "--"
    s[UNARY_POSITIVE] = "+"
    s[UNARY_NEGATIVE] = "-"
    s[UNARY_NOT] = "!"
    s[UNARY_BITWISE_NOT] = "~"

    s[POSTFIX_INCREMENT] = "++"
    s[POSTFIX_DECREMENT] = "--"

    s[TERNARY_IF] = "?"
    s[TERNARY_ELSE] = ":"
    s[COMMA] = ","

    ####        

    def __init__(self):
        self.value = None
        
        
        
        
    def toFC(self):
        rvalue = None
        if self.s.has_key(self.value):
            rvalue = self.s[self.value]
        else:
            raise Exception("Invalid Operator value")
        return " " + rvalue + " "
    
    def toMC(self):
        rvalue = None
        if self.s.has_key(self.value):
            rvalue = self.s[self.value]
        else:
            raise Exception("Invalid Operator value")
        return rvalue
    
    def assignIDs(self):
        pass
            
class ExRegex(ExPart):
    def __init__(self):
        self.value = None
    
    def toFC(self):
        return self.value        
    
    def toMC(self):
        return self.value
    
    def assignIDs(self):
        pass

class ExLiteral(ExPart):
    NULL = 1
    TRUE = 2
    FALSE = 3
    
    KEYWORD = 1
    STRING = 2
    NUMERIC = 3
    
    h = {}
    
    def __init__(self):
        ExPart.__init__(self)
        self.value = ExLiteral.NULL
        self.type = ExLiteral.KEYWORD
    
    def toFC(self):
        rvalue = None
    
        if self.type == ExLiteral.KEYWORD:
            if self.value == ExLiteral.NULL :
                rvalue = "null"
            elif self.value == ExLiteral.TRUE:
                rvalue = "true"
            elif self.value == ExLiteral.FALSE:
                rvalue = "false"
        else:  # string or numeric
            rvalue = self.value            
        return rvalue
    
    def toMC(self):
        rvalue = None
        
        if self.type == ExLiteral.KEYWORD:
            if self.value == ExLiteral.NULL :
                rvalue = "null"
            elif self.value == ExLiteral.TRUE:
                rvalue = "true"
            elif self.value == ExLiteral.FALSE:
                rvalue = "false"
        else:  # string or numeric
            rvalue = self.value
            if self.type == ExLiteral.STRING:
                if ExLiteral.h.has_key(rvalue):
                    ExLiteral.h[rvalue] += 1
                else:
                    ExLiteral.h[rvalue]=1
            
        return rvalue
    
    def assignIDs(self):
        pass
                
def toFC_ExpressionParts(parts):
    line = ""
    
    for p in parts:
        if isinstance(p , list):
            line += toFC_ExpressionParts(p)
        elif isinstance(p , str):
            line += p
        elif isinstance(p, CodeGenerator):
            line += getFormattedCode(p.toFC())
        else:            
            raise Exception("Compile Error unknown ")
    return line

def toMC_ExpressionParts(parts):
    line = ""
    
    for p in parts:
        if isinstance(p , list):
            line += toMC_ExpressionParts(p)
        elif isinstance(p , str):
            line += p
        elif isinstance(p, CodeGenerator):                         
            line += p.toMC()  
        else:            
            raise Exception("Compile Error unknown ")
    return line

def assignIDs_ExpressionParts(parts):
    for p in parts:
        if isinstance(p , list):
            assignIDs_ExpressionParts(p)
        elif isinstance(p , str):
            pass
        elif isinstance(p, CodeGenerator):
            p.assignIDs() 
        else:            
            raise Exception("Compile Error unknown ")
    

            
class Expression(CodeGenerator):
    
    def __init__(self):
        self.parts = None
    
    def toFC(self):
        return toFC_ExpressionParts(self.parts)
    
    def toMC(self):
        return toMC_ExpressionParts(self.parts)
    
    def assignIDs(self):
        return assignIDs_ExpressionParts(self.parts)
        
class Property(CodeGenerator):
    
    def __init__(self):
        self.name = None
        self.value = None   
    
    def toFC(self):
        return self.name + " :" + getFormattedCode(self.value.toFC()) 
    
    def toMC(self):
        return self.name + ":" + self.value.toMC()
    
    def assignIDs(self):
        self.value.assignIDs()

class ObjectLiteral(CodeGenerator):
    
    def __init__(self):
        self.props = []
    
    def toFC(self):
        values = []
        values.append("{")
        inner = []
    
        for p in self.props:
            inner.append(getFormattedCode(p.toFC()))
        
        values.append(",\n".join(inner))
        values.append("}")
        return values
    
    def toMC(self):        
        s = "{"
        inner = []
        for p in self.props:
            inner.append(p.toMC())
        return s + ",".join(inner) + "}" 
    
    def assignIDs(self):        
        for p in self.props:
            p.assignIDs()        

class ArrayLiteral(CodeGenerator):
    
    def __init__(self):
        self.values = []
    
    def toFC(self):                
        inner = []
        for p in self.values:
            inner.append(getFormattedCode(p.toFC()))
        return '[' + ", ".join(inner) + ']'
    
    def toMC(self):                
        inner = []
        for p in self.values:
            inner.append(p.toMC())
        return '[' + ",".join(inner) + ']'
    
    def assignIDs(self):                        
        for p in self.values:
            p.assignIDs()
        
