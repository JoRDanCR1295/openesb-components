 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.exception.ClassGenerationException;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import antlr.RecognitionException;
import antlr.TokenStreamException;
import antlr.collections.AST;

import com.puppycrawl.tools.checkstyle.TreeWalker;
import com.puppycrawl.tools.checkstyle.api.DetailAST;
import com.puppycrawl.tools.checkstyle.api.FileContents;
import com.puppycrawl.tools.checkstyle.api.TokenTypes;


/**
 * This class provides some utility methods used to parse a java source file.
 */
public class UtilJavaSourceParsing {

  /**
   * Logger.
   */
  private static final Logger LOG
    = LoggerFactory.getLogger(UtilJavaSourceParsing.class);
  
  /**
   * Default constructor.
   */
  public UtilJavaSourceParsing () {
  }
  
  /**
   * This method extract the method's signature defined in the java source file.
   *
   * @param   absPath   The absolute path of the java source file.
   *
   * @return  The list of method's signature found.
   *
   * @throws  ClassGenerationException  The class generation exception
   */
  public List<MethodSignature> extractMethodSignature(String absPath, String fullQualifiedName)
    throws ClassGenerationException {
                        
    List<String> listOfLines = null;
    try {

      listOfLines = readFile(absPath);
      
    } catch (FileNotFoundException e) {

      Object[] args = new Object[] { absPath };
      LOG.error("CRB000543_ExtractMethodSignatureError_FileNotFound", args, e);
      throw new ClassGenerationException(
        "CRB000543_ExtractMethodSignatureError_FileNotFound", args, e);

    } catch (IOException e) {

      Object[] args = new Object[] { absPath };
      LOG.error("CRB000544_ExtractMethodSignatureError_IOException", args, e);
      throw new ClassGenerationException(
        "CRB000544_ExtractMethodSignatureError_IOException", args, e);
    }
    LOG.debug("Reading file ... done");

    String [] lines = listOfLines.toArray(new String[listOfLines.size()]);

    FileContents fc = new FileContents(absPath, lines);
    DetailAST aRoot = null;
    try {
        aRoot = TreeWalker.parse(fc);

    } catch (RecognitionException e) {

      Object[] args = new Object[] { absPath };
      LOG.error(
        "CRB000545_ExtractMethodSignatureError_RecognitionException", args, e);
      throw new ClassGenerationException(
        "CRB000545_ExtractMethodSignatureError_RecognitionException", args, e);

    } catch (TokenStreamException e) {

      Object[] args = new Object[] { absPath };
      LOG.error(
        "CRB000546_ExtractMethodSignatureError_TokenStreamException", args, e);
      throw new ClassGenerationException(
        "CRB000546_ExtractMethodSignatureError_TokenStreamException", args, e);
    }
    LOG.debug("Parsing file ... done");

    List<DetailAST> nodeList = getTreeAsListOfNode(aRoot);

    List<MethodSignature> methodSignatureList
      = extractMethodSignature(nodeList, fullQualifiedName);

    LOG.debug("Extractiong method's signature ... done");
    return methodSignatureList;
  }

  /**
   * 
   * @param nodeList  The node list
   * @return          The return
   */
  private List<MethodSignature> extractMethodSignature(
    List<DetailAST> nodeList, String fullQualifiedName) throws ClassGenerationException {

    List<MethodSignature> methodSignatureList
      = new ArrayList<MethodSignature>();

    if (nodeList == null) {
      LOG.debug("The list of node is null. nothing to do.");
      return methodSignatureList;
    }    

    // else

    for (DetailAST currentNode : nodeList) {

      if (currentNode != null
            && currentNode.getType() == TokenTypes.METHOD_DEF) {

        String methodName = currentNode.getFirstChild().getNextSibling()
                                       .getNextSibling().getText();
        
       
        //String returnType = getTypeParameter(currentNode);
        //*************************************************************
        //Changed For Multimodule FIX
        //Now The Return Type contain's also the package Information
        String returnType =getParamFromNode(currentNode).getTypeName();
        
        
        MethodSignature methodSignature = new MethodSignature();

        methodSignature.setClassName(fullQualifiedName);
        methodSignature.setMethodName(methodName);
        methodSignature.setReturnType(returnType);

        methodSignatureList.add(methodSignature);
        LOG.debug("methodSignatureList.add:" + methodSignature);
      }

      if (currentNode != null
            && currentNode.getType() == TokenTypes.PARAMETER_DEF) {

        MethodSignature methodSignature
          = methodSignatureList.get(methodSignatureList.size() - 1);
                
        Param param = getParamFromNode(currentNode);
        LOG.debug("CRB000571_Found_parameter", 
        		new Object[]{param.getName(), param.getTypeName()});

        
        // Adds the Holder Information
        if (isHolderCorbaClass(param.getTypeName())) {
            param.setHolder(true);
            methodSignature.setContainsHolder(true);
        }
        methodSignature.getParameters().add(param);
        LOG.debug("Found Param: "+param);
        LOG.debug("CRB000572_methodSignature_update_Parameter_definition", 
        		new Object[]{methodSignature, param.getName(), param.getTypeName()});
      }

    }

    return methodSignatureList;
  }

  /**
   * Returns the tree (part of) as a List.
   * @param aRoot  The aRoot
   * @return       The return
   */
  private List<DetailAST> getTreeAsListOfNode(DetailAST aRoot) {

    List<DetailAST> nodeList = new ArrayList<DetailAST>();

    DetailAST curNode = aRoot;
    while (curNode != null) {
      DetailAST toVisit = (DetailAST) curNode.getFirstChild();
      nodeList.add(toVisit);

      while ((curNode != null) && (toVisit == null)) {
        toVisit = (DetailAST) curNode.getNextSibling();

        if (toVisit == null) {
          curNode = curNode.getParent();
        } else {
          nodeList.add(toVisit);
        }
      }
      curNode = toVisit;
    }

    return nodeList;
  }


  /**
   * This method is used to read a java sourse file.
   *
   * @param  filePath   The absolute path of the java source file.
   *
   * @return The file as a list of String. (a string for each text line).
   *
   * @throws IOException  The IO exception
   */
  private List<String> readFile(String filePath)
    throws IOException {

    List<String> listOfLines = new ArrayList<String>();

    BufferedReader in = new BufferedReader(new FileReader(filePath));
    String str = null;
    while ((str = in.readLine()) != null) {
        listOfLines.add(str);
    }
    in.close();

    return listOfLines;
  }

  /**
   * 
   * @param tt  The tt
   * @return    The return
   */
  private String tokenTypeToString(int tt) {
    switch (tt) {
    // 1. A
    case TokenTypes.ABSTRACT:              return "ABSTRACT";
    case TokenTypes.ANNOTATION:            return "ANNOTATION";
    case TokenTypes.ANNOTATION_ARRAY_INIT: return "ANNOTATION_ARRAY_INIT";
    case TokenTypes.ANNOTATION_DEF:        return "ANNOTATION_DEF";
    case TokenTypes.ANNOTATION_FIELD_DEF:  return "ANNOTATION_FIELD_DEF";

    case TokenTypes.ANNOTATION_MEMBER_VALUE_PAIR:
      return "ANNOTATION_MEMBER_VALUE_PAIR";

    case TokenTypes.ANNOTATIONS:           return "ANNOTATIONS";
    case TokenTypes.ARRAY_DECLARATOR:      return "ARRAY_DECLARATOR";
    case TokenTypes.ARRAY_INIT:            return "ARRAY_INIT";
    case TokenTypes.ASSIGN:                return "ASSIGN";
    case TokenTypes.AT:                    return "AT";

    // 2. B
    case TokenTypes.BAND:          return "BAND";
    case TokenTypes.BAND_ASSIGN:   return "BAND_ASSIGN";
    case TokenTypes.BNOT:          return "BNOT";
    case TokenTypes.BOR:           return "BOR";
    case TokenTypes.BOR_ASSIGN:    return "BOR_ASSIGN";
    case TokenTypes.BSR:           return "BSR";
    case TokenTypes.BSR_ASSIGN:    return "BSR_ASSIGN";
    case TokenTypes.BXOR:          return "BXOR";
    case TokenTypes.BXOR_ASSIGN:   return "BXOR_ASSIGN";

    // 3. C
    case TokenTypes.CASE_GROUP:    return "CASE_GROUP";
    case TokenTypes.CHAR_LITERAL:  return "CHAR_LITERAL";
    case TokenTypes.CLASS_DEF:     return "CLASS_DEF";
    case TokenTypes.COLON:         return "COLON";
    case TokenTypes.COMMA:         return "COMMA";
    case TokenTypes.CTOR_CALL:     return "CTOR_CALL";
    case TokenTypes.CTOR_DEF:      return "CTOR_DEF";

    // 4. D
    case TokenTypes.DEC:           return "DEC";
    case TokenTypes.DIV:           return "DIV";
    case TokenTypes.DIV_ASSIGN:    return "DIV_ASSIGN";
    case TokenTypes.DO_WHILE:      return "DO_WHILE";
    case TokenTypes.DOT:           return "DOT";

    // 5. E
    case TokenTypes.ELIST:             return "ELIST";
    case TokenTypes.ELLIPSIS:          return "ELLIPSIS";
    case TokenTypes.EMPTY_STAT:        return "EMPTY_STAT";
    case TokenTypes.ENUM:              return "ENUM";
    case TokenTypes.ENUM_CONSTANT_DEF: return "ENUM_CONSTANT_DEF";
    case TokenTypes.ENUM_DEF:          return "ENUM_DEF";
    case TokenTypes.EOF:               return "EOF";
    case TokenTypes.EQUAL:             return "EQUAL";
    case TokenTypes.EXPR:              return "EXPR";
    case TokenTypes.EXTENDS_CLAUSE:    return "EXTENDS_CLAUSE";

    // 6. F
    case TokenTypes.FINAL:           return "FINAL";
    case TokenTypes.FOR_CONDITION:   return "FOR_CONDITION";
    case TokenTypes.FOR_EACH_CLAUSE: return "FOR_EACH_CLAUSE";
    case TokenTypes.FOR_INIT:        return "FOR_INIT";
    case TokenTypes.FOR_ITERATOR:    return "FOR_ITERATOR";

    // 7. G
    case TokenTypes.GE:            return "GE";
    case TokenTypes.GENERIC_END:   return "GENERIC_END";
    case TokenTypes.GENERIC_START: return "GENERIC_START";
    case TokenTypes.GT:            return "GT";

    // 8. H ...

    // 9. I
    case TokenTypes.IDENT:             return "IDENT";
    case TokenTypes.IMPLEMENTS_CLAUSE: return "IMPLEMENTS_CLAUSE";
    case TokenTypes.IMPORT:            return "IMPORT";
    case TokenTypes.INC:               return "INC";
    case TokenTypes.INDEX_OP:          return "INDEX_OP";
    case TokenTypes.INSTANCE_INIT:     return "INSTANCE_INIT";
    case TokenTypes.INTERFACE_DEF:     return "INTERFACE_DEF";

    // 10. J ...
    // 11. K ...

    // 12. L
    case TokenTypes.LABELED_STAT: return "LABELED_STAT";
    case TokenTypes.LAND:         return "LAND";
    case TokenTypes.LCURLY:       return "LCURLY";
    case TokenTypes.LE:           return "LE";
    case TokenTypes.LNOT:         return "LNOT";
    case TokenTypes.LPAREN:       return "LPAREN";
    case TokenTypes.LT:           return "LT";

    // 13. M
    case TokenTypes.METHOD_CALL:  return "METHOD_CALL";
    case TokenTypes.METHOD_DEF:   return "METHOD_DEF";
    case TokenTypes.MINUS:        return "MINUS";
    case TokenTypes.MINUS_ASSIGN: return "MINUS_ASSIGN";
    case TokenTypes.MOD:          return "MOD";
    case TokenTypes.MOD_ASSIGN:   return "MOD_ASSIGN";
    case TokenTypes.MODIFIERS:    return "MODIFIERS";

    // 14. N
    case TokenTypes.NOT_EQUAL:  return "NOT_EQUAL";
    case TokenTypes.NUM_DOUBLE: return "NUM_DOUBLE";
    case TokenTypes.NUM_FLOAT:  return "NUM_FLOAT";
    case TokenTypes.NUM_INT:    return "NUM_INT";
    case TokenTypes.NUM_LONG:   return "NUM_LONG";

    // 15. O
    case TokenTypes.OBJBLOCK: return "OBJBLOCK";

    // 16. P
    case TokenTypes.PACKAGE_DEF:   return "PACKAGE_DEF";
    case TokenTypes.PARAMETER_DEF: return "PARAMETER_DEF";
    case TokenTypes.PARAMETERS:    return "PARAMETERS";
    case TokenTypes.PLUS:          return "PLUS";
    case TokenTypes.PLUS_ASSIGN:   return "PLUS_ASSIGN";
    case TokenTypes.POST_DEC:      return "POST_DEC";
    case TokenTypes.POST_INC:      return "POST_INC";

    // 17. Q
    case TokenTypes.QUESTION: return "QUESTION";

    // 18. R
    case TokenTypes.RBRACK: return "RBRACK";
    case TokenTypes.RCURLY: return "RCURLY";
    case TokenTypes.RPAREN: return "RPAREN";

    // 19. S
    case TokenTypes.SEMI: return "SEMI";
    case TokenTypes.SL: return "SL";
    case TokenTypes.SL_ASSIGN: return "SL_ASSIGN";
    case TokenTypes.SLIST: return "SLIST";
    case TokenTypes.SR: return "SR";
    case TokenTypes.SR_ASSIGN: return "SR_ASSIGN";
    case TokenTypes.STAR: return "STAR";
    case TokenTypes.STAR_ASSIGN: return "STAR_ASSIGN";
    case TokenTypes.STATIC_IMPORT: return "STATIC_IMPORT";
    case TokenTypes.STATIC_INIT: return "STATIC_INIT";
    case TokenTypes.STRICTFP: return "STRICTFP";
    case TokenTypes.STRING_LITERAL: return "STRING_LITERAL";
    case TokenTypes.SUPER_CTOR_CALL: return "SUPER_CTOR_CALL";

    // 20. T
    case TokenTypes.TYPE: return "TYPE";
    case TokenTypes.TYPE_ARGUMENT: return "TYPE_ARGUMENT";
    case TokenTypes.TYPE_ARGUMENTS: return "TYPE_ARGUMENTS";
    case TokenTypes.TYPE_EXTENSION_AND: return "TYPE_EXTENSION_AND";
    case TokenTypes.TYPE_LOWER_BOUNDS: return "TYPE_LOWER_BOUNDS";
    case TokenTypes.TYPE_PARAMETER: return "TYPE_PARAMETER";
    case TokenTypes.TYPE_PARAMETERS: return "TYPE_PARAMETERS";
    case TokenTypes.TYPE_UPPER_BOUNDS: return "TYPE_UPPER_BOUNDS";
    case TokenTypes.TYPECAST: return "TYPECAST";

    // 21. U
    case TokenTypes.UNARY_MINUS: return "UNARY_MINUS";
    case TokenTypes.UNARY_PLUS: return "UNARY_PLUS";

    // 22. V
    case TokenTypes.VARIABLE_DEF: return "VARIABLE_DEF";

    // 23. W
    case TokenTypes.WILDCARD_TYPE: return "SEMI";

    // 24. X ...
    // 25. Y ...
    // 26. Z ...
    }

    // FIXME java keyworld

    return "TokenTypeUnknown";
  }
  
  /**
   * Return true if the class is a holder Type class.
   * In the first implementation, the method check only if the type 
   * ends with <code>Holder</code>.
   * Note that there is no Holder interface or superclass.
   * @param typeName
   * @return
   */
  public static boolean isHolderCorbaClass(String typeName) {
      boolean ret = false;
      if (typeName != null) {
          if (typeName.endsWith("Holder")) {
              ret = true;
          }
      }
   return ret;
  }
 
  /**
     * Gets the type string from the current node. Populate also the array
     * informations (if it's an array and the array dimension).
     * 
     * @param currentNode the AST node
     * 
     * @return the parame populated with type nad name.
     */
  private Param getParamFromNode(DetailAST currentNode) {
      // If the token is a "." (TokenTypes.DOT) the type is in the form "xx.dd.xxx".:  Lets collect the type.
      String type = "";
      int arrayDimension = 0; 
      boolean isArray = false;
      
      // Gets the parameter name
      String pName = currentNode.getFirstChild().getNextSibling()
      .getNextSibling().getText();
      
      // Test if the parameter is an array
      if (currentNode.getFirstChild().getNextSibling()
      .getFirstChild().getType() == TokenTypes.ARRAY_DECLARATOR) {
          isArray = true;
      }
      
      if ((currentNode.getFirstChild().getNextSibling()
          .getFirstChild().getType() == TokenTypes.DOT)  || (currentNode.getFirstChild().getNextSibling()
              .getFirstChild().getType() == TokenTypes.ARRAY_DECLARATOR)) {
   
          // get name type of parameter
          String typeParam = getTypeParameter(currentNode);
          
          AST typeNode = currentNode.getFirstChild().getNextSibling().getFirstChild();            
          List<DetailAST> typeDetails = getTreeAsListOfNode((DetailAST)typeNode);
          // Gets all the token, until the pName is found;   
          boolean paramNotFound = true;
          for (int i = 0; i < typeDetails.size() && paramNotFound; i++) {
              DetailAST node = (DetailAST)typeDetails.get(i);
              if (node != null) {       
                  if (node.getText().equals(pName)) {
                      // Parameter name found: break!
                      paramNotFound = false;                                      
                  } else if ((node.getType() != TokenTypes.DOT) && (node.getType() != TokenTypes.ARRAY_DECLARATOR) 
                          && (node.getType() != TokenTypes.RBRACK)) {
                      // Adds the type string, excludes all the "DOT"and "[" chars
                      type = type + node.getText();     
                      // Adds the dot if the  token is not the final class part
                      if (i != typeDetails.size()) {   
                          //LOG.debug("Adds the dot if the token is not the final class part");
                          DetailAST nextNode = (DetailAST)node.getParent().getNextSibling();
                          
                          // For the types with package with a single DOT
                          if (nextNode == null) {
                              nextNode = (DetailAST)node.getNextSibling();
                          }
                           
                          // Adds a "dot" if there is a parent token of type DOT 
                          // and the node is a package. 
                          if ((nextNode != null) && (node.getParent().getType() == TokenTypes.DOT)
                                  && (!node.getText().equals(typeParam))) {
                              type = type + ".";
                              LOG.debug("Adds the dot");
                          } 
                           
                      }
                  } else if ((node.getType() == TokenTypes.RBRACK)) {
                      // Arry declaration closed
                      arrayDimension++;
                  }
              }                                                              
          } 
          
      } else {
          type = currentNode.getFirstChild().getNextSibling()
          .getFirstChild().getText();              
      }      
      Param param = new Param();
      param.setName(pName);
      param.setTypeName(type);
      param.setArray(isArray);
      param.setArrayDimension(arrayDimension);
      return param;
  }
  
  /**
   * Gets the type name of the parameter into currentNode.
   * 
   * @param currentNode
   * @return type Parameter's Name
   */
  private String getTypeParameter(DetailAST currentNode){
    
    LOG.debug(">>>>> getTypeParameter - begin");
       
    String result ="";
    DetailAST node = (DetailAST)currentNode.getFirstChild().getNextSibling()
                                .getFirstChild();

    // Explore Array...
    while ((node.getType() == TokenTypes.ARRAY_DECLARATOR)){
      LOG.debug("Explore Array...");
      node = (DetailAST) node.getFirstChild();
      
    }
    
    // Explore package
    if ((node.getType() == TokenTypes.DOT)){
      LOG.debug("Explore package");
      node = (DetailAST) node.getFirstChild().getNextSibling();
    
    }
    
    
    
    result = node.getText();
    
    LOG.debug("Parameter's Name: " + result);
    LOG.debug(">>>>> getTypeParameter - end");
    return result;
  }
}
