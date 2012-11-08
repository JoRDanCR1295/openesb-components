/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)XPathModelImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.xpath.impl;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.namespace.QName;

import org.apache.commons.jxpath.JXPathException;
import org.apache.commons.jxpath.ri.Compiler;
import org.apache.commons.jxpath.ri.Parser;
import org.apache.commons.jxpath.ri.compiler.Constant;
import org.apache.commons.jxpath.ri.compiler.CoreFunction;
import org.apache.commons.jxpath.ri.compiler.CoreOperation;
import org.apache.commons.jxpath.ri.compiler.CoreOperationAdd;
import org.apache.commons.jxpath.ri.compiler.CoreOperationAnd;
import org.apache.commons.jxpath.ri.compiler.CoreOperationDivide;
import org.apache.commons.jxpath.ri.compiler.CoreOperationEqual;
import org.apache.commons.jxpath.ri.compiler.CoreOperationGreaterThan;
import org.apache.commons.jxpath.ri.compiler.CoreOperationGreaterThanOrEqual;
import org.apache.commons.jxpath.ri.compiler.CoreOperationLessThan;
import org.apache.commons.jxpath.ri.compiler.CoreOperationLessThanOrEqual;
import org.apache.commons.jxpath.ri.compiler.CoreOperationMod;
import org.apache.commons.jxpath.ri.compiler.CoreOperationMultiply;
import org.apache.commons.jxpath.ri.compiler.CoreOperationNegate;
import org.apache.commons.jxpath.ri.compiler.CoreOperationNotEqual;
import org.apache.commons.jxpath.ri.compiler.CoreOperationOr;
import org.apache.commons.jxpath.ri.compiler.CoreOperationSubtract;
import org.apache.commons.jxpath.ri.compiler.Expression;
import org.apache.commons.jxpath.ri.compiler.ExpressionPath;
import org.apache.commons.jxpath.ri.compiler.ExtensionFunction;
import org.apache.commons.jxpath.ri.compiler.LocationPath;
import org.apache.commons.jxpath.ri.compiler.NodeNameTest;
import org.apache.commons.jxpath.ri.compiler.NodeTest;
import org.apache.commons.jxpath.ri.compiler.NodeTypeTest;
import org.apache.commons.jxpath.ri.compiler.Step;
import org.apache.commons.jxpath.ri.compiler.TreeCompiler;
import org.apache.commons.jxpath.ri.compiler.VariableReference;

import com.sun.bpel.model.common.MessageManager;
import com.sun.bpel.model.xpath.AbstractXPathModelHelper;
import com.sun.bpel.model.xpath.LocationStep;
import com.sun.bpel.model.xpath.StepNodeTest;
import com.sun.bpel.model.xpath.XPathCoreFunction;
import com.sun.bpel.model.xpath.XPathCoreOperation;
import com.sun.bpel.model.xpath.XPathException;
import com.sun.bpel.model.xpath.XPathExpression;
import com.sun.bpel.model.xpath.XPathExpressionPath;
import com.sun.bpel.model.xpath.XPathLocationPath;
import com.sun.bpel.model.xpath.XPathModel;
import com.sun.bpel.model.xpath.XPathNumericLiteral;

/**
 * Implementation of the XPathModel interface using Apache's JXPath.
 * JXPath does not validate function arguments, i.e., whether the
 * number of arguments is correct, so we may have to do that validation
 * ourselves or wait for a later version.
 * <p>
 * We also implement extensions to handle
 * <a href="http://www-106.ibm.com/developerworks/webservices/library/ws-bpel/#Expressions">
 * expressions in BPEL4WS</a>.
 *
 * @author Sun Microsystems
 * @version 
 */
public class XPathModelImpl
    implements XPathModel {
    
    /** The XPath tree compiler. */
    private Compiler mCompiler;

    /** The logger. */
    private static final Logger mLogger = Logger.getLogger(XPathModelImpl.class.getName());
    
    /** The message manager. */
    private static MessageManager mMsgMgr = MessageManager.getManager(XPathModelImpl.class);


    /** Instantiates a new object. */
    public XPathModelImpl() {
        mCompiler = new XPathTreeCompiler();
    }


    /**
     * Parses an XPath expression.
     * @param expression the XPath expression to parse
     * @return an instance of XPathExpression
     * @throws XPathException for any parsing errors
     */
    public XPathExpression parseExpression(String expression)
        throws XPathException {
        try {
            Object expr = Parser.parseExpression(expression, mCompiler);
            if (expr instanceof Expression) {
                return processExpression((Expression) expr);
            } else {
                XPathException xpe = new XPathException(
                    mMsgMgr.getString("Unhandled_XPath_Expression",
                                            expression, expr.toString()));
                mLogger.log(Level.SEVERE, "parseExpression", xpe);
                throw xpe;
            }
        } catch (JXPathException jxe) {
            mLogger.log(Level.SEVERE, "parseExpression", jxe);
            throw new XPathException(jxe);
        }
    }

    /**
     * Processes the parsed expression to build an XPathExpression.
     * @param expression the parsed expression
     * @return an instance of XPathExpression
     * @throws XPathException for any errors
     */
    XPathExpression processExpression(Expression expression)
        throws XPathException {
        if (expression instanceof LocationPath) {
            return processLocationPath((LocationPath) expression);
        } else if (expression instanceof CoreFunction) {
            return processCoreFunction((CoreFunction) expression);
        } else if (expression instanceof ExtensionFunction) {
            return processExtensionFunction((ExtensionFunction) expression);
        } else if (expression instanceof CoreOperation) {
            return processCoreOperation((CoreOperation) expression);
        } else if (expression instanceof Constant) {
            return processConstant((Constant) expression);
        } else if (expression instanceof VariableReference) {
            return processVariableReference((VariableReference) expression);            
        } else if (expression instanceof ExpressionPath) {
        	return processExpressionPath((ExpressionPath) expression);
        } else {
            XPathException xpe = new XPathException(
                mMsgMgr.getString("Unhandled_Expression_Type",
                                    expression.toString()));
            mLogger.log(Level.SEVERE, "processExpression", xpe);
            throw xpe;
        }
    }

    XPathExpression processVariableReference(VariableReference varRef)
        throws XPathException {

    	String vname = varRef.getVariableName().toString();
    	QName qname = com.sun.bpel.xml.NamespaceUtility.getQNameFromString(vname);
        String name = com.sun.bpel.xml.NamespaceUtility.getQNameAsString(qname);
        return AbstractXPathModelHelper.getInstance().newXPathVariableReference(name);    	
    }

    /**
     * Processes a Constant expression.
     * @param constant the constant
     * @return the constant expression
     * @throws XPathException for any errors
     */
    XPathExpression processConstant(Constant constant)
        throws XPathException {
        Object value = constant.computeValue(null);
        XPathExpression xpexpr;

        if (value instanceof String) {
            xpexpr = AbstractXPathModelHelper.getInstance().newXPathStringLiteral((String) value);
        } else if (value instanceof Number) {
            xpexpr = AbstractXPathModelHelper.getInstance().newXPathNumericLiteral((Number) value);
        } else {
            XPathException xpe = new XPathException(
                mMsgMgr.getString("Invalid_XPath_Constant", value));
            mLogger.log(Level.SEVERE, "processConstant", xpe);
            throw xpe;
        }
        
        mLogger.finer("constant=" + constant + " value=" + value);
        
        return xpexpr;
    }


    /**
     * Processes a CoreFunction expression. Converts the CoreFunction into an
     * XPathExpression and then adds it into the list of values. Then each
     * argument of the CoreFunction is processed.
     * @param coreFunction the core function
     * @return the core function expression
     * @throws XPathException for any errors
     */
    XPathExpression processCoreFunction(CoreFunction coreFunction)
        throws XPathException {
        int code = getCoreFunctionCode(coreFunction);
        Expression[] arguments = coreFunction.getArguments();

        if (-1 == code) {
            XPathException xpe = new XPathException(
                mMsgMgr.getString("Unhandled_XPath_Function",
                                    coreFunction.toString()));
            mLogger.log(Level.SEVERE, "processCoreFunction", xpe);
            throw xpe;
        }
        
        XPathExpression xpexpr = AbstractXPathModelHelper.getInstance().newXPathCoreFunction(code);

        // Process the arguments, if any.
        if (arguments != null) {
            for (int i = 0; i < arguments.length; i++) {
                xpexpr.addChild(processExpression(arguments[i]));
            }
        }
        
        mLogger.finer("coreFunction=" + coreFunction);
        
        return xpexpr;
    }


    /**
     * Processes a CoreOperation expression. Converts the CoreOperation into an
     * XPathExpression and then adds it into the list of values. Then each
     * argument of the CoreOperation is processed.
     * @param coreOperation the core operation
     * @return the core operation expression
     * @throws XPathException for any errors
     */
    XPathExpression processCoreOperation(CoreOperation coreOperation)
        throws XPathException {
        int code = getCoreOperationCode(coreOperation);
        Expression[] arguments = coreOperation.getArguments();

        if (-1 == code) {
            XPathException xpe = new XPathException(
                mMsgMgr.getString("Unhandled_XPath_Operator",
                                    coreOperation.toString()));
            mLogger.log(Level.SEVERE, "processCoreOperation", xpe);
            throw xpe;
        }
        
        XPathExpression xpexpr = AbstractXPathModelHelper.getInstance().newXPathCoreOperation(code);

        // Process the arguments. All core operations require arguments.
        // If arguments is null, it means there is a bug -- the error
        // should have been caught in parse expression.
        for (int i = 0; i < arguments.length; i++) {
            xpexpr.addChild(processExpression(arguments[i]));
        }
        
        mLogger.finer("coreOperation=" + coreOperation);
        
        return xpexpr;
    }


    /**
     * Processes an ExtensionFunction expression. Converts the ExtensionFunction
     * into an XPathExpression and then adds it into the list of values. Then
     * each argument of the ExtensionFunction is processed.
     * @param extensionFunction the extension function
     * @return the extension function expression
     * @throws XPathException for any errors
     */
    XPathExpression processExtensionFunction(ExtensionFunction extensionFunction)
        throws XPathException {
        String name = extensionFunction.getFunctionName().toString();
        Expression[] arguments = extensionFunction.getArguments();
        
        if (null == name) {
            XPathException xpe = new XPathException(
                mMsgMgr.getString("Unhandled_XPath_Function",
                                    extensionFunction.toString()));
            mLogger.log(Level.SEVERE, "processExtensionFunction", xpe);
            throw xpe;
        } else if (!isValidFunction(name)) {
            XPathException xpe = new XPathException(
                mMsgMgr.getString("Invalid_XPath_Function",
                                    extensionFunction.toString()));
            mLogger.log(Level.SEVERE, "processExtensionFunction", xpe);
            throw xpe;
        }
        
        XPathExpression xpexpr =
            AbstractXPathModelHelper.getInstance().newXPathExtensionFunction(name);
        
        // Process the arguments, if any.
        if (arguments != null) {
            for (int i = 0; i < arguments.length; i++) {
                xpexpr.addChild(processExpression(arguments[i]));
            }
        }
        
        mLogger.finer("extensionFunction=" + extensionFunction
                        + " name=" + name);
                        
        return xpexpr;
    }


    /**
     * Processes a LocationPath expression. Converts the LocationPath into a
     * TreePath whose components are nodes in the given schema model. Adds the
     * TreePath into the list of values. Each step in the location path is
     * equivalent to a TreePath component. TO DO: handle predicates! TO DO:
     * special handling for relative paths!
     * @param locationPath the location path
     * @return the location path expression
     * @throws XPathException for any errors
     */
    XPathExpression processLocationPath(LocationPath locationPath)
        throws XPathException {
        Step[] steps = locationPath.getSteps();
        
        ArrayList stepList = new ArrayList();
        if (steps != null) {
            for (int i = 0; i < steps.length; i++) {
                stepList.add(processStep(steps[i]));
            }
        }
        
        LocationStep[] locSteps = new LocationStepImpl[stepList.size()];
        Iterator iter = stepList.iterator();
        for (int i = 0; iter.hasNext(); i++) {
            locSteps[i] = (LocationStep) iter.next();
        }          
        
        XPathLocationPath path =
            AbstractXPathModelHelper.getInstance().newXPathLocationPath(locSteps);
        path.setAbsolute(locationPath.isAbsolute());
        path.setSimplePath(locationPath.isSimplePath());
        
        return path;
    }


    /**
     * Processes a ExpressionPath expression. Converts the ExpressionPath into a
     * TreePath whose components are nodes in the given schema model. Adds the
     * TreePath into the list of values. Each step in the location path is
     * equivalent to a TreePath component. TO DO: handle predicates! TO DO:
     * special handling for relative paths!
     * @param expressionPath the location path
     * @return the expression path expression
     * @throws XPathException for any errors
     */
    XPathExpression processExpressionPath(ExpressionPath expressionPath)
        throws XPathException {
    	Expression rootExpression = expressionPath.getExpression();
    	XPathExpression rExpression = null;
    	if(rootExpression != null) {
    		rExpression = processExpression(rootExpression);
    	}
    	
    	Step[] steps = expressionPath.getSteps();
        
        ArrayList stepList = new ArrayList();
        if (steps != null) {
            for (int i = 0; i < steps.length; i++) {
                stepList.add(processStep(steps[i]));
            }
        }
        
        LocationStep[] locSteps = new LocationStepImpl[stepList.size()];
        Iterator iter = stepList.iterator();
        for (int i = 0; iter.hasNext(); i++) {
            locSteps[i] = (LocationStep) iter.next();
        }          
        
        XPathExpressionPath path =
            AbstractXPathModelHelper.getInstance().newXPathExpressionPath(rExpression, locSteps);
        path.setSimplePath(expressionPath.isSimplePath());
        
        return path;
    }
    
    /**
     * Processes a step in a location path.
     * @param step the step in the location path
     * @return the location step
     * @throws XPathException for any errors
     */
    LocationStep processStep(Step step)
        throws XPathException {
        int axis = getAxis(step.getAxis());
        NodeTest nodeTest = step.getNodeTest();
        Expression[] predicates = step.getPredicates();
        String name = null;
        StepNodeTest stepNodeTest = null;

        if (nodeTest instanceof NodeNameTest) {
            stepNodeTest =
                new StepNodeNameTest(
                    ((NodeNameTest) nodeTest).getNodeName().toString());
        } else if (nodeTest instanceof NodeTypeTest) {
            stepNodeTest =
                new StepNodeTypeTest(
                    getNodeType(((NodeTypeTest) nodeTest).getNodeType()));
        } else {
            XPathException xpe = new XPathException(
                mMsgMgr.getString("Invalid_Location_Step", step.toString()));
            mLogger.log(Level.SEVERE, "processStep", xpe);
            throw xpe;
        }
        
        XPathExpression[] xpathPredicates = null;
        if (predicates != null && predicates.length > 0) {
            xpathPredicates = new XPathExpression[predicates.length];
            for (int i = 0, length = predicates.length; i < length; i++) {
                xpathPredicates[i] = processExpression(predicates[i]);
                if (xpathPredicates[i] instanceof XPathNumericLiteral) {
                    xpathPredicates[i] = AbstractXPathModelHelper.getInstance()
                        .newXPathPredicateNumericLiteral
                        (new Long(((XPathNumericLiteral) xpathPredicates[i]).getValue().longValue()));
                }
            }
        }
        return new LocationStepImpl(axis, stepNodeTest, xpathPredicates);
    }
    
    /**
     * Gets the axis.
     * @param code the axis code
     * @return the axis type or -1 if invalid
     */
    int getAxis(int code) {
        switch (code) {
        case Compiler.AXIS_SELF:
            return LocationStep.AXIS_SELF;
        case Compiler.AXIS_CHILD:
            return LocationStep.AXIS_CHILD;
        case Compiler.AXIS_PARENT:
            return LocationStep.AXIS_PARENT;
        case Compiler.AXIS_ANCESTOR:
            return LocationStep.AXIS_ANCESTOR;
        case Compiler.AXIS_ATTRIBUTE:
            return LocationStep.AXIS_ATTRIBUTE;
        case Compiler.AXIS_NAMESPACE:
            return LocationStep.AXIS_NAMESPACE;
        case Compiler.AXIS_PRECEDING:
            return LocationStep.AXIS_PRECEDING;
        case Compiler.AXIS_FOLLOWING:
            return LocationStep.AXIS_FOLLOWING;
        case Compiler.AXIS_DESCENDANT:
            return LocationStep.AXIS_DESCENDANT;
        case Compiler.AXIS_ANCESTOR_OR_SELF:        
            return LocationStep.AXIS_ANCESTOR_OR_SELF;
        case Compiler.AXIS_DESCENDANT_OR_SELF:
            return LocationStep.AXIS_DESCENDANT_OR_SELF;
        case Compiler.AXIS_FOLLOWING_SIBLING:
            return LocationStep.AXIS_FOLLOWING_SIBLING;
        case Compiler.AXIS_PRECEDING_SIBLING:
            return LocationStep.AXIS_PRECEDING_SIBLING;
        }
        
        return -1;
    }
    
    
    /**
     * Gets the node type.
     * @param code the node type code
     * @return the node type or -1 if invalid
     */
    int getNodeType(int code) {
        switch (code) {
        case Compiler.NODE_TYPE_NODE:
            return LocationStep.NODETYPE_NODE;
        case Compiler.NODE_TYPE_TEXT:
            return LocationStep.NODETYPE_TEXT;
        case Compiler.NODE_TYPE_COMMENT:
            return LocationStep.NODETYPE_COMMENT;
        case Compiler.NODE_TYPE_PI:
            return LocationStep.NODETYPE_PI;
        }
        
        return -1;
    }          


    /**
     * Gets the core function code
     * @param coreFunction the core function
     * @return the function code or -1 if invalid
     * @throws XPathException invalid operation
     */
    int getCoreFunctionCode(CoreFunction coreFunction) {
        int code = coreFunction.getFunctionCode();

        switch (code) {
        case Compiler.FUNCTION_LAST:
            return XPathCoreFunction.FUNC_LAST;
        case Compiler.FUNCTION_POSITION:
            return XPathCoreFunction.FUNC_POSITION;
        case Compiler.FUNCTION_COUNT:
            return XPathCoreFunction.FUNC_COUNT;
        case Compiler.FUNCTION_ID:
            return XPathCoreFunction.FUNC_ID;
        case Compiler.FUNCTION_LOCAL_NAME:
            return XPathCoreFunction.FUNC_LOCAL_NAME;
        case Compiler.FUNCTION_NAMESPACE_URI:
            return XPathCoreFunction.FUNC_NAMESPACE_URI;
        case Compiler.FUNCTION_NAME:
            return XPathCoreFunction.FUNC_NAME;
        case Compiler.FUNCTION_STRING:
            return XPathCoreFunction.FUNC_STRING;
        case Compiler.FUNCTION_CONCAT:
            return XPathCoreFunction.FUNC_CONCAT;
        case Compiler.FUNCTION_STARTS_WITH:
            return XPathCoreFunction.FUNC_STARTS_WITH;
        case Compiler.FUNCTION_CONTAINS:
            return XPathCoreFunction.FUNC_CONTAINS;
        case Compiler.FUNCTION_SUBSTRING_BEFORE:
            return XPathCoreFunction.FUNC_SUBSTRING_BEFORE;
        case Compiler.FUNCTION_SUBSTRING_AFTER:
            return XPathCoreFunction.FUNC_SUBSTRING_AFTER;
        case Compiler.FUNCTION_SUBSTRING:
            return XPathCoreFunction.FUNC_SUBSTRING;
        case Compiler.FUNCTION_STRING_LENGTH:
            return XPathCoreFunction.FUNC_STRING_LENGTH;
        case Compiler.FUNCTION_NORMALIZE_SPACE:
            return XPathCoreFunction.FUNC_NORMALIZE_SPACE;
        case Compiler.FUNCTION_TRANSLATE:
            return XPathCoreFunction.FUNC_TRANSLATE;
        case Compiler.FUNCTION_BOOLEAN:
            return XPathCoreFunction.FUNC_BOOLEAN;
        case Compiler.FUNCTION_NOT:
            return XPathCoreFunction.FUNC_NOT;
        case Compiler.FUNCTION_TRUE:
            return XPathCoreFunction.FUNC_TRUE;
        case Compiler.FUNCTION_FALSE:
            return XPathCoreFunction.FUNC_FALSE;
        case Compiler.FUNCTION_LANG:
            return XPathCoreFunction.FUNC_LANG;
        case Compiler.FUNCTION_NUMBER:
            return XPathCoreFunction.FUNC_NUMBER;
        case Compiler.FUNCTION_SUM:
            return XPathCoreFunction.FUNC_SUM;
        case Compiler.FUNCTION_FLOOR:
            return XPathCoreFunction.FUNC_FLOOR;
        case Compiler.FUNCTION_CEILING:
            return XPathCoreFunction.FUNC_CEILING;
        case Compiler.FUNCTION_ROUND:
            return XPathCoreFunction.FUNC_ROUND;
        case Compiler.FUNCTION_NULL:
            return XPathCoreFunction.FUNC_NULL;
        case Compiler.FUNCTION_KEY:
            return XPathCoreFunction.FUNC_KEY;
        case Compiler.FUNCTION_FORMAT_NUMBER:
            return XPathCoreFunction.FUNC_FORMAT_NUMBER;
        }

        return -1;
    }


    /**
     * Gets the core operation code.
     *
     * @param coreOperation the core operation
     * @return the operation code or -1 if invalid
     * @throws XPathException invalid operation
     */
    int getCoreOperationCode(CoreOperation coreOperation) {
        if (coreOperation instanceof CoreOperationAdd) {
            return XPathCoreOperation.OP_SUM;
        } else if (coreOperation instanceof CoreOperationSubtract) {
            return XPathCoreOperation.OP_MINUS;
        } else if (coreOperation instanceof CoreOperationMultiply) {
            return XPathCoreOperation.OP_MULT;
        } else if (coreOperation instanceof CoreOperationDivide) {
            return XPathCoreOperation.OP_DIV;
        } else if (coreOperation instanceof CoreOperationMod) {
            return XPathCoreOperation.OP_MOD;
        } else if (coreOperation instanceof CoreOperationNegate) {
            return XPathCoreOperation.OP_NEGATIVE;
        } else if (coreOperation instanceof CoreOperationAnd) {
            return XPathCoreOperation.OP_AND;
        } else if (coreOperation instanceof CoreOperationOr) {
            return XPathCoreOperation.OP_OR;
        } else if (coreOperation instanceof CoreOperationEqual) {
            return XPathCoreOperation.OP_EQ;
        } else if (coreOperation instanceof CoreOperationNotEqual) {
            return XPathCoreOperation.OP_NE;
        } else if (coreOperation instanceof CoreOperationLessThan) {
            return XPathCoreOperation.OP_LT;
        } else if (coreOperation instanceof CoreOperationLessThanOrEqual) {
            return XPathCoreOperation.OP_LE;
        } else if (coreOperation instanceof CoreOperationGreaterThan) {
            return XPathCoreOperation.OP_GT;
        } else if (coreOperation instanceof CoreOperationGreaterThanOrEqual) {
            return XPathCoreOperation.OP_GE;
        }
       return -1;
    }
    
    
    /**
     * Determines if a function name is valid. Assumes the function name is
     * not one of the core functions.
     * @param functionName the name of the function
     * @return true if the function name is valid, false otherwise
     */
    static boolean isValidFunction(String functionName) {
        for (int i = 0; i < VALID_FUNCTION_NAMES.length; i++) {
            if (functionName.equals(VALID_FUNCTION_NAMES[i])) {
                return true;
            }
        }
        
        // For bpws, strip out the prefix because there's no guarantee that
        // the prefix is bpws.
        String name = functionName;
        int colon = name.indexOf(':');
        if (colon != -1) {
            name = name.substring(colon + 1, name.length());
        }
        for (int i = 0; i < VALID_BPWS_FUNCTION_NAMES.length; i++) {
            if (name.equals(VALID_BPWS_FUNCTION_NAMES[i])) {
                return true;
            }
        }
        
        return false;
    }
}
