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
 * @(#)Utility.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.util;

import java.io.File;
import java.io.FileNotFoundException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;
import javax.xml.namespace.QName;
import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELElement;
import com.sun.bpel.model.BPELHelper;
import com.sun.bpel.model.OnMessage;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.parser.impl.ApplicationVariablesHelper;
import com.sun.bpel.model.visitor.VariablePropertyVisitor;
import com.sun.bpel.model.visitor.XPathExpressionVisitor;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.wsdl4j.ext.bpel.MessagePropertyAlias;
import com.sun.xpath.AbstractXPathModelHelper;
import com.sun.xpath.XPathExpression;
import com.sun.xpath.XPathModel;

/**
 * This class has Utility Methods 
 * 
 * @author Sun Microsystems
 * @version 1.0
 */
public class Utility {

    /** join-only receive */
    public static int RECEIVE_TYPE_NO_COR_JOIN_ONLY = 1;

    /** create-only receive */
    public static int RECEIVE_TYPE_CREATE_ONLY = 2;

    /** create-or-correlate receive */
    public static int RECEIVE_TYPE_CREATE_OR_CORRELATE = 3;

    /** correlate-only receive */
    public static int RECEIVE_TYPE_CORRELATE_ONLY = 4;

    /** create-or-correlate receive for duplicate operations within same BPEL */
    public static int RECEIVE_TYPE_CREATE_OR_CORRELATE_DUP_OPER = 5;

    public static final Logger mLogger = Logger.getLogger(Utility.class.getName());

    private static enum PROPERTY_TYPE {
        MESSAGE, XSD_TYPE, XSD_ELEMENT
    };

    /** Test if a string is empty.
     * @param   s   String to test
     * @return  <code>true</code> if string is empty
     */
    public static boolean isEmpty(String s) {
        return ((null == s) || (s.trim().length() == 0));
    }

    /** Test if a string is reference to an Application Variable
     * @param   s   String to test
     * @return  <code>true</code> if string is reference to an Application Variable
     */
    public static boolean isAppVar(String s) {
        return s != null
                && s.startsWith(ApplicationVariablesHelper.TOKEN_START_SYMBOL)
                && s.endsWith(ApplicationVariablesHelper.TOKEN_END_SYMBOL);
    }

    /** Test if two objects are equal.
     * @param   s1  First string.
     * @param   s2  Second string.
     * @return  <code>true</code> if strings are equal.
     */
    public static boolean areEqual(Object s1, Object s2) {

        return (s1 == null ? s2 == null : s1.equals(s2)); 
    }

    /** Test if two strings are equal in XML.
     * @param   s1  First string.
     * @param   s2  Second string.
     * @return  <code>true</code> if strings are equal.
     */
    public static boolean areEqualXMLValues(String s1, String s2) {

        return ((s1 == null && s2 == null)
                || (s1 == null && isEmpty(s2))
                || (isEmpty(s1) && s2 == null)
                || (s1 != null && s1.equals(s2)));
    }

    /** Wraps a string into lines of specified character width.  Imbedded newlines will
     * be honored.
     * @param   text    Text string to wrap.
     * @param   width   Character width of each line.
     * @return  One string with appropriate line.separators inserted.
     * @author Sun Microsystems
     *          Criterion, Inc.   (http://www.criterioninc.com)
     * @author Sun Microsystems
     */
    public static String lineWrap(String text, int width) {
        int fromIndex = 0;
        int pos = 0;
        int bestpos;
        String largestString;
        List lines = new ArrayList();
        String s = text;
        String retStr = text;

        // while we haven't run past the end of the string...
        while (fromIndex != -1) {
            // Automatically skip any spaces at the beginning of the line
            while ((fromIndex < text.length()) && (text.charAt(fromIndex) == ' ')) {
                ++fromIndex;
                // If we hit the end of line
                // while skipping spaces, we're done.
                if (fromIndex >= text.length()) {
                    break;
                }
            }

            // fromIndex represents the beginning of the line
            pos = fromIndex;
            bestpos = -1;
            largestString = null;

            while (pos >= fromIndex) {
                boolean bHardNewline = false;
                int newlinePos = text.indexOf('\n', pos);
                int spacePos   = text.indexOf(' ', pos);

                if ((newlinePos != -1)          // there is a newline and either
                        && ((spacePos == -1)    // 1. there is no space, or
                                || (spacePos != -1
                                        && (newlinePos < spacePos)))) {
                    // 2. the newline is first
                    pos = newlinePos;
                    bHardNewline = true;
                } else {
                    pos = spacePos;
                    bHardNewline = false;
                }

                // Couldn't find another space?
                if (pos == -1) {
                    s = text.substring(fromIndex);
                } else {
                    s = text.substring(fromIndex, pos);
                }

                // If the string fits, keep track of it.
                if (s.length() < width) {
                    largestString = s;
                    bestpos = pos;

                    // If we've hit the end of the
                    // string or a newline, use it.
                    if (bHardNewline)
                        bestpos++;
                    if (pos == -1 || bHardNewline) break;
                } else {
                    break;
                }

                ++pos;
            }

            if (largestString == null) {
                // Couldn't wrap at a space, so find the largest line
                // that fits and print that.  Note that this will be
                // slightly off -- the width of a string will not necessarily
                // be the sum of the width of its characters, due to kerning.
                int totalWidth = 0;
                int oneCharWidth = 0;

                pos = fromIndex;

                while (pos < text.length()) {
                    oneCharWidth = 1;
                    if ((totalWidth + oneCharWidth) >= width) break;
                    totalWidth += oneCharWidth;
                    ++pos;
                }

                lines.add(text.substring(fromIndex, pos));
                fromIndex = pos;
            } else {
                // Disregard trailing newlines
                if ((largestString.length() > 0) || (bestpos != -1)) {
                    lines.add(largestString);
                }
                fromIndex = bestpos;
            }
        }

        if ((lines != null) && (lines.size() > 0)) {
            StringBuffer wrapped = new StringBuffer(text.length());
            for (int i = 0; i < lines.size(); i++) {
                if ((i > 0) && !((String) lines.get(i - 1)).endsWith("\n")) {
                    wrapped.append(System.getProperty("line.separator"));
                }
                wrapped.append((String) lines.get(i));
            }
            retStr = wrapped.toString();
        }

        return retStr;
    }

    /**
     * in order to support the $ syntex, the expression containting all the
     * xpath variable references will be parsed and stored. 
     * @param expr
     * @return List
     */
    /*public static List getXPathExpressionList(String expr) throws Exception {

        List list = new ArrayList();

        XPathModel model = AbstractXPathModelHelper.getInstance().newXPathModel();
        try {
            XPathExpression xPathExpr = model.parseExpression(expr);
            XPathExpressionVisitor visitor = new XPathExpressionVisitor();
            xPathExpr.accept(visitor);
            list = visitor.getExpressionList();
        } catch (Exception e) {
        //} catch (XPathException e) {
            //TODO This exception needs to be thrown during the deployment, masked currently as this
            // can fail the current test cases and prevent the deployment of the service assembly and no test
            // could be run. 
            mLogger.fine("Exception during parsing xpath expression : " + e.getMessage());
            //throw new Exception(e);
        }
        return list;
    }*/

    //#########ADDED HERE FOR CHANGE ######################

    /**
     * gets create instance value
     *
     * @param createInstance create instance
     *
     * @return boolean: if create instance equals "yes", returns true; otherwise, returns false
     */
    public static boolean getCreateInstanceValue(String createInstance) {
        return "yes".equalsIgnoreCase(createInstance); //$NON-NLS-1$
    }

    public static String[] getArgs2(String expr) {
        // args[0] -> variable (xsd type)
        // args[1] -> part
        // args[2] -> query
        // args[3] -> jxpathvariable (messageType and xsdtype)

        String[] args = new String[4];

        int dollarSignIndex = expr.indexOf('$');

        int queryIndex = expr.indexOf('/');

        if (queryIndex < 0) {
            int partIndex = expr.indexOf('.');

            if(partIndex < 0) {
                args[0] = expr.substring(dollarSignIndex + 1);
                args[1] = null;
                args[2] = null;
                args[3] = args[0];
                return args;
            }

            args[3] = expr.substring(dollarSignIndex + 1);
            String[] args2 = getVariableAndPart(args[3]);
            args[0] = args2[0];
            args[1] = args2[1];
            args[2] = null;
            return args;
        }

        args[3] = expr.substring(dollarSignIndex + 1, queryIndex);
        String[] args3 = getVariableAndPart(args[3]);
        args[0] = args3[0];
        args[1] = args3[1];
        args[2] = expr.substring(1 + queryIndex);
        return args;
    }   

    private static String[] getVariableAndPart(String expr) {
        // args[0] -> variable (xsd type)
        // args[1] -> part

        String[] args = new String[2];
        int partIndex = expr.indexOf('.');

        if(partIndex < 0) {
            args[0] = expr;
            args[1] = null;
            return args;
        }

        args[0] = expr.substring(0, partIndex);
        args[1] = expr.substring(1 + partIndex);

        return args;
    }    

    /**
     * Parses the specified doXslTransform function call to determine the
     * XSL stylesheet's absolute location.
     * 
     * @param xsl The first parameter of a doXslTransform function call.
     * @param baseURI The base uri of the bpel document containing the call.
     * @return The absolute path of the XSL stylesheet, if it exists.
     * @throws FileNotFoundException if the file does not exist.
     * @throws URISyntaxException if the specified URI is not valid.
     */
    public static String parseDoXslTransform(String xsl, String baseURI) 
            throws Exception {
        // determine file's location
        File stylesheet = null;
        if (xsl.startsWith("urn:stylesheets:")) {
            // load from local service unit root path
            String filename = xsl.substring(1 + xsl.indexOf(':', 4));// second colon
            // determine root path of deployed stylesheet
            String rootPath = (new File(baseURI)).getParent();
            stylesheet = new File(rootPath, filename);
            if (!stylesheet.exists()) {
                throw new FileNotFoundException(stylesheet.getAbsolutePath());
            }
        }
        else {
            stylesheet = new File(new URI(xsl));
        }

        return stylesheet.getAbsolutePath();
    }
    
    public static XpathVariablePropertyInfo parseExprForVariables(String expr,
            BPELElement parentElement) throws Exception {
        Map<String, MessagePropertyAlias> propAliasMap = new HashMap<String, MessagePropertyAlias>();
        Set<XpathVariableInfo> varInfoSet = new HashSet<XpathVariableInfo>();

        XPathExpression xPathExpr = getXPathExpression(expr);
        XPathExpressionVisitor visitor = getXPathExpressionList(xPathExpr);

        List xpathExpressionsList = visitor.getExpressionList();

        VariablePropertyVisitor propVisitor = visitVariableProperty(xPathExpr);
        List<VariablePropertyVisitor.PropertyParamObj> xpathPropertyList = propVisitor.getPropertyList();

        BPELDocument doc = (BPELDocument) parentElement.getOwnerDocument();
        RBPELProcess process = (RBPELProcess) doc.getDocumentProcess();

        //identfies the variables
        for (Iterator iterator = xpathExpressionsList.iterator(); iterator.hasNext();) {

            String expr1 = (String) iterator.next();
            varInfoSet.addAll(Utility.getVariables(expr1, parentElement));
        }
        //identifies the properties in getVariableProperty
        for (VariablePropertyVisitor.PropertyParamObj propObj : xpathPropertyList) {
            String variableName = propObj.variableName;
            QName propertyQName = getQNamefromString(parentElement, propObj.propertyName);
            
            if (propertyQName != null) { // In-lined NM property is not a
                                            // QName, so it would be null
                Collection<MessagePropertyAlias> propAliases = process.getBPELPropertyAlias(propertyQName);
                XpathVariableInfo info = getVariableInfo(variableName, varInfoSet);
                String key = variableName + "#" + propObj.propertyName; //$NON-NLS-1$
                MessagePropertyAlias alias = getPropAlias(info, propAliases);
                if (alias == null) {
                    // propertyAlias is defined on NMProperty
                    alias = getNMMessagePropertyAlias(propAliases);
                }
                propAliasMap.put(key, alias);

            } else { //process in-line NMProperty
                //No processing required
            }
        }

        return new XpathVariablePropertyInfo(varInfoSet, propAliasMap);
    }

    private static MessagePropertyAlias getNMMessagePropertyAlias(
            Collection<MessagePropertyAlias> propAliases) {
        for (MessagePropertyAlias pa : propAliases) {
            if (pa.getNMProperty() != null) {
                return pa;
            }
        }
        return null;
    }

    private static QName getQNamefromString(BPELElement parentElement, String propExpr) {
        int colonIndex = propExpr.indexOf(":"); //$NON-NLS-1$
        if(colonIndex < 0){
            return null;
        }
        // extract the namespace prefix from the propName.
        String prefix = propExpr.substring(0, colonIndex);
        String localPart = propExpr.substring(colonIndex + 1);
        String namespaceURI = parentElement.getNamespace(prefix);
        QName propertyQName = new QName(namespaceURI, localPart);
        return propertyQName;
    }

    private static XpathVariableInfo getVariableInfo(String variable,
            Set<XpathVariableInfo> variableInfoSet) {

        XpathVariableInfo ret = null;

        for (XpathVariableInfo info : variableInfoSet) {
            String variableName = info.variableName;
            if (variableName.equals(variable)) {
                ret = info;
            }
        }

        return ret;
    }

    private static MessagePropertyAlias getPropAlias(XpathVariableInfo varInfo,
            Collection propAliases) {
        MessagePropertyAlias ret = null;
        RVariable variableDefn = varInfo.varDefinition;
        QName qname = null;
        if ((qname = variableDefn.getMessageType()) != null) {
            ret = getPropAliasForType(qname, PROPERTY_TYPE.MESSAGE, propAliases);
        } else if ((qname = variableDefn.getElement()) != null) {
            ret = getPropAliasForType(qname, PROPERTY_TYPE.XSD_ELEMENT,
                    propAliases);
        } else {
            qname = variableDefn.getType();
            ret = getPropAliasForType(qname, PROPERTY_TYPE.XSD_TYPE,
                    propAliases);
        }

        return ret;
    }
    
    private static MessagePropertyAlias getPropAliasForType(
            QName variableDefnQname, Utility.PROPERTY_TYPE type,
            Collection propAliases) {
        Iterator itr = propAliases.iterator();
        MessagePropertyAlias propAlias = null;

        while (itr.hasNext()) {
            QName propVarQName = null;
            propAlias = (MessagePropertyAlias) itr.next();

            switch (type) {
            case MESSAGE:
                if (propAlias.getMessageType() != null) {
                    propVarQName = propAlias.getMessageType().getQName();
                }
                break;
            case XSD_ELEMENT:
                propVarQName = propAlias.getElementName();
                break;
            case XSD_TYPE:
                propVarQName = propAlias.getTypeName();
                break;
            }

            if (propVarQName != null && propVarQName.equals(variableDefnQname)) {
                // for a given property and a given message type, there can only
                // be one property alias.
                return propAlias;
            }
        }

        return null;
    }

    private static XPathExpression getXPathExpression(String expr) 
    throws Exception {
        XPathExpression xPathExpr = null;
        try {
            XPathModel model = AbstractXPathModelHelper.getInstance()
            .newXPathModel();
            xPathExpr = model.parseXpathExpression(expr);

        } catch (Exception e) {
            mLogger.severe("Exception during parsing xpath expression : "
                    + e.getMessage());
            throw new Exception(e);
        }
        return xPathExpr;
    }

    private static XPathExpressionVisitor getXPathExpressionList(XPathExpression xPathExpr)
    throws Exception {
        XPathExpressionVisitor visitor = null;
        try {
            if (xPathExpr != null) {
                visitor = new XPathExpressionVisitor();
                xPathExpr.accept(visitor);
            }
        } catch (Exception e) {
            mLogger.severe("Exception during parsing xpath expression : "
                    + e.getMessage());
            throw new Exception(e);
        }
        return visitor;
    } 

    private static VariablePropertyVisitor visitVariableProperty(XPathExpression xPathExpr) 
    throws Exception {
        VariablePropertyVisitor visitor = null;
        try {
            if (xPathExpr != null) {
                visitor = new VariablePropertyVisitor();
                xPathExpr.accept(visitor);
            }
        } catch (Exception e) {
            mLogger.severe("Exception during parsing xpath expression : "
                    + e.getMessage());
            throw new Exception(e);
        }
        return visitor;
    }

    private static Set<XpathVariableInfo> getVariables(String expr, BPELElement parentElement) {
        Set<XpathVariableInfo> varInfoSet = new HashSet<XpathVariableInfo>();
        char exprArray[] = expr.toCharArray();
        int exprLength = exprArray.length;
        boolean inVariable = false;
        int dotPos = -1;
        int varStartPos = -1;
        int cursor = 0;

        for (; cursor < exprLength; cursor++) {
            if (inVariable) {
                if (isPartVariable(exprArray[cursor])) {
                    dotPos = cursor;
                } else if (isVariableEndIndicator(exprArray[cursor])) {
                    inVariable = false;
                    XpathVariableInfo varInfo = new XpathVariableInfo(parentElement, expr, varStartPos, dotPos, cursor);
                    varInfoSet.add(varInfo);
                    dotPos = -1;
                }
            } else {
                if (isVariableBeginIndicator(exprArray[cursor])) {
                    inVariable = true;
                    varStartPos = cursor + 1;
                }
            }
        }
        // for a variable defined in from/to construct, just by itself
        if (inVariable) {
            XpathVariableInfo varInfo = new XpathVariableInfo(parentElement, expr, varStartPos, dotPos, cursor);
            varInfoSet.add(varInfo);
        }
        return varInfoSet;
    }    

    private static boolean isVariableBeginIndicator(char cursorChar) {
        if (cursorChar == '$') {
            return true;
        }
        return false;
    }

    private static boolean isVariableEndIndicator(char cursorChar) {
        if ((cursorChar == ' ') || (cursorChar == '/') || (cursorChar == '[') || (cursorChar == ']')
                || (cursorChar == ',') || (cursorChar == '(') || (cursorChar == ')')) {
            return true;
        }
        return false;
    }

    private static boolean isPartVariable(char cursorChar) {
        if (cursorChar == '.') {
            return true;
        }
        return false;
    }

    public static void setStartType(Set startElementsWhichDontCreateInstance, 
            Set startElemsWhichCreateInstance, String tns) {

        for (Iterator itr = startElementsWhichDontCreateInstance.iterator();  itr.hasNext(); ) {

            RStartElement rElem = (RStartElement) itr.next();
            Utility.setStartTypeForCorrelatingElems(rElem, tns);
        }
        boolean hasMultipleStartElems = (startElemsWhichCreateInstance.size() > 1);
        boolean areAllSamePickBasedStartElements = areAllSamePickBasedStartElements(startElemsWhichCreateInstance);

        for (Iterator itr = startElemsWhichCreateInstance.iterator();  itr.hasNext(); ) {

            RStartElement rElem = (RStartElement) itr.next();
            Utility.setStartTypeForInstanceCreatingElems(rElem, tns, 
                    hasMultipleStartElems, areAllSamePickBasedStartElements);
        }
    } 

    private static void setStartTypeForCorrelatingElems(RStartElement startElem, 
            String tns) {


        boolean create = startElem.getRCreateInstance();
        Collection corrList = null;

        if (startElem.getCorrelations() != null) {
            corrList = startElem.getCorrelations().getCorrelations();
        }

        int createFlag = -1;

        if (corrList == null) {
            /**
             * Rec
             */
            createFlag = RECEIVE_TYPE_NO_COR_JOIN_ONLY;
        } else {

            /**
             * Rec - Correlation="no (and ("yes" or "join"))"
             * (Or)
             * 
             * Flow
             *  Rec - Correlation="no (and ("yes" or "join"))"
             *  
             *  Receive replaceable with a pick in the above cases
             */
            createFlag = RECEIVE_TYPE_CORRELATE_ONLY;
        }

        startElem.setStartType(createFlag);   
    }    

    /** sets startType for StartElements which createInstance.
     * 
     * @param startElem
     * @param tns
     * @param isCreateOrCorrelatePossible
     */
    private static void setStartTypeForInstanceCreatingElems(RStartElement startElem, 
            String tns, boolean hasMultipleStartElems, boolean areAllSamePickBasedStartElements) {


        Collection corrList = null;

        if (startElem.getCorrelations() != null) {
            corrList = startElem.getCorrelations().getCorrelations();
        }

        int createFlag = -1;

        if (corrList == null) {
            /** 
             * Rec - CreateInstance="yes"
             * 
             * Flow
             *  Rec - CreateInstance="yes" 
             * </Flow>
             * (Or)
             *  
             * Pick - OnMesg - CreateInstance="yes"
             * (Or)
             * 
             * Pick 
             *      - OnMesg - CreateInstance="yes"
             *      - OnMesg - CreateInstance="yes"
             * (Or)
             * 
             * Flow
             *      Pick - OnMesg - CreateInstance="yes"
             * </Flow>
             *  (Or)
             *  
             * Flow
             *      Pick 
             *           - OnMesg - CreateInstance="yes"
             *           - OnMesg - CreateInstance="yes"
             * </Flow>
             * 
             */
            createFlag = RECEIVE_TYPE_CREATE_ONLY;
        } else if (corrList != null) {

            if (hasMultipleStartElems && !areAllSamePickBasedStartElements) {
                // boolean isCreateOrCorrelatePossible = checkIfCreateOrCorrelate(corrList, tns);
                // boolean isInFlow = (((Activity) startElem).getXPath().indexOf(Flow.TAG) != -1);
                //    if (isCreateOrCorrelatePossible && isInFlow) {

                /** Only case where createOrCorrelate is possible is of the following 
                 * pattern, where the number of Receives(Picks) is greater than 1
                 * 
                 * Flow
                 *  Rec1 - CreateInstance="yes", Correlation="join 
                 *  Rec2 - CreateInstance="yes", Correlation="join 
                 * </Flow>
                 * (Or)
                 * 
                 * Flow
                 *  Rec1 - CreateInstance="yes", Correlation="join 
                 *  Pick 
                 *      - OnMesg - CreateInstance="yes", Correlation="join"
                 *      - OnMesg - CreateInstance="yes", Correlation="join"
                 * </Flow>
                 * (Or)
                 * 
                 * Flow
                 *  Pick1 
                 *      - OnMesg1 - CreateInstance="yes", Correlation="join"
                 *      - OnMesg2 - CreateInstance="yes", Correlation="join"
                 *      
                 *  Pick2 
                 *      - OnMesg3 - CreateInstance="yes", Correlation="join"
                 *      - OnMesg4 - CreateInstance="yes", Correlation="join"
                 * </Flow>
                 * 
                 */
                createFlag = RECEIVE_TYPE_CREATE_OR_CORRELATE;
            } else {
                /**
                 * Rec - CreateInstance="yes", Correlation="yes"
                 * (Or)
                 * 
                 * Flow
                 *  Rec - CreateInstance="yes", Correlation="yes"
                 * </Flow>
                 *  (Or)
                 *  
                 * Flow
                 *  Rec1 - CreateInstance="yes", Correlation="join 
                 * </Flow>
                 * 
                 * Pick - OnMesg - CreateInstance="yes", Correlation="yes"
                 * (Or)
                 * 
                 * Pick 
                 *      - OnMesg - CreateInstance="yes", Correlation="yes (or join)"
                 *      - OnMesg - CreateInstance="yes", Correlation="yes (or join)"
                 * (Or)
                 * 
                 * Flow
                 *      Pick - OnMesg - CreateInstance="yes", Correlation="yes (or join)"
                 * </Flow>
                 *  (Or)
                 *  
                 * Flow
                 *      Pick 
                 *           - OnMesg - CreateInstance="yes", Correlation="yes (or join)"
                 *           - OnMesg - CreateInstance="yes", Correlation="yes (or join)"
                 * </Flow>
                 * 
                 */
                createFlag = RECEIVE_TYPE_CREATE_ONLY;
            }
        }

        startElem.setStartType(createFlag);
    }    

    /**
     * @param startElemsWhichCreateInstance
     * @return
     */
    private static boolean areAllSamePickBasedStartElements(Set startElemsWhichCreateInstance) {
        if (startElemsWhichCreateInstance.size() == 0) {
            return false;
        }
        Iterator itr = startElemsWhichCreateInstance.iterator();
        RStartElement elem = (RStartElement) itr.next();
        boolean isOnMesgTypeElem = (elem instanceof OnMessage);
        if (!isOnMesgTypeElem) {
            // a possibility of a receive, implies a flow too.
            //      - Receive
            // Flow
            //      - Pick - OnMessage
            return false;
        }
        // get the Pick XML Element object of the OnMessage
        XMLNode oldParentNode = ((XMLNode) elem).getParent();
        while (itr.hasNext()) {
            elem = (RStartElement) itr.next();
            isOnMesgTypeElem = (elem instanceof OnMessage);
            if (!isOnMesgTypeElem) {
                // a possibility of a receive, implies a flow too.
                //      - Receive
                // Flow
                //      - Pick - OnMessage
                return false;
            }
            // get the Pick XML Element object of the OnMessage
            XMLNode parentNode = ((XMLNode) elem).getParent();
            if (parentNode != oldParentNode) {
                return false;
            }
        }

        return true;
    }    

    public static class XpathVariableInfo {
        public String variableName;
        public String xpathVariableName;
        public String partName;
        public RVariable varDefinition;

        XpathVariableInfo(BPELElement parentElement, String expr, int beginIndex, int dotPos, int endIndex) {
            xpathVariableName = expr.substring(beginIndex, endIndex);
            if (dotPos > 0) {
                variableName = expr.substring(beginIndex, dotPos);
                partName = expr.substring(dotPos + 1, endIndex);
            } else {
                variableName = xpathVariableName;
                partName = null;
            }
            varDefinition = (RVariable) BPELHelper.getMatchingVariable(variableName, parentElement);
        }

        public boolean equals(Object obj) {
            if (obj == this) {
                return true;
            }
            if (obj instanceof XpathVariableInfo) {
                XpathVariableInfo other = (XpathVariableInfo) obj;
                if (variableName.equals(other.variableName) && xpathVariableName.equals(other.xpathVariableName)) {
                    return true;
                }
            }
            return false;
        }

        public int hashCode() {
            return variableName.hashCode();
        }

        public String toString() {
            return "variableName: " + variableName + " xpathVariableName: " + xpathVariableName + " partName: "
            + partName;
        }
    }

    public static class XpathVariablePropertyInfo {
        public Set<XpathVariableInfo> varInfoSet;
        public Map<String, MessagePropertyAlias> propAliasMap;

        public XpathVariablePropertyInfo(Set<XpathVariableInfo> set, Map<String, MessagePropertyAlias> map) {
            varInfoSet = set;
            propAliasMap = map;
        }
    }
}
